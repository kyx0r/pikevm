// Copyright 2007-2009 Russ Cox.  All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

#define nelem(x) (sizeof(x)/sizeof((x)[0]))

typedef struct rinst rinst;
struct rinst
{
	int opcode;
	int c;
	int n;
	rinst *x;
	rinst *y;
};

typedef struct rprog rprog;
struct rprog
{
	rinst *start;
	int len;
};

typedef struct rcode rcode;
struct rcode
{
	int unilen;
	int len;
	int sub;
	int insts[];
};

enum	/* rinst.opcode */
{
	// Instructions which consume input bytes (and thus fail if none left)
	CHAR = 1,
	ANY,
	CLASS,
	CLASSNOT,
	NAMEDCLASS,
	// Assert position
	BOL = 0x50,
	EOL,
	// Instructions which take relative offset as arg
	JMP = 0x60,
	SPLIT,
	RSPLIT,
	// Other (special) instructions
	SAVE = 0x7e,
	MATCH = 0x7f,
};

// Return codes for re_sizecode() and re_comp()
enum {
	RE_SUCCESS = 0,
	RE_SYNTAX_ERROR = -2,
	RE_UNSUPPORTED_ESCAPE = -3,
	RE_UNSUPPORTED_SYNTAX = -4,
};

#define inst_is_consumer(inst) ((inst) < BOL)
typedef struct rsub rsub;
struct rsub
{
	int ref;
	int nsub;
	const char *sub[128];
};

typedef struct rthread rthread;
struct rthread
{
	int *pc;
	rsub *sub;
};

typedef struct rthreadlist rthreadlist;
struct rthreadlist
{
	int n;
	rthread t[1];
};

#define INSERT_CODE(at, num, pc) \
if (code) \
	memmove(code + at + num, code + at, (pc - at)*sizeof(int)); \
pc += num; 
#define REL(at, to) (to - at - 2)
#define EMIT(at, byte) (code ? (code[at] = byte) : at)
#define PC (prog->unilen)

void re_fatal(char *msg)
{
	fprintf(stderr, "fatal error: %s\n", msg);
	exit(2);
}

static rsub *freesub;
static rsub subs[10];
static int subidx;

rsub* newsub(int n)
{
	rsub *s = freesub;
	if(s != NULL)
		freesub = (rsub*)s->sub[0];
	else
		s = &subs[subidx++];
	s->nsub = n;
	s->ref = 1;
	return s;
}

rsub* incref(rsub *s)
{
	s->ref++;
	return s;
}

rsub* update(rsub *s, int i, const char *p)
{
	rsub *s1;
	int j;

	if(s->ref > 1) {
		s1 = newsub(s->nsub);
		for(j=0; j<s->nsub; j++)
			s1->sub[j] = s->sub[j];
		s->ref--;
		s = s1;
	}
	s->sub[i] = p;
	return s;
}

void decref(rsub *s)
{
	if(--s->ref == 0) {
		s->sub[0] = (char*)freesub;
		freesub = s;
	}
}

int re_classmatch(const int *pc, const char *sp)
{
	// pc points to "cnt" byte after opcode
	int is_positive = (pc[-1] == CLASS);
	int cnt = *pc++;
	while (cnt--) {
		if (*sp >= *pc && *sp <= pc[1]) return is_positive;
		pc += 2;
	}
	return !is_positive;
}

int re_namedclassmatch(const int *pc, const char *sp)
{
	// pc points to name of class
	int off = (*pc >> 5) & 1;
	if ((*pc | 0x20) == 'd') {
		if (!(*sp >= '0' && *sp <= '9'))
			off ^= 1;
	} else if ((*pc | 0x20) == 's') {
		if (!(*sp == ' ' || (*sp >= '\t' && *sp <= '\r')))
			off ^= 1;
	} else { // w
		if (!((*sp >= 'A' && *sp <= 'Z') || (*sp >= 'a' && *sp <= 'z') ||
			(*sp >= '0' && *sp <= '9') || *sp == '_'))
			off ^= 1;
	}
	return off;
}

void re_dumpcode(rcode *prog)
{
	int pc = 0;
	int *code = prog->insts;
	while (pc < prog->unilen) {
		printf("%4d: ", pc);
		switch(code[pc++]) {
		default:
			pc = prog->unilen;
			break;
		case SPLIT:
			printf("split %d (%d)\n", pc + code[pc] + 1, code[pc]);
			pc++;
			break;
		case RSPLIT:
			printf("rsplit %d (%d)\n", pc + code[pc] + 1, code[pc]);
			pc++;
			break;
		case JMP:
			printf("jmp %d (%d)\n", pc + code[pc] + 1, code[pc]);
			pc++;
			break;
		case CHAR:
			printf("char %c\n", code[pc]);
			pc++;
			break;
		case ANY:
			printf("any\n");
			break;
		case CLASS:
		case CLASSNOT: {
			int num = code[pc];
			printf("class%s %d", (code[pc - 1] == CLASSNOT ? "not" : ""), num);
			pc++;
			while (num--) {
				printf(" 0x%02x-0x%02x", code[pc], code[pc + 1]);
				pc += 2;
			}
			printf("\n");
			break;
		}
		case NAMEDCLASS:
			printf("namedclass %c\n", code[pc++]);
			break;
		case MATCH:
			printf("match\n");
			break;
		case SAVE:
			printf("save %d\n", code[pc++]);
			break;
		case BOL:
			printf("assert bol\n");
			break;
		case EOL:
			printf("assert eol\n");
			break;
		}
	}
	printf("Unilen: %d, insts: %d\n", prog->unilen, prog->len);
}

static int _compilecode(const char **re_loc, rcode *prog, int sizecode)
{
	const char *re = *re_loc;
	int *code = sizecode ? NULL : prog->insts;
	int start = PC;
	int term = PC;
	int alt_label = 0;

	for (; *re && *re != ')'; re++) {
		switch (*re) {
		case '\\':;
			re++;
			if (!*re) goto syntax_error; // Trailing backslash
			char c = *re | 0x20;
			if (c == 'd' || c == 's' || c == 'w') {
				term = PC;
				EMIT(PC++, NAMEDCLASS);
				EMIT(PC++, *re);
				prog->len++;
				break;
			}
			if ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z'))
				goto unsupported_escape;
		default:
			term = PC;
			EMIT(PC++, CHAR);
			EMIT(PC++, *re);
			prog->len++;
			break;
		case '.':
			term = PC;
			EMIT(PC++, ANY);
			prog->len++;
			break;
		case '[':;
			int cnt;
			term = PC;
			re++;
			if (*re == '^') {
				EMIT(PC++, CLASSNOT);
				re++;
			} else {
				EMIT(PC++, CLASS);
			}
			PC++; // Skip "# of pairs" byte
			prog->len++;
			for (cnt = 0; *re != ']'; re++, cnt++) {
				if (!*re) goto syntax_error;
				if (*re == '\\') {
					re++;
					if (!*re) goto syntax_error;
					if (*re != '\\' && *re != ']')
						goto unsupported_escape;
				}
				EMIT(PC++, *re);
				if (re[1] == '-' && re[2] != ']')
					re += 2;
				EMIT(PC++, *re);
			}
			EMIT(term + 1, cnt);
			break;
		case '(':;
			term = PC;
			int sub;
			int capture = 1;
			re++;
			if (*re == '?') {
				re++;
				if (*re == ':') {
					capture = 0;
					re++;
				} else {
					*re_loc = re;
					return RE_UNSUPPORTED_SYNTAX;
				}
			}
	
			if (capture) {
				sub = ++prog->sub;
				EMIT(PC++, SAVE);
				EMIT(PC++, 2 * sub);
				prog->len++;
			}
			int res = _compilecode(&re, prog, sizecode);
			*re_loc = re;
			if (res < 0) return res;
			if (*re != ')') return RE_SYNTAX_ERROR;
	
			if (capture) {
				EMIT(PC++, SAVE);
				EMIT(PC++, 2 * sub + 1);
				prog->len++;
			}
			break;
		case '{':;
			int maxcnt = 0, mincnt = 0,
			i = 0, icnt = 1, size, split;
			re++;
			while (isdigit((unsigned char) *re))
				mincnt = mincnt * 10 + *re++ - '0';
			if (*re == ',') {
				re++;
				if (*re == '}')
					maxcnt = 256;
				while (isdigit((unsigned char) *re))
					maxcnt = maxcnt * 10 + *re++ - '0';
			} else
				maxcnt = mincnt;
			for (size = PC - term; i < mincnt-1; i++) {
				if (code)
					memcpy(&code[PC], &code[term], size*sizeof(int));
				PC += size;
			}
			split = *(re+1) == '[' ? RSPLIT : SPLIT;
			for (i = maxcnt-mincnt; i > 0; i--)
			{
				EMIT(PC++, split);
				EMIT(PC++, REL(PC, PC+((size+2)*i)));
				if (code)
					memcpy(&code[PC], &code[term], size*sizeof(int));
				PC += size;
			}
			if (code) {
				for (i = 0; i < size; i++)
					switch (code[term]) {
					case CLASS:
					case CLASSNOT:
					case NAMEDCLASS:
					case JMP:
					case SPLIT:
					case RSPLIT:
					case SAVE:
					case CHAR:
						icnt++;	
					}
			}
			prog->len += maxcnt * icnt;
			break;
		case '?':
			if (PC == term) goto syntax_error; // nothing to repeat
			INSERT_CODE(term, 2, PC);
			if (re[1] == '?') {
				EMIT(term, RSPLIT);
				re++;
			} else {
				EMIT(term, SPLIT);
			}
			EMIT(term + 1, REL(term, PC));
			prog->len++;
			term = PC;
			break;
		case '*':
			if (PC == term) goto syntax_error; // nothing to repeat
			INSERT_CODE(term, 2, PC);
			EMIT(PC, JMP);
			EMIT(PC + 1, REL(PC, term));
			PC += 2;
			if (re[1] == '?') {
				EMIT(term, RSPLIT);
				re++;
			} else {
				EMIT(term, SPLIT);
			}
			EMIT(term + 1, REL(term, PC));
			prog->len += 2;
			term = PC;
			break;
		case '+':
			if (PC == term) goto syntax_error; // nothing to repeat
			if (re[1] == '?') {
				EMIT(PC, SPLIT);
				re++;
			} else {
				EMIT(PC, RSPLIT);
			}
			EMIT(PC + 1, REL(PC, term));
			PC += 2;
			prog->len++;
			term = PC;
			break;
		case '|':
			if (alt_label) {
				EMIT(alt_label, REL(alt_label, PC) + 1);
			}
			INSERT_CODE(start, 2, PC);
			EMIT(PC++, JMP);
			alt_label = PC++;
			EMIT(start, SPLIT);
			EMIT(start + 1, REL(start, PC));
			prog->len += 2;
			term = PC;
			break;
		case '^':
			EMIT(PC++, BOL);
			prog->len++;
			term = PC;
			break;
		case '$':
			EMIT(PC++, EOL);
			prog->len++;
			term = PC;
			break;
		}
	}
	if (alt_label) {
		EMIT(alt_label, REL(alt_label, PC) + 1);
	}
	*re_loc = re;
	return RE_SUCCESS;
syntax_error:
	*re_loc = re;
	return RE_SYNTAX_ERROR;
unsupported_escape:
	*re_loc = re;
	return RE_UNSUPPORTED_ESCAPE;
}

int re_sizecode(const char *re)
{
	rcode dummyprog;
	// SAVE 0, SAVE 1, MATCH; more bytes for "search" (vs "match") prefix code
	dummyprog.unilen = 10;

	int res = _compilecode(&re, &dummyprog, /*sizecode*/1);
	if (res < 0) return res;
	// If unparsed chars left
	if (*re) return RE_SYNTAX_ERROR;

	return dummyprog.unilen;
}

int re_comp(rcode *prog, const char *re, int anchored)
{
	prog->len = 0;
	prog->unilen = 0;
	prog->sub = 0;

	// Add code to implement non-anchored operation ("search").
	// For anchored operation ("match"), this code will be just skipped.
	// TODO: Implement search in much more efficient manner
	if (!anchored) {
		prog->insts[prog->unilen++] = RSPLIT;
		prog->insts[prog->unilen++] = 3;
		prog->insts[prog->unilen++] = ANY;
		prog->insts[prog->unilen++] = JMP;
		prog->insts[prog->unilen++] = -5;
	
		prog->insts[prog->unilen++] = SAVE;
		prog->insts[prog->unilen++] = 0;
		prog->len += 4;
	}
	int res = _compilecode(&re, prog, /*sizecode*/0);
	if (res < 0) return res;
	// If unparsed chars left
	if (*re) return RE_SYNTAX_ERROR;

	prog->insts[prog->unilen++] = SAVE;
	prog->insts[prog->unilen++] = 1;

	prog->insts[prog->unilen++] = MATCH;
	prog->len += 2;

	return RE_SUCCESS;
}

void cleanmarks(rcode *prog)
{
	int *pc = prog->insts;
	int *end = pc + prog->unilen;
	while (pc < end) {
		*pc &= 0x7f;
		switch (*pc) {
		case CLASS:
		case CLASSNOT:
			pc += pc[1] * 2;
		case NAMEDCLASS:
		case JMP:
		case SPLIT:
		case RSPLIT:
		case SAVE:
		case CHAR:
			pc++;
			break;
		}
		pc++;
	}
}

static rthread thread(int *pc, rsub *sub)
{
	rthread t = {pc, sub};
	return t;
}

static void addthread(rthreadlist *l, rthread t, const char *beg, const char *sp)
{
	int off;
	if(*t.pc & 0x80) {
		decref(t.sub);
		return;	// already on list
	}
	*t.pc |= 0x80;

	switch(*t.pc & 0x7f) {
	default:
		l->t[l->n++] = t;
		break;
	case JMP:
		off = t.pc[1];
		t.pc += 2;
		addthread(l, thread(t.pc + off, t.sub), beg, sp);
		break;
	case SPLIT:
		off = t.pc[1];
		t.pc += 2;
		addthread(l, thread(t.pc, incref(t.sub)), beg, sp);
		addthread(l, thread(t.pc + off, t.sub), beg, sp);
		break;
	case RSPLIT:
		off = t.pc[1];
		t.pc += 2;
		addthread(l, thread(t.pc + off, incref(t.sub)), beg, sp);
		addthread(l, thread(t.pc, t.sub), beg, sp);
		break;
	case SAVE:
		off = t.pc[1];
		t.pc += 2;
		addthread(l, thread(t.pc, update(t.sub, off, sp)), beg, sp);
		break;
	case BOL:
		if(sp == beg)
			addthread(l, thread(t.pc + 1, t.sub), beg, sp);
		break;
	case EOL:
		if(!*sp)
			addthread(l, thread(t.pc + 1, t.sub), beg, sp);
		break;
	}
}

int re_pikevm(rcode *prog, const char *s, const char **subp, int nsubp)
{
	int i, *pc;
	const char *sp;
	rsub *sub, *matched = NULL;
	rthreadlist _clist[1+prog->len]; 
	rthreadlist _nlist[1+prog->len]; 
	rthreadlist *clist = _clist, *nlist = _nlist, *tmp;
	memset(clist, 0, (1+prog->len)*sizeof(rthread));
	memset(nlist, 0, (1+prog->len)*sizeof(rthread));

	subidx = 0;
	freesub = NULL;
	for(i=0; i<nsubp; i++)
		subp[i] = NULL;
	sub = newsub(nsubp);
	for(i=0; i<nsubp; i++)
		sub->sub[i] = NULL;

	cleanmarks(prog);
	addthread(clist, thread(prog->insts, sub), s, s);
	for(sp=s;; sp++) {
		if(clist->n == 0)
			break;
		cleanmarks(prog);
		for(i=0; i<clist->n; i++) {
			pc = clist->t[i].pc;
			sub = clist->t[i].sub;
			if (inst_is_consumer(*pc & 0x7f) && !*sp) {
				// If we need to match a character, but there's none left,
				// it's fail (we don't schedule current thread for continuation)
				decref(sub);
				continue;
			}
			switch(*pc++ & 0x7f) {
			case CHAR:
				if(*sp != *pc++) {
					decref(sub);
					break;
				}
			case ANY:
			addthread:
				addthread(nlist, thread(pc, sub), s, sp+1);
				break;
			case CLASS:
			case CLASSNOT:
				if (!re_classmatch(pc, sp)) {
					decref(sub);
					break;
				}
				pc += *pc * 2 + 1;
				goto addthread;
			case NAMEDCLASS:
				if (!re_namedclassmatch(pc, sp)) {
					decref(sub);
					break;
				}
				pc++;
				goto addthread;
			case MATCH:
				if(matched)
					decref(matched);
				matched = sub;
				for(i++; i < clist->n; i++)
					decref(clist->t[i].sub);
				goto BreakFor;
			}
		}
	BreakFor:
		tmp = clist;
		clist = nlist;
		nlist = tmp;
		nlist->n = 0;
	}
	if(matched) {
		for(i=0; i<nsubp; i++)
			subp[i] = matched->sub[i];
		decref(matched);
		return 1;
	}
	return 0;
}

int main(int argc, char *argv[])
{
	int sz = re_sizecode(argv[1]) * sizeof(int);
	printf("Precalculated size: %d\n", sz);
	char code[sizeof(rcode)+sz];
	rcode *_code = (rcode*)&code;
	if (re_comp(_code, argv[1], 0))
		re_fatal("Error in re_comp");
	re_dumpcode(_code);
	if (argc > 2) {
		int sub_els = (_code->sub + 1) * 2;
		const char *sub[sub_els];
		for (int i = 2; i < argc; i++) {
			printf("sub depth %d\n", subidx);
			if(!re_pikevm(_code, argv[i], sub, sub_els))
				{ printf("-no match-\n"); continue; }
			for(int k=sub_els; k>0; k--)
				if(sub[k-1])
					break;
			for(int l=0; l<sub_els; l+=2) {
				printf(" (");
				if(sub[l] == NULL)
					printf("?");
				else
					printf("%d", (int)(sub[l] - argv[i]));
				printf(",");
				if(sub[l+1] == NULL)
					printf("?");
				else
					printf("%d", (int)(sub[l+1] - argv[i]));
				printf(")");
			}
			printf("\n");
		}
		
	}
	return 0;
}
