// Copyright 2007-2009 Russ Cox.  All Rights Reserved.
// Use of this source code is governed by a BSD-style

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

#define nelem(x) (sizeof(x)/sizeof((x)[0]))

const unsigned char utf8_length[256] = {
	/* 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F */
	/* 0 */ 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	/* 1 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	/* 2 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	/* 3 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	/* 4 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	/* 5 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	/* 6 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	/* 7 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	/* 8 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	/* 9 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	/* A */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	/* B */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	/* C */ 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
	/* D */ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
	/* E */ 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	/* F */ 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

/* return the length of a utf-8 character */
#define uc_len(dst, s) \
dst = utf8_length[(unsigned char)s[0]]; \

/* the unicode codepoint of the given utf-8 character */
#define uc_code(dst, s) \
dst = (unsigned char) s[0]; \
if (~dst & 0xc0); \
else if (~dst & 0x20) \
	dst = ((dst & 0x1f) << 6) | (s[1] & 0x3f); \
else if (~dst & 0x10) \
	dst = ((dst & 0x0f) << 12) | ((s[1] & 0x3f) << 6) | (s[2] & 0x3f); \
else if (~dst & 0x08) \
	dst = ((dst & 0x07) << 18) | ((s[1] & 0x3f) << 12) | \
		((s[2] & 0x3f) << 6) | (s[3] & 0x3f); \
else \
	dst = 0; \

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
	BOL,
	EOL,
	// Instructions which take relative offset as arg
	JMP,
	SPLIT,
	RSPLIT,
	// Other (special) instructions
	SAVE,
	MATCH,
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
	int cnt = *pc++, c;
	uc_code(c, sp)
	while (cnt--) {
		if (c >= *pc && c <= pc[1]) return is_positive;
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
		case CLASSNOT:;
			int num = code[pc];
			printf("class%s %d", (code[pc - 1] == CLASSNOT ? "not" : ""), num);
			pc++;
			while (num--) {
				printf(" 0x%02x-0x%02x", code[pc], code[pc + 1]);
				pc += 2;
			}
			printf("\n");
			break;
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
	int start = PC, term = PC;
	int alt_label = 0, c;

	for (; *re && *re != ')';) {
		switch (*re) {
		case '\\':
			re++;
			if (!*re) goto syntax_error; // Trailing backslash
			c = *re | 0x20;
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
			uc_code(c, re) EMIT(PC++, c);
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
			for (cnt = 0; *re != ']'; cnt++) {
				if (!*re) goto syntax_error;
				if (*re == '\\') {
					re++;
					if (!*re) goto syntax_error;
					if (*re != '\\' && *re != ']')
						goto unsupported_escape;
				}
				uc_code(c, re) EMIT(PC++, c);
				uc_len(c, re)
				if (re[c] == '-' && re[c+1] != ']')
					re += c+1;
				uc_code(c, re) EMIT(PC++, c);
				uc_len(c, re) re += c;
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
		uc_len(c, re) re += c;
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

static void addthread(const int *pbeg, int *plist, int gen, rthreadlist *l,
			 int *pc, rsub *sub, const char *beg, const char *sp)
{
	int i = 0, *pcs[10];
	rsub *subs[10];
	rec:
	if(plist[pc - pbeg] == gen) {
		decref(sub);
		rec_check:
		if (i) {
			pc = pcs[--i];
			sub = subs[i];
			goto rec;
		}
		return;	// already on list
	}
	plist[pc - pbeg] = gen;

	switch(*pc) {
	default:
		l->t[l->n].sub = sub;
		l->t[l->n++].pc = pc;
		goto rec_check;
	case JMP:
		pc += 2 + pc[1];
		goto rec;
	case SPLIT:
		subs[i] = sub;
		sub->ref++;
		pc += 2;
		pcs[i++] = pc + pc[-1];
		goto rec;
	case RSPLIT:
		subs[i] = sub;
		sub->ref++;
		pc += 2;
		pcs[i++] = pc;
		pc += pc[-1];
		goto rec;
	case SAVE:
		sub = update(sub, pc[1], sp);
		pc += 2;
		goto rec;
	case BOL:
		if(sp != beg)
			goto rec_check;
		pc++; goto rec;
	case EOL:
		if(*sp)
			goto rec_check;
		pc++; goto rec;
	}
}

int re_pikevm(rcode *prog, const char *s, const char **subp, int nsubp)
{
	int i, c, l, gen, *pc;
	const char *sp;
	int plist[prog->unilen];
	rsub *sub, *matched = NULL;
	rthreadlist _clist[1+prog->len]; 
	rthreadlist _nlist[1+prog->len]; 
	rthreadlist *clist = _clist, *nlist = _nlist, *tmp;
	memset(plist, 0, prog->unilen*sizeof(plist[0]));
	memset(clist, 0, (1+prog->len)*sizeof(rthread));
	memset(nlist, 0, (1+prog->len)*sizeof(rthread));

	subidx = 0;
	freesub = NULL;
	for(i=0; i<nsubp; i++)
		subp[i] = NULL;
	sub = newsub(nsubp);
	for(i=0; i<nsubp; i++)
		sub->sub[i] = NULL;

	gen = 1;
	addthread(prog->insts, plist, gen, clist, prog->insts, sub, s, s);
	for(sp=s;; sp += l) {
		if(clist->n == 0)
			break;
		gen++; uc_len(l, s)
		for(i=0; i<clist->n; i++) {
			pc = clist->t[i].pc;
			sub = clist->t[i].sub;
			if (inst_is_consumer(*pc) && !*sp) {
				// If we need to match a character, but there's none left,
				// it's fail (we don't schedule current thread for continuation)
				decref(sub);
				continue;
			}
			switch(*pc++) {
			case CHAR:
				uc_code(c, sp)
				if(c != *pc++) {
					decref(sub);
					break;
				}
			case ANY:
			addthread:
				addthread(prog->insts, plist, gen, nlist, pc, sub, s, sp+l);
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
	if (argc < 2) {
		printf("usage: <regex> <str...> <str...> ...\n");
		return 0;
	}
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
			printf("input bytelen: %d\n", strlen(argv[i]));
			if(!re_pikevm(_code, argv[i], sub, sub_els))
				{ printf("-nomatch-\n"); continue; }
			for(int k=sub_els; k>0; k--)
				if(sub[k-1])
					break;
			for(int l=0; l<sub_els; l+=2) {
				printf("(");
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
