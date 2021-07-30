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
	/* 8 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	/* 9 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	/* A */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	/* B */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	/* C */ 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
	/* D */ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
	/* E */ 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	/* F */ 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
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

static int isword(const char *s)
{
	int c = (unsigned char) s[0];
	return isalnum(c) || c == '_' || c > 127;
}

static char *uc_beg(char *beg, char *s)
{
	while (s > beg && (((unsigned char) *s) & 0xc0) == 0x80)
		s--;
	return s;
}

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
	int splits;
	int insts[];
};

enum	/* rinst.opcode */
{
	// Instructions which consume input bytes (and thus fail if none left)
	CHAR = 1,
	ANY,
	CLASS,
	// Assert position
	ASSERT,
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

#define inst_is_consumer(inst) ((inst) < ASSERT)
typedef struct rsub rsub;
struct rsub
{
	int ref;
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

int re_classmatch(const int *pc, int c)
{
	// pc points to "classnot" byte after opcode
	int is_positive = *pc++;
	int cnt = *pc++;
	while (cnt--) {
		if (c >= *pc && c <= pc[1]) return is_positive;
		pc += 2;
	}
	return !is_positive;
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
		case CLASS:;
			pc += 2;
			int num = code[pc - 1];
			printf("class%s %d", (code[pc - 2] ? "" : "not"), num);
			while (num--) {
				printf(" 0x%02x-0x%02x", code[pc], code[pc + 1]);
				pc += 2;
			}
			printf("\n");
			break;
		case MATCH:
			printf("match\n");
			break;
		case SAVE:
			printf("save %d\n", code[pc++]);
			break;
		case ASSERT:
			if (code[pc] == BOL)
				printf("assert bol\n");
			else if (code[pc] == EOL)
				printf("assert eol\n");
			pc++;
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
			EMIT(PC++, CLASS);
			if (*re == '^') {
				EMIT(PC++, 0);
				re++;
			} else
				EMIT(PC++, 1);
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
			EMIT(term + 2, cnt);
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
			i = 0, icnt = 0, size, split;
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
				prog->splits++;
				EMIT(PC++, split);
				EMIT(PC++, REL(PC, PC+((size+2)*i)));
				if (code)
					memcpy(&code[PC], &code[term], size*sizeof(int));
				PC += size;
			}
			if (code) {
				for (i = 0; i < size; i++)
					switch (code[term+i]) {
					case CLASS:
						i += code[term+i+2] * 2 + 1;
					case JMP:
					case SPLIT:
					case RSPLIT:
					case SAVE:
					case CHAR:
						i++;
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
			prog->splits++;
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
			prog->splits++;
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
			prog->splits++;
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
			prog->splits++;
			prog->len += 2;
			term = PC;
			break;
		case '^':
			EMIT(PC++, ASSERT);
			EMIT(PC++, BOL);
			prog->len++;
			term = PC;
			break;
		case '$':
			EMIT(PC++, ASSERT);
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

int re_comp(rcode *prog, const char *re)
{
	prog->len = 0;
	prog->unilen = 0;
	prog->sub = 0;
	prog->splits = 0;

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

#define save(nn, csub) \
if (csub->ref > 1) { \
	for (j = 0; j < subidx; j++) { \
		if (!nsubs[j].ref) { \
			s1 = &nsubs[j]; \
			goto freedsub##nn; \
		} \
	} \
	s1 = &nsubs[subidx++]; \
	freedsub##nn: \
	for (j = 0; j < nsubp; j++) \
		s1->sub[j] = csub->sub[j]; \
	csub->ref--; \
	csub = s1; \
	csub->ref = 1; \
} \

#define addthread(nn, list, _pc, _sub, cont) \
{ \
	int i = 0, *pc = _pc; \
	const char *_sp = sp+l; \
	rsub *sub = _sub; \
	rec##nn: \
	if(plist[pc - prog->insts] == gen) { \
		sub->ref--; \
		rec_check##nn: \
		if (i) { \
			pc = pcs[--i]; \
			sub = subs[i]; \
			goto rec##nn; \
		} \
		cont; \
	} \
	plist[pc - prog->insts] = gen; \
	switch(*pc) { \
	default: \
		list->t[list->n].sub = sub; \
		list->t[list->n++].pc = pc; \
		goto rec_check##nn; \
	case JMP: \
		pc += 2 + pc[1]; \
		goto rec##nn; \
	case SPLIT: \
		subs[i] = sub; \
		sub->ref++; \
		pc += 2; \
		pcs[i++] = pc + pc[-1]; \
		goto rec##nn; \
	case RSPLIT: \
		subs[i] = sub; \
		sub->ref++; \
		pc += 2; \
		pcs[i++] = pc; \
		pc += pc[-1]; \
		goto rec##nn; \
	case SAVE: \
		save(nn, sub) \
		sub->sub[pc[1]] = _sp; \
		pc += 2; \
		goto rec##nn; \
	case ASSERT: \
		pc++; \
		if(*pc == BOL && _sp != s) \
			goto rec_check##nn; \
		if(*pc == EOL && *_sp) \
			goto rec_check##nn; \
		pc++; goto rec##nn; \
	} \
} \

#define swaplist() \
tmp = clist; \
clist = nlist; \
nlist = tmp; \
nlist->n = 0; \

int re_pikevm(rcode *prog, const char *s, const char **subp, int nsubp)
{
	int i, j, c, l = 0, *npc, gen = 1, subidx = 1;
	const char *sp = s;
	rsub nsubs[256];
	int plist[prog->unilen];
	int *pcs[prog->splits];
	rsub *subs[prog->splits];
	rsub *nsub = nsubs, *lsub = nsub, *matched = NULL, *s1;
	rthreadlist _clist[1+prog->len]; 
	rthreadlist _nlist[1+prog->len]; 
	rthreadlist *clist = _clist, *nlist = _nlist, *tmp;
	memset(plist, 0, prog->unilen*sizeof(plist[0]));
	memset(clist, 0, (1+prog->len)*sizeof(rthread));
	memset(nlist, 0, (1+prog->len)*sizeof(rthread));

	for(i=0; i<nsubp; i++) {
		subp[i] = NULL;
		nsub->sub[i] = NULL;
	}

	gen = 1;
	nsub->ref = 2;
	save(0, nsub);
	nsub->sub[0] = sp;
	goto jmp_start;
	for(; clist->n; sp += l) {
		gen++; uc_len(l, sp) uc_code(c, sp)
		for(i=0; i<clist->n; i++) {
			npc = clist->t[i].pc;
			nsub = clist->t[i].sub;
			// If we need to match a character, but there's none left,
			// it's fail (we don't schedule current thread for continuation)
			if (inst_is_consumer(*npc) && !*sp) {
				if (i >= clist->n-1)
					goto break_for;
				continue;
			}
			switch(*npc++) {
			case CHAR:
				if(c != *npc++)
					break;
			case ANY:
			addthread:
				addthread(2, nlist, npc, nsub, continue)
			case CLASS:
				if (!re_classmatch(npc, c))
					break;
				npc += *(npc+1) * 2 + 2;
				goto addthread;
			case MATCH:
				matched = nsub;
				subidx = 0;
				goto break_for;
			}
			nsub->ref--;
		}
		if (!matched) {
			nsub = lsub;
			nsub->ref++;
			save(3, nsub)
			nsub->sub[0] = sp + l;
			swaplist()
			jmp_start:
			while (1)
				addthread(1, clist, prog->insts, nsub, break)
			continue;
		}
	break_for:
		swaplist()
	}
	if(matched) {
		for(i=0; i<nsubp; i++)
			subp[i] = matched->sub[i];
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
	rcode *_code = (rcode*)code;
	if (re_comp(_code, argv[1]))
		re_fatal("Error in re_comp");
	re_dumpcode(_code);
	#include <time.h>
	if (argc > 2) {
		int sub_els = (_code->sub + 1) * 2;
		const char *sub[sub_els];
		for (int i = 2; i < argc; i++) {
			printf("input bytelen: %d\n", strlen(argv[i]));
			clock_t start_time = clock();
			if(!re_pikevm(_code, argv[i], sub, sub_els))
				{ printf("-nomatch-\n"); continue; }
			for(int k=sub_els; k>0; k--)
				if(sub[k-1])
					break;
			double elapsed_time = (double)(clock() - start_time) / CLOCKS_PER_SEC;
			printf("Done in %f seconds\n", elapsed_time);
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
