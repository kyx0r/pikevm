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

typedef struct rcode rcode;
struct rcode
{
	int unilen;
	int len;
	int sub;
	int splits;
	int insts[];
};

enum
{
	// Instructions which consume input bytes (and thus fail if none left)
	CHAR = 1,
	ANY,
	CLASS,
	MATCH,
	// Assert position
	ASSERT,
	BOL,
	EOL,
	WBEG,
	WEND,
	// Instructions which take relative offset as arg
	JMP,
	SPLIT,
	RSPLIT,
	// Other (special) instructions
	SAVE,
};

// Return codes for re_sizecode() and re_comp()
enum {
	RE_SUCCESS = 0,
	RE_SYNTAX_ERROR = -2,
	RE_UNSUPPORTED_ESCAPE = -3,
	RE_UNSUPPORTED_SYNTAX = -4,
};

typedef struct rsub rsub;
struct rsub
{
	int ref;
	const char *sub[];
};

typedef struct rthread rthread;
struct rthread
{
	const int *pc;
	rsub *sub;
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
			else if (code[pc] == WBEG)
				printf("assert wbeg\n");
			else if (code[pc] == WEND)
				printf("assert wend\n");
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
			if (*re == '<' || *re == '>') {
				EMIT(PC++, ASSERT);
				EMIT(PC++, *re == '<' ? WBEG : WEND);
				prog->len++;
				term = PC;
				break;
			}
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
	dummyprog.unilen = 3;

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

#define newsub(n, csub) \
s1 = freesub; \
if (s1) \
	freesub = (rsub*)s1->sub[0]; \
else \
	s1 = (rsub*)&nsubs[rsubsize * subidx++]; \
for (j = n; j < nsubp; j++) \
	s1->sub[j] = csub->sub[j]; \
csub = s1; \
csub->ref = 1; \

#define decref(csub) \
if (--csub->ref == 0) { \
	csub->sub[0] = (char*)freesub; \
	freesub = csub; \
} \

#define addthread(nn, list, listidx, _pc, _sub) \
{ \
	int i = 0; \
	const int *pc = _pc; \
	const char *_sp = sp+l; \
	rsub *sub = _sub; \
	rec##nn: \
	if(plist[pc - insts] == gen) { \
		dec_check##nn: \
		decref(sub) \
		rec_check##nn: \
		if (i) { \
			pc = pcs[--i]; \
			sub = subs[i]; \
			goto rec##nn; \
		} \
		continue; \
	} \
	plist[pc - insts] = gen; \
	if (*pc < ASSERT) { \
		list[listidx].sub = sub; \
		list[listidx++].pc = pc; \
		goto rec_check##nn; \
	} \
	switch(*pc) { \
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
		if (sub->ref > 1) { \
			sub->ref--; \
			newsub(0, sub) \
		} \
		sub->sub[pc[1]] = _sp; \
		pc += 2; \
		goto rec##nn; \
	case ASSERT: \
		pc++; \
		if (*pc == BOL && _sp != s) { \
			if (!i && !listidx) \
				return 0; \
			goto dec_check##nn; \
		} \
		if (*pc == EOL && *_sp) \
			goto dec_check##nn; \
		if (*pc == WBEG && (!isword(_sp) || isword(sp)) \
				&& !(sp == s && isword(sp))) \
			goto dec_check##nn; \
		if (*pc == WEND && isword(_sp)) \
			goto dec_check##nn; \
		pc++; goto rec##nn; \
	} \
} \

int re_pikevm(rcode *prog, const char *s, const char **subp, int nsubp)
{
	int i, j, c, l = 0, gen = 1, subidx = 1;
	int rsubsize = sizeof(rsub)+(sizeof(char*)*nsubp);
	int clistidx = 0, nlistidx = 0;
	const char *sp = s;
	const int *insts = prog->insts;
	int plist[prog->unilen];
	const int *pcs[prog->splits], *npc;
	rsub *subs[prog->splits];
	char nsubs[rsubsize*256];
	rsub *nsub = (rsub*)nsubs, *lsub = nsub, *matched = NULL, *s1;
	rsub *freesub = NULL;
	rthread _clist[prog->len]; 
	rthread _nlist[prog->len]; 
	rthread *clist = _clist, *nlist = _nlist, *tmp;
	memset(plist, 0, prog->unilen*sizeof(plist[0]));

	for(i = 0; i < nsubp; i++) {
		subp[i] = NULL;
		nsub->sub[i] = NULL;
	}

	gen = 1;
	goto jmp_start;
	for(;; sp += l) {
		gen++; uc_len(l, sp) uc_code(c, sp)
		for(i = 0; i < clistidx; i++) {
			npc = clist[i].pc;
			nsub = clist[i].sub;
			switch(*npc++) {
			case CHAR:
				if(c != *npc++)
					break;
			case ANY:
			addthread:
				addthread(2, nlist, nlistidx, npc, nsub)
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
			decref(nsub)
		}
		break_for:
		if (!c)
			break;
		tmp = clist;
		clist = nlist;
		nlist = tmp;
		clistidx = nlistidx;
		nlistidx = 0;
		if (!matched) {
			nsub = lsub;
			jmp_start:
			newsub(1, nsub)
			nsub->sub[0] = sp + l;
			addthread(1, clist, clistidx, insts, nsub)
		} else if (!clistidx)
			break;
	}
	if(matched) {
		for(i = 0; i < nsubp; i++)
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
			printf("input bytelen: %ld\n", strlen(argv[i]));
			clock_t start_time = clock();
			sz = re_pikevm(_code, argv[i], sub, sub_els);
			double elapsed_time = (double)(clock() - start_time) / CLOCKS_PER_SEC;
			printf("Done in %f seconds\n", elapsed_time);
			if (!sz)
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
