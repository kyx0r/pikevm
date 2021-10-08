// Copyright 2007-2009 Russ Cox.  All Rights Reserved.
// Use of this source code is governed by a BSD-style

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

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
	int presub;
	int splits;
	int gen;
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
	WBEG,
	WEND,
	BOL,
	EOL,
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
	RE_UNSUPPORTED_SYNTAX = -3,
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
	int *pc;
	rsub *sub;
};

#define INSERT_CODE(at, num, pc) \
if (code) \
	memmove(code + at + num, code + at, (pc - at)*sizeof(int)); \
pc += num;
#define REL(at, to) (to - at - 2)
#define EMIT(at, byte) (code ? (code[at] = byte) : at)
#define PC (prog->unilen)

static int re_classmatch(const int *pc, int c)
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
	int pc = 0, i = 0;
	int *code = prog->insts;
	while (pc < prog->unilen) {
		printf("%4d: ", pc); i++;
		switch(code[pc++]) {
		default:
			pc = prog->unilen;
			break;
		case SPLIT:
			printf("split %d (%d)\n", pc + code[pc] + 2, code[pc]);
			pc+=2;
			break;
		case RSPLIT:
			printf("rsplit %d (%d)\n", pc + code[pc] + 2, code[pc]);
			pc+=2;
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
		case WBEG:
			printf("assert wbeg\n");
			break;
		case WEND:
			printf("assert wend\n");
			break;
		case BOL:
			printf("assert bol\n");
			break;
		case EOL:
			printf("assert eol\n");
			break;
		}
	}
	printf("Unilen: %d, insts: %d, counted insts: %d\n", prog->unilen, prog->len, i);
}

/* next todo: crack and factor out this recursion,
no recursion will allow to make a meta macro out
of this, such that re_sizecode() becomes efficient
difficulty: very high, probably not any time soon */
static int _compilecode(const char **re_loc, rcode *prog, int sizecode)
{
	const char *re = *re_loc;
	int *code = sizecode ? NULL : prog->insts;
	int start = PC, term = PC;
	int alt_label = 0, c;
	int alt_stack[5000], altc = 0;

	for (; *re && *re != ')';) {
		switch (*re) {
		case '\\':
			re++;
			if (!*re) goto syntax_error; // Trailing backslash
			if (*re == '<' || *re == '>') {
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
				if (*re == '\\') re++;
				if (!*re) goto syntax_error;
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
				EMIT(PC++, sub);
				prog->len++;
			}
			int res = _compilecode(&re, prog, sizecode);
			*re_loc = re;
			if (res < 0) return res;
			if (*re != ')') return RE_SYNTAX_ERROR;
			if (capture) {
				EMIT(PC++, SAVE);
				EMIT(PC++, sub + prog->presub + 1);
				prog->len++;
			}
			break;
		case '{':;
			int maxcnt = 0, mincnt = 0,
			i = 0, icnt = 0, inf = 0, size;
			re++;
			while (isdigit((unsigned char) *re))
				mincnt = mincnt * 10 + *re++ - '0';
			if (*re == ',') {
				re++;
				if (*re == '}')
					inf = 1;
				while (isdigit((unsigned char) *re))
					maxcnt = maxcnt * 10 + *re++ - '0';
			} else
				maxcnt = mincnt;
			for (size = PC - term; i < mincnt-1; i++) {
				if (code)
					memcpy(&code[PC], &code[term], size*sizeof(int));
				PC += size;
			}
			if (inf) {
				EMIT(PC, RSPLIT);
				EMIT(PC+1, REL(PC, PC - size -1));
				EMIT(PC+2, 0);
				PC += 3;
				prog->len++;
				prog->splits++;
				maxcnt = mincnt;
			}
			for (i = maxcnt-mincnt; i > 0; i--) {
				EMIT(PC++, SPLIT);
				EMIT(PC++, REL(PC-1, PC+((size+3)*i)));
				EMIT(PC++, 0);
				prog->splits++;
				prog->len++;
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
					case ANY:
						icnt++;
					}
				prog->len += (maxcnt-1) * icnt;
			}
			break;
		case '?':
			if (PC == term) goto syntax_error;
			INSERT_CODE(term, 3, PC);
			if (re[1] == '?') {
				EMIT(term, RSPLIT);
				re++;
			} else
				EMIT(term, SPLIT);
			EMIT(term + 1, REL(term, PC-1));
			EMIT(term + 2, 0);
			prog->len++;
			prog->splits++;
			term = PC;
			break;
		case '*':
			if (PC == term) goto syntax_error;
			INSERT_CODE(term, 3, PC);
			EMIT(PC, JMP);
			EMIT(PC + 1, REL(PC, term));
			PC += 2;
			if (re[1] == '?') {
				EMIT(term, RSPLIT);
				re++;
			} else
				EMIT(term, SPLIT);
			EMIT(term + 1, REL(term, PC-1));
			EMIT(term + 2, 0);
			prog->splits++;
			prog->len += 2;
			term = PC;
			break;
		case '+':
			if (PC == term) goto syntax_error;
			if (re[1] == '?') {
				EMIT(PC, SPLIT);
				re++;
			} else
				EMIT(PC, RSPLIT);
			EMIT(PC + 1, REL(PC-1, term));
			EMIT(PC + 2, 0);
			PC += 3;
			prog->splits++;
			prog->len++;
			term = PC;
			break;
		case '|':
			if (alt_label)
				alt_stack[altc++] = alt_label;
			INSERT_CODE(start, 3, PC);
			EMIT(PC++, JMP);
			alt_label = PC++;
			EMIT(start, SPLIT);
			EMIT(start + 1, REL(start, PC-1));
			EMIT(start + 2, 0);
			prog->splits++;
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
	if (code && alt_label) {
		EMIT(alt_label, REL(alt_label, PC) + 1);
		for (int alts = altc; altc; altc--) {
			int at = alt_stack[alts-altc]+altc*3;
			EMIT(at, REL(at, PC) + 1);
		}
	}
	*re_loc = re;
	return RE_SUCCESS;
syntax_error:
	*re_loc = re;
	return RE_SYNTAX_ERROR;
}

int re_sizecode(const char *re, int *nsub)
{
	rcode dummyprog;
	dummyprog.unilen = 3;
	dummyprog.sub = 0;

	int res = _compilecode(&re, &dummyprog, /*sizecode*/1);
	if (res < 0) return res;
	// If unparsed chars left
	if (*re) return RE_SYNTAX_ERROR;
	*nsub = dummyprog.sub;
	return dummyprog.unilen;
}

int re_comp(rcode *prog, const char *re, int nsubs)
{
	prog->len = 0;
	prog->unilen = 0;
	prog->sub = 0;
	prog->presub = nsubs;
	prog->splits = 0;
	prog->gen = 1;

	int res = _compilecode(&re, prog, /*sizecode*/0);
	if (res < 0) return res;
	// If unparsed chars left
	if (*re) return RE_SYNTAX_ERROR;

	prog->insts[prog->unilen++] = SAVE;
	prog->insts[prog->unilen++] = prog->sub + 1;
	prog->insts[prog->unilen++] = MATCH;
	prog->len += 2;

	return RE_SUCCESS;
}

#define _return(state) \
{ prog->gen = gen + 1; return state; } \

#define newsub(init, copy) \
if (freesub) \
	{ s1 = freesub; freesub = (rsub*)s1->sub[0]; copy } \
else \
	{ s1 = (rsub*)&nsubs[suboff+=rsubsize]; init } \

#define decref(csub) \
if (--csub->ref == 0) { \
	csub->sub[0] = (char*)freesub; \
	freesub = csub; \
} \

#define deccheck(nn) \
{ decref(nsub) goto rec_check##nn; } \

#define fastrec(nn, list, listidx) \
nsub->ref++; \
if (*npc < WBEG) { \
	list[listidx].sub = nsub; \
	list[listidx++].pc = npc; \
	npc = pcs[i]; \
	goto rec##nn; \
} \
subs[i++] = nsub; \
goto next##nn; \

#define saveclist() \
newsub(memcpy(s1->sub, nsub->sub, osubp);, \
memcpy(s1->sub, nsub->sub, osubp / 2);) \

#define savenlist() \
newsub(/*nop*/, /*nop*/) \
memcpy(s1->sub, nsub->sub, osubp); \

#define onnlist(nn) \
if (npc[2] == gen) \
	deccheck(nn) \
npc[2] = gen; \

#define onclist(nn) /* nop */ \

#define addthread(nn, list, listidx) \
{ \
	int i = 0; \
	rec##nn: \
	if (*npc < WBEG) { \
		list[listidx].sub = nsub; \
		list[listidx++].pc = npc; \
		rec_check##nn: \
		if (i) { \
			npc = pcs[--i]; \
			nsub = subs[i]; \
			goto rec##nn; \
		} \
		continue; \
	} \
	next##nn: \
	switch(*npc) { \
	case JMP: \
		npc += 2 + npc[1]; \
		goto rec##nn; \
	case SPLIT: \
		on##list(nn) \
		npc += 3; \
		pcs[i] = npc + npc[-2]; \
		fastrec(nn, list, listidx) \
	case RSPLIT: \
		on##list(nn) \
		npc += 3; \
		pcs[i] = npc; \
		npc += npc[-2]; \
		fastrec(nn, list, listidx) \
	case SAVE: \
		if (nsub->ref > 1) { \
			nsub->ref--; \
			save##list() \
			nsub = s1; \
			nsub->ref = 1; \
		} \
		nsub->sub[npc[1]] = _sp; \
		npc += 2; \
		goto rec##nn; \
	case WBEG: \
		if (((sp != s || sp != _sp) && isword(sp)) \
				|| !isword(_sp)) \
			deccheck(nn) \
		npc++; goto rec##nn; \
	case WEND: \
		if (isword(_sp)) \
			deccheck(nn) \
		npc++; goto rec##nn; \
	case BOL: \
		if (_sp != s) { \
			if (!i && !listidx) \
				_return(0) \
			deccheck(nn) \
		} \
		npc++; goto rec##nn; \
	case EOL: \
		if (*_sp) \
			deccheck(nn) \
		npc++; goto rec##nn; \
	} \
} \

int re_pikevm(rcode *prog, const char *s, const char **subp, int nsubp)
{
	int rsubsize = sizeof(rsub)+(sizeof(char*)*nsubp);
	int i, j, c, gen, suboff = rsubsize, *npc;
	int clistidx = 0, nlistidx = 0, osubp = nsubp * sizeof(char*);
	const char *sp = s, *_sp = s;
	int *insts = prog->insts;
	int *pcs[prog->splits];
	rsub *subs[prog->splits];
	char nsubs[500000];
	rsub *nsub, *s1, *matched = NULL, *freesub = NULL;
	rthread _clist[prog->len], _nlist[prog->len];
	rthread *clist = _clist, *nlist = _nlist, *tmp;
	gen = prog->gen;
	goto jmp_start;
	for (;; sp = _sp) {
		gen++; uc_len(i, sp) uc_code(c, sp)
		_sp = sp+i;
		for (i = 0; i < clistidx; i++) {
			npc = clist[i].pc;
			nsub = clist[i].sub;
			switch(*npc++) {
			case CHAR:
				if (c != *npc++)
					break;
			case ANY:
			addthread:
				addthread(2, nlist, nlistidx)
			case CLASS:
				if (!re_classmatch(npc, c))
					break;
				npc += *(npc+1) * 2 + 2;
				goto addthread;
			case MATCH:
				if (matched) {
					decref(matched)
					suboff = 0;
				}
				matched = nsub;
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
			jmp_start:
			newsub(memset(s1->sub, 0, osubp);, /*nop*/)
			s1->ref = 1;
			s1->sub[0] = _sp;
			nsub = s1;
			npc = insts;
			addthread(1, clist, clistidx)
		} else if (!clistidx)
			break;
	}
	if (matched) {
		for (i = 0, j = i; i < nsubp; i+=2, j++) {
			subp[i] = matched->sub[j];
			subp[i+1] = matched->sub[nsubp / 2 + j];
		}
		_return(1)
	}
	_return(0)
}

int main(int argc, char *argv[])
{
	if (argc < 2) {
		printf("usage: <regex> <str...> <str...> ...\n");
		return 0;
	}
	int sub_els;
	int sz = re_sizecode(argv[1], &sub_els) * sizeof(int);
	printf("Precalculated size: %d\n", sz);
	char code[sizeof(rcode)+sz];
	rcode *_code = (rcode*)code;
	if (re_comp(_code, argv[1], sub_els)) {
		printf("Error in re_comp");
		return 1;
	}
	re_dumpcode(_code);
	#include <time.h>
	if (argc > 2) {
		sub_els = (sub_els + 1) * 2;
		const char *sub[sub_els];
		for (int i = 2; i < argc; i++) {
			printf("input bytelen: %ld\n", strlen(argv[i]));
			clock_t start_time = clock();
			sz = re_pikevm(_code, argv[i], sub, sub_els);
			double elapsed_time = (double)(clock() - start_time) / CLOCKS_PER_SEC;
			printf("Done in %f seconds\n", elapsed_time);
			if (!sz)
				{ printf("-nomatch-\n"); continue; }
			for (int k=sub_els; k>0; k--)
				if (sub[k-1])
					break;
			for (int l=0; l<sub_els; l+=2) {
				printf("(");
				if (sub[l] == NULL || sub[l+1] == NULL)
					printf("?");
				else
					printf("%d", (int)(sub[l] - argv[i]));
				printf(",");
				if (sub[l+1] == NULL || sub[l] == NULL)
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
