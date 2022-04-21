/*
Copyright 2007-2009 Russ Cox.  All Rights Reserved.
Copyright 2020-2021 Kyryl Melekhin.  All Rights Reserved.
Use of this source code is governed by a BSD-style
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

unsigned char utf8_length[256] = {
	/*	0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F */
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
	/* C */ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
	/* D */ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
	/* E */ 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	/* F */ 4, 4, 4, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1
};

/* return the length of a utf-8 character */
#define uc_len(dst, s) dst = utf8_length[(unsigned char)s[0]];
/* the unicode codepoint of the given utf-8 character */
#define uc_code(dst, s) \
dst = (unsigned char)s[0]; \
if (dst < 192){} \
else if (dst < 224) \
	dst = ((dst & 0x1f) << 6) | (s[1] & 0x3f); \
else if (dst < 240) \
	dst = ((dst & 0x0f) << 12) | ((s[1] & 0x3f) << 6) | (s[2] & 0x3f); \
else if (dst < 248) \
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
	int unilen;	/* number of integers in insts */
	int len;	/* number of atoms/instructions */
	int sub;	/* interim val = save count; final val = nsubs size */
	int presub;	/* interim val = save count; final val = 1 rsub size */
	int splits;	/* number of split insts */
	int sparsesz;	/* sdense size */
	int insts[];	/* re code */
};

enum
{
	/* Instructions which consume input bytes */
	CHAR = 1,
	CLASS,
	MATCH,
	ANY,
	/* Assert position */
	WBEG,
	WEND,
	BOL,
	EOL,
	/* Other (special) instructions */
	SAVE,
	/* Instructions which take relative offset as arg */
	JMP,
	SPLIT,
	RSPLIT,
};

typedef struct rsub rsub;
struct rsub
{
	int ref;
	rsub *freesub;
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
	/* pc points to "classnot" byte after opcode */
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
			if (code[pc-1] < 0)
				printf("rsplit %d (%d) #%d\n", pc + code[pc] + 1, code[pc], code[pc-1]);
			else
				printf("split %d (%d) #%d\n", pc + code[pc] + 1, code[pc], code[pc-1]);
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
	printf("unilen: %d, insts: %d, splits: %d, counted insts: %d\n",
		prog->unilen, prog->len, prog->splits, i);
}

static int _compilecode(const char *re_loc, rcode *prog, int sizecode)
{
	const char *re = re_loc;
	int *code = sizecode ? NULL : prog->insts;
	int start = PC, term = PC;
	int alt_label = 0, c;
	int alt_stack[4096], altc = 0;
	int cap_stack[4096 * 5], capc = 0;

	while (*re) {
		switch (*re) {
		case '\\':
			re++;
			if (!*re) return -1; /* Trailing backslash */
			if (*re == '<' || *re == '>') {
				if (re - re_loc > 2 && re[-2] == '\\')
					break;
				EMIT(PC++, *re == '<' ? WBEG : WEND);
				term = PC;
				break;
			}
		default:
			term = PC;
			EMIT(PC++, CHAR);
			uc_code(c, re) EMIT(PC++, c);
			break;
		case '.':
			term = PC;
			EMIT(PC++, ANY);
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
			PC++; /* Skip "# of pairs" byte */
			for (cnt = 0; *re != ']'; cnt++) {
				if (*re == '\\') re++;
				if (!*re) return -1;
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
			if (*(re+1) == '?') {
				re += 2;
				if (*re == ':')
					capture = 0;
				else
					return -1;
			}
			if (capture) {
				sub = ++prog->sub;
				EMIT(PC++, SAVE);
				EMIT(PC++, sub);
			}
			cap_stack[capc++] = capture;
			cap_stack[capc++] = term;
			cap_stack[capc++] = alt_label;
			cap_stack[capc++] = start;
			cap_stack[capc++] = altc;
			alt_label = 0;
			start = PC;
			break;
		case ')':
			if (--capc-4 < 0) return -1;
			if (code && alt_label) {
				EMIT(alt_label, REL(alt_label, PC) + 1);
				int _altc = cap_stack[capc];
				for (int alts = altc; altc > _altc; altc--) {
					int at = alt_stack[_altc+alts-altc]+(altc-_altc)*2;
					EMIT(at, REL(at, PC) + 1);
				}
			}
			start = cap_stack[--capc];
			alt_label = cap_stack[--capc];
			term = cap_stack[--capc];
			if (cap_stack[--capc]) {
				EMIT(PC++, SAVE);
				EMIT(PC++, code[term+1] + prog->presub + 1);
			}
			break;
		case '{':;
			int maxcnt = 0, mincnt = 0, i = 0, size = PC - term;
			re++;
			while (isdigit((unsigned char) *re))
				mincnt = mincnt * 10 + *re++ - '0';
			if (*re == ',') {
				re++;
				if (*re == '}') {
					EMIT(PC, RSPLIT);
					EMIT(PC+1, REL(PC, PC - size));
					PC += 2;
					maxcnt = mincnt;
				}
				while (isdigit((unsigned char) *re))
					maxcnt = maxcnt * 10 + *re++ - '0';
			} else
				maxcnt = mincnt;
			for (; i < mincnt-1; i++) {
				if (code)
					memcpy(&code[PC], &code[term], size*sizeof(int));
				PC += size;
			}
			for (i = maxcnt-mincnt; i > 0; i--) {
				EMIT(PC++, SPLIT);
				EMIT(PC++, REL(PC, PC+((size+2)*i)));
				if (code)
					memcpy(&code[PC], &code[term], size*sizeof(int));
				PC += size;
			}
			break;
		case '?':
			if (PC == term) return -1;
			INSERT_CODE(term, 2, PC);
			if (re[1] == '?') {
				EMIT(term, RSPLIT);
				re++;
			} else
				EMIT(term, SPLIT);
			EMIT(term + 1, REL(term, PC));
			term = PC;
			break;
		case '*':
			if (PC == term) return -1;
			INSERT_CODE(term, 2, PC);
			EMIT(PC, JMP);
			EMIT(PC + 1, REL(PC, term));
			PC += 2;
			if (re[1] == '?') {
				EMIT(term, RSPLIT);
				re++;
			} else
				EMIT(term, SPLIT);
			EMIT(term + 1, REL(term, PC));
			term = PC;
			break;
		case '+':
			if (PC == term) return -1;
			if (re[1] == '?') {
				EMIT(PC, SPLIT);
				re++;
			} else
				EMIT(PC, RSPLIT);
			EMIT(PC + 1, REL(PC, term));
			PC += 2;
			term = PC;
			break;
		case '|':
			if (alt_label)
				alt_stack[altc++] = alt_label;
			INSERT_CODE(start, 2, PC);
			EMIT(PC++, JMP);
			alt_label = PC++;
			EMIT(start, SPLIT);
			EMIT(start + 1, REL(start, PC));
			term = PC;
			break;
		case '^':
			EMIT(PC++, BOL);
			term = PC;
			break;
		case '$':
			EMIT(PC++, EOL);
			term = PC;
			break;
		}
		uc_len(c, re) re += c;
	}
	if (code && alt_label) {
		EMIT(alt_label, REL(alt_label, PC) + 1);
		for (int alts = altc; altc; altc--) {
			int at = alt_stack[alts-altc]+altc*2;
			EMIT(at, REL(at, PC) + 1);
		}
	}
	return capc ? -1 : 0;
}

int re_sizecode(const char *re, int *nsub)
{
	rcode dummyprog;
	dummyprog.unilen = 3;
	dummyprog.sub = 0;

	int res = _compilecode(re, &dummyprog, 1);
	if (res < 0) return res;
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

	int res = _compilecode(re, prog, 0);
	if (res < 0) return res;
	int icnt = 0, scnt = SPLIT;
	for (int i = 0; i < prog->unilen; i++)
		switch (prog->insts[i]) {
		case CLASS:
			i += prog->insts[i+2] * 2 + 2;
			icnt++;
			break;
		case SPLIT:
			prog->insts[i++] = scnt;
			scnt += 2;
			icnt++;
			break;
		case RSPLIT:
			prog->insts[i] = -scnt;
			scnt += 2;
		case JMP:
		case SAVE:
		case CHAR:
			i++;
		case ANY:
			icnt++;
		}
	prog->insts[prog->unilen++] = SAVE;
	prog->insts[prog->unilen++] = prog->sub + 1;
	prog->insts[prog->unilen++] = MATCH;
	prog->splits = (scnt - SPLIT) / 2;
	prog->len = icnt + 2;
	prog->presub = sizeof(rsub)+(sizeof(char*) * (nsubs + 1) * 2);
	prog->sub = prog->presub * (prog->len - prog->splits + 3);
	prog->sparsesz = scnt;
	return 0;
}

#define newsub(init, copy) \
if (freesub) \
	{ s1 = freesub; freesub = s1->freesub; copy } \
else \
	{ if (suboff == prog->sub) suboff = 0; \
	s1 = (rsub*)&nsubs[suboff]; suboff += rsubsize; init } \

#define decref(csub) \
if (--csub->ref == 0) { \
	csub->freesub = freesub; \
	freesub = csub; \
} \

#define rec_check(nn) \
if (si) { \
	npc = pcs[--si]; \
	nsub = subs[si]; \
	goto rec##nn; \
} \

#define deccheck(nn) { decref(nsub) rec_check(nn) continue; } \

#define onclist(nn)
#define onnlist(nn) \
if (sdense[spc] < sparsesz) \
	if (sdense[sdense[spc] * 2] == (unsigned int)spc) \
		deccheck(nn) \
sdense[spc] = sparsesz; \
sdense[sparsesz++ * 2] = spc; \

#define fastrec(nn, list, listidx) \
nsub->ref++; \
spc = *npc; \
if ((unsigned int)spc < WBEG) { \
	list[listidx].sub = nsub; \
	list[listidx++].pc = npc; \
	npc = pcs[si]; \
	goto rec##nn; \
} \
subs[si++] = nsub; \
goto next##nn; \

#define saveclist() \
newsub(memcpy(s1->sub, nsub->sub, osubp);, \
memcpy(s1->sub, nsub->sub, osubp / 2);) \

#define savenlist() \
newsub(/*nop*/, /*nop*/) \
memcpy(s1->sub, nsub->sub, osubp); \

#define instclist(nn) \
else if (spc == BOL) { \
	if (_sp != s) { \
		if (!si && !clistidx) \
			return 0; \
		deccheck(nn) \
	} \
	npc++; goto rec##nn; \
} \

#define instnlist(nn) \
else if (spc == JMP) { \
	npc += 2 + npc[1]; \
	goto rec##nn; \
} \

#define clistmatch()
#define nlistmatch() \
if (spc == MATCH) \
	for (i++; i < clistidx; i++) { \
		npc = clist[i].pc; \
		nsub = clist[i].sub; \
		if (*npc == MATCH) \
			goto matched; \
		decref(nsub) \
	} \

#define addthread(nn, list, listidx) \
rec##nn: \
spc = *npc; \
if ((unsigned int)spc < WBEG) { \
	list[listidx].sub = nsub; \
	list[listidx++].pc = npc; \
	rec_check(nn) \
	list##match() \
	continue; \
} \
next##nn: \
if (spc > JMP) { \
	on##list(nn) \
	npc += 2; \
	pcs[si] = npc + npc[-1]; \
	fastrec(nn, list, listidx) \
} else if (spc == SAVE) { \
	if (nsub->ref > 1) { \
		nsub->ref--; \
		save##list() \
		nsub = s1; \
		nsub->ref = 1; \
	} \
	nsub->sub[npc[1]] = _sp; \
	npc += 2; \
	goto rec##nn; \
} else if (spc == WBEG) { \
	if (((sp != s || sp != _sp) && isword(sp)) \
			|| !isword(_sp)) \
		deccheck(nn) \
	npc++; goto rec##nn; \
} else if (spc < 0) { \
	spc = -spc; \
	on##list(nn) \
	npc += 2; \
	pcs[si] = npc; \
	npc += npc[-1]; \
	fastrec(nn, list, listidx) \
} else if (spc == WEND) { \
	if (isword(_sp)) \
		deccheck(nn) \
	npc++; goto rec##nn; \
} else if (spc == EOL) { \
	if (*_sp) \
		deccheck(nn) \
	npc++; goto rec##nn; \
} inst##list(nn) \
deccheck(nn) \

#define swaplist() \
tmp = clist; \
clist = nlist; \
nlist = tmp; \
clistidx = nlistidx; \

#define deccont() { decref(nsub) continue; }

int re_pikevm(rcode *prog, const char *s, const char **subp, int nsubp)
{
	int rsubsize = prog->presub, suboff = 0;
	int spc, i, j, c, *npc, osubp = nsubp * sizeof(char*);
	int si = 0, clistidx = 0, nlistidx, mcont = MATCH;
	const char *sp = s, *_sp = s;
	int *insts = prog->insts;
	int *pcs[prog->splits];
	rsub *subs[prog->splits];
	unsigned int sdense[prog->sparsesz], sparsesz;
	rsub *nsub, *s1, *matched = NULL, *freesub = NULL;
	rthread _clist[prog->len], _nlist[prog->len];
	rthread *clist = _clist, *nlist = _nlist, *tmp;
	char nsubs[prog->sub];
	goto jmp_start;
	for (;; sp = _sp) {
		uc_len(i, sp) uc_code(c, sp)
		_sp = sp+i;
		nlistidx = 0; sparsesz = 0;
		for (i = 0; i < clistidx; i++) {
			npc = clist[i].pc;
			nsub = clist[i].sub;
			spc = *npc;
			if (spc == CHAR) {
				if (c != *(npc+1))
					deccont()
				npc += 2;
			} else if (spc == CLASS) {
				if (!re_classmatch(npc+1, c))
					deccont()
				npc += *(npc+2) * 2 + 3;
			} else if (spc == MATCH) {
				matched:
				nlist[nlistidx++].pc = &mcont;
				if (npc != &mcont) {
					if (matched)
						decref(matched)
					matched = nsub;
				}
				if (sp == _sp || nlistidx == 1) {
					for (i = 0, j = i; i < nsubp; i+=2, j++) {
						subp[i] = matched->sub[j];
						subp[i+1] = matched->sub[nsubp / 2 + j];
					}
					return 1;
				}
				swaplist()
				goto _continue;
			} else
				npc++;
			addthread(2, nlist, nlistidx)
		}
		if (sp == _sp)
			break;
		swaplist()
		jmp_start:
		newsub(memset(s1->sub, 0, osubp);, /*nop*/)
		s1->ref = 1;
		s1->sub[0] = _sp;
		nsub = s1; npc = insts;
		addthread(1, clist, clistidx)
		_continue:;
	}
	return 0;
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
	if (sz < 0) {
		printf("Error in re_sizecode\n");
		return 1;
	}
	char code[sizeof(rcode)+sz];
	rcode *_code = (rcode*)code;
	if (re_comp(_code, argv[1], sub_els)) {
		printf("Error in re_comp\n");
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
			for (int l = 0; l < sub_els; l+=2) {
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
