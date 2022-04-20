#!/bin/sh

regex="\
abc
cde
abc*
abc*
abc+
abc+
(([0-9]*)([a-z]*)[0-9]*)
([0-9]*)(([a-z]*)([0-9]*))
(([0-9]*)(?:[a-z]*)[0-9]*)
(?:[0-9]*)(([a-z]*)(?:[0-9]*))
([0-9]*)(?:([a-z]*)(?:[0-9]*))
(?:)
1?:
[0-9]+
[a-zA-Z0-9_]+
(([0-9]*)([a-z]*)[a-zA-Z0-9_]*)
[a]*
([yab]*)(e*)([cd])
([yab]*)(e*)([^y]?)$
[-]*
[-a]*
[-ab]*
[-a-c]*
[a-]*
[ab-]*
[a-c-]*
(\\\?:)
\\\(?:
[^a]*b
a{5}
[^a]{1,3}
(abc+){5}|[0-9]{1,}
(abc+){5}|[0-9]{1,}
b[^c]*
۱۲۳۴۵۶۷۸۹۰
[йцукен]
日本語*
([^ひらがな])|(な+)
([^ひらがな])|(な+)
([^abc])|(a+)
[a-g]+
[а-г]+
called|chief|dust|familiar|forth|waif|campaign|divers|smile|notice|kill|human|stands|nightshade|dollar|doughty|gloaming|twist|July|officers|wrest|coop|one|ability|welcome|significance|writer|spring|it's|helped|set|Paris|from|coomb|stay|hummock|taken|anon|makes|boat|nearly|am|justice|further|expression|contemporary|sooth|order|about|question|lived|apply|educational|of|night|satisfy|opened|never|success|until|visit|promise|parts|beneath|matter|typical|bade|apartment|rapidly|primary|bring|throat|hold|laws|understand|trade|desire|material|evidence|another|often|plash|model|someone|bond|hell|relationship|probably|exercise|performance|wants|known|countries|gammer|leeward|took|itself|representative|objection|aircraft
abc+h+d+f
([0-9]|abc){5}[0-9]+(grh)
[A-Fa-f0-9]{64}
<tag>[^<]*</tag>
^([a-z0-9_.-]+)@([0-9a-z.-]+)\\\\.([a-z.]{2,5})$
abc\$d
abc$|cdb
abc$|c
^ac|cdb
^abc+d
^(abc|kj)
^(abc|kj)
(^abc)|(abc)
(abc)|(^abc)
(^abc)|(abc$)
(^abc)|(abc$)
(^abc)|(abc$)
([^qwe]*rty)|(asd[^fgh]*)
([^qwe]*rty+)|(asd[^fgh]*)
((abc))(fv)
\\\\<abc
\\\\<abc
\\\\<(as|js)
\\\\<(as|js)
ab\\\\<d
\\\\<d+(abc|fbc|bcd)
\\\\<d+(abc|fbc|bcd)
\\\\<d+(abc|fbc|bcd)
b|\\\\<(abc|fbc|bcd)
\\\\<abc
\\\\<abc\\\\>
abc\\\\>
abc\\\\>
\\\\<(hello|world|word|nice|try)\\\\>
\\\\<(hello|world|word|nice|try)\\\\>
\\\\<(hello|world|word|nice|try)\\\\>
\\\\<(hello|world|word|nice|try)\\\\>
\\\\<(hello|world)\\\\>|\\\\<(word|nice|try)\\\\>
(abc+)|\\\\<[^k]*\\\\>
[-+]?\\\\<(0[xX][0-9a-fA-FUL]+|[0-9.]{1,}[0-9eEfFuULl]+|[0-9]+)\\\\>
[-+]?\\\\<(0[xX][0-9a-fA-FUL]+|[0-9.]{1,}[0-9eEfFuULl]+|[0-9]+)\\\\>
[-+]?\\\\<(0[xX][0-9a-fA-FUL]+|[0-9.]{1,}[0-9eEfFuULl]+|[0-9]+)\\\\>
[-+]?\\\\<(0[xX][0-9a-fA-FUL]+|[0-9.]{1,}[0-9eEfFuULl]+|[0-9]+)\\\\>
[-+]?\\\\<(0[xX][0-9a-fA-FUL]+|[0-9.]{1,}[0-9eEfFuULl]+|[0-9]+)\\\\>
[-+]?\\\\<(0[xX][0-9a-fA-FUL]+|[0-9.]{1,}[0-9eEfFuULl]+|[0-9]+)\\\\>
[-+]?\\\\<(0[xX][0-9a-fA-FUL]+|[0-9.]{1,}[0-9eEfFuULl]+|[0-9]+)\\\\>
[-+]?\\\\<(0[xX][0-9a-fA-FUL]+|[0-9.]{1,}[0-9eEfFuULl]+|[0-9]+)\\\\>
(([-+]?\\\\<(0[xX][0-9a-fA-FUL]+|[0-9.]{1,}[0-9eEfFuULl]+|[0-9]+)\\\\>))
qwerty.*$
([a-zA-Z0-9_][^1]*[a-zA-Z0-9_])|(\\\\\$([^\$]+)\\\\\$)
([a-zA-Z0-9_][^1]*[a-zA-Z0-9_])|(\\\\\$([^\$]+)\\\\\$)
(h[^1]*b)|(\\\\\$([^\$]+)\\\\\$)
(h[^1]*b)|(\\\\\$([^\$]+)\\\\\$)
(a|aa)*
(a|aa)*
(a|aa)*
(a|aa)*
(a|aa)*
(a|aa)*
(aaaa|aaa|a){3,4}
(a)(a)
(a){2}
(a|bcdef|g|ab|c|d|e|efg|fg)*
(.*) (.*) (.*) (.*) (.*)
(.+?)(.+?)
(a?)(a?)(a?)aaa
(ab)?(ab)?(ab)?aaa
(.+)(.+)
a(?:b|c|d){4,5}(.)
(?:c|d)(?:)(?:a(?:)(?:b)(?:b(?:))(?:b(?:)(?:b)))
\\\\<abc
a\\\\([COM]+\\\\)|([A-Za-z_%.]+):
xyz\\\\([COM]+\\\\)|(abcd[A-Za-z_%.]+):cd
aaaaa(aa)aa(aa(a)a)?aa
^\\\\.+(((/)\\\\.\\\\.+)+)?
^(http|https|ftp):[/]{2}([a-zA-Z0-9\\\\\\\\.]+\\\\.[a-zA-Z]{2,4})(:[0-9]+)?/?([a-zA-Z0-9\\\\\\\\._?,'/+&amp;%$#=~]*)
(\\\\$\\\\([a-zA-Z0-9_]+\\\\))|(([A-Za-z_%.]+):)
.{5}
.{10,15}
(a(abc)+){3,}
(aa(aa)|a(a)a|a){3,4}
(aa(aa)|a(a)a|a){3,4}
(aa(aa)|a(a)a|a){3,4}
(aa(aa)|a(a)a|a){3,4}
(aa(aa)|a(a)a|a){3,4}
(aa(aa)|a(a)a|a){3,4}
(aa(aa)|a(a)a|a){3,4}
(a(a)(aa)|aaa|a){3,4}
(a(a)(aa)|aaa|a){6}
(a(a)(aa)|aaa|a){5,8}
(a(a)(aa)|(aa)a|(a)){9}
(a(a)(aa)|(aa)a|(a)){10}
(a(a)(aa)|(aa)a|(a)){11}
(a(a)a|(a)|a(aa)|aa){2,5}
((a)a|a(aa)|(aaaa)|(a*)){3,10}
((a)a|a(aa)|(aaaa)|(a+)){3,10}
(aa(aa){3}|(a)a+(a)|a){3,4}
((ax)+b(ax)*)*
(a*)(b?)(b+)b{3}
(a*){2,3}
(a|at|ate|aten)(ten|en|n|d)
(a|aa|aaa|aaaaa)(aa|aaaa|aaa)
(a|a.|a..|a....)(a.|a...|a..)
(a*b|b*a)(a*b|b*a)
(ab|((ab)c|abc))c*
((a*b*c*)|(a*c*b*))*
.*|.*(a|b)?
((.a|.ab)(bc.|c.)|abc.)
\\\\\\\\<
\\\\\\\\\\\\\\\\<
[^a]*b
^(.+):([0-9]+):(.+)
^(.+):([0-9]+).(.+)
^(.+):([0-9]+).(.+){2,5}
^(.+):([0-9]+):(.+)
^(.+):([0-9]+).(.+)
^(.+):([0-9]+):(.+)
^(.+):([0-9]+):(.+)(.+)
(.*):([0-9]*):(.*)
(.*):([0-9]*)(   ):((.*)+)
(((aaa+)+)bb*)(.*):([0-9]*):(.+)
^(.+):([0-9]+):(.{8})+
^(.+):([0-9]+):((aaaa)|(.+))\"
[0-9]+.(.*)
[0-9]+.(.*)
([0-9])+.(.*)
(([0-9])+)(.)(.*)
"
input="\
abcdef
abcdef
abdef
abcccdef
abdef
abcccdef
1234hello567
1234hello567
1234hello568
1234hello568
1234hello568
1234hello568
1:
123abc456
123abc_456 abc
123hello456
a
xyac
xyac
--
-a-b
-a-b
-a-b-d-
-a-b
-a-b
-a-b-d-
:
(:
hhagbdbdbjsjjjda
aaaaaaaa
vbcvb
abcabcabcabcabcabchsdfhsdh
62374623
djfjgjsdfjbshdhfhshd
۱۲۳۴۵۶۷۸۹۰
ке
日本語語語語語語語本本本本
なななな
abc
aaaa
aaaabcdefghij
ааааабвг...
hhfd h23  performance
abcccccccccccchdf
344444442344grhhhsdfg
bf33d4a0dbbee85061531c9d47e5aae692c0729e5c9c1fa21c46d9bcab5f52c5
ajdas <tag> sidufisudf hsdfhshdfh sdf </tag> asjdfjs
veloval596@godpeed.com
abc
abccdb
abcc
abccdb
abccdb
kj
jhdfh kj hhd
 abc
 abc
 abc
abc
 abc bc
qweasd     qqqq fff
qwehh  sjsjsj rtyyyyyyyyyj sdj
abcfv
   	   abc
 hsdh  abc
     js hashasd
     gjs hashasd
ab   d
     bcddd bddddfbc
     bcddd ddvddfbc
     bcddd ddddfbc
     bcddd fbc
abc
   abc   
abcccc
abc
world
 world 
    worldfsd
    dworld
    nice   
    nicehdhfd
21361264
0x2346ULL
1.234423
	abc23321abb
   3245 jjfjjj
   0x663q
 x37247
  124435.7727ULL
str + len - 1;
 jjdfjk sjdjjsqwerty jdfjdfhhdhfdjjjfj jjjdf
$\"},  /* email */
$\"},  /* email */$
$  hbbbb
$ hsdhs $ 
a
aa
aaa
aaaa
aaaaa
aaaaaa
aaaaaaaaaa
aaaa
aaaa
abcdefg
a  c d ee fff
abcd
aaa
abaaa
abcd
acdbcdbe
cabbbb
dabc
a(COM:=.o):
xyz(abcdCOM:cd):
aaaaaaaaaaaaaaaaaaaaaaaaa
../../..
https://kyryl.tk/404
OBJ = \$(SRC:.c=.o)
рврыр
рврырdhsjhh
aabcabcaabcaabc
aaaaaaaaaaaaaaaaaa
aaaaaaaaaaaaaaa
aaaaaaaaaaaaaa
aaaaaaaaaaaaa
aaaaaaaaaaaa
aaaaaaaaaaa
aaaaaaaaaa
aaaaaaaaaa
aaaaaaaaaa
aaaaaaaaaa
aaaaaaaaaa
aaaaaaaaaa
aaaaaaaaaa
aaaaaaaaaaaaaa
aaaaaaaaaaaaaaaaa
aaaaaaaaaaaaaaaaa
aaaaaaaaaaaaaaa
axbaxaxbaxaxaxbaxaxaxax
aaabbbbbbb
aaaaaaaaaaaaaaaaa
atend
aaaaaa
aaaaaa
bbaa
abc
abcacb
aa
xabcx
\\<
\\\\\\\\<
hhagbdbdbjsjjjda..b
userspace-api/media/v4l/vbi_625.svg:98:   :34bstroke-linejoin:m;stroke-miteit:10;stroke-day:n;se-ty:1\"
userspace-api/media/v4l/vbi_625.svg:98:   :34bstroke-linejoin:m;stroke-miteit:10;stroke-day:n;se-ty:1\"
userspace-api/media/v4l/vbi_625.svg:98:   :34bstroke-linejoin:m;stroke-miteit:10;stroke-day:n;se-ty:1\"
h:98:   :3234434butt;stroke-linejoin:miter;stroke-miterlimit:10;stroke-dasharray:none;stroke-opacity:1\"
h:98:   :3234utt;strokeliin:miter;stroke-mirlimit:10;stroke-dasharray:none;stroke-opacity:1n\"
h:98:   :3234utt;strokeliin:miter;stroke-mirlimit:10;stroke-dasharray:none;stroke-opacity:1n\"
h:98:   :3234utt;strokeliin:miter;stroke-mirlimit:10;stroke-dasharray:none;stroke-opacity:1n\"
h:98:   :3234utt;strokeliin:miter;stroke-mirlimit:10;stroke-dasharray:none;stroke-opacity:1n\"
h:98:   :3234utt;strokeliin:miter;stroke-mirlimit:10;stroke-dasharray:none;stroke-opacity:1n\"
aaaaabb grt:123:....
h:98:   :3234utt;strokeliin:miter;stroke-mirlimit:10;stroke-dasharray:none;stroke-opacity:1n\"
h:98:   :3234utt;strokeliin:miter;stroke-mirlimit:10;stroke-dasharray:none;stroke-opacity:1n\"
650-253-0001
650-253-000123434-45551221
650-253-000123434-45551221
650-253-000123434-455512213224hsaqer
"
expect="\
(0,3)
(2,5)
(0,2)
(0,5)
-nomatch-
(0,5)
(0,12)(0,12)(0,4)(4,9)
(0,12)(0,4)(4,12)(4,9)(9,12)
(0,12)(0,12)(0,4)
(0,12)(4,12)(4,9)
(0,12)(0,4)(4,9)
(0,0)
(0,2)
(0,3)
(0,10)
(0,11)(0,11)(0,3)(3,8)
(0,1)
(1,4)(1,3)(3,3)(3,4)
(1,4)(1,3)(3,3)(3,4)
(0,2)
(0,3)
(0,4)
(0,5)
(0,3)
(0,4)
(0,5)
-nomatch-
(0,2)
(3,9)
(0,5)
(0,3)
(0,15)(12,15)
(0,8)(?,?)
(10,20)
(0,20)
(0,2)
(0,27)
(0,12)(?,?)(0,12)
(0,1)(0,1)(?,?)
(0,4)(?,?)(0,4)
(0,10)
(0,16)
(10,21)
(0,17)
(0,15)(4,5)(12,15)
(0,64)
(6,44)
(0,22)(0,10)(11,18)(19,22)
-nomatch-
(3,6)
(2,3)
(3,6)
(0,5)
(0,2)(0,2)
-nomatch-
(1,4)(?,?)(1,4)
(1,4)(1,4)(?,?)
(1,4)(?,?)(1,4)
(0,3)(0,3)(?,?)
-nomatch-
(3,16)(?,?)(3,16)
(3,25)(3,25)(?,?)
(0,5)(0,3)(0,3)(3,5)
(7,10)
(7,10)
(5,7)(5,7)
-nomatch-
-nomatch-
-nomatch-
-nomatch-
(11,18)(15,18)
(5,6)(?,?)
(0,3)
(3,6)
-nomatch-
(0,3)
(0,5)(0,5)
(1,6)(1,6)
-nomatch-
-nomatch-
(4,8)(?,?)(4,8)
(4,13)(?,?)
(0,8)(0,8)
(0,9)(0,9)
(0,8)(0,8)
-nomatch-
(3,7)(3,7)
-nomatch-
-nomatch-
(2,16)(2,16)
(12,13)(12,13)(12,13)(12,13)
(14,44)
(9,14)(9,14)(?,?)(?,?)
(0,18)(?,?)(0,18)(1,17)
(3,8)(3,8)(?,?)(?,?)
(0,9)(?,?)(0,9)(1,8)
(0,1)(0,1)
(0,2)(1,2)
(0,3)(2,3)
(0,4)(3,4)
(0,5)(4,5)
(0,6)(5,6)
(0,10)(9,10)
(0,2)(0,1)(1,2)
(0,2)(1,2)
(0,7)(6,7)
(0,13)(0,2)(3,4)(5,6)(7,9)(10,13)
(0,2)(0,1)(1,2)
(0,3)(0,0)(0,0)(0,0)
(0,5)(0,2)(?,?)(?,?)
(0,4)(0,3)(3,4)
(0,7)(6,7)
(0,6)
-nomatch-
(2,6)(2,5)
(4,14)(4,11)
(0,15)(5,7)(9,13)(11,12)
(0,8)(2,8)(5,8)(5,6)
(0,20)(0,5)(8,16)(?,?)(17,20)
(8,12)(?,?)(8,12)(8,11)
(0,10)
(0,16)
(0,15)(11,15)(12,15)
(0,16)(12,16)(14,16)(?,?)
(0,15)(12,15)(10,12)(13,14)
(0,13)(12,13)(10,12)(?,?)
(0,13)(12,13)(10,12)(?,?)
(0,12)(8,12)(10,12)(?,?)
(0,11)(8,11)(6,8)(9,10)
(0,10)(9,10)(6,8)(?,?)
(0,10)(9,10)(5,6)(6,8)
(0,9)(8,9)(1,2)(2,4)
(0,10)(9,10)(1,2)(2,4)
(0,9)(8,9)(?,?)(?,?)(?,?)(8,9)
(0,10)(9,10)(?,?)(?,?)(?,?)(9,10)
-nomatch-
(0,13)(12,13)(10,11)(12,13)(?,?)
(0,17)(17,17)(14,15)(?,?)(?,?)(17,17)
(0,17)(16,17)(14,15)(?,?)(?,?)(16,17)
(0,15)(14,15)(6,8)(8,9)(13,14)
(0,7)(0,7)(0,2)(5,7)
(0,10)(0,3)(3,4)(4,7)
(0,17)(17,17)
(0,4)(0,1)(1,4)
(0,3)(0,1)(1,3)
(0,3)(0,1)(1,3)
(0,2)(0,1)(1,2)
(0,3)(0,2)(?,?)(?,?)
(0,6)(5,6)(5,6)(?,?)
(0,2)(?,?)
(0,5)(0,5)(0,2)(2,5)
(0,2)
(2,5)
(3,9)
(0,102)(0,35)(36,38)(39,102)
(0,102)(0,77)(78,80)(81,102)
(0,102)(0,77)(78,80)(101,102)
(0,103)(0,1)(2,4)(5,103)
(0,93)(0,89)(90,91)(92,93)
(0,93)(0,1)(2,4)(5,93)
(0,93)(0,1)(2,4)(5,92)(92,93)
(0,93)(0,1)(2,4)(5,93)
(0,93)(0,4)(5,5)(5,8)(9,93)(9,93)
(0,20)(0,7)(0,5)(0,5)(7,11)(12,15)(16,20)
(0,93)(0,1)(2,4)(85,93)
(0,93)(0,1)(2,4)(5,92)(?,?)(5,92)
(0,12)(4,12)
(0,26)(4,26)
(0,26)(2,3)(4,26)
(0,36)(0,3)(2,3)(3,4)(4,36)
(0,0)
"

if [ ! -f ./a.out ]; then
	gcc pike.c -pedantic -Wall -Wfatal-errors -std=c99 $CFLAGS
fi

c=1
printf '%s\n' "$regex" | while read re; do
	inp=$(printf '%s\n' "$input" | awk -v c=$c 'BEGIN{ RS = "" ; FS = "\n" }{print $c}')
	exp=$(printf '%s\n' "$expect" | awk -v c=$c 'BEGIN{ RS = "" ; FS = "\n" }{print $c}')
	var=$(./a.out "$re" "$inp")
	if [ "$1" ]; then
		printf '%s\n' "$var"
	fi
	var1=$(printf '%s\n' "$var" | tail -1)
	if [ ! "$exp" = "$var1" ]; then
		printf '%s\n' "fail test$c regex:$re input:$inp expect:$exp output:$var1"
		if [ ! "$1" == 1 ]; then
			exit 1
		fi
		c=$((c+1))
		continue
	fi
	time=$(printf '%s\n' "$var" | tail -2 | head -n1)
	printf '%s\n' "pass test$c regex:$re input:$inp expect:$exp output:$var1 $time"
	c=$((c+1))
done

gcc pike.c -pedantic -Wall -Wfatal-errors -std=c99 $CFLAGS
