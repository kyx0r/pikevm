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
\\\d+
\\\s+
\\\w+
(\\\w+)\\\s+(\\\w+)
(\\\S+)\\\s+(\\\D+)
(([0-9]*)([a-z]*)\\\d*)
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
 	123abc456
123abc_456 abc
ABC 	123hello456 abc
ABC 	helloabc456 abc
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
(0,2)
(0,10)
(0,16)(0,3)(5,16)
(0,13)(0,3)(5,13)
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
(0,0)
"
c=1
echo "$regex" | tr '\n' | while read re; do
	inp=$(echo "$input" | awk -v c=$c 'BEGIN{ RS = "" ; FS = "\n" }{print $c}')
	exp=$(echo "$expect" | awk -v c=$c 'BEGIN{ RS = "" ; FS = "\n" }{print $c}')
	var=$(echo $(./a.out "$re" "$inp" | awk 'END{print}'))
	if [ ! "$exp" = "$var" ]; then
		echo "fail test$c regex:$re input:$inp expect:$exp output:$var"
		exit 1
	fi
	echo "pass test$c regex:$re input:$inp expect:$exp output:$var"
	c=$((c+1))
done

