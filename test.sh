




regex="abc cde (a|b)|c"
input="abcdef abcdef abc"
expect="(0,3) (2,5) (0,1)(0,1)"

c=1
echo "$regex" | tr ' ' '\n' | while read re; do
	inp=$(echo $input | awk -v c=$c '{print $c}')
	exp=$(echo $expect | awk -v c=$c '{print $c}')
	var=$(echo $(./a.out "$re" "$inp" | awk 'END{print}'))
	if [ ! "$exp" = "$var" ]; then
		echo "fail test$c regex:$re input:$inp expect:$exp output:$var"
		exit 1
	fi
	c=$((c+1))
done

