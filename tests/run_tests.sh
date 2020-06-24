#!/bin/sh

compiler="./shuxc"
runtime=lli
test_ext=".shux"
obj_ext=".ll"
out_ext=".out"
expected_ext=".test"
passes=0
warnings=0
fails=0
skip_pass=0

if [ $# -eq 1 ]; then
    if [ "$1" = "COMPILE" ]; then
        echo "running only COMPILE/FAIL tests\n"
        skip_pass=1
        test_names=`ls *.shux`
    else
        echo "running single test\n"
        test_names=$1
        # this should be extended to just run the below with test_names=our one file
    fi 
else
    echo "running all tests sequentially\n"
    test_names=`ls *.shux`
fi

for test in $test_names 
do
	test=`echo "$test" | cut -d'.' -f1`
	echo -n $test

    # what intermediate objects do we need?
	test_obj=$test$obj_ext
	test_out=$test$out_ext
	expected_out=$test$expected_ext
    expected_head=$(head -n 1 $expected_out)
	expected_body=/tmp/expected_body
    echo -n " ($expected_head)... "

    # run compiler
    ERROR="$($compiler $test$test_ext 2>&1 > $test_obj)"
    if [ -n "$ERROR" ]; then    
        echo $ERROR > $test_out

        # if echo $ERROR | grep -q "Uncaught exception"; then
        #     if [ "$expected_head" = "PASS" ]; then
        #         echo "passed! ✅"
        #         passes=$((passes+1))
        #         continue
        #     else
        #         echo "compilation failed! saving $test_out ❌"
        #         fails=$((fails+1))
        #         continue
        #     fi
        # fi
    fi

    # check for valid test file
    if [ ! -f $expected_out ]; then
        # expected output file not found
        echo "expected output file not found! ❌"
        fails=$((fails+1))
        continue
    fi

    # check type of test and invoke runtime if necessary
    echo -n $(tail -n +2 $expected_out) > $expected_body
    if [ "$expected_head" = "PASS" ]; then
        if [ $skip_pass -eq 1 ]; then
            echo "skipping."
        else
            # diff against expected output
            $runtime $test_obj > $test_out 2>&1
            if diff -q $test_out $expected_body > /dev/null; then
                echo "passed! ✅"
                passes=$((passes+1))
            else
                echo "failed! please check $test$out_ext for debug info ❌"
                fails=$((fails+1))
            fi
        fi
    elif [ "$expected_head" = "FAIL" ]; then
        # pattern match against execption keywords
        exp_holder=$(cat $expected_body)
        if grep -q "$exp_holder" $test_out > /dev/null 2>&1; then
            echo "passed! ✅"
            passes=$((passes+1))
        elif grep -q "WARN" $test_out > /dev/null 2>&1; then
            echo "compilation threw warning 🔶"
            warnings=$((warnings+1))
        else
            echo "failed! expected error keyword(s) not found. please see $test$out_ext ❌"
            fails=$((fails+1))
        fi
    elif [ "$expected_head" = "COMPILE" ]; then
        if [ -f $test_out ]; then
            if grep -q "Uncaught exception" $test_out > /dev/null; then
                echo "compilation failed ❌"
                fails=$((fails+1))
            elif grep -q "WARN" $test_out > /dev/null 2>&1; then
                echo "compilation threw warning 🔶"
                warnings=$((warnings+1))
            else
                echo "passed! ✅"
                passes=$((passes+1))
            fi
        else
            echo "passed! ✅"
            passes=$((passes+1))
        fi
    else
        echo "invalid test specification ❌"
        fails=$((fails+1))
        continue
    fi
done

echo "\n$((fails+passes+warnings)) tests run", "$fails fail(s)", "$warnings warning(s)", "$passes passes"
if [ $((fails+warnings)) -eq 0 ]; then
    echo "all tests passed. you done it 💯"
fi
