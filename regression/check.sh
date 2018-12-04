#! /bin/sh
# usage: ./check.sh [TESL SPECFICATION FILE]
# WARNING: an expected output file must exist in the same directory
#          in TikZ format

does_file_exists () {
    if (!(cat $1 >/dev/null))
    then
	 echo "ERROR: Specification file does not exist"
	 exit 1
    fi
    if (!(cat $1.expected >/dev/null))
    then
	 echo "ERROR: Expected run file does not exist"
	 exit 1
    fi
}

does_output_exists () {
    if (!(cat $1.out >/dev/null))
    then
	 echo "  --> FAIL (no output found)"
	 exit 1
    fi
}

# From TESL file, generates output
generate () {
    OS_NAME="$(uname -m)"
    if [ "$OS_NAME" = "linux" ]
    then /usr/bin/time -f "  -> Time:   %E\n  -> Memory: %M kB" ./heron --use $1  >/dev/null
    else /usr/bin/time ./heron --use $1  >/dev/null
    fi
    mv output.tex $1.out
}

# Checks output against expectation
run_check () {
    if (! (diff $1 $2  | grep -v % | grep -w tick)) && (! (diff $1 $2  | grep -v % | grep -w date)) && (! (diff $1 $2  | grep -v % | grep -w Cross)) && (! (diff $1 $2  | grep -v % | grep -w Skull))
    then echo "  -> PASS" ; exit 0 # \e[1m\e[32mPASS\e[0m
    else echo "  -> FAIL" ; exit 1 # \e[1m\e[31mFAIL\e[0m
    fi
}

# Checks specification
check () {
    generate $1
    does_output_exists $1
    run_check $1.out $1.expected
}

# Main entry-point
does_file_exists $1
echo "Testing $1..."
check $1
