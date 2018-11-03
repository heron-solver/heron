#! /bin/sh

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

# From TESL file, generates output
generate () {
    ./heron --use $1  >/dev/null
    mv output.tex $1.out
}

# Checks output against expectation
run_check () {
    if (! (diff $1 $2  | grep -v % | grep tick)) && (! (diff $1 $2  | grep -v % | grep date)) && (! (diff $1 $2  | grep -v % | grep Cross)) && (! (diff $1 $2  | grep -v % | grep Skull))
    then echo "  --> \e[1m\e[32mPASS\e[0m" ; exit 0
    else echo "  --> \e[1m\e[31mFAIL\e[0m" ; exit 1
    fi
}

# Checks specification
check () {
    generate $1
    run_check $1.out $1.expected
}

# Main entry-point
does_file_exists $1
echo "Testing $1..."
check $1
