#! /bin/sh

# usage: ./check.sh [TESL SPECIFICATION FILE]
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
    if (!(cat $1.out >/dev/null 2>/dev/null))
    then
	 printf "\r$(tput bold)$(tput setaf 1)[ EMPTY  ]$(tput sgr0) $1\n"
	 exit 1
    fi
}

# From TESL file, generates output
generate () {
    # OS_NAME="$(uname -m)"
    # if [ "$OS_NAME" = "linux" ]
    # then /usr/bin/time -f "  -> Time:   %E\n  -> Memory: %M kB" ./heron --use $1  >/dev/null
    # else /usr/bin/time ./heron --use $1  >/dev/null
    # fi
    ./heron --use $1  >/dev/null 2>/dev/null
    mv output.tex $1.out 2>/dev/null
}

# Checks output against expectation
run_check () {
    #if (! (diff $1 $2  | grep -v % | grep -w tick)) && (! (diff $1 $2  | grep -v % | grep -w date)) && (! (diff $1 $2  | grep -v % | grep -w Cross)) && (! (diff $1 $2  | grep -v % | grep -w Skull))
    (cat $1 | egrep "\[tick\]|\[date\]|Cross|Skull" | grep -v % | sort -u > $1.sorted) 2>/dev/null
    sort -u $2 > $2.sorted
    INCLUSION_DIFF1=`comm -13 $1.sorted $2.sorted` # $2.sorted ⊆ $1.sorted
    INCLUSION_DIFF2=`comm -13 $2.sorted $1.sorted` # $1.sorted ⊆ $2.sorted
    if [ -z "$INCLUSION_DIFF1" ]
    then if [ -z "$INCLUSION_DIFF2" ]
	  then printf "\r$(tput bold)$(tput setaf 2)[ PASSED ]$(tput sgr0) $1\n" ; exit 0 # \e[1m\e[32mPASS\e[0m
	  else printf "\r$(tput bold)$(tput setaf 1)[ FAILED ]$(tput sgr0) $1\n" ; echo $INCLUSION_DIFF2 ; exit 1 # \e[1m\e[31mFAIL\e[0m
	  fi
    else echo "  -> FAIL" ; echo $INCLUSION_DIFF1 ; exit 1 # \e[1m\e[31mFAIL\e[0m
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
/bin/echo -n "[        ] $1"
check $1
