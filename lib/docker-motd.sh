#! /bin/bash

HERON=./heron #/heron/heron
HERON_VER=$(( /bin/echo 2>/dev/null ) | ($HERON 2>/dev/null )| grep Heron | cut -d ' ' -f 2)
HERON_VER=$(if [ -z "$HERON_VER" ] ; then printf "(not found)          " ; else printf "$HERON_VER" ; fi)
CPUS=$(lscpu | egrep 'Model name|Socket|Thread|NUMA|CPU\(s\)' | head -n 1 | cut -d ':' -f 2 | awk 'gsub(/^[ \t]+/,"")')
HOSTNAME=$(hostname)

BGN1=$(tput setab 4)
BGN2=$(tput setab 5)
YELLOW=$(tput setaf 3)
RESET=$(tput sgr0)
BOLD=$(tput bold)

# Graphics
printf "$BGN1                                   $RESET"                     ; printf "  ${BGN2}                                       $RESET\n" 
printf "$BGN1$BOLD  Heron                            $RESET"		      ; printf "  ${BGN2}$BOLD  For assistance, just type:           $RESET\n" 
printf "$BGN1$BOLD  Version: $HERON_VER   $RESET"                           ; printf "  ${BGN2}$BOLD    heron --help                       $RESET\n" 
printf "$BGN1$BOLD  Docker:  $HOSTNAME            $RESET"		      ; printf "  ${BGN2}$BOLD  To run some examples                 $RESET\n" 
printf "$BGN1$BOLD  CPU(s):  %02d                      $RESET" $CPUS	      ; printf "  ${BGN2}$BOLD    heron --use examples/[TESL FILE]   $RESET\n" 
printf "$BGN1                                   $RESET"  		      ; printf "  ${BGN2}                                       $RESET\n\n" 

printf "Some examples you can find in ${BOLD}directory \`examples/'${RESET}:\n"
ls --color=always --group-directories-first examples
printf "\n"

printf "You can also run regression tests:\n"
printf "${BOLD}  make test${RESET}\n\n"

# Check for updates
git remote update 2>/dev/null >/dev/null
if (git status -uno | grep behind >/dev/null)
then
    printf "${YELLOW}${BOLD}NOTICE: A newer version exists. Please update this container.${RESET}\n\n"
fi
