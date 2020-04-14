#! /bin/bash

HERON_VER=$(/bin/echo | /heron/heron | grep Heron | cut -d ' ' -f 2)
BACKGROUND_COLOR=$(tput setab 4)

printf "$BACKGROUND_COLOR                                     $(tput sgr0)\n"
printf "$BACKGROUND_COLOR  Heron                              $(tput sgr0)\n"
printf "$BACKGROUND_COLOR  Docker container                   $(tput sgr0)\n"
printf "$BACKGROUND_COLOR  Version: " ; if [ -z "$HERON_VER" ] ; then printf "(unknown)" ; else printf "$HERON_VER" ; fi ; printf "     $(tput sgr0)\n"
printf "$BACKGROUND_COLOR                                     $(tput sgr0)\n\n"

printf "Here are a few examples you can find in directory \`examples/':\n"
ls examples
printf "\n"
printf "To execute these files, just type:\n"
printf "  $(tput bold)heron --use examples/[TESL FILE]$(tput sgr0)\n"
printf "\n"
printf "You can also run regression tests:\n"
printf "  $(tput bold)make test$(tput sgr0)\n"
