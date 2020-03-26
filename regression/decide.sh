#! /bin/bash

if (! (grep "FAIL" regression/_results.log >/dev/null))
then
    /bin/echo "$(tput bold)$(tput setaf 2)Congrats! ALL REGRESSION TESTS PASSED.$(tput sgr0)"
    exit 0
else
    /bin/echo "$(tput bold)$(tput setaf 1)Sorry! SOME REGRESSION TESTS HAVE FAILED.$(tput sgr0)"
     exit 1
fi
