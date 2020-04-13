#! /bin/sh

HERON_VER=$(/bin/echo | ./heron | grep Heron | cut -d ' ' -f 2)

/usr/bin/printf "System:   " ; uname -srp
/usr/bin/printf "Hostname: " ; hostname
/usr/bin/printf "User:     " ; whoami
/usr/bin/printf "Date:     " ; LC_TIME=en_US date
/usr/bin/printf "Version:  " ; if [ -z "$HERON_VER" ] ; then echo "(unknown)" ; else echo "$HERON_VER" ; fi
