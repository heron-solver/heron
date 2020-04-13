#! /bin/sh

echo -n "System:   " ; uname -sorp
echo -n "Hostname: " ; hostname
echo -n "User:     " ; whoami
echo -n "Date:     " ; LC_TIME=en_US date
echo -n "Version:  " ; ((./heron --help | head -n 1)  2>/dev/null) | cut -d ' ' -f 2 2>/dev/null
