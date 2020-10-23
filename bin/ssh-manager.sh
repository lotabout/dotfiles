#!/usr/bin/env bash

FILE=~/Dropbox/Dump/ssh-hosts.txt

set -o pipefail

eval $(cat $FILE |\
    sk --header-lines=1 \
    --layout=reverse \
    --height=30% \
    --delimiter '\|' \
    --with-nth='..-2' \
    --bind "ctrl-e:execute(vi $FILE < /dev/tty > /dev/tty)" |\
    awk -F ' *\\| *' '{printf("USERNAME=%s;HOST=%s;PASS=%s",$1,$2,$NF)}')

if [ -z $HOST ]; then
    echo "no host selected, quit"
    exit 0;
fi

USERNAME=${USERNAME:-$(whoami)}
if [ ! -z $PASS ]; then
    echo "password is: $PASS, consider using ssh-copy-id instead"
fi
ssh $USERNAME@$HOST
