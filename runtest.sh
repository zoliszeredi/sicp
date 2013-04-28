#!/bin/sh

FILES=$(find . | egrep scm$)

for SOURCE in $FILES
do
    OUTPUT=$(guile $SOURCE 2>/dev/null)

    if [ $? -eq 0 ]
    then
        MESSAGE="OK!"
    else
	MESSAGE="FAIL!"
    fi

    printf "Testing %-60s %5s\n" $SOURCE $MESSAGE

    if [ ! -z "$OUTPUT" ]
    then
        echo "$OUTPUT"
    fi
done
