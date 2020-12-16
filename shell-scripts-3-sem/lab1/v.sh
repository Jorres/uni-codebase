#!/bin/bash

for i in {1..4}
do
    echo $i

    case $i in
    1)
        echo "   Launch vi"
        ;;
    2)
        echo "   Launch nano"
        ;;
    3)
        echo "   Launch links"
        ;;
    4) 
        echo "   Exit menu"
        ;;
    esac
done

read

case $REPLY in
    1)
        vi
        ;;
    2)
        nano
        ;;
    3)
        links
        ;;
    4)
        exit 0
        ;;
esac

