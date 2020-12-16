#!/bin/bash                                      

# or -s for grep instead of sudo
for FILE in $(sudo find /var/log); do
    if [ -f $FILE ] && [ -r $FILE ]; then
        grep "ACPI" -I "$FILE" >> errors.log
    fi
done

for FILE in $(sudo find /var/log); do
    if [ -f $FILE ] && [ -r $FILE ]; then
        grep "$FILE" -I errors.log
    fi
done
