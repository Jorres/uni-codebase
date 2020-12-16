#!/bin/bash

sudo grep -E -o -r -h -I "\b[A-Za-z0-9.-]+@[A-Za-z0-9.-]+\.[A-Za-z]+\b" /etc > emails.lst
cat emails.lst
