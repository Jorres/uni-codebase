#!/bin/bash

# surveillance after ~/report file

at now +1 minutes -f ./i.sh

tail -n 0 -f ~/report
