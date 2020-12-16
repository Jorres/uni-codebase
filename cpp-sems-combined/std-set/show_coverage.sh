#!/bin/sh
set -euo pipefail    # see http://redsymbol.net/articles/unofficial-bash-strict-mode/ for details
IFS=$' \t\n'

lcov --no-external --base-directory . --capture --directory . --output-file cov.info
genhtml cov.info --ignore-errors source --output-directory out
echo The coverage is stored to $(pwd)/out/index.html

