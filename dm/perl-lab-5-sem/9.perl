while (<>) {
    print if /((\w|[0-9])+(\g{1})/;
}
