BEGIN {
    if (CS <= 0)
    {
        print "define corpus size CS" > "/dev/stderr" ;
        exit(1);
    }
}
NF == 2 {
    printf("%s\t%f\n", $2, -log($1/CS));
}
