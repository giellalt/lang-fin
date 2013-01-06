BEGIN {
    # we need to getline to get filename :-\
    getline
    printf("# yaml test generated from %s with doccomments2yaml.awk\n", 
           FILENAME)
    print("Config:");
    print("  hfst:");
    print("    Gen: ../../../src/generator-gt-norm.hfst");
    print("    Morph: ../../../src/analyser-gt-norm.hfst");
    print("  xerox:");
    print("    Gen: ../../../src/generator-gt-norm.xfst");
    print("    Morph: ../../../src/analyser-gt-norm.hfst");
    print("");
    print("Tests:");
}
/^LEXICON / {printf("  %s: #\n", $2);}
/^!! €/ {printf("    %s: %s # %s\n", $4, $3, $5);}
/^!! \$/ {printf("    %s+?: %s # %s\n", $3, $3, $4);}
