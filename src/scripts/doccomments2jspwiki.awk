# An AWK preprocessor to pull doc comments out of Xerox lexc, twolc and VISL CG3
# files. 
#
# The doc comments are recognised by sequence of /^!! /, that is, two comment
# signs at the beginning of a line.
#
# The doc comments are assumed to be already in jspwiki markup with
# following special additions:
# - Multichar_Symbols block is handled specially and its contents are used in
#   output as lists
# - LEXICONs and Rules blocks are handled specially and their names are saved
#   for use in headings that directly follow them with @LEXNAME@ and @RULENAME@
# - a doc comment starting with $ or € and space
#   is used to denote an example, and a test case
# - a doc comment starting with $ or € without space is used to change test
#   settings
BEGIN { 
    LEXNAME="@OUTSIDE_LEXICONS@";
}
/Multichar_Symbols/,/LEXICON/ {
    if ($1 ~ /Multichar_Symbols/)
    {
        print("!!Multicharacter symbols for analyses");
    }
    else if ($1 ~ /LEXICON/)
    {
        LEXNAME = $2;
    }
    else if ($0 ~ /^ *[^!]/)
    {
        if ($0 ~ /!! \|/)
        {
            # table form
            printf("| ");
        }
        else
        {
            # definition list
            printf(";");
        }
        for (i = 1; i <= NF; i++)
        {
            if ($i ~ /^!!/)
            {
                break;
            }
            else if ((i == 1) && (NF == 1))
            {
                printf("{{%s}}\n", $i);
            }
            else if (i == 1)
            {
                printf("{{%s}}", $i);
            }
            else if (i == NF)
            {
                printf(", {{%s}}\n", $i);
            }
            else
            {
                printf(", {{%s}}", $i);
            }
        }
    }
}
/^[[:space:]]*$/ {printf("\n");}
/^!! [€$][^ ]/ {printf("(subsequent examples are for *%s*)\n", $3);}
/^!! € / {
    printf("* __%s__: {{%s}} (%s)\n", $3, $4, $5);
}
/^!! \$ / {
    printf("* *__%s__ (is not standard language)\n", $3);
}
/^!! !/ {print(gensub("@LEXNAME@", LEXNAME, "g", gensub("!! ", "", ""))); }
/^!! [^$€!]/ {print(gensub("!! ", "", "")); }
/..*!! \|/ {print(gensub(".*!! ", "", "")); }
/..*!! [^|]/ {printf(gensub(".*!! ", ":", "")); }
/^LEXICON / {
    LEXNAME=$2;
}
/^"[^"]*"/ {
    RULENAME=gensub("!.*", "", "", gensub("\"", "", "g"));
}
