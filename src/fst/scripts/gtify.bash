for f in src/fst/morphology/**/*.lexc ; do
    sed -i -e 's/\[POS=NOUN\]/+N/' $f
    sed -i -e 's/\[POS=ADJECTIVE\]/+A/' $f
    sed -i -e 's/\[POS=ADVERB\]/+Adv/' $f
    sed -i -e 's/\[POS=VERB\]/+Verb/' $f
    sed -i -e 's/\[POS=PROADJECTIVE\]/+A/' $f
    sed -i -e 's/\[POS=PRONOUN\]/+Pron/' $f
    sed -i -e 's/\[POS=PROADVERB\]/+Adv/' $f
    sed -i -e 's/\[POS=PARTICLE\]/+Pcle/' $f
    sed -i -e 's/\[POS=INTERJECTION\]/+Interj/' $f
    sed -i -e 's/\[POS=NUMERAL\]/+Num/' $f
    sed -i -e 's/\[POS=ADPOSITION\]/+Po/' $f
    sed -i -e 's/\[SUBCAT=PROPER\]/+Prop/' $f
    sed -i -e 's/\[SUBCAT=DEMONSTRATIVE\]/+Dem/' $f
    sed -i -e 's/\[SUBCAT=PERSONAL\]/+Pers/' $f
    sed -i -e 's/\[SUBCAT=INTERROG\]/+Interr/' $f
    sed -i -e 's/\[SUBCAT=RELATIVE\]/+Rel/' $f
    sed -i -e 's/\[SUBCAT=QUANTOR\]/+Qu/' $f
    sed -i -e 's/\[SUBCAT=REFLEX\]/+Refl/' $f
    sed -i -e 's/\[SUBCAT=RECIPROC\]/+Recipr/' $f
    sed -i -e 's/\[SUBCAT=INDEF\]/+Indef/' $f
    sed -i -e 's/\[SUBCAT=DEMONSTR\]/+Dem/' $f
    sed -i -e 's/\[SUBCAT=CARD\]/+Card/' $f
    sed -i -e 's/\[SUBCAT=ORD\]/+Ord/' $f
    sed -i -e 's/\[SUBCAT=APPROX\]//' $f
    sed -i -e 's/\[SUBCAT=DIGIT\]//' $f
    sed -i -e 's/\[SUBCAT=REAL\]//' $f
    sed -i -e 's/\[SUBCAT=CONJUNCTION\]//' $f
    sed -i -e 's/\[SUBCAT=COORD\]/+CC/' $f
    sed -i -e 's/\[SUBCAT=SUBORD\]/+CS/' $f
    sed -i -e 's/\[SUBCAT=POSTPOSITION\]/+Po/' $f
    sed -i -e 's/\[SUBCAT=PREPOSITION\]/+Pr/' $f
    sed -i -e 's/\[SUBCAT=PREFIX\]/+Pref/' $f
    sed -i -e 's/\[SUBCAT=SUFFIX\]/+Suff/' $f
    sed -i -e 's/\[SUBCAT=ABBREVIATION\]//' $f
    sed -i -e 's/\[SUBCAT=ACRONYM\]//' $f
    sed -i -e 's/\[SUBCAT=PUNCTUATION\]/+Punct/' $f
    sed -i -e 's/\[SUBCAT=SYMBOL\]/+Punct/' $f
    sed -i -e 's/\[SUBCAT=QUOTATIOM\]/+Punct/' $f
    sed -i -e 's/\[SUBCAT=QUOTATION\]/+Punct/' $f
    sed -i -e 's/\[SUBCAT=BRACKET\]/+Punct/' $f
    sed -i -e 's/\[SUBCAT=DASH\]/+Punct/' $f
    sed -i -e 's/\[SUBCAT=SPACE\]//' $f
    sed -i -e 's/\[SUBCAT=CURRENCY\]//' $f
    sed -i -e 's/\[SUBCAT=MATH\]//' $f
    sed -i -e 's/\[SUBCAT=RELATION\]//' $f
    sed -i -e 's/\[SUBCAT=OPERATION\]//' $f
    sed -i -e 's/\[SUBCAT=INITIAL\]//' $f
    sed -i -e 's/\[SUBCAT=FINAL\]//' $f
    for c in $(seq 1 100) ; do sed -i -e "s/\[KTN=$c\]//" $f; done
    for c in 1%0 2%0 3%0 4%0 5%0 6%0 7%0 8%0 ; do sed -i -e "s/\[KTN=$c\]//" $f; done
    for c in A B C D E F G H I J K L M N O P Q R S T U V W X Y Z ; do sed -i -e "s/\[KAV=$c\]//" $f; done
    sed -i -e 's/\[CASE=NOM\]/+Nom/' $f
    sed -i -e 's/\[CASE=PAR\]/+Par/' $f
    sed -i -e 's/\[CASE=GEN\]/+Gen/' $f
    sed -i -e 's/\[CASE=ACC\]/+Acc/' $f
    sed -i -e 's/\[CASE=INE\]/+Ine/' $f
    sed -i -e 's/\[CASE=ELA\]/+Ela/' $f
    sed -i -e 's/\[CASE=ILL\]/+Ill/' $f
    sed -i -e 's/\[CASE=ADE\]/+Ade/' $f
    sed -i -e 's/\[CASE=ABL\]/+Abl/' $f
    sed -i -e 's/\[CASE=ALL\]/+All/' $f
    sed -i -e 's/\[CASE=ESS\]/+Ess/' $f
    sed -i -e 's/\[CASE=INS\]/+Ins/' $f
    sed -i -e 's/\[CASE=ABE\]/+Abe/' $f
    sed -i -e 's/\[CASE=TRA\]/+Tra/' $f
    sed -i -e 's/\[CASE=CMT\]/+Cmt/' $f
    sed -i -e 's/\[CASE=LAT\]/+Lat/' $f
    sed -i -e 's/\[CASE=PRL\]/+Ptl/' $f
    sed -i -e 's/\[CASE=DIS\]/+Distr/' $f
    sed -i -e 's/\[CASE=TMP\]/+Tempr/' $f
    sed -i -e 's/\[NUM=SG\]/+Sg/' $f
    sed -i -e 's/\[NUM=PL\]/+Pl/' $f
    sed -i -e 's/\[POSS=SG1\]/+PxSg1/' $f
    sed -i -e 's/\[POSS=SG2\]/+PxSg2/' $f
    sed -i -e 's/\[POSS=SG3\]/+PxSg3/' $f
    sed -i -e 's/\[POSS=PL1\]/+PxPl1/' $f
    sed -i -e 's/\[POSS=PL2\]/+PxPl2/' $f
    sed -i -e 's/\[POSS=PL3\]/+PxPl3/' $f
    sed -i -e 's/\[POSS=SG3,PL3\]/+Px3/' $f
    sed -i -e 's/\[COMPOUND_FORM=S\]/+Der\/s/' $f
    sed -i -e 's/\[COMPOUND_FORM=OMIT\]/#/' $f
    sed -i -e 's/\[TENSE=PAST\]/+Past/' $f
    sed -i -e 's/\[TENSE=PRES\]/+NonPast/' $f
    sed -i -e 's/\[MOOD=INDV\]/+Ind/' $f
    sed -i -e 's/\[MOOD=IMPV\]/+Imprt/' $f
    sed -i -e 's/\[MOOD=COND\]/+Cond/' $f
    sed -i -e 's/\[MOOD=POTN\]/+Pot/' $f
    sed -i -e 's/\[MOOD=OPTV\]/+Optat/' $f
    sed -i -e 's/\[MOOD=EVNV\]/+Eventv/' $f
    sed -i -e 's/\[PRS=SG1\]/+Sg1/' $f
    sed -i -e 's/\[PRS=SG2\]/+Sg2/' $f
    sed -i -e 's/\[PRS=SG3\]/+Sg3/' $f
    sed -i -e 's/\[PRS=PL3\]/+Pl3/' $f
    sed -i -e 's/\[PRS=PL2\]/+Pl2/' $f
    sed -i -e 's/\[PRS=PL1\]/+Pl1/' $f
    sed -i -e 's/\[PRS=PE4\]//' $f
    sed -i -e 's/\[NEG=CON\]/+ConNeg/' $f
    sed -i -e 's/\[SUBCAT=NEG\]/+Neg/' $f
    sed -i -e 's/\[VOICE=ACT\]/+Act/' $f
    sed -i -e 's/\[VOICE=PSS\]/+Pss/' $f
    sed -i -e 's/\[INF=A\]/+InfA/' $f
    sed -i -e 's/\[INF=E\]/+InfE/' $f
    sed -i -e 's/\[INF=MA\]/+InfMa/' $f
    sed -i -e 's/\[DRV=MINEN\]/+Der\/minen/' $f
    sed -i -e 's/\[DRV=MAISILLA\]/+Der\/maisilla/' $f
    sed -i -e 's/\[PCP=NUT\]/+PrfPrc/' $f
    sed -i -e 's/\[PCP=MA\]/+AgentPrc/' $f
    sed -i -e 's/\[PCP=VA\]/+PrsPrc/' $f
    sed -i -e 's/\[PCP=NEG\]/+NegPrc/' $f
    sed -i -e 's/\[CMP=POS\]//' $f
    sed -i -e 's/\[CMP=CMP\]/+Comp/' $f
    sed -i -e 's/\[CMP=SUP\]/+Sup/' $f
    sed -i -e 's/\[CLIT=HAN\]/+han/' $f
    sed -i -e 's/\[CLIT=KAAN\]/+kaan/' $f
    sed -i -e 's/\[CLIT=KIN\]/+kin/' $f
    sed -i -e 's/\[CLIT=KO\]/+ko/' $f
    sed -i -e 's/\[CLIT=PAS\]/+pas/' $f
    sed -i -e 's/\[CLIT=KA\]/+ka/' $f
    sed -i -e 's/\[CLIT=S\]/+s/' $f
    sed -i -e 's/\[CLIT=PA\]/+pa/' $f
    sed -i -e 's/\[DRV=STI\]/+Der\/sti/' $f
    sed -i -e 's/\[DRV=JA\]/+Der\/ja/' $f
    sed -i -e 's/\[DRV=INEN\]/+Der\/inen/' $f
    sed -i -e 's/\[DRV=LAINEN\]/+Der\/lainen/' $f
    sed -i -e 's/\[DRV=TAR\]/+Der\/tar/' $f
    sed -i -e 's/\[DRV=LLINEN\]/+Der\/llinen/' $f
    sed -i -e 's/\[DRV=TON\]/+Der\/ton/' $f
    sed -i -e 's/\[DRV=TSE\]/+Der\/tse/' $f
    sed -i -e 's/\[DRV=OI\]/+Der\/oi/' $f
    sed -i -e 's/\[DRV=VS\]/+Der\/vs/' $f
    sed -i -e 's/\[DRV=U\]/+Der\/u/' $f
    sed -i -e 's/\[DRV=TTAA\]/+Der\/ttaa/' $f
    sed -i -e 's/\[DRV=TATTAA\]/+Der\/tattaa/' $f
    sed -i -e 's/\[DRV=TATUTTAA\]/+Der\/tatuttaa/' $f
    sed -i -e 's/\[DRV=UUS\]/+Der\/uus/' $f
    sed -i -e 's/\[DRV=TTAIN\]/+Der\/ttain/' $f
    sed -i -e 's/\[STYLE=NONSTANDARD\]/+Use\/Sub/' $f
    sed -i -e 's/\[STYLE=RARE\]/+Use\/Marg/' $f
    sed -i -e 's/\[STYLE=DIALECTAR\]/+Dial/' $f
    sed -i -e 's/\[STYLE=DIALECTAL\]/+Dial/' $f
    sed -i -e 's/\[STYLE=ARCHAIC\]/+Use\/Marg/' $f
    sed -i -e 's/\[GUESS=COMPOUND\]/+Cmp/' $f
    sed -i -e 's/\[GUESS=DERIVE\]//' $f
    sed -i -e 's/\[GUESS=URI\]//' $f
    sed -i -e 's/\[BOUNDARY=COMPOUND\]//' $f
    sed -i -e 's/\[BOUNDARY=SENTENCE\]//' $f
    sed -i -e 's/\[BOUNDARY=WORD\]//' $f
    sed -i -e 's/\[BOUNDARY=CLAUSE\]//' $f
    sed -i -e 's/\[BOUNDARY=LEXITEM\]//' $f
    sed -i -e 's/\[BOUNDARY=PARAGRAPH\]//' $f
    sed -i -e 's/\[ALLO=.\]//' $f
    sed -i -e 's/\[ALLO=..\]//' $f
    sed -i -e 's/\[ALLO=...\]//' $f
    sed -i -e 's/\[ALLO=....\]//' $f
    sed -i -e 's/\[ALLO=...\]//' $f
    sed -i -e 's/\[ALLO=..\]//' $f
    sed -i -e 's/\[ALLO=.\]//' $f
    sed -i -e 's/\[ALLO=.....\]//' $f
    sed -i -e 's/\[FILTER=NO_PROC\]//' $f
    sed -i -e "s/'_'//" $f
    sed -i -e "s/'|'//" $f
    sed -i -e "s/\[LEMMA='//" $f
    sed -i -e "s/'\]//" $f
    sed -i -e "s/\[BOUNDARY=COMPOUNDDASH\]//" $f
    for c in 1%%%0 2%%%0 3%%%0 4%%%0 5%%%0 6%%%0 7%%%0 8%%%0 ; do sed -i -e "s/\[KTN=$c\]//" $f; done
    sed -i -e "s/\[POS=SUFFIX\]//" $f
    sed -i -e "s/\[POS=PUNCTUATION\]//" $f
    sed -i -e "s/\[STYLE=SUBSTANDARD\]/+Use\/Sub/" $f
    sed -i -e "s/\[STY=NONSTANDARD\]/+Use\/Sub/" $f
    sed -i -e "s/\[SUBCAT=OPERATOR\]//" $f
    sed -i -e "s/\[POS=CONJUNCTION\]/+CC/" $f
    sed -i -e "s/\[<_io.TextIOWrapper //" $f
    sed -i -e 's/^:0//' $f
done
