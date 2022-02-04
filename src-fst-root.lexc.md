

# Morphology
The morphological division of Finnish words has three classes: verbal,
nominal and others. The verbs are identified by personal, temporal,
modal and infinite inflection. The nominals are identified by numeral and
case inflection. The others are, apart from being the rest, identified by
defective or missing inflection.

## Symbols used for analysis `Multichar_Symbols`
The Finnish morphological implementation uses analysis symbols mainly to
encode morphological analyses, the rest are implemented else where. Some
non-morphological analyses or classifications are retained for
interoperability and historical reasons. There are further details and
examples of this classification in other parts of this documents; this page
merely summarises the codes used in this version of the system.
### Parts-of-speech
The main morphological division of words is merely: Verbs, Nominals,
Rest.
The syntactic and semantic subdivision is realised in POS tags.
The nominals consist of nouns (substantiivi), adjectives, pro words
and numerals. Verbs are non-divisible, but include infinitive and particple
forms. The others are subdivided into adpositions, adverbs and particles.
Further reading: [VISK s.v.
sanaluokka|http://scripta.kotus.fi/visk/visk_termit.cgi?h_id=sCABBIDAI],
[VISK § 438](http://scripta.kotus.fi/visk/sisallys.php?p=438).

### Temporary list of added tags
These tags were added as part of a tag unification, and should be
put where they belong.

Are there tags not declared in root.lexc or misspelled?
Have a look at these:
* `+Dash		            `: XXX check this tag!
* `+Dial/Finland           `: XXX check this tag!
* `+Dial/standard          `: XXX check this tag!
* `+Gyr		            `: XXX check this tag!
* `+Pref-		            `: XXX check this tag!
* `+Pro		            `: XXX check this tag!
* `+TruncPrefix            `: XXX check this tag!
* `+Use/N		            `: XXX check this tag!
* `+Use/sub	            `: XXX check this tag!
* `+s			            `: XXX check this tag!

* **+CLBfinal**  Sentence final abbreviated expression ending in full stop, so that the full stop is ambiguous





### Parts of speech


* `+V `: Verb
* `+N `: Noun
* `+A `: Adjective
* `+ACR `: Acronym
* `+ABBR `: Abbreviation
* +Symbol = independent symbols in the text stream, like £, €, ©
* `+Acr `: Acronym
* `+Num `: Numerals
* `+Adv `: Adverb
* `+Pron `: Pronoun
* `+Pcle  `: Particle, except:
* `+Interj `: Interjection

The part-of-speech analyses are typically the first:

*Analysis examples examples:*
* *kutoo:* `kutoa+V+Act+Ind+Prs+Sg3` (Eng. # knits)
* *talo:* `talo+N+Sg+Nom` (Eng. # house)
* *nopea:* `nopea+A+Sg+Nom` (Eng. # fast)
* *yksi:* `yksi+Num+Card+Sg+Nom` (Eng. # one)
* *nopeasti:* `nopeasti+Adv` (Eng. # fast)
* *hän:* `hän+Pron+Pers+Sg+Nom` (Eng. # he)
* *ahaa:* `ahaa+Pcle` (Eng. # ah)
* *äh:* `äh+Interj` (Eng. # uh)

### Nouns
In nominal analyses, the proper nouns have additional subanalysis.
Proper nouns are usually written with initial capitals–or more recently,
totally arbitrary capitalisations, such as in brand names nVidia and ATi.
Proper nouns do have full inflectional morphology exactly as other nouns, but
work slightly differently in derivation and compounding. Some capitalised
nouns may also lose capitalisation in derivation.
[VISK § 98](http://scripta.kotus.fi/visk/sisallys.php?p=98)

The code for proper nouns:
* `+Prop `: Proper noun

Proper noun tag follows noun analysis:
* *Pekka:* `Pekka+N+Prop+Sg+Nom` (Eng. # Pekka)

### Pronouns
Pronominal analyses have some semantic classes.
[VISK § 101](http://scripta.kotus.fi/visk/sisallys.php?p=101)–104.
Codes for various semantic classes:
* `+Pers  `: Personal
* `+Dem `: Demonstrative
* `+Interr `: Interrogative
* `+Rel `: Relative
* `+Qu `: Quantor
* `+Qnt `: Quantor?????
* `+Refl `: Reflexive
* `+Recipr `: Reciprocal
* `+Indef `: Indefinite

Semantic tags follow pronoun analyses:
* *minä:* `minä+Pron+Pers+Sg+Nom` (Eng. # I)
* *tämä:* `tämä+Pron+Dem+Sg+Nom` (Eng. # this)
* *kuka:* `kuka+Pron+Interr+Sg+Nom` (Eng. # who)
* *joka:* `joka+Pron+Rel+Sg+Nom` (Eng. # which)
* *kaikki:* `kaikki+Pron+Qu+Sg+Nom` (Eng. # every)
* *itse:* `itse+Pron+Refl+Sg+Nom` (Eng. # self)
* *toistaan:* `toinen+Pron+Recipr+Sg+Par+PxSg3` (Eng. # each other)
* *joku:* `joku+Pron+Qu+Indef+Sg+Nom` (Eng. # someone)

* `+Sem/Human `: Semantic class: Human
* `+Sem/Geo `: Semantic class: Geographic
* `+Sem/Org `: Semantic class: Organisation

* `+Sem/Build		        `: 
* `+Sem/Build-room	        `: 
* `+Sem/Cat		        `: 
* `+Sem/Date		        `: 
* `+Sem/Domain		        `: 
* `+Sem/Dummytag	        `: 
* `+Sem/Event		        `: 
* `+Sem/Fem		        `: 
* `+Sem/Group_Hum	        `: 
* `+Sem/Hum		        `: 
* `+Sem/ID			        `: 
* `+Sem/Mal		        `: 
* `+Sem/Mat		        `: 
* `+Sem/Measr		        `: 
* `+Sem/Money		        `: 
* `+Sem/Obj		        `: 
* `+Sem/Obj-el		        `: 
* `+Sem/Obj-ling	        `: 
* `+Sem/Org_Prod-audio     `: 
* `+Sem/Org_Prod-vis       `: 
* `+Sem/Plc		        `: 
* `+Sem/Prod-vis	        `: 
* `+Sem/Route		        `: 
* `+Sem/Rule		        `: 
* `+Sem/State-sick	        `: 
* `+Sem/Substnc	        `: 
* `+Sem/Time-clock	        `: 
* `+Sem/Tool-it	        `: 
* `+Sem/Txt		        `: 
* `+Sem/Veh		        `: 
* `+Sem/Year		        `: 

### Numerals
In numeral analyses, there are multiple analyses.
The numerals have semantic subcategories
([VISK § 770](http://scripta.kotus.fi/visk/sisallys.php?p=770)).
The classical ordinal  numbers  have been adjectivised in current
descriptions ([VISK § 771](http://scripta.kotus.fi/visk/sisallys.php?p=771)),
the  ordinal interpretation is still spelled out in subcategories. The
numbers are often  written with digits or other specific notations.
Numeral class tags:
* `+Card  `: Cardinal
* `+Ord `: Ordinals

* *kolme:* `kolme+Num+Card+Sg+Nom` (Eng. # three)
* *kolmas:* `kolmas+Num+Ord+Sg+Nom` (Eng. # third)

### Particles
The particles are subcategorised syntax-wise into conjunctions for all words,
that govern subclauses
([VISK § 812](http://scripta.kotus.fi/visk/sisallys.php?p=812)). The
conjunctions are further divided, whether the subclause is coordinant or
subordinant to the governing clause and few other syntactic types
([VISK § 816](http://scripta.kotus.fi/visk/sisallys.php?p=816)). _N.B._ that
the division to subordinating and coordinating conjucntions is motivated
by other systems, including legacy systems, whereas the grammar presents
also different  categorisations for conjunctions (including naming
subordination adverbials).
Conjunction syntax tags:
* `+CC `: Coordinating
* `+CS `: Adverbial

The conjunction tags take place of part-of-speech tags for legacy reasons:
* *ja:* `ja+CC`
* *vaikka:* `vaikka+CS`

### Adpositions
In adposition anlayses, the syntactic tendencies are shown in sub-analyses;
whether they appear typically before or after their heads
[VISK § 687](http://scripta.kotus.fi/visk/sisallys.php?p=687).

Adposition syntax tags:
* `+Adp `: Adposition
* `+Po `: Postposition
* `+Pr `: Preposition

Adpositions are tagged in POS position:
* *läpi:* `läpi+Po`

Tags for sub-POS

* `+Arab			        `: 
* `+Attr			        `: 
* `+Coll			        `: 
* `+Rom			        `: 


### Bound root morphs
The lexical items that appear as bound morphemes before head word are
classified as prefixes ([VISK §
172|http://scripta.kotus.fi/visk/sisallys.php?p=172]). Prefixes are rare and
mostly of foreign  origin. The singular forms of plurale tantums are also
potential prefixes.

* `+Pref `: Prefixes

Suffixes are typically word forms or derivations that only appear as
bound morphs. Other than that Finnish does not really have proper suffixes.
This means that suffixed words are in effect compounds of where the last
word just doesn't appear as free morph.
* `+Suff `: Suffixes

### Symbols
Symbols are not part of linguistic data per se so we classify them according
to the needs of end user applications

* `+Punct `: any punctuation
* `+Quote `: quote marks

The analyses for symbols are like POSes:
* *.:* `.+Punct` (Eng. # .)

### Nominal analyses
The analyses of nominals show the inflection in number. Nominals inflect in
number, to mark plurality of the word. The number for nouns is either
singular or plural.
Further reading: [VISK § 79](http://scripta.kotus.fi/visk/sisallys.php?p=79)
Number tags:
* `+Sg `: Singular
* `+Pl `: Plural

Number tags are next to POSes in nominal analyses, and in order of morphs:
* *padassa:* `pata+N+Sg+Ine` (Eng. # pot)
* *padoissa:* `pata+N+Pl+Ine`

The analyses of nominals have case inflection marked.
The nominals have case inflection
([VISK § 81](http://scripta.kotus.fi/visk/sisallys.php?p=81))
to mark syntactic roles
(nominative, partitive, accusative-genitive) and semantics
(others, partially even syntactic cases).
* `+Nom `: (Mostly) Syntactic cases: Nominative
* `+Par `: Partitive
* `+Gen `: Genitive
* `+Ine `: Inner Locative cases: Inessive
* `+Ela `: Elative
* `+Ill `: Illative
* `+Ade `: Outer locative cases: Adessive
* `+Abl `: Ablative
* `+All `: Allative
* `+Ess `: Others, semantic, marginal: Essive
* `+Ins `: Instructive
* `+Abe `: Abessive
* `+Tra `: Translative
* `+Com `: Comitative

The case is next to number and last obligatory analysis in nominals:
* *taloa:* `talo+N+Sg+Par`
* *talon:* `talo+N+Sg+Gen`
* *talossa:* `talo+N+Sg+Ine`
* *talosta:* `talo+N+Sg+Ela`
* *taloon:* `talo+N+Sg+Ill`
* *talolla:* `talo+N+Sg+Ade`
* *talolta:* `talo+N+Sg+Abl`
* *talolle:* `talo+N+Sg+All`
* *talona:* `talo+N+Sg+Ess`
* *taloin:* `talo+N+Pl+Ins`
* *talotta:* `talo+N+Sg+Abe`
* *taloksi:* `talo+N+Sg+Tra`
* *taloine:* `talo+N+Com`

The analyses of a infinitive short form have lative ending; this is largely
historical ([VISK § 120](http://scripta.kotus.fi/visk/sisallys.php?p=120)).
Some adpositions might have same analysis in diachronic analyses.

* `+Lat `: Lative case

The analyses of certain nominals give explicit analysis for accusative case.
The accusative case has distinctive marker in few pronouns and these are only
cases that are analysed as accusatives.
([VISK § 81](http://scripta.kotus.fi/visk/sisallys.php?p=81)).
Other accusatives have the same case marking as genitive form, and only use
that analysis in synchronic analyses.

* `+Acc `: Explicit accusative analysis

* *hänet:* `hän+Pron+Pers+Sg+Acc`

Adverbs and adpositions may have some special analyses in diachronic
analyses.
Further reading: [VISK §
371|http://scripta.kotus.fi/visk/sisallys.php?p=371] – 385

* `+Prl `: Adverbial cases: Prolative
* `+Distr `: Distributive
* `+Tempr `: Temporal


### Possessives
The analyses of nominals include possessive if present.
Posessive ending indicates ownership. The possessive can take six possible
values from singular and plural, first, second and third person references,
where third person form is always ambiguous over plurality.
Further reading: [VISK § 95](http://scripta.kotus.fi/visk/sisallys.php?p=95)
* `+PxSg1 `: Possessives: First singular (mine)
* `+PxSg2 `: Second singular (yours)
* `+PxSg3 `: Third singular (his)
* `+PxPl1 `: First plural (ours)
* `+PxPl2 `: Second plural (yours)
* `+PxPl3 `: Third plural (theirs)
* `+Px3 `: Third ambiguous (his/theirs)

* `+Sg1 `: Verbs: First singular (I)
* `+Sg2 `: Second singular (your
* `+Sg3 `: Third singular (he)
* `+Pl1 `: First plural (we)
* `+Pl2 `: Second plural (you)
* `+Pl3 `: Third plural (thy)

* *taloni:* `talo+N+Sg+Nom+PxSg1`
* *talosi:* `talo+N+Sg+Nom+PxSg2`
* *talonsa:* `talo+N+Sg+Nom+PxSg3`
* *talomme:* `talo+N+Sg+Nom+PxPl1`
* *talonne:* `talo+N+Sg+Nom+PxPl2`

### Compound forms
In compound analyses, the derived compound form that is not a free morph
is marked with special analysis.  Some words have forms only appearing in
compounds.
Further reading: [VISK § 406](http://scripta.kotus.fi/visk/sisallys.php?p=406)
+Der/s  Compound form

* *naisien:* `nainen+N+Der/s#ien+N+Sg+Nom` (Eng. # female gum)

### Finite verbs
All verb analyses contain voice marking. For finite verb forms active voice
is tied to personal forms and passive voice to non-personal verb endings.
The voice is also marked in the infinite verb forms.
Further reading: [VISK § 110](http://scripta.kotus.fi/visk/sisallys.php?p=110)
* `+Act `: Active voice
* `+Pss `: Passive voice

It is the first analysis of verb strings:
* *kudot:* `kutoa+V+Act+Ind+Prs+Sg2`

Finite verb form analyses have a reading for tense. The tense has two values.
For moods other than indicative the tense is not distinctive in surface form,
and therefore not marked in the analyses.
The morphologically distinct forms in Finnish are only past and non-past
tenses, while other are created syntactically and not marked in morphological
analyses.
Further reading: [VISK § 111](http://scripta.kotus.fi/visk/sisallys.php?p=111)
– 112
* `+Prs `: Non-past (present)
* `+Prt `: Past (preterite)

The tense is marked in indicative forms after mood:
* *kudon:* `kutoa+V+Act+Ind+Prs+Sg1`
* *kudoin:* `kutoa+V+Act+Ind+Prt+Sg1`

Finite verb form analyses have a reading for mood. Mood has four central
readings and few archaic and marginal. The mood is marked in analyses for
all finite forms, even the unmarked indicative.
Further reading: [VISK § 115](http://scripta.kotus.fi/visk/sisallys.php?p=115)
– 118
* `+Ind `: Common moods: Indicative
* `+Cond `: Conditional
* `+Pot `: Potential
* `+Imprt `: Imperative
* `+Opt `: Archaic moods: Optative
* `+Eventv `: Eventive

The mood is after voice in the analysis string and in morph order:
* *kutonen:* `kutoa+V+Act+Pot+Sg1`

Finite verb form analyses have a reading for person. Personal ending of verb
defines the actors.  The person analysis has seven possible values,
six for the singular and plural groups of first, second and third person
forms, and one specifically for passive. The passive personal form is encoded
as fourth person passive, which had been the common practice in past systems.
Further reading: [VISK § 106](http://scripta.kotus.fi/visk/sisallys.php?p=106)
– 107
* `+Sg1 `: First singular
* `+Sg2 `: Second singular
* `+Sg3 `: Third singular
* `+Pl1 `: First plural
* `+Pl2 `: Second plural
* `+Pl3 `: Third plural

The person is the last required analysis for verbs, after the mood:
* *kudon:* `kutoa+V+Act+Ind+Prs+Sg1`
* *kudot:* `kutoa+V+Act+Ind+Prs+Sg2`
* *kutoo:* `kutoa+V+Act+Ind+Prs+Sg3`
* *kudomme:* `kutoa+V+Act+Ind+Prs+Pl1`
* *kudotte:* `kutoa+V+Act+Ind+Prs+Pl2`
* *kutovat:* `kutoa+V+Act+Ind+Prs+Pl3`

### Negation and verbs
The analyses of verb for the forms that require negation verb have a special
analysis for it.
* `+ConNeg `: Connegative form

* *kudo:* `kutoa+V+Ind+Prs+ConNeg`

The suitable negation verbs have sub-analysis that can be matched to negated
forms on syntactic level.
* `+Neg `: Negation verb

* *ei:* `ei+V+Neg+Act+Sg3`

### Infinite verb forms
Infinitive verb forms have infinitive or nominal derivation analyses.
In traditional grammars the infinitive forms were called I, II, III, IV and V
infinitive, the modern grammar replaces the first three with A, E and MA
respectively. The IV infinitive, which has *minen* suffix marker, has been
re-analysed as derivational and this is reflected in |omorfi|. The V
infinitive is also assumed to be mainly derivational, but included here for
reference.
Further reading: [VISK § 120](http://scripta.kotus.fi/visk/sisallys.php?p=120)
– 121
The infinitives have limited nominal inflection.
* `+InfA `: A infinitive (first)
* `+InfE `: E infinitive (second)
* `+InfMa `: MA infinitive (third)
* `+Der/minen `: minen derivation (fourth)
* `+Der/maisilla `: maisilla derivation (fifth)

Infinitive analysis comes after voice, followed by nominal analyses:
* *kutoa:* `kutoa+V+Act+InfA+Sg+Lat`
* *kutoessa:* `kutoa+V+Act+InfE+Sg+Ine`
* *kutomatta:* `kutoa+V+Act+InfMa+Sg+Abe`
* *kutominen:* `kutoa+V+Der/minen+Sg+Nom`
* *kutomaisillani:* `kutoa+V+Act+Der/maisilla+PxSg1`

### Participles
Participial verb forms have participle readings. There are 4 participle
forms. Like infinitives, participles in traditional grammars were named
I and II where NUT and VA are used in modern grammars.  The agent and
negation participle have sometimes been considered outside regular
inflection, but in modern Finnish grammars are alongside other participles
and so they are included in inflection in omorfi as well. In some grammars
the NUT and VA participles have been called past and present participles
respectively, drawing parallels from other languages. The modern grammar
avoids them as misleading but this description uses them
Further reading: [VISK § 122](http://scripta.kotus.fi/visk/sisallys.php?p=122)
* `+PrfPrc `: NUT participle (first, perfect)
* `+PrsPrc `: VA participle (second, present)
* `+NegPrc `: Negation participle
* `+AgPrc `: Agent partiicple

Participle analyses are right after voice, followed by adjectival analyses:
* *kutonut:* `kutoa+V+Act+PrfPrc+Sg+Nom`
* *kutova:* `kutoa+V+Act+PrsPrc+Sg+Nom`
* *kutomaton:* `kutoa+V+NegPrc+Sg+Nom`
* *kutomani:* `kutoa+V+AgPrc+Sg+Nom+PxSg1`

There are number of implementations that mix up MA infinitives and Agent
participles, and they share part of the same forms but no semantics and very
little of syntax.

### Comparation
Adjective and some adverbial analyses are marked for comparation. The
non-marked forms are comparative and superlative. For adjectives, comparative
suffixes precede the nominal inflection.
c.f. [VISK § 300](http://scripta.kotus.fi/visk/sisallys.php?p=300)
* `+Comp `: Comparative
* `+Superl `: Superlative (was +Sup, now standardised)

The comparison analysis occupies derivation spot, after POS:
* *nopeampi:* `nopea+A+Comp+Sg+Nom`
* *nopein:* `nopea+A+Sup+Sg+Nom`

### Enclitic focus particles
All word forms can have clitics which are analysed by their orthography.
Clitics are suffixes which can attach almost anywhere in the ends of words,
both verb forms and nominals. They also attach on end of other clitics,
theoretically infinite chains. In practice it is usual to see at most three
in one word form. Two clitics have limited use: -s only appears in few
verb forms and combined to other clitics and -kA only appears with few
adverbs and negation verb.
[VISK § 126](http://scripta.kotus.fi/visk/sisallys.php?p=126) – 131
* `+Foc/han `: -hAn; affirmative etc.
* `+Foc/kaan `: -kAAn; "neither"
* `+Foc/kin `: -kin; "also"
* `+Foc/pa `: -pA; "indeed"
* `+Foc/s `: -s; polite?
* `+Foc/ka `: -kA; "nor"
* `+Qst `: -kO: Question focus


### Derivation
The derivation is not a central feature of this morphology, it is mainly
used to collect new roots for dictionaries. This is roughly in order of
perceived productivity already:
* `+Der/sti `: Common derivations: A→Adv (in A manner)
* `+Der/ja `: V→N (doer of V)
* `+Der/inen `: N→A (containing N)
* `+Der/lainen `: N→A (style of N)
* `+Der/tar `: N→N (feminine N)
* `+Der/llinen `: N→N (consisting of N)
* `+Der/ton `: N→A (without N)
* `+Der/tse `: N→Adp (via N)
* `+Der/vs `: A→N (quality of A)
* `+Der/u `: V→N (act of V)
* `+Der/ttain `: N→Adv (by amounts of N)
* `+Der/ttaa `: V→V (make someone do V)
* `+Der/tattaa `: V→V (make someone do V; "first indirection")
* `+Der/tatuttaa `: V→V (make someone do V; "second indirection")
* `+Der/uus `: A→N (A-ness)
* `+Der/nti `: V→N (regular derivation from all but 2 -da/-dä V)

### Usage
The analyses of some words and word-forms indicate limitedness of usage.
This includes common mispellings, archaic words and forms
and otherwise rare words and forms. Especially, the forms that are in
parentheses in lexical sources and word-forms that had the usage annotation
in there have been carried over.
* `+Err/Orth `: Sub-standard usage
* `+Err/Hyph		        `: 
* `+Err/Lex		        `: 
* `+Err/SpaceCmp	        `: 

* `+Use/Marg `: Marginal
* `+Use/Rare  `: Rare
* `+Use/NG   `: Do not generate
* `+Use/Hyphen `: With hyphens
* `+Use/NoHyphens `: With hyphens

* **+Use/PMatch** means that the following is only used in the analyser feeding the disambiguator. This is missing.

* `+Use/-PMatch	        `: 

* `+Use/-Spell		        `: 
* `+Use/Arch		        `: 
* `+Use/SpellNoSugg        `: 

Usage tags are pushed wherever appropriate:
* *nallein:* `nalle+N+Pl+Gen+Use/Rare`

###  Homonym tags

* `+v1				        `: 
* `+v2				        `: 


### Dialects
The informal language use contains different Finnish than the literary
standard, this is marked as standard dialect (yleispuhekieli):
common features include
dropping final vowels, dropping final i components of unstressed diphtongs,
few other shortenings.
Other dialects are also sometimes analysed;
the geographical division has three levels:
East versus West,
East containing Savo and South-East (North?)
West containing North, perä, keski and eteläpohjalaiset, southwest and Häme
The third level dialect division is traditionally by "town" borders, be
cautious when adding these though; it's not the main target of this
mrophology.

* `+Dial `: any unclassified dialect
* `+Dial/Standard `: standard spoken Finnish
* `+Dial/East `: Eastern dialects
* `+Dial/West `: Western dialects
* `+Dial/Southwest `: South-western dialects
* `+Dial/Häme `: Tavastian dialects
* `+Dial/Eteläpohjalaiset `: South Osthrobotnian dialects
* `+Dial/Keskipohjalaiset `: Middle Osthrobotnian dialects
* `+Dial/Peräpohjalaiset `: North Osthrobotnian dialects
* `+Dial/North `: North Finnish dialects
* `+Dial/Savo `: Savonian dialects
* `+Dial/Southeast `: South-eastern dialects


### Tags for language of unassimilated name
* `+OLang/ENG		        `: 
* `+OLang/eng		        `: is a typo, FIX
* `+OLang/FIN		        `: 
* `+OLang/NNO		        `: 
* `+OLang/NOB		        `: 
* `+OLang/RUS		        `: 
* `+OLang/SMA		        `: 
* `+OLang/SME		        `: 
* `+OLang/SWE		        `: 
* `+OLang/UND		        `: 

### Others
The boundaries of compounds that are not lexicalised in the dictionary will
have compound analyses, the compounds may also have usage tags. The
compounding analyses concern also syntagmatic melting mishmash.
+Use/Circ     Compound boundary

* **+Cmp** - Dynamic compound. This tag should always be part
           of a dynamic compound. It is important for
           Apertium, and useful in other cases as well.
* `+Cmp/Hyph		        `: 
* `+CmpNP/None		        `: 

The word and morpheme boundaries are used to limit the effective range of
far-reaching rules, such as vowel harmony. The boundaries are marked by
curly bracketed hashes or underscores. The word boundaries are marked by #,
The lexical item boundaries by ##, the inflectional morpheme boundaries
by >, the derivational morpheme boundaries by », and some etymological and
soft boundaries by _.
* `## `: Lexical boundary
* `#` word boundary
* `>` inflectional morph boundary
* `»` derivational morph boundary
* _ weak boundary



## Flag diacritics
We have manually optimised the structure of our lexicon using following
flag diacritics to restrict morhpological combinatorics - only allow compounds
with verbs if the verb is further derived into a noun again:
|  @P.NeedNoun.ON@ | (Dis)allow compounds with verbs unless nominalised
|  @D.NeedNoun.ON@ | (Dis)allow compounds with verbs unless nominalised
|  @C.NeedNoun@ | (Dis)allow compounds with verbs unless nominalised

|  @C.ErrOrth@
|  @D.ErrOrth.ON@
|  @P.ErrOrth.ON@


For languages that allow compounding, the following flag diacritics are needed
to control position-based compounding restrictions for nominals. Their use is
handled automatically if combined with +CmpN/xxx tags. If not used, they will
do no harm.
|  @P.CmpFrst.FALSE@ | Require that words tagged as such only appear first
|  @D.CmpPref.TRUE@ | Block such words from entering ENDLEX
|  @P.CmpPref.FALSE@ | Block these words from making further compounds
|  @D.CmpLast.TRUE@ | Block such words from entering R
|  @D.CmpNone.TRUE@ | Combines with the next tag to prohibit compounding
|  @U.CmpNone.FALSE@ | Combines with the prev tag to prohibit compounding
|  @P.CmpOnly.TRUE@ | Sets a flag to indicate that the word has passed R
|  @D.CmpOnly.FALSE@ | Disallow words coming directly from root.

Use the following flag diacritics to control downcasing of derived proper
nouns (e.g. Finnish Pariisi -> pariisilainen). See e.g. North Sámi for how to use
these flags. There exists a ready-made regex that will do the actual down-casing
given the proper use of these flags.
|  @U.Cap.Obl@ | Allowing downcasing of derived names: deatnulasj.
|  @U.Cap.Opt@ | Allowing downcasing of derived names: deatnulasj.


The start of the dictionary `Root`
The Finnish morphological description starts from any of the parts of speech
dictionaries, prefix or hyphenated suffix

*Parts-of-speech examples:*
* *talo:* `talo+N+Sg+Nom` (Eng. # house)
* *nopea:* `nopea+A+Sg+Nom` (Eng. # fast)
* *kutoa:* `kutoa+V+Act+InfA+Sg+Lat` (Eng. # to knit)
* * *
<small>This (part of) documentation was generated from [../src/fst/root.lexc](http://github.com/giellalt/lang-fin/blob/main/../src/fst/root.lexc)</small>