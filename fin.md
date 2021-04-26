

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
+Der/s   Compound form

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
+Use/Circ      Compound boundary

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
## Determiners
Finnish don’t traditionally have determiners. Some claim that few
pronouns are used like determiners, and can be analysed as such.

























# Continuation lexicons for Finnish abbreviations

 Abbreviations are shortened forms that do not inflect. They have whatever
 classification they would have if they were read out, mostly that of nouns.
 Lot of abbreviations end in a full stop, which may complicate analysis and
 tokenisation in real-world applications


*Abbreviations examples:*
* *eaa.:* `eaa.+N`
* *esim.:* `esim.+N`
* ★*esim:* `esim+N` (is not standard language)
 Some ar less classified
* *ab:* `ab+ABBR`







 * `KalPa+N:KalPa  ab-nodot-noun-itrab  ; `+N: Kalmiston pallo
 * `MyPa+N:MyPa   ab-nodot-noun-itrab   ; `+N: Myllykosken Pallo
 * `RoPS+N:RoPS   ab-nodot-noun-itrab  ; `+N: Rovaniemen palloseura
 * `jssk+N:jssk   ab-nodot-noun-itrab   ; `+N: jossakin (somewhere)
 * `jstk+N:jstk   ab-nodot-noun-itrab  ; `+N: jostakin (from)
 * `jtak+N:jtak   ab-nodot-noun-itrab   ; `+N: jotakin (to)
 * `k%:lo+N:k%:lo   ab-dot-noun-trnumab   ; `+N: kello

# Continuation lexicons for Finnish abbreviations

## Lexica for adding tags and periods

## The sublexica

### Continuation lexicons for abbrs both with and witout final period



 * **LEXICON ab-noun   **

 * **LEXICON ab-adj   **

 * **LEXICON ab-adv   **

 * **LEXICON ab-num   **

### Lexicons without final period

 * **LEXICON ab-nodot-noun   **  The bulk

 * **LEXICON ab-nodot-adj   **

 * **LEXICON ab-nodot-adv   **

 * **LEXICON ab-nodot-num   **

### Lexicons with final period

 * **LEXICON ab-dot-noun   **  This is the lexicon for abbrs that must have a period.

 * **LEXICON ab-dot-adj   **  This is the lexicon for abbrs that must have a period.

 * **LEXICON ab-dot-adv   **  This is the lexicon for abbrs that must have a period.

 * **LEXICON ab-dot-num   **  This is the lexicon for abbrs that must have a period.

 * **LEXICON ab-dot-cc   **





 * **LEXICON ab-dot-verb   **

 * **LEXICON ab-nodot-verb   **


 * **LEXICON ab-dot-IVprfprc   **


 * **LEXICON nodot-attrnomaccgen-infl   **

 * **LEXICON nodot-attr-infl   **

 * **LEXICON nodot-nomaccgen-infl   **


 * **LEXICON dot-attrnomaccgen-infl   **

 * **LEXICON dot-attr   **

 * **LEXICON dot-nomaccgen-infl   **


 * **LEXICON DOT   ** - Adds the dot to dotted abbreviations.


# Acronym classification
 Acronyms are shortenings that inflect. They all have two inflection
 patterns, one read letter by letter, and one word-by-word. They are separate
 entries in this dictionary. For example OY will have singular illatives
 _OY:hyn_ and _OY:öön_ for _yyhyn_ and _yhtiöön_ resp., although latter is
 much rarer.
 A big majority of popular acronyms in everyday use comes from English, and
 the word-based inflection is virtually non-existent and would be very
 confusing so there's no high priority for adding that.

 The first classification for acronyms should be along the final letter, then
 if the final word inflection is used, the class of that word.

*Acronyms examples:*
* *OY:hyn:* `OY+N+Sg+Ill`
* *OY:öön* `OY+N+Sg+Ill`


 * `ADSL+N:ADSL  ACRO_ÄKS  ; ` Asymmetric Digital Subscriber Line
 * `AKT+N:AKT  ACRO_EE  ;    `
 * `API+N:API  ACRO_II  ;    ` Application Programming Interface
 * `atk+N:atk  ACRO_OO  ;    ` automaattinen tekstinkäsittely
 * `BBC+N:BBC  ACRO_EE  ;    ` British Broadcasting Company
 * `BKT+N:BKT  ACRO_EE  ;    ` Brutto-Kansan-Tuote
 * `BMW+N:BMW  ACRO_EE  ;    ` Bayerische Motor W
 * `BSE+N:BSE  ACRO_EE  ;    ` 
 * `cd+N:cd  ACRO_EE  ;      ` compact disc
 * `CD+N:CD  ACRO_ÄKS  ;     ` Compact Disc
 * `CD-R+N:CD-R  ACRO_ÄKS  ; ` Compact Disc Read-Only
 * `CD-RW+N:CD-RW  ACRO_EE  ;` Compact Disc Read-Write
 * `CIA+N:CIA  ACRO_AA  ;    ` Central Intelligence Agency
 * `DDR+N:DDR  ACRO_ÄKS  ;   ` Deutsches Democratische Republic
 * `DE+N:DE  ACRO_EE  ;      ` 
 * `DFI+N:DFI  ACRO_II  ;    `
 * `DJ+N:DJ  ACRO_II  ;   ` Disc Jockey
 * `dna+N:dna  ACRO_AA  ;    ` deoxyribonucleic acid
 * `DNA+N:DNA  ACRO_AA  ;    ` Deoxyribo-Nucleic Acid
 * `DPI+N:DPI  ACRO_II  ;    ` Dots Per Inch
 * `DVD+N:DVD  ACRO_ÄKS  ;   ` Digital Versatile Disc
 * `EEC+N:EEC  ACRO_EE  ;    ` European Economy Council
 * `EKP+N:EKP  ACRO_EE  ;    ` Euroopan Keskus-Pankki
 * `EMU+N:EMU  ACRO_UU  ;    ` European Monetary Union
 * `ETY+N:ETY  ACRO_YY  ;    ` Euroopan Turvallisuus-Yhteisö
 * `EU+N:EU  ACRO_UU  ;      ` Euroopan Unioni
 * `EY+N:EY  ACRO_YY  ;      ` Euroopan Yhteisö
 * `FBI+N:FBI  ACRO_II  ;     ` Federal Bureau of Investigation
 * `FIFA+N:FIFA  ACRO_AA  ;  ` Football
 * `FTP+N:FTP  ACRO_EE  ;    ` File Transfer Protocol
 * `GM+N:GM  ACRO_ÄKS  ;     ` General Motors
 * `GPRS+N:GPRS  ACRO_ÄKS  ; ` 
 * `HIFK+N:HIFK  ACRO_OO  ;  ` Helsingfors 
 * `HIV+N:HIV  ACRO_EE  ;    ` ... Immunodeficiency Virus
 * `HJK+N:HJK  ACRO_OO  ;    ` Helsingin Jalkapallo-Klubi
 * `HKL+N:HKL  ACRO_ÄKS  ;   ` Helsingin Kaupungin Liikenne
 * `HK+N:HK  ACRO_OO  ;      ` 
 * `HS+N:HS  ACRO_ÄKS  ;     ` Helsingin Sanomat
 * `HTML+N:HTML  ACRO_ÄKS  ; ` Hyper-Text Markup Language
 * `HTTP+N:HTTP  ACRO_EE  ;  ` Hyper-Text Transfer Protocol
 * `IBM+N:IBM  ACRO_ÄKS  ;   ` International Business Machines
 * `internet-tv+N:internet-tv  ACRO_EE  ; ` Tele-Visio
 * `IP+N:IP  ACRO_EE  ;    ` Internet Protocol
 * `IRC+N:IRC  ACRO_EE  ;  ` Internet Relay Chat
 * `ISBN+N:ISBN  ACRO_ÄKS  ;   ` International Serial Book Number
 * `IT+N:IT  ACRO_EE  ;    ` Informaatio-Teknologia
 * `KHO+N:KHO  ACRO_OO  ; ` Korkein Hovi-Oikeus
 * `KOP+N:KOP  ACRO_EE  ;  ` Kansallis-Osake-Pankki
 * `lehtiö-pc+N:lehtiö-pc  ACRO_EE  ; ` Personal Computer
 * `media-pc+N:media-pc  ACRO_EE  ; ` Personal Computer
 * `MTV3+N:MTV3  ACRO_KOLME  ; ` Mainos-Tele-Visio
 * `MTV+N:MTV  ACRO_EE  ;    ` Music TeleVision
 * `NASA+N:NASA  ACRO_AA  ;    ` National Air and Space A
 * `NATO+N:NATO  ACRO_OO  ;    ` North-Atlantic T Organisation
 * `NBA+N:NBA  ACRO_AA  ;    ` National Basketball Association
 * `netti-tv+N:netti-tv  ACRO_EE  ; ` Tele-visio
 * `NHL+N:NHL  ACRO_ÄKS  ; ` National Hockey League
 * `NL+N:NL  ACRO_ÄKS  ;   ` Neuvosto-Liitto
 * `NPC+N:NPC  ACRO_EE  ;  ` Non-Player Character
 * `OK+N:OK  ACRO_OO  ;    ` not pronounced in words
 * `oyj+N:oyj  ACRO_II  ;  ` Julkinen Osake-Yhtiö (!)
 * `oy+N:oy  ACRO_YY  ;    ` Osake-Yhtiö
 * `OY+N:OY  ACRO_YY  ;    ` Osake-Yhtiö
 * `paneeli-pc+N:paneeli-pc  ACRO_EE  ;    ` Personal Computer
 * `PDF+N:PDF  ACRO_ÄKS  ; ` Portable Document Format
 * `pH+N:pH  ACRO_OO  ;    ` per Hydrogen
 * `pien-pc+N:pien-pc  ACRO_EE  ;  ` Personal Computer
 * `pinta-WWW+N:pinta-WWW  ACRO_EE  ;  ` World Wide Web
 * `PPI+N:PPI  ACRO_II  ;  ` Points Per Inch
 * `PR+N:PR  ACRO_ÄKS  ;   ` Public Relations
 * `RNA+N:RNA  ACRO_AA  ;   ` Ribonucleic Acid
 * `RSS+N:RSS  ACRO_ÄKS  ; ` R
 * `SAK+N:SAK  ACRO_OO  ;  ` Suomen Ammattiliittojen Keskusjärjestö
 * `SDP+N:SDP  ACRO_EE  ;  ` Sosiali-Demokraattinen Puolue 
 * `SGML+N:SGML  ACRO_ÄKS  ;   ` Standardised General Markup Language
 * `SKDL+N:SKDL  ACRO_ÄKS  ; ` Suomen Kristillis-Demokraattien Liitto
 * `Skp+N:Skp  ACRO_EE  ; ` Suomen keskuspankki
 * `SOK+N:SOK  ACRO_OO  ;  ` Suomen Osuus-Kauppa
 * `SPR+N:SPR  ACRO_ÄKS  ; ` Suomen Punainen Risti
 * `SQL+N:SQL  ACRO_ÄKS  ; ` Structured Query Language
 * `STT+N:STT  ACRO_EE  ;  ` Suomen Tieto-Toimisto
 * `SUL+N:SUL  ACRO_ÄKS  ; ` Suomen Urheilu-Liitto
 * `TCP/IP+N:TCP/IP  ACRO_EE  ; ` Transmission Control Protocol / Internet Protocol
 * `TCP+N:TCP  ACRO_EE  ;  ` Transmission Control Protocol
 * `TI+N:TI  ACRO_II  ;    ` Texas Instruments
 * `TM+N:TM  ACRO_ÄKS  ;   ` Tekniikan Maailma
 * `TPS+N:TPS  ACRO_ÄKS  ; ` Turun Pallo-Seura
 * `TS+N:TS  ACRO_ÄKS  ;   ` Turun Sanomat
 * `tv+N:tv  ACRO_EE  ;    ` tele-visio
 * `URL+N:URL  ACRO_ÄKS  ; ` Uniform Resource Locator
 * `USA+N:USA  ACRO_AA  ;  ` United States of America
 * `VIP+N:VIP  ACRO_EE  ;  ` Very Important Person
 * `VR+N:VR  ACRO_ÄKS  ;   ` Valtion Rautatiet
 * `VTT+N:VTT  ACRO_EE  ;  ` 
 * `VW+N:VW  ACRO_EE  ;    ` Volks Wagen
 * `WAP+N:WAP  ACRO_EE  ;  ` Wireless Application Protocol
 * `WC+N:WC  ACRO_EE  ;    ` Water Closet
 * `WHO+N:WHO  ACRO_OO  ;  ` World Health Organisation
 * `WSOY+N:WSOY  ACRO_YY  ;   ` Werner-Söderström Osake-Yhtiö
 * `WTO+N:WTO  ACRO_OO  ;  ` World Trade Organisation
 * `WWF+N:WWF  ACRO_ÄKS  ; ` World Wildlife Foundation
 * `www+N:www  ACRO_EE  ;  ` world wide web
 * `WWW+N:WWW  ACRO_EE  ;  ` World Wide Web
 * `XML+N:XML  ACRO_ÄKS  ; ` Extensible Markup Language
 * `Y2K+N:Y2K  ACRO_OO  ;  ` not spelled out
 * `YK+N:YK  ACRO_OO  ;    ` Yhdistyneet Kansakunnat
 * `YMP+N:YMP  ACRO_EE  ;  ` 
 * `YT+N:YT  ACRO_EE  ;    ` Yhteistyö?
 * `ÄO+N:ÄO  ACRO_OO  ;    ` Älykkyysosamäärä

















































































































































































# Acronym inflection
Acronyms are inflected using a colon, followed by the inflectional endings,
which depend on either last letter of the word or inflection class of the
last word of the abbreviation. The exception to the inflection scheme is the
singular nominative, which appears without colon.
Pronouncable abbreviations such as aids, hiv, kela, alko etc. are actually
counted as regular words with regular inflection patterns.
c.f. [VISK § 169](http://scripta.kotus.fi/visk/sisallys.php?p=169)


## Acronyms ending in numbers inflect like the numbers are pronounced.









































































































































































































































































































































# Adjective classification
 Adjectives are words that are inflected like nouns, with few additions. For
 adjectives, the comparative derivations are usually allowed and the
 possessive suffixes are unlikely. The syntactic adjectives that do not have
 comparative derivations are nouns, if they have nominal inflection, or
 particles, if they do not inflect. The examples you need to find the 
 correct classification are same as for nouns, with addition of comparative
 and superlative.

 The classification of adjectives combines the stem changes, the final 
 allomorph selection and the harmony. See the list from:

*Adjective examples examples:*
* *aakkostettu:* `aakkostettu+A+Sg+Nom`
* *aakkostettuja:* `aakkostettu+A+Pl+Par`
* *aakkostetut:* `aakkostettu+A+Pl+Nom`
* *aakkostetumpi:* `aakkostettu+A+Comp+Sg+Nom`
* *aakkostetumpina:* `aakkostettu+A+Comp+Pl+Ess`
* *aakkostetuin:* `aakkostettu+A+Superl+Sg+Nom`
* *aakkostetuimmat:* `aakkostettu+A+Superl+Pl+Nom`




# Adjective inflection
The adjectives are inflected like regular nouns. The only morphological
difference in adjectives compared to other nouns is higher likelihood of
comparative derivations–they are fully productive. For adjectives that do
not compare, use qualifiers classification instead.
VISK § 300



## Adjective stem variation and allomorph selection
 Adjective stems are formed like noun stems, with similar patterns. Adjectives
 have additionally the productive comparative derivations, which may have
 their own stems, particularly an e-stem for a-stem words. The examples in
 this chapter are the same set of cases as with nouns:
 singular nominative, singular essive, singular inessive, plural essive,
 plural elative, singular partitives, singular illatives, plural partitives
 plural genitives, plural illatives and the compound forms. And also the
 comparative derivations: comparative singular nominative and superlative
 singular nominative. Majority of adjeciteves are equivalent to
 corresponding noun classes, so some examples have been omitted.

### Bisyllabic / derivational adjective stems without stem variation
 The most basic adjective stems do not have any stem internal variation.
 They end in o, u, y or ö, and with some limited set of new words, e.
 This class has the fewest allomorphs. There are a number of productive
 adjective classes in this section, including all lexicalised nut 
 participle’s passives (-tu, -ty), moderative derivations (-hko, -hkö)
 and ...
 Examples follow in specific sub-classes.

 The words in this class ending in o belong to `ADJ_TUMMAHKO`, the old
 dictionaries use class ¹. The stems should be entered in dictionary like:
 ```tummahko+A:tummahko A_TUMMAHKO  ;```
 This class includes back vowel moderative
 derivations. N.B. the comparative derivation of moderatives is semantically
 awkward, but morphologically plausible.

*Adjectives 1 o examples:*
* *tummahko:* `tummahko+A+Sg+Nom` (Eng. # darkish)
* *tummahkona:* `tummahko+A+Sg+Ess`
* *tummahkossa:* `tummahko+A+Sg+Ine`
* *tummahkoina:* `tummahko+A+Pl+Ess`
* *tummahkoissa:* `tummahko+A+Pl+Ine`
* *tummahkoa:* `tummahko+A+Sg+Par`
* ★*tummahkota:* `tummahko+A+Sg+Par` (is not standard language)
* *tummahkoon:* `tummahko+A+Sg+Ill`
* ★*tummahkohon:* `tummahko+A+Sg+Ill` (is not standard language)
* ★*tummahkoseen:* `tummahko+A+Sg+Ill` (is not standard language)
* *tummahkoja:* `tummahko+A+Pl+Par`
* ★*tummahkoita:* `tummahko+A+Pl+Par` (is not standard language)
* *tummahkojen:* `tummahko+A+Pl+Gen`
* ★*tummahkoiden:* `tummahko+A+Pl+Gen` (is not standard language)
* ★*tummahkoitten:* `tummahko+A+Pl+Gen` (is not standard language)
* *tummahkoihin:* `tummahko+A+Pl+Ill`
* ★*tummahkoisiin:* `tummahko+A+Pl+Ill` (is not standard language)
* *tummahkompi:* `tummahko+A+Comp+Sg+Nom`
* *tummahkoin:* `tummahko+A+Superl+Sg+Nom`

 The stems ending in u are in class `ADJ_VALKAISTU`, and in old dictionaries
 the class is ¹. These stems should be entered in dictionary like:
 ```valkaistu+A:valkaistu A_VALKAISTU  ;```
 Common part of this class is formed by nut participle 
 passive’s _back_ vowel versions after s stem verbs:

*Adjectives 1 u examples:*
* *valkaistu:* `valkaistu+A+Sg+Nom` (Eng. # bleached)
* *valkaistua:* `valkaistu+A+Sg+Par`
* *valkaistuun:* `valkaistu+A+Sg+Ill`
* *valkaistuna:* `valkaistu+A+Sg+Ess`
* *valkaistussa:* `valkaistu+A+Sg+Ine`
* *valkaistuja:* `valkaistu+A+Pl+Par`
* *valkaistujen:* `valkaistu+A+Pl+Gen`
* *valkaistuihin:* `valkaistu+A+Pl+Ill`
* *valkaistuina:* `valkaistu+A+Pl+Ess`
* *valkaistuissa:* `valkaistu+A+Pl+Ine`
* *valkaistumpi:* `valkaistu+A+Comp+Sg+Nom`
* *valkaistuin:* `valkaistu+A+Superl+Sg+Nom`

 The stems ending in y are in class `ADJ_HÄPÄISTY`, and in old dictionaries
 the class is ¹. Common part of this class is formed by nut participle 
 passive’s _front_ vowel versions after s stem verbs:

*Adjectives 1 y examples:*
* *häpäisty:* `häpäisty+A+Sg+Nom` (Eng. # defiled)
* *häpäistyä:* `häpäisty+A+Sg+Par`
* *häpäistyyn:* `häpäisty+A+Sg+Ill`
* *häpäistynä:* `häpäisty+A+Sg+Ess`
* *häpäistyssä:* `häpäisty+A+Sg+Ine`
* *häpäistyjä:* `häpäisty+A+Pl+Par`
* *häpäistyjen:* `häpäisty+A+Pl+Gen`
* *häpäistyihin:* `häpäisty+A+Pl+Ill`
* *häpäistyinä:* `häpäisty+A+Pl+Ess`
* *häpäistyissä:* `häpäisty+A+Pl+Ine`
* *häpäistympi:* `häpäisty+A+Comp+Sg+Nom`
* *häpäistyin:* `häpäisty+A+Superl+Sg+Nom`

 The words in this class ending in ö belong to `ADJ_HÖLÖ`, the old
 dictionaries use class ¹. This class includes front vowel moderative
 derivations.

*Adjectives 1 ö examples:*
* *hölö:* `hölö+A+Sg+Nom` (Eng. # blabbermouth)
* *hölöä:* `hölö+A+Sg+Par`
* *hölöön:* `hölö+A+Sg+Ill`
* *hölönä:* `hölö+A+Sg+Ess`
* *hölössä:* `hölö+A+Sg+Ine`
* *hölöjä:* `hölö+A+Pl+Par`
* *hölöjen:* `hölö+A+Pl+Gen`
* *hölöihin:* `hölö+A+Pl+Ill`
* *hölöinä:* `hölö+A+Pl+Ess`
* *hölöissä:* `hölö+A+Pl+Ine`
* *hölömpi:* `hölö+A+Comp+Sg+Nom`
* *hölöin:* `hölö+A+Superl+Sg+Nom`

 The new words with e stem have same allomorph selection as old short
 unchanging bisyllabic u, y, o and ö stems, and no stem-internal variation.
 The classification for the back vowel variant of this class is
 `ADJ_TOOPE`, and old dictionaries used the class ⁸.

*Adjectives 8 back examples:*
* *toope:* `toope+A+Sg+Nom` (Eng. # doofus)
* *toopea:* `toope+A+Sg+Par`
* *toopeen:* `toope+A+Sg+Ill`
* *toopeja:* `toope+A+Pl+Par`
* *toopejen:* `toope+A+Pl+Gen`
* *toopeihin:* `toope+A+Pl+Ill`
* *toopempi:* `toope+A+Comp+Sg+Nom`
* *toopein:* `toope+A+Superl+Sg+Nom`
 They also have slightly greater probability for archaic form of plural
 genitive:
* *toopein:* `toope+A+Pl+Gen+Use/Rare`


 The front variation of unchanging e stems is class `@LEXNAME`, and in
 old dictionaries ⁸.

*Adjectives 8 front examples:*
* *beige:* `beige+A+Sg+Nom` (Eng. # beige)
* *beigeä:* `beige+A+Sg+Par`
* *beigeen:* `beige+A+Sg+Ill`
* *beigejä:* `beige+A+Pl+Par`
* *beigejen:* `beige+A+Pl+Gen`
* *beigeihin:* `beige+A+Pl+Ill`
* *beigempi:* `beige+A+Comp+Sg+Nom`
* *beigein:* `beige+A+Superl+Sg+Nom`


### Trisyllabic and longer non-derived adjectuve stems
 The trisyllabic and longer words with stem vowels o, u, y and ö also
 have no stem variation either, but selection of suffix allomorphs for 
 plural genitives and partitives is wider than for bisyllabic and derived
 ones.

 The o final trisyllabic stems are in class `ADJ_KOHELO`, and the old
 dictionaries used ². 

*Adjectives 2 o examples:*
* *tohelo:* `tohelo+A+Sg+Nom` (Eng. # clumsy)
* *toheloita:* `tohelo+A+Pl+Par`
* *toheloja:* `tohelo+A+Pl+Par`
* *tohelojen:* `tohelo+A+Pl+Gen`
* *toheloiden:* `tohelo+A+Pl+Gen`
* *toheloitten:* `tohelo+A+Pl+Gen`
* *tohelompi:* `tohelo+A+Comp+Sg+Nom`
* *toheloin:* `tohelo+A+Superl+Sg+Nom`

 And the trisyllabic ö stem is classified `ADJ_LÖPERÖ`.

*Adjectives 2 ö examples:*
* *löperö:* `löperö+A+Sg+Nom` (Eng. # sloppy)
* *löperöjen:* `löperö+A+Pl+Gen`
* *löperöiden:* `löperö+A+Pl+Gen`
* *löperöitten:* `löperö+A+Pl+Gen`
* *löperömpi:* `löperö+A+Comp+Sg+Nom`
* *löperöin:* `löperö+A+Superl+Sg+Nom`

### Unchanging long vowel stems
 The words with stem vowels o, u, y and ö preceded by vowels still have no
 stem variation, but use yet another pattern of allomorphs for
 singular and plural partitives and plural genitive

 The class for o final long vowel stems is `ADJ_AUTIO`, and old
 old dictionaries used ³. 

*Adjectives 3 o examples:*
* *autio:* `autio+A+Sg+Nom` (Eng. # deserted)
* *autiota:* `autio+A+Sg+Par`
* ★*autioa:* `autio+A+Sg+Par` (is not standard language)
* *autioita:* `autio+A+Pl+Par`
* *autioiden:* `autio+A+Pl+Gen`
* *autioitten:* `autio+A+Pl+Gen`
* *autiompi:* `autio+A+Comp+Sg+Nom`
* *autioin:* `autio+A+Superl+Sg+Nom`

 The front voweled stems with ö after vowels go to class `ADJ_RIIVIÖ`,
 and used the old dictionary class ³.

*Adjectives 3 ö examples:*
* *riiviö:* `riiviö+A+Sg+Nom` (Eng. # nasty)
* *riiviötä:* `riiviö+A+Sg+Par`
* *riiviöiden:* `riiviö+A+Pl+Gen`
* *riiviöitten:* `riiviö+A+Pl+Gen`
* *riiviömpi:* `riiviö+A+Comp+Sg+Nom`
* *riiviöin:* `riiviö+A+Superl+Sg+Nom`

 There are no examples of new loan word adjectives ending in long vowel
 Furthermore there are no examples of adjectives in other classes without
 stem variation yet. There are some examples of these in nouns if you need
 new classes at some point.

 The abovementioned o, u, y and ö stems as well as new e stems can all
 form combinations with gradation feature as well. Not all combinations are
 yet found for adjectives, for full reference, read the noun classes.

 The quantitative k gradations with o bisyllabic o stem use class
 `ADJ_KOLKKO`, and old dictionaries use classes ¹⁻A and ¹⁻D.

*Adjective gradations 1 k~0 o examples:*
* *kolkko:* `kolkko+A+Sg+Nom` (Eng. # gloomy)
* *kolkossa:* `kolkko+A+Sg+Ine`
* *kolkoista:* `kolkko+A+Pl+Ela`
* *kolkkoihin:* `kolkko+A+Pl+Ill`
* *kolkompi:* `kolkko+A+Comp+Sg+Nom`
* *kolkoin:* `kolkko+A+Superl+Sg+Nom`


 The quantitative k gradations with u bisyllabic o stem use class
 `ADJ_VIRKKU`, and old dictionaries use classes ¹⁻A and ¹⁻D.

*Adjective gradations 1 k~0 u examples:*
* *virkku:* `virkku+A+Sg+Nom` (Eng. # ???)
* *virkussa:* `virkku+A+Sg+Ine`
* *virkuista:* `virkku+A+Pl+Ela`
* *virkkuihin:* `virkku+A+Pl+Ill`
* *virkumpi:* `virkku+A+Comp+Sg+Nom`
* *virkuin:* `virkku+A+Superl+Sg+Nom`


 The quantitative k gradations with y bisyllabic o stem use class
 `ADJ_SÄIKKY`, and old dictionaries use classes ¹⁻A and ¹⁻D.

*Adjective gradations 1 k~0 y examples:*
* *säikky:* `säikky+A+Sg+Nom` (Eng. # ???)
* *säikyssä:* `säikky+A+Sg+Ine`
* *säikyistä:* `säikky+A+Pl+Ela`
* *säikkyihin:* `säikky+A+Pl+Ill`
* *säikympi:* `säikky+A+Comp+Sg+Nom`
* *säikyin:* `säikky+A+Superl+Sg+Nom`

 The quantitative k gradations with o bisyllabic ö stem use class
 `ADJ_KÖKKÖ`, and old dictionaries use classes ¹⁻A and ¹⁻D.

*Adjective gradations 1 k~0 ö examples:*
* *kökkö:* `kökkö+A+Sg+Nom` (Eng. # ???)
* *kökössä:* `kökkö+A+Sg+Ine`
* *kököistä:* `kökkö+A+Pl+Ela`
* *kökköihin:* `kökkö+A+Pl+Ill`
* *kökömpi:* `kökkö+A+Comp+Sg+Nom`
* *kököin:* `kökkö+A+Superl+Sg+Nom`

 there is no unvarying e final adjective with _k ~ 0_ gradation.

 The quantitative gradation of p before o is in class `ADJ_SUIPPO` and
 old dictionaries would use ¹⁻B.

*Adjective gradations 1 p~0 o examples:*
* *suippo:* `suippo+A+Sg+Nom` (Eng. # ?)
* *suipossa:* `suippo+A+Sg+Ine`
* *suipoista:* `suippo+A+Pl+Ela`
* *suippoihin:* `suippo+A+Pl+Ill`
* *suipompi:* `suippo+A+Comp+Sg+Nom`
* *suipoin:* `suippo+A+Superl+Sg+Nom`


 The quantitative gradation of p before u is in class `ADJ_IKÄLOPPU` and
 old dictionaries would use ¹⁻B.
 It is only a nominal compound based adjective that ends in u and has
 p ~ 0 gradation here:

*Adjective gradations 1 p~0 u examples:*
* *ikäloppu:* `ikäloppu+A+Sg+Nom` (Eng. # age-old)
* *ikälopussa:* `ikäloppu+A+Sg+Ine`
* *ikälopuista:* `ikäloppu+A+Pl+Ela`
* *ikäloppuihin:* `ikäloppu+A+Pl+Ill`
* *ikälopumpi:* `ikäloppu+A+Comp+Sg+Nom`
* *ikälopuin:* `ikäloppu+A+Superl+Sg+Nom`


 and none of the adjectives end in y and quantitative p gradation.

 The quantitative gradation of p before ö is in class `ADJ_LÖRPPÖ` and
 old dictionaries would use ¹⁻B.

*Adjective gradations 1 p~0 ö examples:*
* *lörppö:* `lörppö+A+Sg+Nom` (Eng. # blabbermouth)
* *lörpössä:* `lörppö+A+Sg+Ine`
* *lörpöistä:* `lörppö+A+Pl+Ela`
* *lörppöihin:* `lörppö+A+Pl+Ill`
* *lörpömpi:* `lörppö+A+Comp+Sg+Nom`
* *lörpöin:* `lörppö+A+Superl+Sg+Nom`

 The quantitative gradation of t before o is in class `ADJ_VELTTO`, 
 which was ¹⁻C in the dictionary.

*Adjective gradations 1 t~0 o examples:*
* *veltto:* `veltto+A+Sg+Nom` (Eng. # lazy)
* *veltossa:* `veltto+A+Sg+Ine`
* *veltoista:* `veltto+A+Pl+Ela`
* *velttoihin:* `veltto+A+Pl+Ill`
* *veltompi:* `veltto+A+Comp+Sg+Nom`
* *veltoin:* `veltto+A+Superl+Sg+Nom`


 The quantitative gradation of t before u is in class `ADJ_VIMMATTU`, 
 which was ¹⁻C in the dictionary. 
 The u stems with quantitative t gradation are commonest with nut participle
 passive derivation’s back form (-ttu).

*Adjective gradations 1 t~0 u examples:*
* *vimmattu:* `vimmattu+A+Sg+Nom` (Eng. # wicked)
* *vimmatussa:* `vimmattu+A+Sg+Ine`
* *vimmatuista:* `vimmattu+A+Pl+Ela`
* *vimmattuihin:* `vimmattu+A+Pl+Ill`
* *vimmatumpi:* `vimmattu+A+Comp+Sg+Nom`
* *vimmatuin:* `vimmattu+A+Superl+Sg+Nom`


 The quantitative gradation of t before y is in class `ADJ_YLENNETTY`, 
 which was ¹⁻C in the dictionary. 
 The u stems with quantitative t gradation are commonest with nut participle
 passive derivation’s front (-tty).

*Adjective gradations 1 t~0 y examples:*
* *ylennetty:* `ylennetty+A+Sg+Nom` (Eng. # promoted)
* *ylennetyssä:* `ylennetty+A+Sg+Ine`
* *ylennetyistä:* `ylennetty+A+Pl+Ela`
* *ylennettyihin:* `ylennetty+A+Pl+Ill`
* *ylennetympi:* `ylennetty+A+Comp+Sg+Nom`
* *ylennetyin:* `ylennetty+A+Superl+Sg+Nom`

 The quantitative gradation of t before y is in class `ADJ_KYYTTÖ`, 
 which was ¹⁻C in the dictionary. 

*Adjective gradations 1 t~0 ö examples:*
* *kyyttö:* `kyyttö+A+Sg+Nom` (Eng. # ?????)
* *kyytössä:* `kyyttö+A+Sg+Ine`
* *kyytöistä:* `kyyttö+A+Pl+Ela`
* *kyyttöihin:* `kyyttö+A+Pl+Ill`
* *kyytömpi:* `kyyttö+A+Comp+Sg+Nom`
* *kyytöin:* `kyyttö+A+Superl+Sg+Nom`

 The quantitave k gradation has a variant that allows use of apostrophe
 instead of nothing in the weak grade.

 The class for o final bisyllabic stems with optional ’ is `ADJ_LAKO`,
 this is a subset of dictionary class ¹⁻D. 

*Adjective gradations 1 k~’~0 o examples:*
* *lako:* `lako+A+Sg+Nom`
* *laossa:* `lako+A+Sg+Ine`
* *la’ossa:* `lako+A+Sg+Ine`
* *laompi:* `lako+A+Comp+Sg+Nom`
* *laoin:* `lako+A+Superl+Sg+Nom`


 There's no k to optional apostrophe with u.
 nor with y and k:
 nor ö with k:
 There's none with k gradating to always apostrophe either.
 For examples of these, see noun classes


 The qualitative gradation of p between vowels in o stems goes to v,
 the class for this is `ADJ_KELPO`, the dictionary class for this is
 ¹⁻E.

*Adjective gradations 1 p~v o examples:*
* *kelpo:* `kelpo+A+Sg+Nom` (Eng. # proper)
* *kelvossa:* `kelpo+A+Sg+Ine`
* *kelpoina:* `kelpo+A+Pl+Ess`
* *kelvoista:* `kelpo+A+Pl+Ela`
* *kelvompi:* `kelpo+A+Comp+Sg+Nom`
* *kelvoin:* `kelpo+A+Superl+Sg+Nom`


 There are none ending in vowel + pu
 nor with y and p
 nor ö with p.

 The gradation of t ~ d after o is in class `ADJ_MIETO`, the
 dictionary class for this is ¹⁻F.

*Adjective gradations 1 t~d o examples:*
* *mieto:* `mieto+A+Sg+Nom` (Eng. # mild)
* *miedossa:* `mieto+A+Sg+Ine`
* *mietoihin:* `mieto+A+Pl+Ill`
* *miedoista:* `mieto+A+Pl+Ela`
* *miedompi:* `mieto+A+Comp+Sg+Nom`
* *miedoin:* `mieto+A+Superl+Sg+Nom`


 The gradation of t ~ d after u is in class `ADJ_VIIPALOITU`, the
 dictionary class for this is ¹⁻F.
 The commonest t ~ d variation in u stems comes from nut participle’s
 passive’s back form (-tu).

*Adjective gradations 1 t~d u examples:*
* *viipaloitu:* `viipaloitu+A+Sg+Nom` (Eng. # sliced)
* *viipaloidussa:* `viipaloitu+A+Sg+Ine`
* *viipaloituihin:* `viipaloitu+A+Pl+Ill`
* *viipaloiduista:* `viipaloitu+A+Pl+Ela`
* *viipaloidumpi:* `viipaloitu+A+Comp+Sg+Nom`
* *viipaloiduin:* `viipaloitu+A+Superl+Sg+Nom`


 The gradation of t ~ d after u is in class `ADJ_YKSILÖITY`, the
 dictionary class for this is ¹⁻F.
 The commonest t ~ d variation in u stems comes from nut participle’s
 passive’s front form (-ty).

*Adjective gradations 1 t~d y examples:*
* *yksilöity:* `yksilöity+A+Sg+Nom` (Eng. # singled)
* *yksilöidyssä:* `yksilöity+A+Sg+Ine`
* *yksilöityihin:* `yksilöity+A+Pl+Ill`
* *yksilöidyistä:* `yksilöity+A+Pl+Ela`
* *yksilöidympi:* `yksilöity+A+Comp+Sg+Nom`
* *yksilöidyin:* `yksilöity+A+Superl+Sg+Nom`

 And there's none with t and ö

 The adjectives with -nko stem belong to class `ADJ_LENKO`, and the
 dictionary class was ¹⁻G.

*Adjective gradations 1 k~g o examples:*
* *lenko:* `lenko+A+Sg+Nom` (Eng. # crooked)
* *lengossa:* `lenko+A+Sg+Ine`
* *lengoista:* `lenko+A+Pl+Ela`
* *lenkoihin:* `lenko+A+Pl+Ill`
* *lengompi:* `lenko+A+Comp+Sg+Nom`
* *lengoin:* `lenko+A+Superl+Sg+Nom`


 There's no adjectives ending in nku
 nor with y and nk
 And ö with nk
 There are no gradating p's after m's in unchanging stems
 For all these, check the noun patterns.
 The quantitative gradation of t after l in o stems is in class 
 `ADJ_MELTO`, which corresponds to dictionary class ¹⁻I.

*Adjective gradations 1 t~l o examples:*
* *melto:* `melto+A+Sg+Nom` (Eng. # ?????)
* *mellossa:* `melto+A+Sg+Ine`
* *meltoihin:* `melto+A+Pl+Ill`
* *melloista:* `melto+A+Pl+Ela`
* *mellompi:* `melto+A+Comp+Sg+Nom`
* *melloin:* `melto+A+Superl+Sg+Nom`


 The quantitative gradation of t after l in o stems is in class 
 `ADJ_PARANNELTU`, which corresponds to dictionary class ¹⁻I.
 The common u stem after l is in nut participles passive (-tu):

*Adjective gradations 1 t~l u examples:*
* *paranneltu:* `paranneltu+A+Sg+Nom` (Eng. # embettered)
* *parannellussa:* `paranneltu+A+Sg+Ine`
* *paranneltuihin:* `paranneltu+A+Pl+Ill`
* *parannelluista:* `paranneltu+A+Pl+Ela`
* *parannellumpi:* `paranneltu+A+Comp+Sg+Nom`
* *parannelluin:* `paranneltu+A+Superl+Sg+Nom`


 The quantitative gradation of t after l in o stems is in class 
 `ADJ_VÄHÄTELTY`, which corresponds to dictionary class ¹⁻I.
 As with y and t:

*Adjective gradations 1 t~l y examples:*
* *vähätelty:* `vähätelty+A+Sg+Nom` (Eng. # belittled)
* *vähätellyssä:* `vähätelty+A+Sg+Ine`
* *vähäteltyihin:* `vähätelty+A+Pl+Ill`
* *vähätellyistä:* `vähätelty+A+Pl+Ela`
* *vähätellympi:* `vähätelty+A+Comp+Sg+Nom`
* *vähätellyin:* `vähätelty+A+Superl+Sg+Nom`

 There's no adjective ending -ltö in our lexical database.

 The quantitative gradation of t after n in o stems is in class 
 `ADJ_VENTO`, which corresponds to dictionary class ¹⁻J.

*Adjective gradations 1 t~n o examples:*
* *vento:* `vento+A+Sg+Nom` (Eng. # ?)
* *vennossa:* `vento+A+Sg+Ine`
* *ventoihin:* `vento+A+Pl+Ill`
* *vennoista:* `vento+A+Pl+Ela`
* *vennompi:* `vento+A+Comp+Sg+Nom`
* *vennoin:* `vento+A+Superl+Sg+Nom`

 The quantitative gradation of t after n in u stems is in class 
 `ADJ_PANTU`, which corresponds to dictionary class ¹⁻I.
 The common u stem after n is in nut participle’s passive’s back form (-tu):

*Adjective gradations 1 t~n u examples:*
* *pantu:* `pantu+A+Sg+Nom` (Eng. # brewed)
* *pannussa:* `pantu+A+Sg+Ine`
* *pantuihin:* `pantu+A+Pl+Ill`
* *pannuista:* `pantu+A+Pl+Ela`
* *pannumpi:* `pantu+A+Comp+Sg+Nom`
* *pannuin:* `pantu+A+Superl+Sg+Nom`

 The quantitative gradation of t after n in y stems is in class 
 `ADJ_MENTY`, which corresponds to dictionary class ¹⁻I.
 The common u stem after n is in nut participle’s passive’s front form
 (-ty):

*Adjective gradations 1 t~n y examples:*
* *menty:* `menty+A+Sg+Nom` (Eng. # belittled)
* *mennyssä:* `menty+A+Sg+Ine`
* *mentyihin:* `menty+A+Pl+Ill`
* *mennyistä:* `menty+A+Pl+Ela`
* *mennympi:* `menty+A+Comp+Sg+Nom`
* *mennyin:* `menty+A+Superl+Sg+Nom`

 There are no adjectives ending in -ntö

 The quantitative gradation of t after r in o stems is in class
 `ADJ_MARTO`, which corresponds to dictionary class ¹⁻J.

*Adjective gradations 1 t~r o examples:*
* *marto:* `marto+A+Sg+Nom` (Eng. # ???)
* *marrossa:* `marto+A+Sg+Ine`
* *martoihin:* `marto+A+Pl+Ill`
* *marroista:* `marto+A+Pl+Ela`
* *marrompi:* `marto+A+Comp+Sg+Nom`
* *marroin:* `marto+A+Superl+Sg+Nom`


 The quantitative gradation of t after r in u stems is in class
 `ADJ_PURTU`, which corresponds to dictionary class ¹⁻J.
 The common u stem after r is in nut participle’s passive’s back form (-tu):

*Adjective gradations 1 t~r u examples:*
* *purtu:* `purtu+A+Sg+Nom` (Eng. # bitten)
* *purrussa:* `purtu+A+Sg+Ine`
* *purtuihin:* `purtu+A+Pl+Ill`
* *purruista:* `purtu+A+Pl+Ela`
* *purrumpi:* `purtu+A+Comp+Sg+Nom`
* *purruin:* `purtu+A+Superl+Sg+Nom`

 The quantitative gradation of t after r in y stems is in class
 `ADJ_PIERTY`, which corresponds to dictionary class ¹⁻J.
 The common u stem after r is in nut participle’s passive’s front fomr
 (-ty):

*Adjective gradations 1 t~r y examples:*
* *pierty:* `pierty+A+Sg+Nom` (Eng. # befarted)
* *pierryssä:* `pierty+A+Sg+Ine`
* *piertyihin:* `pierty+A+Pl+Ill`
* *pierryistä:* `pierty+A+Pl+Ela`
* *pierrympi:* `pierty+A+Comp+Sg+Nom`
* *pierryin:* `pierty+A+Superl+Sg+Nom`

 There are no adjectives ending in -rtö. Just as well, the
 class for UkU : UvU- is limited to few nouns we know.

### The special illative alternation with k gradation, unaltering stems
 The trisyllabic words ending with gradating long k have plural illative
 in both strong and weak forms. 


 The class for trisyllabic -kko stems is `ADJ_HUPAKKO`, the corresponding
 dictionary class is ⁴⁻D. 

*Adjectives 4 o examples:*
* *hupakko:* `hupakko+A+Sg+Nom` (Eng. # silly girl)
* *hupakoihin:* `hupakko+A+Pl+Ill`
* *hupakkoihin:* `hupakko+A+Pl+Ill`
* *hupakompi:* `hupakko+A+Comp+Sg+Nom`
* *hupakoin:* `hupakko+A+Superl+Sg+Nom`

### Adjective stems with i:e variations
 The i stems of new i final words have i : e : 0 variation. These classes
 include new loans ending in consonant, which use -i to form inflectional
 stems.
 The i stems combined with gradation of will form five separate stem
 variants:

 The i finals with back vowel harmony go to class `ADJ_ABNORMI`, where
 old dictionary classification was ⁵.

*Adjectives 5 back examples:*
* *abnormi:* `abnormi+A+Sg+Nom` (Eng. # abnorm)
* *abnormeina:* `abnormi+A+Pl+Ess`
* *abnormien:* `abnormi+A+Pl+Gen`
* *abnormimpi:* `abnormi+A+Comp+Sg+Nom`
* *abnormein:* `abnormi+A+Superl+Sg+Nom`

 The i finals with front vowel harmony go to class `ADJ_STYDI`, where
 old dictionary classification was ⁵.

*Adjectives 5 front examples:*
* *stydi:* `stydi+A+Sg+Nom` (Eng. # stiff)
* *stydeinä:* `stydi+A+Pl+Ess`
* *stydien:* `stydi+A+Pl+Gen`
* *stydimpi:* `stydi+A+Comp+Sg+Nom`
* *stydein:* `stydi+A+Superl+Sg+Nom`

 Stems with quantitative k gradation, i final and back harmony are in class 
 `ADJ_OPAAKKI` and dictionary class ⁵⁻A or ⁵⁻D.

*Adjectives 5 k~0 back examples:*
* *opaakki:* `opaakki+A+Sg+Nom` (Eng. # opaque)
* *opaakissa:* `opaakki+A+Sg+Ine`
* *opaakkeina:* `opaakki+A+Pl+Ess`
* *opaakeista:* `opaakki+A+Pl+Ela`
* *opaakkien:* `opaakki+A+Pl+Gen`
* *opaakimpi:* `opaakki+A+Comp+Sg+Nom`
* *opaakein:* `opaakki+A+Superl+Sg+Nom`

 Stems with quantitative k gradation, i final and front harmony are in class
 `ADJ_PINKKI` and dictionary class ⁵⁻A or ⁵⁻D.

*Adjectives 5 k~0 front examples:*
* *pinkki:* `pinkki+A+Sg+Nom` (Eng. # pink)
* *pinkissä:* `pinkki+A+Sg+Ine`
* *pinkkeinä:* `pinkki+A+Pl+Ess`
* *pinkeistä:* `pinkki+A+Pl+Ela`
* *pinkkien:* `pinkki+A+Pl+Gen`
* *pinkimpi:* `pinkki+A+Comp+Sg+Nom`
* *pinkein:* `pinkki+A+Superl+Sg+Nom`

 There's no back vowel version of the bisyllabic gradating -ppi form.

 Stems with quantitative p gradation, i final and front harmony are in class
 `ADJ_SIPPI` and dictionary class ⁵⁻B.

*Adjectives 5 p~0 front examples:*
* *sippi:* `sippi+A+Sg+Nom` (Eng. # bust)
* *sipissä:* `sippi+A+Sg+Ine`
* *sippeinä:* `sippi+A+Pl+Ess`
* *sipeistä:* `sippi+A+Pl+Ela`
* *sippien:* `sippi+A+Pl+Gen`
* *sipimpi:* `sippi+A+Comp+Sg+Nom`
* *sipein:* `sippi+A+Superl+Sg+Nom`

 Stems with quantitative t gradation, i final and back harmony are in class
 `ADJ_HURTTI` and dictionary class ⁵⁻C.

*Adjectives 5 t~0 back examples:*
* *hurtti:* `hurtti+A+Sg+Nom` (Eng. # grunchy)
* *hurtissa:* `hurtti+A+Sg+Ine`
* *hurtteina:* `hurtti+A+Pl+Ess`
* *hurteista:* `hurtti+A+Pl+Ela`
* *hurttien:* `hurtti+A+Pl+Gen`
* *hurtimpi:* `hurtti+A+Comp+Sg+Nom`
* *hurtein:* `hurtti+A+Superl+Sg+Nom`

 Stems with quantitative t gradation, i final and front harmony are in class
 `ADJ_VÄÄRTTI` and dictionary class ⁵⁻C.

*Adjectives 5 t~0 front examples:*
* *väärtti:* `väärtti+A+Sg+Nom` (Eng. # worthy)
* *väärtissä:* `väärtti+A+Sg+Ine`
* *väärtteinä:* `väärtti+A+Pl+Ess`
* *väärteistä:* `väärtti+A+Pl+Ela`
* *väärttien:* `väärtti+A+Pl+Gen`
* *väärtimpi:* `väärtti+A+Comp+Sg+Nom`
* *väärtein:* `väärtti+A+Superl+Sg+Nom`

 There are no bisyllabic adjectives ending in vowel and gradating -pi.

 Stems with t ~ d gradation, i final and back harmony are in class
 `ADJ_TUHTI` and dictionary class ⁵⁻F.

*Adjectives 5 t~d back examples:*
* *tuhti:* `tuhti+A+Sg+Nom` (Eng. # stocky)
* *tuhdissa:* `tuhti+A+Sg+Ine`
* *tuhteina:* `tuhti+A+Pl+Ess`
* *tuhdeista:* `tuhti+A+Pl+Ela`
* *tuhtien:* `tuhti+A+Pl+Gen`
* *tuhdimpi:* `tuhti+A+Comp+Sg+Nom`
* *tuhdein:* `tuhti+A+Superl+Sg+Nom`

 Stems with t ~ d gradation, i final and front harmony are in class
 `ADJ_REHTI` and dictionary class ⁵⁻F.

*Adjectives 5 t~d front examples:*
* *rehti:* `rehti+A+Sg+Nom`
* *rehdissä:* `rehti+A+Sg+Ine`
* *rehteinä:* `rehti+A+Pl+Ess`
* *rehdeistä:* `rehti+A+Pl+Ela`
* *rehtien:* `rehti+A+Pl+Gen`
* *rehdimpi:* `rehti+A+Comp+Sg+Nom`
* *rehdein:* `rehti+A+Superl+Sg+Nom`

 There are no adjectives with i stems with other gradations.

### Trisyllabic and longer i stems

 The i stems with trisyllabic allomorph sets have class `ADJ_ABNORMAALI`, and
 dictionary class of ⁶.

*Adjectives 6 back examples:*
* *abnormaali:* `abnormaali+A+Sg+Nom` (Eng. # abnormal)
* *abnormaaleja:* `abnormaali+A+Pl+Par`
* *abnormaaleiden:* `abnormaali+A+Pl+Gen`
* *abnormaalimpi:* `abnormaali+A+Comp+Sg+Nom`
* *abnormaalein:* `abnormaali+A+Superl+Sg+Nom`

 The i stems with trisyllabic allomorph sets have class `ADJ_ÖYKKÄRI`, and
 dictionary class of ⁶.

*Adjectives 6 front examples:*
* *öykkäri:* `öykkäri+A+Sg+Nom` (Eng. # chav)
* *öykkärejä:* `öykkäri+A+Pl+Par`
* *öykkäreiden:* `öykkäri+A+Pl+Gen`
* *öykkärimpi:* `öykkäri+A+Comp+Sg+Nom`
* *öykkärein:* `öykkkäri+A+Superl+Sg+Nom`


 There are no adjectives acting like nouns where i-final nominatives have
 singular e stems.

### Bisyllabic A-stem adjectives
 The a stems differ from regular nouns in one more feature: some of them,
 but not all, have a:e variation before the comparative stem. The selection
 of this feature may be phonological, but it is complex, and often there
 is variation between speakers. The plural partitives and genitives are a
 good indicator of classificaion of a, ä final stems, as are the vowels
 of comparative and plural stems.

 Bisyllabic a stems with e comparative and j plurals are in class 
 `ADJ_AAVA`, and dictionary class ⁹.

*Adjectives 9 examples:*
* *aava:* `aava+A+Sg+Nom` (Eng. # open (of fields, plains))
* *aavaa:* `aava+A+Sg+Par`
* *aavana:* `aava+A+Sg+Ess`
* *aavassa:* `aava+A+Sg+Ine`
* *aavoina:* `aava+A+Pl+Ess`
* *aavojen:* `aava+A+Pl+Gen`
* *aavoihin:* `aava+A+Pl+Ill`
* *aavempi:* `aava+A+Comp+Sg+Nom`
* *aavoin:* `aava+A+Superl+Sg+Nom`

 The ka stem with e comparative and j plurals is `ADJ_TARKKA`, and the 
 dictionary class is ⁹-A or ⁹⁻D.

*Adjectives 9 k~0 examples:*
* *tarkka:* `tarkka+A+Sg+Nom` (Eng. # pedantic)
* *tarkkaa:* `tarkka+A+Sg+Par`
* *tarkkana:* `tarkka+A+Sg+Ess`
* *tarkassa:* `tarkka+A+Sg+Ine`
* *tarkkoina:* `tarkka+A+Pl+Ess`
* *tarkkojen:* `tarkka+A+Pl+Gen`
* *tarkkoihin:* `tarkka+A+Pl+Ill`
* *tarkempi:* `tarkka+A+Comp+Sg+Nom`
* *tarkoin:* `tarkka+Superl+Sg+Nom`












 No a final adjectives with quantitative p gradation.

 The ta stem with j plurals is `ADJ_MATTA`, and the dictionary class
 is ⁹-C.

*Adjectives 9 t~0 examples:*
* *matta:* `matta+A+Sg+Nom` (Eng. # matta)
* *mattaa:* `matta+A+Sg+Par`
* *mattana:* `matta+A+Sg+Ess`
* *matassa:* `matta+A+Sg+Ine`
* *mattoina:* `matta+A+Pl+Ess`
* *mattojen:* `matta+A+Pl+Gen`
* *mattoihin:* `matta+A+Pl+Ill`
* *matempi:* `matta+A+Comp+Sg+Nom`
* *matoin:* `matta+A+Superl+Sg+Nom`


 The pa : va stem with e comparative and j plurals is `ADJ_HALPA`, and
 the dictionary class is ⁹⁻E.

*Adjectives 9 p~v examples:*
* *halpa:* `halpa+A+Sg+Nom` (Eng. # cheap)
* *halpaa:* `halpa+A+Sg+Par`
* *halpana:* `halpa+A+Sg+Ess`
* *halvassa:* `halpa+A+Sg+Ine`
* *halpoina:* `halpa+A+Pl+Ess`
* *halpojen:* `halpa+A+Pl+Gen`
* *halpoihin:* `halpa+A+Pl+Ill`
* *halvempi:* `halpa+A+Comp+Sg+Nom`
* *halvoin:* `halpa+A+Superl+Sg+Nom`

 The ta : da stem with a comparative and j plurals is `ADJ_EHTA`, and
 the dictionary class is ⁹⁻F.

*Adjectives 9 t~d examples:*
* *ehta:* `ehta+A+Sg+Nom` (Eng. # legit)
* *ehtaa:* `ehta+A+Sg+Par`
* *ehtana:* `ehta+A+Sg+Ess`
* *ehdassa:* `ehta+A+Sg+Ine`
* *ehtoina:* `ehta+A+Pl+Ess`
* *ehtojen:* `ehta+A+Pl+Gen`
* *ehtoihin:* `ehta+A+Pl+Ill`
* *ehdampi:* `ehta+A+Comp+Sg+Nom`
* *ehdoin:* `ehta+A+Superl+Sg+Nom`

 None with k:g gradation.

 The pa : ma stem with e comparative and j plurals is `ADJ_RAMPA`, and
 the dictionary class is ⁹⁻H.

*Adjectives 9 p~m examples:*
* *rampa:* `rampa+A+Sg+Nom` (Eng. # crippled)
* *rampaa:* `rampa+A+Sg+Par`
* *rampana:* `rampa+A+Sg+Ess`
* *rammassa:* `rampa+A+Sg+Ine`
* *rampoina:* `rampa+A+Pl+Ess`
* *rampojen:* `rampa+A+Pl+Gen`
* *rampoihin:* `rampa+A+Pl+Ill`
* *rammempi:* `rampa+A+Comp+Sg+Nom`
* *rammoin:* `rampa+A+Superl+Sg+Nom`

 No a stems with t:l gradations

 The ta : na stem with a comparative and j plurals is `ADJ_VIHANTA`, and
 the dictionary class is ⁹⁻J.

*Adjectives 9 p~n examples:*
* *vihanta:* `vihanta+A+Sg+Nom` (Eng. # blooming)
* *vihantaa:* `vihanta+A+Sg+Par`
* *vihantana:* `vihanta+A+Sg+Ess`
* *vihannassa:* `vihanta+A+Sg+Ine`
* *vihantoina:* `vihanta+A+Pl+Ess`
* *vihantojen:* `vihanta+A+Pl+Gen`
* *vihantoihin:* `vihanta+A+Pl+Ill`
* *vihannampi:* `vihanta+A+Comp+Sg+Nom`
* *vihannoin:* `vihanta+A+Superl+Sg+Nom`

 And finally, no a stems with t:r or t:l gradations.

### Some other trisyllabic a finals with a : 0 plurals
 Mostly regular a comparatives, no a : o variation and more syllables. 
 Common for va participles, and other derivations

 The a : 0 stem is in class `ADJ_AALTOILEVA` and the old dictionary used ¹⁰
 as the paradigm class.

*Adjectives 10 back examples:*
* *aaltoileva:* `aaltoileva+A+Sg+Nom` (Eng. # wavy)
* *aaltoilevaa:* `aaltoileva+A+Sg+Par`
* *aaltoilevana:* `aaltoileva+A+Sg+Ess`
* *aaltoilevassa:* `aaltoileva+A+Sg+Ine`
* *aaltoilevina:* `aaltoileva+A+Pl+Ess`
* *aaltoilevien:* `aaltoileva+A+Pl+Gen`
* *aaltoileviin:* `aaltoileva+A+Pl+Ill`
* *aaltoilevampi:* `aaltoileva+A+Comp+Sg+Nom`
* *aaltoilevin:* `aaltoileva+A+Superl+Sg+Nom`

 The ä : 0 stem is in class `ADJ_TYÖLLISTETTÄVÄ` and the old dictionary used ¹⁰
 as the paradigm class.

*Adjectives 10 front examples:*
* *työllistettävä:* `työllistettävä+A+Sg+Nom` (Eng. # employable)
* *työllistettävää:* `työllistettävä+A+Sg+Par`
* *työllistettävänä:* `työllistettävä+A+Sg+Ess`
* *työllistettävässä:* `työllistettävä+A+Sg+Ine`
* *työllistettävinä:* `työllistettävä+A+Pl+Ess`
* *työllistettävien:* `työllistettävä+A+Pl+Gen`
* *työllistettäviin:* `työllistettävä+A+Pl+Ill`
* *työllistettävämpi:* `työllistettävä+A+Comp+Sg+Nom`
* *työllistettävin:* `työllistettävä+A+Superl+Sg+Nom`

 For a:e comparatives in a:0 class use `ADJ_RUMA`. No dictionary
 classification or ~¹⁰, 

*Adjectives 10 a:e comparative examples:*
* *ruma:* `ruma+A+Sg+Nom` (Eng. # ugly)
* *rumaa:* `ruma+A+Sg+Par`
* *rumana:* `ruma+A+Sg+Ess`
* *rumassa:* `ruma+A+Sg+Ine`
* *rumina:* `ruma+A+Pl+Ess`
* *rumien:* `ruma+A+Pl+Gen`
* *rumiin:* `ruma+A+Pl+Ill`
* *rumempi:* `ruma+A+Comp+Sg+Nom`
* *rumin:* `ruma+A+Superl+Sg+Nom`

 For ä:e comparatives in a:0 class use `ADJ_TYHMÄ`. No dictionary
 classification or ~¹⁰, 

*Adjectives 10 ä:e comparative examples:*
* *tyhmä:* `tyhmä+A+Sg+Nom` (Eng. # stupid)
* *tyhmää:* `tyhmä+A+Sg+Par`
* *tyhmänä:* `tyhmä+A+Sg+Ess`
* *tyhmässä:* `tyhmä+A+Sg+Ine`
* *tyhminä:* `tyhmä+A+Pl+Ess`
* *tyhmien:* `tyhmä+A+Pl+Gen`
* *tyhmiin:* `tyhmä+A+Pl+Ill`
* *tyhmempi:* `tyhmä+A+Comp+Sg+Nom`
* *tyhmin:* `tyhmä+A+Superl+Sg+Nom`






THE EPSILON-to-ZILCH SEEMS TO INTERFER WITH COMPILATION 2015-08-23, Jaska
0:  >> 0:0 SO TOMMI FINDS THEM















THE EPSILON-to-ZILCH SEEMS TO INTERFER WITH COMPILATION 2015-08-23, Jaska
0:  >> 0:0 SO TOMMI FINDS THEM







*Adjectives 10 k~0 a:e comparative examples:*
* *hoikka:* `hoikka+A+Sg+Nom` (Eng. # slim)
* *hoikkaa:* `hoikka+A+Sg+Par`
* *hoikkana:* `hoikka+A+Sg+Ess`
* *hoikassa:* `hoikka+A+Sg+Ine`
* *hoikkina:* `hoikka+A+Pl+Ess`
* *hoikissa:* `hoikka+A+Pl+Ine`
* *hoikkien:* `hoikka+A+Pl+Gen`


*Adjectives 10 k~0 ä:e comparative examples:*
* *mykkä:* `mykkä+A+Sg+Nom` (Eng. # mute)
* *mykkää:* `mykkä+A+Sg+Par`
* *mykkänä:* `mykkä+A+Sg+Ess`
* *mykässä:* `mykkä+A+Sg+Ine`
* *mykkinä:* `mykkä+A+Pl+Ess`
* *mykissä:* `mykkä+A+Pl+Ine`
* *mykkien:* `mykkä+A+Pl+Gen`


*Adjectives 10 p~0 a:e comparative examples:*
* *poppa:* `poppa+A+Sg+Nom` (Eng. # hocus-pocus)
* *poppaa:* `poppa+A+Sg+Par`
* *poppana:* `poppa+A+Sg+Ess`
* *popassa:* `poppa+A+Sg+Ine`
* *poppina:* `poppa+A+Pl+Ess`
* *popissa:* `poppa+A+Pl+Ine`
* *poppien:* `poppa+A+Pl+Gen`


*Adjectives 10 p~0 ä:e comparative examples:*
* *hömppä:* `hömppä+A+Sg+Nom` (Eng. # bogus)
* *hömppää:* `hömppä+A+Sg+Par`
* *hömppänä:* `hömppä+A+Sg+Ess`
* *hömpässä:* `hömppä+A+Sg+Ine`
* *hömppinä:* `hömppä+A+Pl+Ess`
* *hömpissä:* `hömppä+A+Pl+Ine`
* *hömppien:* `hömppä+A+Pl+Gen`

 The quantitative k and t gradations are not found for adjectives with this
 a stem.


*Adjectives 10 p~v a:e comparative examples:*
* *voipa:* `voipa+A+Sg+Nom` (Eng. # capable)
* *voipaa:* `voipa+A+Sg+Par`
* *voipana:* `voipa+A+Sg+Ess`
* *voivassa:* `voipa+A+Sg+Ine`
* *voipina:* `voipa+A+Pl+Ess`
* *voivissa:* `voipa+A+Pl+Ine`
* *voipien:* `voipa+A+Pl+Gen`


*Adjectives 10 p~v ä:e comparative examples:*
* *käypä:* `käypä+A+Sg+Nom` (Eng. # "good enough")
* *käypää:* `käypä+A+Sg+Par`
* *käypänä:* `käypä+A+Sg+Ess`
* *käyvässä:* `käypä+A+Sg+Ine`
* *käypinä:* `käypä+A+Pl+Ess`
* *käyvissä:* `käypä+A+Pl+Ine`
* *käypien:* `käypä+A+Pl+Gen`

 The t:d is missing from this a stem.


*Adjectives 10 t~d ä:e comparative examples:*
* *mätä:* `mätä+A+Sg+Nom` (Eng. # rotten)
* *mätää:* `mätä+A+Sg+Par`
* *mätänä:* `mätä+A+Sg+Ess`
* *mädässä:* `mätä+A+Sg+Ine`
* *mätinä:* `mätä+A+Pl+Ess`
* *mädissä:* `mätä+A+Pl+Ine`
* *mätien:* `mätä+A+Pl+Gen`


*Adjectives 10 k~g a:e comparative examples:*
* *sanka:* `sanka+A+Sg+Nom` (Eng. # ???)
* *sankaa:* `sanka+A+Sg+Par`
* *sankana:* `sanka+A+Sg+Ess`
* *sangassa:* `sanka+A+Sg+Ine`
* *sankina:* `sanka+A+Pl+Ess`
* *sangissa:* `sanka+A+Pl+Ine`
* *sankien:* `sanka+A+Pl+Gen`


*Adjectives 10 k~g ä:e comparative examples:*
* *vänkä:* `vänkä+A+Sg+Nom` (Eng. # goofy)
* *vänkää:* `vänkä+A+Sg+Par`
* *vänkänä:* `vänkä+A+Sg+Ess`
* *vängässä:* `vänkä+A+Sg+Ine`
* *vängissä:* `vänkä+A+Pl+Ine`
* *vänkiin:* `vänkä+A+Pl+Ill`













 p:m is missing from this a, ä stems.


*Adjectives 10 t~l a:e comparative examples:*
* *kulta:* `kulta+A+Sg+Nom` (Eng. # dear)
* *kultaa:* `kulta+A+Sg+Par`
* *kultana:* `kulta+A+Sg+Ess`
* *kullassa:* `kulta+A+Sg+Ine`
* *kultina:* `kulta+A+Pl+Ess`
* *kullissa:* `kulta+A+Pl+Ine`
* *kultien:* `kulta+A+Pl+Gen`

 T:l and t:n are missing from this a stem and t:l from ä stem.


*Adjectives 10 t~l ä:e comparative examples:*
* *lyhyenläntä:* `lyhyenläntä+A+Sg+Nom` (Eng. # shorty)
* *lyhyenläntää:* `lyhyenläntä+A+Sg+Par`
* *lyhyenläntänä:* `lyhyenläntä+A+Sg+Ess`
* *lyhyenlännässä:* `lyhyenläntä+A+Sg+Ine`
* *lyhyenläntinä:* `lyhyenläntä+A+Pl+Ess`
* *lyhyenlännissä:* `lyhyenläntä+A+Pl+Ine`
* *lyhyenläntien:* `lyhyenläntä+A+Pl+Gen`


*Adjectives 10 t~r a:e comparative examples:*
* *turta:* `turta+A+Sg+Nom` (Eng. # numb)
* *turtaa:* `turta+A+Sg+Par`
* *turtana:* `turta+A+Sg+Ess`
* *turrassa:* `turta+A+Sg+Ine`
* *turtina:* `turta+A+Pl+Ess`
* *turrissa:* `turta+A+Pl+Ine`
* *turtien:* `turta+A+Pl+Gen`

 K:j is missing.

 Certain trisyllabic or longer a stems allow a lot of allomorphs and both
 a : o : 0 variations:

*Adjectives 11 back examples:*
* *hapera:* `hapera+A+Sg+Nom` (Eng. # brittle)
* *haperia:* `hapera+A+Pl+Par`
* *haperoita:* `hapera+A+Pl+Par`
* *haperien:* `hapera+A+Pl+Gen`
* *haperoiden:* `hapera+A+Pl+Gen`
* *haperoitten:* `hapera+A+Pl+Gen`
* *haperiin:* `hapera+A+Pl+Ill`
* *haperoihin:* `hapera+A+Pl+Ill`
 Also less commonly
* *haperojen:* `hapera+A+Use/Rare+Pl+Gen`
* *haperoja:* `hapera+A+Use/Rare+Pl+Par`
* *haperain:* `hapera+A+Pl+Gen+Use/Rare`


*Adjectives 11 front examples:*
* *höppänä:* `höppänä+A+Sg+Nom` (Eng. # senile)
* *höppänää:* `höppänä+A+Sg+Par`
* *höppänänä:* `höppänä+A+Sg+Ess`
* *höppänässä:* `höppänä+A+Sg+Ine`
* *höppäninä:* `höppänä+A+Pl+Ess`
* *höppänien:* `höppänä+A+Pl+Gen`
* *höppäniin:* `höppänä+A+Pl+Ill`



*Adjectives 12 examples:*
* *harmaja:* `harmaja+A+Sg+Nom` (Eng. # ?)
* *harmajaa:* `harmaja+A+Sg+Par`
* *harmajana:* `harmaja+A+Sg+Ess`
* *harmajoina:* `harmaja+A+Pl+Ess`
* *harmajoita:* `harmaja+A+Pl+Par`

 Certain trisyllabic or longer a stems allow a lot of allomorphs and both
 a : o : 0 variations:

*Adjectives 12++ examples:*
* *latuska:* `latuska+A+Sg+Nom` (Eng. # flat)
* *latuskoita:* `latuska+A+Pl+Par`
* *latuskoja:* `latuska+A+Pl+Par`
* *latuskoiden:* `latuska+A+Pl+Gen`
* *latuskoitten:* `latuska+A+Pl+Gen`
* *latuskojen:* `latuska+A+Pl+Gen`
* *latuskoihin:* `latuska+A+Pl+Ill`

### Special illative gradation in a stems

 The a : o stem variation combines with trisyllabic class of special illatives

*Adjectives 14 back examples:*
* *hailakka:* `hailakka+A+Sg+Nom` (Eng. # pale)
* *hailakkoina:* `hailakka+A+Pl+Ess`
* *hailakoissa:* `hailakka+A+Pl+Ine`
* *hailakkoihin:* `hailakka+A+Pl+Ill`
* *hailakoihin:* `hailakka+A+Pl+Ill`


*Adjectives 14 front examples:*
* *räväkkä:* `räväkkä+A+Sg+Nom` (Eng. # flamboyant)
* *räväköihin:* `räväkkä+A+Pl+Ill`
* *räväkköihin:* `räväkkä+A+Pl+Ill`



*Adjectives 15 t~0 back examples:*
* *pohatta:* `pohatta+A+Sg+Nom` (Eng. # fat-cat)
* *pohattoina:* `pohatta+A+Pl+Ess`
* *pohatoissa:* `pohatta+A+Pl+Ine`
* *pohattoihin:* `pohatta+A+Pl+Ill`
* *pohatoihin:* `pohatta+A+Pl+Ill`

 A-final words with long vowels and syllable boundary


*Adjectives 15 oa examples:*
* *ainoa:* `ainoa+A+Sg+Nom` (Eng. # singleton)
* *ainoana:* `ainoa+A+Sg+Ess`
* *ainoina:* `ainoa+A+Pl+Ess`
* *ainoissa:* `ainoa+A+Pl+Ine`
* *ainoihin:* `ainoa+A+Pl+Ill`
* *ainoisiin:* `ainoa+A+Pl+Ill`


*Adjectives 15 ea examples:*
* *korkea:* `korkea+A+Sg+Nom` (Eng. # high)
* *korkeana:* `korkea+A+Sg+Ess`
* *korkeina:* `korkea+A+Pl+Ess`
* *korkeissa:* `korkea+A+Pl+Ine`
* *korkeihin:* `korkea+A+Pl+Ill`
* *korkeisiin:* `korkea+A+Pl+Ill`


*Adjectives 15 eä examples:*
* *järeä:* `järeä+A+Sg+Nom` (Eng. # rough)
* *järeänä:* `järeä+A+Sg+Ess`
* *järeinä:* `järeä+A+Pl+Ess`
* *järeissä:* `järeä+A+Pl+Ine`
* *järeihin:* `järeä+A+Pl+Ill`
* *järeisiin:* `järeä+A+Pl+Ill`

### Lexicalised comparatives
 Most of the lexicalised comparatives are adjectives that go to this class.
 The comparatives that are not lexicalised inflect exactly the same, though
 some versions of morphology may cut off long comparative chains.


*Adjectives 16 back examples:*
* *aiempi:* `aiempi+A+Sg+Nom` (Eng. # earlier)
* *aiempana:* `aiempi+A+Sg+Ess`
* *aiemmassa:* `aiempi+A+Sg+Ine`
* *aiempina:* `aiempi+A+Pl+Ess`
* *aiemmista:* `aiempi+A+Pl+Ela`


*Adjectives 16 front examples:*
* *lähempi:* `lähempi+A+Sg+Nom` (Eng. # closer)
* *lähempänä:* `lähempi+A+Sg+Ess`
* *lähemmässä:* `lähempi+A+Sg+Ine`
* *lähempinä:* `lähempi+A+Pl+Ess`
* *lähemmistä:* `lähempi+A+Pl+Ela`

### Long vowel stems


*Adjectives 17 a examples:*
* *vapaa:* `vapaa+A+Sg+Nom` (Eng. # free)

 There are no other bisyllabic long vowel stems in adjectives


*Adjectives 18 aa examples:*
* *peeaa:* `peeaa+A+Sg+Nom` (Eng. # bust)


*Adjectives 18 uu examples:*
* *muu:* `muu+A+Sg+Nom` (Eng. # other)


*Adjectives 18 ää examples:*
* *syypää:* `syypää+A+Sg+Nom` (Eng. # guilty)

 There are no other monosyllabic long vowel stems for adjectives. For full
 listing of possibilities, see nouns.

 Some loan words inflect irregularly, either more along the written form or
 the pronunciation. 

*Adjectives 21 gay examples:*
* *gay:* `gay+A+Sg+Nom` (Eng. # gay)

 There are not many direct adjective loans in general.

### Old e stems with i nominative
 Some of the old e stems have i nominative but e as stem vowel for singular
 forms. Most of these are not adjectives though, see full listing from the
 noun pages.


*Adjectives 24~26 back examples:*
* *suuri:* `suuri+A+Sg+Nom` (Eng. # big)
THE EPSILON-to-ZILCH SEEMS TO INTERFER WITH COMPILATION 2015-08-23, Jaska
0:  >> 0:0 SO TOMMI FINDS THEM



*Adjectives 24~26 front examples:*
* *pieni:* `pieni+A+Sg+Nom` (Eng. # small)


*Adjectives 27 back examples:*
* *uusi:* `uusi+A+Sg+Nom` (Eng. # new)


*Adjectives 27 front examples:*
* *täysi:* `täysi+A+Sg+Nom` (Eng. # full)

 There are no adjective examples of other gradation variants or consonant
 cluster simplifications in this class.

### Consonant-final stems
 The consonant stems use inverted gradation if applicable, that is, the 
 nominatives have end in consonants and their gradating consonants are in
 weak form. Most of these are rarer for adjectives than nouns.




*Adjectives 32 examples:*
* *tyven:* `tyven+A+Sg+Nom` (Eng. # calm (of bodies of water))

 There are no back vowel variants or gradating words in the basic e
 conjoining pattern.



*Adjectives 33 examples:*
* *avoin:* `avoin+A+Sg+Nom` (Eng. # open)

 There are no other examples of n:m final variation before conjoining e.

### Caritives
 The common case of n:m variation with conjoining a before singular stems is
 from caritive suffix -tOn, that forms adjectives productively. 



*Adjectives 34 back examples:*
* *alaston:* `alaston+A+Sg+Nom` (Eng. # naked)


*Adjectives 34 0~t back examples:*
* *viaton:* `viaton+A+Sg+Nom` (Eng. # innocent)


*Adjectives 34 0~t front examples:*
* *kyvytön:* `kyvytön+A+Sg+Nom` (Eng. # unskilled)

 This one word, hapan, also takes the same variation as normative variant.
 The expected e variant is not normative, but used.


*Adjectives 34 0~p back examples:*
* *hapan:* `hapan+A+Sg+Nom` (Eng. # sour)


*Adjectives 34 m~p front examples:*
* *lämmin:* `lämmin+A+Sg+Nom` (Eng. # warm)

### Lexicalised superlatives


*Adjectives 35 back examples:*
* *uloin:* `uloin+A+Sg+Nom` (Eng. # outermost)


*Adjectives 35 front examples:*
* *sisin:* `sisin+A+Sg+Nom` (Eng. # innermost)

 Vasen inflects almost like superlative


*Adjectives 36 examples:*
* *vasen:* `vasen+A+Sg+Nom` (Eng. # left)

### nen suffixes
 Adjectives are commonly formed with nen derivatonns.


*Adjectives 38 back examples:*
* *aakkosellinen:* `aakkosellinen+A+Sg+Nom` (Eng. # alphabetic)




*Adjectives 38 front examples:*
* *kylmäjärkinen:* `kylmäjärkinen+A+Sg+Nom` (Eng. # levelminded)

### s-final adjectives

 Most of the cases here are nouns from noun derivations.


*Adjectives 39 examples:*
* *symppis:* `symppis+A+Sg+Nom` (Eng. # sympathetic)


*Adjectives 40 examples:*
* *lähteisyys:* `lähteisyys+A+Sg+Nom` (Eng. # sourceful)


*Adjectives 41 as examples:*
* *autuas:* `autuas+A+Sg+Nom` (Eng. # ignorant)


*Adjectives 41 is examples:*
* *valmis:* `valmis+A+Sg+Nom` (Eng. # ready)















*Adjectives 41 äs examples:*
* *työläs:* `työläs+A+Sg+Nom` (Eng. # studious)


*Adjectives 41 kas examples:*
* *voimakas:* `voimakas+A+Sg+Nom` (Eng. # powerfui)


*Adjectives 41 käs examples:*
* *tyylikäs:* `tyylikäs+A+Sg+Nom` (Eng. # stylish)


*Adjectives 41 pas examples:*
* *reipas:* `reipas+A+Sg+Nom`

 No adjectives end in -päs


*Adjectives 41 tas examples:*
* *rietas:* `rietas+A+Sg+Nom` (Eng. # lewd)


*Adjectives 41 tis examples:*
* *raitis:* `raitis+A+Sg+Nom` (Eng. # sober)

 Gaps.


*Adjectives 41 das examples:*
* *hidas:* `hidas+A+Sg+Nom` (Eng. # slow)

 Gaps


*Adjectives 41 ras examples:*
* *harras:* `harras+A+Sg+Nom` (Eng. # humble)

### t-finals


*Adjectives 43 back examples:*
* *ohut:* `ohut+A+Sg+Nom` (Eng. # thin)


*Adjectives 43 front examples:*
* *ehyt:* `ehyt+A+Sg+Nom` (Eng. # unbroken)

### Lexicalised nut-participles
 Majority of lexicalised nut participles are adjectives.


*Adjectives 47 back examples:*
* *kulunut:* `kulunut+A+Sg+Nom` (Eng. # used)


*Adjectives 47 front examples:*
* *ällistynyt:* `ällistynyt+A+Sg+Nom` (Eng. # amazed)

### Old -e^ final stems


*Adjectives 48 back examples:*
* *ahne:* `ahne+A+Sg+Nom` (Eng. # greedy)




*Adjectives 48 front examples:*
* *terve:* `terve+A+Sg+Nom` (Eng. # healthy)


*Adjectives 48 d~t back examples:*
* *kade:* `kade+A+Sg+Nom` (Eng. # jealous)


*Adjectives 48 l~t back examples:*
* *helle:* `helle+A+Sg+Nom` (Eng. # warm weather)

 Gapping almost all variants of gradations with e, as well as all dual
 nominative stems.

### Exceptional adjectives
 The ones that do not fit in the official classes shown in dictionaries.


*Adjective pitkä examples:*
* *pitkä:* `pitkä+A+Sg+Nom`
* *pidempi:* `pitkä+A+Comp+Sg+Nom`
* *pisin:* `pitkä+A+Superl+Sg+Nom`







THE EPSILON-to-ZILCH SEEMS TO INTERFER WITH COMPILATION 2015-08-23, Jaska
0:  >> 0:0 SO TOMMI FINDS THEM



THE EPSILON-to-ZILCH SEEMS TO INTERFER WITH COMPILATION 2015-08-23, Jaska
0:  >> 0:0 SO TOMMI FINDS THEM




### Plurales tantum?
 Adjectives aren't typically plural words, but there are some in the
 dictionaries.


*Adjective plurales examples:*
* *leuattomat:* `leuattomat+A+Pl+Nom` (Eng. # ?)





























## Adjective inflection proper



The superlative derivation is formed by in suffix, which creates a new
adjective baseform. This baseform is handled separately to avoid double
superlatives.

*Adjective superlative front examples:*
* *rumin:* `ruma+A+Superl+Sg+Nom` (Eng. # ugliest)



*Adjective superlative back examples:*
* *tyhmin:* `tyhmä+A+Superl+Sg+Nom` (Eng. # stupidest)

The comparative derivation is formed by mpi suffix, which creates a new
adjective baseform. This adjective is handled separately
to avoid double comparative forms.

*Adjective comparative front examples:*
* *rumempi:* `ruma+A+Comp+Sg+Nom`


*Adjective comparative back examples:*
* *tyhmempi:* `tyhmä+A+Comp+Sg+Nom`


This inflectional part attached to adjective comparative stems to avoid
circularity in comparative derivations:

*Adjective comparative inflection back examples:*
* *nopeampi:* `nopea+A+Comp+Sg+Nom` (Eng. # faster)
* ★*nopeammampi:* `nopea+A+Comp+Comp+Sg+Nom` (is not standard language)
* ★*nopeammammampi:* `nopea+A+Comp+Comp+Comp+Sg+Nom` (is not standard language)


*Adjective comparative inflection front examples:*
* *tyhmempänä:* `tyhmä+A+Comp+Sg+Ess`
* ★*tyhmemmämpi:* `tyhmä+A+Comp+Comp+Sg+Nom` (is not standard language)

This inflectional part is attached to adjective superlative stems to avoid
circularity in superlative  derivations:

*Adjective superlative inflection back examples:*
* *nopein:* `nopea+A+Superl+Sg+Nom`
* ★*nopeimmimpia:* `nopea+A+Superl+Superl+Sg+Par` (is not standard language)
* ★*nopeimmimmin:* `nopea+A+Superl+Superl+Superl+Sg+Nom` (is not standard language)


*Adjective superlative inflection front examples:*
* *tyhmin:* `tyhmä+A+Superl+Sg+Nom`
* ★*tyhmimmimpiä:* `tyhmä+A+Superl+Superl+Sg+Par` (is not standard language)


## Regular adjective inflection
The adjective inflection apart from the comparative and superlative
derivations is same as with nouns. I will only show examples here.


*Adjective nominative back examples:*
* *rumapa:* `ruma+A+Sg+Nom+Foc/pa`


*Adjective nominative front examples:*
* *tyhmäpä:* `tyhmä+A+Sg+Nom+Foc/pa`


*Adjective pl tant back examples:*
* *rumat:* `ruma+A+Pl+Nom`


*Adjective pl tant front examples:*
* *tyhmät:* `tyhmä+A+Pl+Nom`


*Adjective regular singular back examples:*
* *ruman:* `ruma+A+Sg+Gen`
* *rumatta:* `ruma+A+Sg+Abe`


*Adjective regular singular front examples:*
* *tyhmän:* `tyhmä+A+Sg+Gen`
* *tyhmättä:* `tyhmä+A+Sg+Abe`


*Adjective regular singular back strongs examples:*
* *rumana:* `ruma+A+Sg+Ess`
* *rumani:* `ruma+A+Sg+Nom+PxSg1`


*Adjective regular singular front strongs examples:*
* *tyhmänä:* `tyhmä+A+Sg+Ess`
* *tyhmäni:* `tyhmä+A+Sg+Nom+PxSg1`



*Adjective regular plural back examples:*
* *heikoitta:* `heikko+A+Pl+Abe`
* *heikoilta:* `heikko+A+Pl+Abl`


*Adjective regular plural front examples:*
* *jäykittä:* `jäykkä+A+Pl+Abe`
* *jäykiltä:* `jäykkä+A+Pl+Abl`


*Adjective regular plural back strong examples:*
* *jäykkinä:* `jäykkä+A+Pl+Ess`
* *jäykkine:* `jäykkä+A+Cmt`


*Adjective regular plural front strong examples:*
* *heikkoina:* `heikko+A+Pl+Ess`
* *heikkoine:* `heikko+A+Cmt`


*Adjective singular partitive a examples:*
* *tarkkaa:* `tarkka+A+Sg+Par`
* *tarkkaansa:* `tarkka+A+Sg+Par+PxSg3`


*Adjective singular partitive ä examples:*
* *tyhmää:* `tyhmä+A+Sg+Par`
* *tyhmäänsä:* `tyhmä+A+Sg+Par+PxSg3`


*Adjective singular partitive a poss aan examples:*
* *jaloa:* `jalo+A+Sg+Par`
* *jaloaan:* `jalo+A+Sg+Par+PxSg3`


*Adjective singular partitive ä poss ään examples:*
* *hölöä:* `hölö+A+Sg+Par`
* *hölöään:* `hölö+A+Sg+Par+PxSg3`


*Adjective singular partitive ta examples:*
* *vapaata:* `vapaa+A+Sg+Par`


*Adjective singular partitive tä examples:*
* *pientä:* `pieni+A+Sg+Par`


*Adjective singular illative han examples:*
* *peeaahan:* `peeaa+A+Sg+Ill`


*Adjective singular illative hin examples:*
* *gayhin:* `gay+A+Sg+Ill`


*Adjective singular illative hun examples:*
* *muuhun:* `muu+A+Sg+Ill`


*Adjective singular illative hyn examples:*
* *gayhynpä:* `gay+A+Sg+Ill+Foc/pa`


*Adjective singular illative hän examples:*
* *syypäähän:* `syypää+A+Sg+Ill`


*Adjective singular illative seen back examples:*
* *vapaaseen:* `vapaa+A+Sg+Ill`


*Adjective singular illative seen front examples:*
* *työlääseen:* `työläs+A+Sg+Ill`


*Adjective singular illative an examples:*
* *rumaan:* `ruma+A+Sg+Ill`



*Adjective singular illative en back examples:*
* *suureen:* `suuri+A+Sg+Ill`


*Adjective singular illative en front examples:*
* *pieneen:* `pieni+A+Sg+Ill`


*Adjective singular illative in back examples:*
* *tuhtiin:* `tuhti+A+Sg+Ill`


*Adjective singular illative in front examples:*
* *rehtiin:* `rehti+A+Sg+Ill`


*Adjective singular illative on examples:*
* *huonoon:* `huono+A+Sg+Ill`


*Adjective singular illative un examples:*
* *fiksuun:* `fiksu+A+Sg+Ill`


*Adjective singular illative yn examples:*
* *häijyyn:* `häijy+A+Sg+Ill`


*Adjective singular illative än examples:*
* *tyhmään:* `tyhmä+A+Sg+Ill`


*Adjective singular illative ön examples:*
* *hölmöön:* `hölmö+A+Sg+Ill`


*Adjective plural partitive ia examples:*
* *rumia:* `ruma+A+Pl+Par`


*Adjective plural partitive iä examples:*
* *tyhmiä:* `tyhmä+A+Pl+Par`


*Adjective plural partitive ita examples:*
* *korkeita:* `korkea+A+Pl+Par`


*Adjective plural partitive itä examples:*
* *järeitä:* `järeä+A+Pl+Par`


*Adjective plural partitive ja examples:*
* *vahvoja:* `vahva+A+Pl+Par`


*Adjective plural partitive jä examples:*
* *hölmöjä:* `hölmö+A+Pl+Par`


*Adjective plural genitive iden back examples:*
* *tanakoiden:* `tanakka+A+Pl+Gen`


*Adjective plural genitive iden front examples:*
* *räväköiden:* `räväkkä+A+Pl+Gen`


*Adjective plural genitive ien back examples:*
* *rumien:* `ruma+A+Pl+Gen`


*Adjective plural genitive ien front examples:*
* *tyhmien:* `tyhmä+A+Pl+Gen`


*Adjective plural genitive itten back examples:*
* *nopeitten:* `nopea+A+Pl+Gen`


*Adjective plural genitive itten front examples:*
* *järeitten:* `järeä+A+Pl+Gen`


*Adjective plural genitive jen back examples:*
* *vahvojen:* `vahva+A+Pl+Gen`


*Adjective plural genitive jen front examples:*
* *hölmöjen:* `hölmö+A+Pl+Gen`


*Adjective plural genitive ten back examples:*
* *suurten:* `suuri+A+Pl+Gen`


*Adjective plural genitive ten front examples:*
* *pienten:* `pieni+A+Pl+Gen`


*Adjective plural genitive in back examples:*
* *rumain:* `ruma+A+Pl+Gen+Use/Rare`


*Adjective plural genitive in front examples:*
* *tyhmäin:* `tyhmä+A+Pl+Gen+Use/Rare`


*Adjective plural illative ihin bacl examples:*
* *pahoihin:* `paha+A+Pl+Ill`


*Adjective plural illative ihin front examples:*
* *hölmöihin:* `hölmö+A+Pl+Ill`


*Adjective plural illative iin back examples:*
* *punaisiin:* `punainen+A+Pl+Ill`


*Adjective plural illative iin front examples:*
* *sinisiin:* `sininen+A+Pl+Ill`


*Adjective plural illative isiin back examples:*
* *korkeisiin:* `korkea+A+Pl+Ill`


*Adjective plural illative isiin front examples:*
* *järeisiin:* `järeä+A+Pl+Ill`



*Adjective possessive back examples:*
* *tyhmäni:* `tyhmä+A+Sg+Nom+PxSg1`


*Adjective possessive front examples:*
* *rumani:* `ruma+A+Sg+Nom+PxSg1`


*Adjective possessive an examples:*
* *kieroaan:* `kiero+A+Sg+Par+PxSg3`


*Adjective possessive en back examples:*
* *rumalleen:* `ruma+A+Sg+All+PxSg3`


*Adjective possessive en front examples:*
* *tyhmälleen:* `tyhmä+A+Sg+All+PxSg3`


*Adjective possessive än examples:*
* *hölmöään:* `hölmö+A+Sg+Par+PxSg3`


*Adjective clitic back examples:*
* *rumahan:* `ruma+A+Sg+Nom+Foc/han`


*Adjective clitic front examples:*
* *tyhmähän:* `tyhmä+A+Sg+Nom+Foc/han`


















Adjectives can usually be derived into sti adverbs productively

*Deadjectival adverb derivations sti examples:*
* *nopeasti* `nopea+A+Der/sti` (Eng. # fastly)






























































































# Adverb inflection
Most adverbs are morphologically either sti-derivations of adjectives or
some specific form of an existing or archaic noun, and they have limited
inflection in form of possessives and clitics carried over. According to
modern dictionaries different forms of same root are separate adverbs,
so they are not inflected here, but listed in roots.


*Adverb possessive back examples:*
* *takanani:* `takana+Adv+PxSg1` (Eng. # behind me)


*Adverb possessive front examples:*
* *edessäni:* `edessä+Adv+PxSg1` (Eng. # in front of me)


*Adverb possessive back an examples:*
* *takanaan:* `takana+Adv+PxSg3`


*Adverb possessive back en examples:*
* *jalkeilleen:* `jalkeille+Adv+PxSg3` (Eng. # he awakened)


*Adverb possessive front en examples:*
* *keskelleen:* `keskelle+Adv+PxPl3` (Eng. # amidst them)


*Adverb possessive front än examples:*
* *edessään:* `edessä+Adv+PxSg3`


*Adverb clitic back examples:*
* *nopeastihan:* `nopeasti+Adv+Foc/han` (Eng. # fast)


*Adverb clitic front examples:*
* *tyhmästihän:* `tyhmästi+Adv+Foc/han` (Eng. # stupidly)













# Adverb classification
 Adverbs are a heterogenous mass of words with defective inflectional,
 usually sourced from various forms of nominals. It would be possible to
 classify adverbs along etymology and semantics, but we do not yet use such
 classification. Only the morphology is recorded in the continuation classes
 and analyses.

 The classification of the adverbs in morphology goes along the possessives
 and clitics they take or require:

*Adverbs examples:*
* *aakkosellisesti:* `aakkosellisesti+Adv`
* *aakkosellisestikin:* `aakkosellisesti+Adv+Foc/kin`
* *mukaani:* `mukaan+Adv+PxSg1`
* *mukaamme:* `mukaan+Adv+PxPl1`
* *ajassa:* `ajassa+Adv`
* *ajassako:* `ajassa+Adv+Qst`












































Coordinating conjunctions
Coordinating conjunctions combine equal clauses and phrases. As subset of
particles, they do not inflect. The classification is solely syntactic and
semantic, but it is used in this system for compatibility with other stuff.

The coordinating conjunctions are: eli(kkä), ja, joko – tai, kuin – myös,
‑kä, mutta, niin – kuin ‑kin/ myös, sekä, sekä – että, sun; tai, vaan, vai,
ynnä, (saati), (sillä)
Further reading: [VISK § 816](http://scripta.kotus.fi/visk/sisalto.php?p=816))

*Coordinating conjunctions examples:*
* *eikä:* `eikä+CC`
* *ja:* `ja+CC`






# Adverbial conjunctions
The adverbial conjunctions join two unequal clauses or phrases together.
The traditional term for this is sub-ordinating conjunction, it is assumed
here for compatibility with other languages. Adverbial conjunctions are a
subset of particles, so they do not inflect at all.

The adverbial conjunctions are:
ellei, että, jahka, jollei, jos, joskin, jos kohta, jotta, koska, kun,
 kunhan, mikäli, vaikka, (kunnes). 
Further reading: [VISK § 818](http://scripta.kotus.fi/visk/sisalto.php?p=818)

*Adverbial conjunctions examples:*
* *että:* `että+CS`
* *jotta:* `jotta+CS`
* *koska:* `koska+CS`
* *kun:* `kun+CS`
* *jos:* `jos+CS`
* *vaikka:* `vaikka+CS`










Digit strings inflect with colons, lot like abbreviations.

The digit strings ending in digit 1 pronounced as number

* examples:*
* *1:* `1+Num+Card+Sg+Nom`
* *1:t:* `1+Num+Card+Pl+Nom`
* *1:ssä:* `1+Num+Card+Sg+Ine`

The digit strings ending in digit 2 pronounced as number
* *2:*
* *2:t:*
* *2:ssa:*

The digit strings ending in digit 3 pronounced as number
* *3:*
* *3:t:*
* *3:ssa:*

The digit strings ending in digit 4 pronounced as number
* *4:*
* *4:t:*
* *4:ssä:*

The digit strings ending in digit 5 pronounced as number
* *5:*
* *5:t:*
* *5:ssä:*

The digit strings ending in digit 6 pronounced as number
* *6:*
* *6:t:*
* *6:ss:*

The digit strings ending in digit 7 pronounced as number
* *7:*
* *7:t:*
* *7:ssä:*

The digit strings ending in digit 8 pronounced as number
* *8:*
* *8:t:*
* *8:ssa:*


The digit strings ending in digit 9 pronounced as number
* *9:*
* *9:t:*
* *9:ssä:*

The digit strings ending in digit 0 pronounced as number
* *0:*
* *0:t:*
* *0:aa:*

The digit string ending in 0 pronounced as tens
* *10:*
* *10:t:*
* *10:tä:*

The digit string ending in 00 pronounced as hundreds
* *100:*
* *100:t:*
* *100:aa:*

The digit string ending in 000 pronounced as thousands
* *1000:*
* *1000:t:*
* *1000:ta:*

The digit string ending in 0's pronounced as millions
* *1000000:*
* *1000000:t:*
* *1000000:aa:*

The digit string ending in 0's pronounced as milliards
* *1000000000:*
* *1000000000:t:*
* *1000000000:a:*

The digit string ending in 1. pronounced as first
* *1.:*
* *1:nen:*
* *1:stä:*

The digit string ending in 2. pronounced as second
* *2.:*
* *2:nen:*
* *2:sta:*

The digit string ending in 3, 6, 8, 00 or 2 pronounced as ordinal
* *3.:*
* *3:s:*
* *3:tta:*

The digit string ending in 4, 5, 7, 9, 0, or 1. pronounced as ordinal
* *4.:*
* *4:s:*
* *4:ttä:*

The roman digit string ending in II, III, VI, VIII, C, L or M
* *II:*
* *II:ta:*

The roman digit string ending in I, IV, V, VII, IX, X or 
* *I:*
* *I:tä:*

* *1:nsä:*

* *2:nsa:*

* *3:aan:*

* *2:kseen:*

* *1:kseen:*

* *5:ttään:*

* *2:han:*

* *4:hän:*







































































































































































































































































































































































Exceptions are quite strange word-forms. the ones that do not fit anywhere 
else. This file contains all enumerated word forms that cannot reasonably be
created from lexical data by regular inflection. Usually there should be next
to none exceptions, it's always better to have a paradigm that covers only
one or few words than an exception since these will not work nicely with e.g.
compounding scheme or possibly many end applications.


negation verb has partial inflection:

* examples:*
* *en:* `ei+V+Neg+Act+Sg1`
* *älkää:* `ei+V+Neg+Act+Imprt+Pl2`
* *älkööt:* `ei+V+Neg+Act+Imprt+Pl3`

Some verbs only have few word-forms left:
* *kutiaa:*
* *taita:*
* *paratkoon:*
* *eläköön:*


The noun ruoka has irregular forms:
* *ruuassa:*
* *ruuilla:*

The adjective hyvä has heteroclitic comparative derivations too:
* *parempi:*
* *paremmissa:*
* *paras:*
* *parhaat:*

Some of the nouns have archaic consonat stem forms left:
* *vuonna:*
* *sydännä:*
* *jumalten:*
* *sankarten:*

few verbs have shortened forms in standard spoken Finnish
* *meen:*
* *tuut:*






























































































# Interjections
 Interjections are mainly parts of spoken language that are minimal turns
 in dialogue, curses, onomatopoeia and such. Interjections are a subset
 of particles, and do not inflect. They are quite productive kind of, though
 limited in form  ; they stem from arbitrary combinations of characters to
 

 Only add new interjections that are found from corpora.

*Interjections examples:*
* *aah:* `aah+Interj`
* *äh:* `äh+Interj`



# Nouns and their classification
 Noun is the part-of-speech for words which require declination in number
 and case. Additionally nouns may have optional possessive suffixes and
 clitics combined freely at the end. While some of the nouns may exhibit 
 limited comparative derivations, generally words that can undergo
 comparation must be  classified into adjectives. The proper nouns that
 are written in initial  capital letters except when derived are handled
 separately under proper nouns, but the classification is the same.

 The nominals are classified by combination of the stem variations, suffix
 allomorphs and the vowel harmony. The nouns have number, case, possessive
 and clitic suffixes:

*Noun examples:*
* *talo:* `talo+N+Sg+Nom` (Eng. # house)
* *taloa:* `talo+N+Sg+Par`
* *taloissa:* `talo+N+Pl+Ine`
* *talostani:* `talo+N+Sg+Ela+PxSg1`
* *talollako:* `talo+N+Sg+Ade+Qst`
* *taloiltammepa:* `talo+N+Pl+Abl+PxPl1+Foc/pa`
 The classification is based on suffix allomorphs, harmony, and the stem
 variation:
* *taloja:* `talo+N+Pl+Par`
* *sälöjä:* `sälö+N+Pl+Par` (Eng. # splinter)
* *valtioita:* `valtio+N+Pl+Par` (Eng. # state)
* *lepakot:* `lepakko+N+Pl+Nom` (Eng. # bat)
* *padoissa:* `pato+N+Pl+Ine` (Eng. # dam)
 The minimal set to determine which paradigm or class noun belongs to is to
 check how it inflects in singulars of nominative,
 essive and inessive, plurals of essive, elative, partitive, illative, and
 GENITIVE. Find out stems and suffixes and match.








# Noun inflection and derivation




## Noun stem variation and allomorph selection
 The nominal stems were classified according to what is the stem variation
 and what allomorphs they select. This section lists all the possible
 variations and all their allomorph selections. Each class description 
 gives the key forms that can be used when classifying new words, the 
 examples of inflection and negative examples for the most obvious missed
 allomorphs and differentiating factors. The example list should be at 
 least:
 singular nominative, singular essive, singular inessive, plural essive,
 plural elative, singular partitives, singular illatives, plural partitives
 plural GENITIVEs, plural illatives and the compound forms.

### Noun stems without stem variation
 The most basic noun stem does not have any stem internal variation and
 uses few commonest allomorphs. The words in this class are either 
 bisyllabic or have one of common derivational suffixes: sto, ....
 The nouns in this class end in o, u, y or ö, which determines their 
 illative suffix and therefore exact classification:

*Noun 1o examples:*
* *talo:* `talo+N+Sg+Nom` (Eng. # house)
* *talona:* `talo+N+Sg+Ess`
* *talossa:* `talo+N+Sg+Ine`
* *taloina:* `talo+N+Pl+Ess`
* *taloista:* `talo+N+Pl+Ela`
* *taloa:* `talo+N+Sg+Par`
* ★*talota:* `talo+N+Sg+Par` (is not standard language)
* *taloon:* `talo+N+Sg+Ill`
* ★*taloseen:* `talo+N+Sg+Ill` (is not standard language)
* *taloja:* `talo+N+Pl+Par`
* ★*taloita:* `talo+N+Pl+Par` (is not standard language)
* *talojen:* `talo+N+Pl+Gen`
* ★*taloiden:* `talo+N+Pl+Gen` (is not standard language)
* ★*taloitten:* `talo+N+Pl+Gen` (is not standard language)
* *taloihin:* `talo+N+Pl+Ill`
* *talojuttu:* `talo+N+Sg+Nom#juttu+N+Sg+Nom` (Eng. # house thing)
* *talonjuttu:* `talo+N+Sg+Gen#juttu+N+Sg+Nom` (Eng. # house’s thing)
* *talojenjuttu:* `talo+N+Pl+Gen#juttu+N+Sg+Nom` (Eng. # houses’ thing)

*Nouns 1o non-standard examples:*
* *taloin:* `talo+N+Use/Rare+Pl+Gen`
* *talohon:* `talo+N+Err/Orth+Sg+Ill`

 The stems ending in u are also without variationm and the bisyllabic ones
 have the same simple allomorph pattern:

*Noun 1u examples:*
* *asu:* `asu+N+Sg+Nom` (Eng. # outfit)
* *asua:* `asu+N+Sg+Par`
* *asuun:* `asu+N+Sg+Ill`
* *asuna:* `asu+N+Sg+Ess`
* *asussa:* `asu+N+Sg+Ine`
* *asuja:* `asu+N+Pl+Par`
* *asujen:* `asu+N+Pl+Gen`
* *asuihin:* `asu+N+Pl+Ill`
* *asuina:* `asu+N+Pl+Ess`
* *asuissa:* `asu+N+Pl+Ine`

*Nouns 1u non-standard examples:*
* *asuin:* `asu+N+Use/Rare+Pl+Gen`
* *asuhun:* `asu+N+Err/Orth+Sg+Ill`




*Noun 1y examples:*
* *kärry:* `kärry+N+Sg+Nom` (Eng. # cart)
* *kärryä:* `kärry+N+Sg+Par`
* *kärryyn:* `kärry+N+Sg+Ill`
* *kärrynä:* `kärry+N+Sg+Ess`
* *kärryssä:* `kärry+N+Sg+Ine`
* *kärryjä:* `kärry+N+Pl+Par`
* *kärryjen:* `kärry+N+Pl+Gen`
* *kärryihin:* `kärry+N+Pl+Ill`
* *kärryinä:* `kärry+N+Pl+Ess`
* *kärryissä:* `kärry+N+Pl+Ine`

*Nouns 1y non-standard examples:*
* *kärryin:* `kärry+N+Use/Rare+Pl+Gen`
* *kärryhyn:* `kärry+N+Err/Orth+Sg+Ill`


*Nouns 1ö examples:*
* *mömmö:* `mömmö+N+Sg+Nom` (Eng. # gunk)
* *mömmöä:* `mömmö+N+Sg+Par`
* *mömmöön:* `mömmö+N+Sg+Ill`
* *mömmönä:* `mömmö+N+Sg+Ess`
* *mömmössä:* `mömmö+N+Sg+Ine`
* *mömmöjä:* `mömmö+N+Pl+Par`
* *mömmöjen:* `mömmö+N+Pl+Gen`
* *mömmöihin:* `mömmö+N+Pl+Ill`
* *mömmöinä:* `mömmö+N+Pl+Ess`
* *mömmöissä:* `mömmö+N+Pl+Ine`

*Nouns 1ö non-standard examples:*
* *mömmöin:* `mömmö+N+Use/Rare+Pl+Gen`
* *mömmöhön:* `mömmö+N+Err/Orth+Sg+Ill`

### Basic gradation cases
 The basic stems without variations other than consonant gradation.

*Nouns 1ko examples:*
* *ukko:* `ukko+N+Sg+Nom` (Eng. # geezer)
* *ukossa:* `ukko+N+Sg+Ine`
* *ukoista:* `ukko+N+Pl+Ela`
* *ukkoihin:* `ukko+N+Pl+Ill`

*Nouns 1ko non-standard examples:*
* *ukkoin:* `ukko+N+Use/Rare+Pl+Gen`
* *ukkohon:* `ukko+N+Err/Orth+Sg+Ill`



*Nouns 1ku examples:*
* *tikku:* `tikku+N+Sg+Nom` (Eng. # stick)
* *tikussa:* `tikku+N+Sg+Ine`
* *tikuista:* `tikku+N+Pl+Ela`
* *tikkuihin:* `tikku+N+Pl+Ill`

*Nouns 1ku non-standard examples:*
* *tikkuin:* `tikku+N+Use/Rare+Pl+Gen`
* *tikkuhun:* `tikku+N+Err/Orth+Sg+Ill`






*Nouns 1ky examples:*
* *myrkky:* `myrkky+N+Sg+Nom` (Eng. # poison)
* *myrkyssä:* `myrkky+N+Sg+Ine`
* *myrkyistä:* `myrkky+N+Pl+Ela`
* *myrkkyihin:* `myrkky+N+Pl+Ill`

*Nouns 1ky non-standard examples:*
* *myrkkyin:* `myrkky+N+Use/Rare+Pl+Gen`
* *myrkkyhyn:* `myrkky+N+Err/Orth+Sg+Ill`


*Nouns 1kö examples:*
* *yökkö:* `yökkö+N+Sg+Nom` (Eng. # nightfly)
* *yökössä:* `yökkö+N+Sg+Ine`
* *yököistä:* `yökkö+N+Pl+Ela`
* *yökköihin:* `yökkö+N+Pl+Ill`

*Nouns 1kö non-standard examples:*
* *yökköin:* `yökkö+N+Use/Rare+Pl+Gen`
* *yökköhön:* `yökkö+N+Err/Orth+Sg+Ill`


*Nouns 1po examples:*
* *happo:* `happo+N+Sg+Nom` (Eng. # acid)
* *hapossa:* `happo+N+Sg+Ine`
* *hapoista:* `happo+N+Pl+Ela`
* *happoihin:* `happo+N+Pl+Ill`

*Nouns 1po non-standard examples:*
* *happoin:* `happo+N+Use/Rare+Pl+Gen`
* *happohon:* `happo+N+Err/Orth+Sg+Ill`



*Nouns 1pu examples:*
* *lippu:* `lippu+N+Sg+Nom`
* *lipussa:* `lippu+N+Sg+Ine`
* *lipuista:* `lippu+N+Pl+Ela`
* *lippuihin:* `lippu+N+Pl+Ill`

*Nouns 1pu non-standard examples:*
* *lippuin:* `lippu+N+Use/Rare+Pl+Gen`
* *lippuhun:* `lippu+N+Err/Orth+Sg+Ill`



*Nouns 1py examples:*
* *ryyppy:* `ryyppy+N+Sg+Nom` (Eng. # swirl)
* *ryypyssä:* `ryyppy+N+Sg+Ine`
* *ryypyistä:* `ryyppy+N+Pl+Ela`
* *ryyppyihin:* `ryyppy+N+Pl+Ill`

*Nouns 1py non-standard examples:*
* *ryyppyin:* `ryyppy+N+Use/Rare+Pl+Gen`
* *ryyppyhyn:* `ryyppy+N+Err/Orth+Sg+Ill`


*Nouns 1pö examples:*
* *törppö:* `törppö+N+Sg+Nom` (Eng. # bozo)
* *törpössä:* `törppö+N+Sg+Ine`
* *törpöistä:* `törppö+N+Pl+Ela`
* *törppöihin:* `törppö+N+Pl+Ill`

*Nouns 1pö non-standard examples:*
* *törppöin:* `törppö+N+Use/Rare+Pl+Gen`
* *törppöhön:* `törppö+N+Err/Orth+Sg+Ill`


*Nouns 1to examples:*
* *hirtto:* `hirtto+N+Sg+Nom` (Eng. # hanging)
* *hirtossa:* `hirtto+N+Sg+Ine`
* *hirtoista:* `hirtto+N+Pl+Ela`
* *hirttoihin:* `hirtto+N+Pl+Ill`

*Nouns 1to non-standard examples:*
* *hirttoin:* `hirtto+N+Use/Rare+Pl+Gen`
* *hirttohon:* `hirtto+N+Err/Orth+Sg+Ill`



*Nouns 1tu examples:*
* *torttu:* `torttu+N+Sg+Nom` (Eng. # tart)
* *tortussa:* `torttu+N+Sg+Ine`
* *tortuista:* `torttu+N+Pl+Ela`
* *torttuihin:* `torttu+N+Pl+Ill`

*Nouns 1tu non-standard examples:*
* *torttuin:* `torttu+N+Use/Rare+Pl+Gen`
* *torttuhon:* `torttu+N+Err/Orth+Sg+Ill`



*Nouns 1ty examples:*
* *pytty:* `pytty+N+Sg+Nom` (Eng. # cup)
* *pytyssä:* `pytty+N+Sg+Ine`
* *pytyistä:* `pytty+N+Pl+Ela`
* *pyttyihin:* `pytty+N+Pl+Ill`

*Nouns 1ty non-standard examples:*
* *pyttyin:* `pytty+N+Use/Rare+Pl+Gen`
* *pyttyhyn:* `pytty+N+Err/Orth+Sg+Ill`


*Nouns 1tö examples:*
* *pönttö:* `pönttö+N+Sg+Nom` (Eng. # pot)
* *pöntössä:* `pönttö+N+Sg+Ine`
* *pöntöistä:* `pönttö+N+Pl+Ela`
* *pönttöihin:* `pönttö+N+Pl+Ill`

*Nouns 1tö non-standard examples:*
* *pönttöin:* `pönttö+N+Use/Rare+Pl+Gen`
* *pönttöhön:* `pönttö+N+Err/Orth+Sg+Ill`


 Between two vowels, the weak grade of k is optionally an apostrophe
 instead. For k that is not optionally ’, for example when it is after a
 consonant other than s, the variation is k ~ 0 instead (e.g. NOUN_UKKO).

*Nouns 1ko examples:*
* *teko:* `teko+N+Sg+Nom` (Eng. # action)
* *teossa:* `teko+N+Sg+Ine`
* *te’ossa:* `teko+N+Sg+Ine`

*Nouns 1ko non-standard examples:*
* *tekoin:* `teko+N+Use/Rare+Pl+Gen`
* *tekohon:* `teko+N+Err/Orth+Sg+Ill`



*Nouns 1ku examples:*
* *maku:* `maku+N+Sg+Nom` (Eng. # taste)
* *maussa:* `maku+N+Sg+Ine`
* *ma’ussa:* `maku+N+Sg+Ine`

*Nouns 1ku non-standard examples:*
* *makuin:* `maku+N+Use/Rare+Pl+Gen`
* *makuhon:* `maku+N+Err/Orth+Sg+Ill`



*Nouns 1ky examples:*
* *näky:* `näky+N+Sg+Nom` (Eng. # vision)
* *näyssä:* `näky+N+Sg+Ine`
* *nä’yssä:* `näky+N+Sg+Ine`

*Nouns 1ky non-standard examples:*
* *näkyin:* `näky+N+Use/Rare+Pl+Gen`
* *näkyhyn:* `näky+N+Err/Orth+Sg+Ill`


*Nouns 1kö examples:*
* *näkö:* `näkö+N+Sg+Nom` (Eng. # eyesight)
* *näössä:* `näkö+N+Sg+Ine`
* *nä’össä:* `näkö+N+Sg+Ine`

*Nouns 1kö non-standard examples:*
* *näköin:* `näkö+N+Use/Rare+Pl+Gen`
* *näköhön:* `näkö+N+Err/Orth+Sg+Ill`

 Between three vowels where k is surrounded by same vowels the k becomes
 obligatorily ’. When the vowels are different, it becomes optionally
 ’ (as in NOUN_TEKO), and after consonant other than s it is k ~ 0 
 (as in NOUN_UKKO).

*Nouns 1ko examples:*
* *ruoko:* `ruoko+N+Sg+Nom` (Eng. # straw)
* *ruo’ossa:* `ruoko+N+Sg+Ine`

*Nouns 1ko non-standard examples:*
* *ruokoin:* `ruoko+N+Use/Rare+Pl+Gen`
* *ruokohon:* `ruoko+N+Err/Orth+Sg+Ill`



*Nouns 1ko examples:*
* *liuku:* `liuku+N+Sg+Nom` (Eng. # slide)
* *liu’ussa:* `liuku+N+Sg+Ine`

*Nouns 1ku non-standard examples:*
* *liukuin:* `liuku+N+Use/Rare+Pl+Gen`
* *liukuhun:* `liuku+N+Err/Orth+Sg+Ill`

 There is a gap in paradigms in y and ö finals of k:’

 Other gradations can be more easily caught from the preceding context.

*Nouns 1po examples:*
* *hepo:* `hepo+N+Sg+Nom` (Eng. # horse)
* *hevossa:* `hepo+N+Sg+Ine`
* *hepoina:* `hepo+N+Pl+Ess`
* *hevoista:* `hepo+N+Pl+Ela`

*Nouns 1ku non-standard examples:*
* *hepoin:* `hepo+N+Use/Rare+Pl+Gen`
* *hepohon:* `hepo+N+Err/Orth+Sg+Ill`



*Nouns 1pu examples:*
* *apu:* `apu+N+Sg+Nom` (Eng. # help)
* *avussa:* `apu+N+Sg+Ine`
* *apuihin:* `apu+N+Pl+Ill`
* *avuista:* `apu+N+Pl+Ela`

*Nouns 1pu non-standard examples:*
* *apuin:* `apu+N+Use/Rare+Pl+Gen`
* *apuhun:* `apu+N+Err/Orth+Sg+Ill`



*Nouns 1py examples:*
* *käpy:* `käpy+N+Sg+Nom` (Eng. # pinecone)
* *kävyssä:* `käpy+N+Sg+Ine`
* *käpyihin:* `käpy+N+Pl+Ill`
* *kävyistä:* `käpy+N+Pl+Ela`

*Nouns 1py non-standard examples:*
* *käpyin:* `käpy+N+Use/Rare+Pl+Gen`
* *käpyhyn:* `käpy+N+Err/Orth+Sg+Ill`


*Nouns 1pö examples:*
* *löpö:* `löpö+N+Sg+Nom` (Eng. # gasoline)
* *lövössä:* `löpö+N+Sg+Ine`
* *löpöihin:* `löpö+N+Pl+Ill`
* *lövöistä:* `löpö+N+Pl+Ela`

*Nouns 1pö non-standard examples:*
* *löpöin:* `löpö+N+Use/Rare+Pl+Gen`
* *löpöhyn:* `löpö+N+Err/Orth+Sg+Ill`


*Nouns 1to examples:*
* *veto:* `veto+N+Sg+Nom` (Eng. # draft)
* *vedossa:* `veto+N+Sg+Ine`
* *vetoihin:* `veto+N+Pl+Ill`
* *vedoista:* `veto+N+Pl+Ela`

*Nouns 1to non-standard examples:*
* *vetoin:* `veto+N+Use/Rare+Pl+Gen`
* *vetohon:* `veto+N+Err/Orth+Sg+Ill`



*Nouns 1to examples:*
* *kuitu:* `kuitu+N+Sg+Nom` (Eng. # fiber)
* *kuidussa:* `kuitu+N+Sg+Ine`
* *kuituihin:* `kuitu+N+Pl+Ill`
* *kuiduista:* `kuitu+N+Pl+Ela`

*Nouns 1tu non-standard examples:*
* *kuituin:* `kuitu+N+Use/Rare+Pl+Gen`
* *kuituhon:* `kuitu+N+Err/Orth+Sg+Ill`



*Nouns 1ty examples:*
* *vety:* `vety+N+Sg+Nom` (Eng. # hydrogen)
* *vedyssä:* `vety+N+Sg+Ine`
* *vetyihin:* `vety+N+Pl+Ill`
* *vedyistä:* `vety+N+Pl+Ela`

*Nouns 1ty non-standard examples:*
* *vetyin:* `vety+N+Use/Rare+Pl+Gen`
* *vetyhyn:* `vety+N+Err/Orth+Sg+Ill`


*Nouns 1tö examples:*
* *häätö:* `häätö+N+Sg+Nom` (Eng. # evictment)
* *häädössä:* `häätö+N+Sg+Ine`
* *häädöistä:* `häätö+N+Pl+Ela`
* *häätöihin:* `häätö+N+Pl+Ill`

*Nouns 1tö non-standard examples:*
* *häätöin:* `häätö+N+Use/Rare+Pl+Gen`
* *häätöhön:* `häätö+N+Err/Orth+Sg+Ill`


*Nouns 1nko examples:*
* *runko:* `runko+N+Sg+Nom` (Eng. # body)
* *rungossa:* `runko+N+Sg+Ine`
* *rungoista:* `runko+N+Pl+Ela`
* *runkoihin:* `runko+N+Pl+Ill`

*Nouns 1nko non-standard examples:*
* *runkoin:* `runko+N+Use/Rare+Pl+Gen`
* *runkohon:* `runko+N+Err/Orth+Sg+Ill`



*Nouns 1nku examples:*
* *vinku:* `vinku+N+Sg+Nom` (Eng. # squeal)
* *vingussa:* `vinku+N+Sg+Ine`
* *vinguista:* `vinku+N+Pl+Ela`
* *vinkuihin:* `vinku+N+Pl+Ill`

*Nouns 1nku non-standard examples:*
* *vinkuin:* `vinku+N+Use/Rare+Pl+Gen`
* *vinkuhun:* `vinku+N+Err/Orth+Sg+Ill`



*Nouns 1nky examples:*
* *sänky:* `sänky+N+Sg+Nom` (Eng. # bed)
* *sängyssä:* `sänky+N+Sg+Ine`
* *sängyistä:* `sänky+N+Pl+Ela`
* *sänkyihin:* `sänky+N+Pl+Ill`

*Nouns 1nky non-standard examples:*
* *sänkyin:* `sänky+N+Use/Rare+Pl+Gen`
* *sänkyhyn:* `sänky+N+Err/Orth+Sg+Ill`


*Nouns 1nkö examples:*
* *ylänkö:* `ylänkö+N+Sg+Nom` (Eng. # highlands)
* *ylängössä:* `ylänkö+N+Sg+Ine`
* *ylängöistä:* `ylänkö+N+Pl+Ela`
* *ylänköihin:* `ylänkö+N+Pl+Ill`

*Nouns 1nkö non-standard examples:*
* *ylänköin:* `ylänkö+N+Use/Rare+Pl+Gen`
* *ylänköhön:* `ylänkö+N+Err/Orth+Sg+Ill`


*Nouns 1mpo examples:*
* *sampo:* `sampo+N+Sg+Nom` (Eng. # sampo)
* *sammossa:* `sampo+N+Sg+Ine`
* *sampoina:* `sampo+N+Pl+Ess`
* *sammoista:* `sampo+N+Pl+Ela`

*Nouns 1mpo non-standard examples:*
* *sampoin:* `sampo+N+Use/Rare+Pl+Gen`
* *sampohon:* `sampo+N+Err/Orth+Sg+Ill`



*Nouns 1mpu examples:*
* *rumpu:* `rumpu+N+Sg+Nom` (Eng. # drum)
* *rummussa:* `rumpu+N+Sg+Ine`
* *rumpuihin:* `rumpu+N+Pl+Ill`
* *rummuista:* `rumpu+N+Pl+Ela`

*Nouns 1mpu non-standard examples:*
* *rumpuin:* `rumpu+N+Use/Rare+Pl+Gen`
* *rumpuhun:* `rumpu+N+Err/Orth+Sg+Ill`

 There is a gap in paradigms in y finals with p:m variation


*Nouns 1mpö examples:*
* *lämpö:* `lämpö+N+Sg+Nom` (Eng. # warmth)
* *lämmössä:* `lämpö+N+Sg+Ine`
* *lämpöihin:* `lämpö+N+Pl+Ill`
* *lämmöistä:* `lämpö+N+Pl+Ela`

*Nouns 1mpö non-standard examples:*
* *lämpöin:* `lämpö+N+Use/Rare+Pl+Gen`
* *lämpöhön:* `lämpö+N+Err/Orth+Sg+Ill`


*Nouns 1lto examples:*
* *kielto:* `kielto+N+Sg+Nom` (Eng. # denial)
* *kiellossa:* `kielto+N+Sg+Ine`
* *kieltoihin:* `kielto+N+Pl+Ill`
* *kielloista:* `kielto+N+Pl+Ela`

*Nouns 1lto non-standard examples:*
* *kieltoin:* `kielto+N+Use/Rare+Pl+Gen`
* *kieltohon:* `kielto+N+Err/Orth+Sg+Ill`



*Nouns 1ltu examples:*
* *huoliteltu:* `huoliteltu+N+Sg+Nom` (Eng. # neat)
* *huolitellussa:* `huoliteltu+N+Sg+Ine`
* *huoliteltuihin:* `huoliteltu+N+Pl+Ill`
* *huolitelluista:* `huoliteltu+N+Pl+Ela`

*Nouns 1ltu non-standard examples:*
* *huoliteltuin:* `huoliteltu+N+Use/Rare+Pl+Gen`
* *huoliteltuhun:* `huoliteltu+N+Err/Orth+Sg+Ill`



*Nouns 1lty examples:*
* *epäilty:* `epäilty+N+Sg+Nom` (Eng. # suspect)
* *epäillyssä:* `epäilty+N+Sg+Ine`
* *epäiltyihin:* `epäilty+N+Pl+Ill`
* *epäillyistä:* `epäilty+N+Pl+Ela`

*Nouns 1lty non-standard examples:*
* *epäiltyin:* `epäilty+N+Use/Rare+Pl+Gen`
* *epäiltyhyn:* `epäilty+N+Err/Orth+Sg+Ill`


*Nouns 1ltö examples:*
* *sisältö:* `sisältö+N+Sg+Nom` (Eng. # content)
* *sisällössä:* `sisältö+N+Sg+Ine`
* *sisällöistä:* `sisältö+N+Pl+Ela`
* *sisältöihin:* `sisältö+N+Pl+Ill`

*Nouns 1ltö non-standard examples:*
* *sisältöin:* `sisältö+N+Use/Rare+Pl+Gen`
* *sisältöhön:* `sisältö+N+Err/Orth+Sg+Ill`


*Nouns 1nto examples:*
* *tunto:* `tunto+N+Sg+Nom` (Eng. # sense)
* *tunnossa:* `tunto+N+Sg+Ine`
* *tuntoihin:* `tunto+N+Pl+Ill`
* *tunnoista:* `tunto+N+Pl+Ela`

*Nouns 1nto non-standard examples:*
* *tuntoin:* `tunto+N+Use/Rare+Pl+Gen`
* *tuntohon:* `tunto+N+Err/Orth+Sg+Ill`



*Nouns 1ntu examples:*
* *lintu:* `lintu+N+Sg+Nom` (Eng. # bird)
* *linnussa:* `lintu+N+Sg+Ine`
* *lintuihin:* `lintu+N+Pl+Ill`
* *linnuista:* `lintu+N+Pl+Ela`

*Nouns 1ntu non-standard examples:*
* *lintuin:* `lintu+N+Use/Rare+Pl+Gen`
* *lintuhon:* `lintu+N+Err/Orth+Sg+Ill`



*Nouns 1nty examples:*
* *mänty:* `mänty+N+Sg+Nom` (Eng. pine tree)
* *männyssä:* `mänty+N+Sg+Ine`
* *mäntyihin:* `mänty+N+Pl+Ill`
* *männyistä:* `mänty+N+Pl+Ela`

*Nouns 1nty non-standard examples:*
* *mäntyin:* `mänty+N+Use/Rare+Pl+Gen`
* *mäntyhyn:* `mänty+N+Err/Orth+Sg+Ill`


*Nouns 1ntö examples:*
* *kääntö:* `kääntö+N+Sg+Nom` (Eng. # turn)
* *käännössä:* `kääntö+N+Sg+Ine`
* *käännöistä:* `kääntö+N+Pl+Ela`
* *kääntöihin:* `kääntö+N+Pl+Ill`

*Nouns 1ntö non-standard examples:*
* *kääntöin:* `kääntö+N+Use/Rare+Pl+Gen`
* *kääntöhön:* `kääntö+N+Err/Orth+Sg+Ill`


*Nouns 1rto examples:*
* *siirto:* `siirto+N+Sg+Nom` (Eng. # move)
* *siirrossa:* `siirto+N+Sg+Ine`
* *siirtoihin:* `siirto+N+Pl+Ill`
* *siirroista:* `siirto+N+Pl+Ela`

*Nouns 1rto non-standard examples:*
* *siirtoin:* `siirto+N+Use/Rare+Pl+Gen`
* *siirtohon:* `siirto+N+Err/Orth+Sg+Ill`


 There is a gap that misses all other stem vowels of r:t variation except
 o

 The k:v variation is unique to handful of words of form CUkU, such as


*Nouns 1cuku examples:*
* *luku:* `luku+N+Sg+Nom` (Eng. # figure)
* *luvussa:* `luku+N+Sg+Ine`
* *luvuista:* `luku+N+Pl+Ela`
* *lukuihin:* `luku+N+Pl+Ill`

*Nouns 1cuku non-standard examples:*
* *lukuin:* `luku+N+Use/Rare+Pl+Gen`
* *lukuhun:* `luku+N+Err/Orth+Sg+Ill`



*Nouns 1cyky examples:*
* *kyky:* `kyky+N+Sg+Nom` (Eng. # skill)
* *kyvyssä:* `kyky+N+Sg+Ine`
* *kyvyistä:* `kyky+N+Pl+Ela`
* *kykyihin:* `kyky+N+Pl+Ill`

*Nouns 1cyky non-standard examples:*
* *kykyin:* `kyky+N+Use/Rare+Pl+Gen`
* *kykyhyn:* `kyky+N+Err/Orth+Sg+Ill`





 The trisyllabic and longer words with stem vowels o, u, y, ö
 have no stem variation either, but selection of suffix allomorphs for
 plural GENITIVEs and partitives is different:

*Noun 2o examples:*
* *ruipelo:* `ruipelo+N+Sg+Nom` (Eng. # squirt)
* *ruipeloita:* `ruipelo+N+Pl+Par`
* *ruipeloja:* `ruipelo+N+Pl+Par`
* *ruipelojen:* `ruipelo+N+Pl+Gen`
* *ruipeloiden:* `ruipelo+N+Pl+Gen`
* *ruipeloitten:* `ruipelo+N+Pl+Gen`

*Nouns 2o non-standard examples:*
* *ruipeloin:* `ruipelo+N+Use/Rare+Pl+Gen`
* *ruipelohon:* `ruipelo+N+Err/Orth+Sg+Ill`


*Noun 2u examples:*
* *seikkailu:* `seikkailu+N+Sg+Nom` (Eng. # adventure)
* *seikkailuja:* `seikkailu+N+Pl+Par`
* *seikkailuita:* `seikkailu+N+Pl+Par`
* *seikkailujen:* `seikkailu+N+Pl+Gen`
* *seikkailuiden:* `seikkailu+N+Pl+Gen`
* *seikkailuitten:* `seikkailu+N+Pl+Gen`

*Nouns 2u non-standard examples:*
* *seikkailuin:* `seikkailu+N+Use/Rare+Pl+Gen`
* *seikkailuhun:* `seikkailu+N+Err/Orth+Sg+Ill`


*Noun 2y examples:*
* *vehkeily:* `vehkeily+N+Sg+Nom` (Eng. # mutiny)
* *vehkeilyjä:* `vehkeily+N+Pl+Par`
* *vehkeilyitä:* `vehkeily+N+Pl+Par`
* *vehkeilyjen:* `vehkeily+N+Pl+Gen`
* *vehkeilyitten:* `vehkeily+N+Pl+Gen`
* *vehkeilyiden:* `vehkeily+N+Pl+Gen`

*Nouns 2y non-standard examples:*
* *vehkeilyin:* `vehkeily+N+Use/Rare+Pl+Gen`
* *vehkeilyhyn:* `vehkeily+N+Err/Orth+Sg+Ill`


*Noun 2ö examples:*
* *jäätelö:* `jäätelö+N+Sg+Nom` (Eng. iced cream)
* *jäätelöjen:* `jäätelö+N+Pl+Gen`
* *jäätelöiden:* `jäätelö+N+Pl+Gen`
* *jäätelöitten:* `jäätelö+N+Pl+Gen`

*Nouns 2ö non-standard examples:*
* *jäätelöin:* `jäätelö+N+Use/Rare+Pl+Gen`
* *jäätelöhön:* `jäätelö+N+Err/Orth+Sg+Ill`

 The words with stem vowel o, u, y, ö preceded by vowels still have no
 more stem  variation than other cases, but give yet another pattern of 
 allomorphs for plural partitives and GENITIVEs:

*Nouns 3o examples:*
* *tuomio:* `tuomio+N+Sg+Nom` (Eng. # judgment)
* *tuomiota:* `tuomio+N+Sg+Par`
* ★*tuomioa:* `tuomio+N+Sg+Par` (is not standard language)
* *tuomioita:* `tuomio+N+Pl+Par`
* *tuomioiden:* `tuomio+N+Pl+Gen`
* *tuomioitten:* `tuomio+N+Pl+Gen`
* ★*tuomiojen:* `tuomio+N+Pl+Gen` (is not standard language)
 There's plenty of imaginable non-standard allomorph forms after long
 vowels:

*Nouns 3o non-standard examples:*
* *tuomioa:* `tuomio+N+Use/Rare+Pl+Par`
* *tuomioin:* `tuomio+N+Use/Rare+Pl+Gen`
* *tuomiohon:* `tuomio+N+Err/Orth+Sg+Ill`
* *tuomioja:* `tuomio+N+Err/Orth+Sg+Par`
* *tuomiojen:* `tuomio+N+Err/Orth+Sg+Gen`


*Nouns 3ö examples:*
* *häiriö:* `häiriö+N+Sg+Nom` (Eng. # disruption)
* *häiriötä:* `häiriö+N+Sg+Par`
* *häiriöiden:* `häiriö+N+Pl+Gen`
* *häiriöitten:* `häiriö+N+Pl+Gen`

*Nouns 3ö non-standard examples:*
* *häiriöä:* `häiriö+N+Use/Rare+Pl+Par`
* *häiriöin:* `häiriö+N+Use/Rare+Pl+Gen`
* *häiriöhön:* `häiriö+N+Err/Orth+Sg+Ill`
* *häiriöjä:* `häiriö+N+Err/Orth+Sg+Par`
* *häiriöjen:* `häiriö+N+Err/Orth+Sg+Gen`

 Similar inflection exists in limited amounts in new loan words that are
 written as pronounced (thus taking no stem variation but still ending
 with long vowels with a syllable boundary):

*Nouns 3eta examples:*
* *zombie:* `zombie+N+Sg+Nom` (Eng. # zombie)
* *zombieta:* `zombie+N+Sg+Par`
* *zombieita:* `zombie+N+Pl+Par`
* *zombieiden:* `zombie+N+Pl+Gen`
* *zombieitten:* `zombie+N+Pl+Gen`

*Nouns 3eta non-standard examples:*
* *zombiea:* `zombie+N+Use/Rare+Pl+Par`
* *zombiein:* `zombie+N+Use/Rare+Pl+Gen`
* *zombiehen:* `zombie+N+Err/Orth+Sg+Ill`
* *zombieja:* `zombie+N+Err/Orth+Sg+Par`
* *zombiejen:* `zombie+N+Err/Orth+Sg+Gen`

 This class includes a set of new proper nouns that get nativised a bit:

*Nouns 3etä examples:*
* *Bernie:* `Bernie+N+Prop+Sg+Nom` (Eng. # Bernie)
* *Bernietä:* `Bernie+N+Prop+Sg+Par`
* *Bernieitä:* `Bernie+N+Prop+Pl+Par`
* *Bernieiden:* `Bernie+N+Prop+Pl+Gen`
* *Bernieitten:* `Bernie+N+Prop+Pl+Gen`

*Nouns 3etä non-standard examples:*
* *Berniea:* `Bernie+N+Use/Rare+Pl+Par`
* *Berniein:* `Bernie+N+Use/Rare+Pl+Gen`
* *Berniehen:* `Bernie+N+Err/Orth+Sg+Ill`
* *Bernieja:* `Bernie+N+Err/Orth+Sg+Par`
* *Berniejen:* `Bernie+N+Err/Orth+Sg+Gen`

### Optional gradation with illatives
 In some trisyllabic words ending with quantitative consonant gradation, 
 the illative form can attach to either strong or weak stem, even in 
 standard written Finnish. Otherwise words in this class behave like other
 trisyllabic stems.

*Noun 4 kko examples:*
* *lepakko:* `lepakko+N+Sg+Nom` (Eng. # bat)
* *lepakoihin:* `lepakko+N+Pl+Ill`
* *lepakkoihin:* `lepakko+N+Pl+Ill`

*Nouns 4ko non-standard examples:*
* *lepakkoin:* `lepakko+N+Use/Rare+Pl+Gen`
* *lepakkohon:* `lepakko+N+Err/Orth+Sg+Ill`


*Noun 4 kkö examples:*
* *yksikkö:* `yksikkö+N+Sg+Nom` (Eng. # unit)
* *yksiköihin:* `yksikkö+N+Pl+Ill`
* *yksikköihin:* `yksikkö+N+Pl+Ill`

*Nouns 4ko non-standard examples:*
* *yksikköin:* `yksikkö+N+Use/Rare+Pl+Gen`
* *yksikköhön:* `yksikkö+N+Err/Orth+Sg+Ill`

### I final stems
 The basic variation of stem final i in nominative is that it becomes
 e in plural stems. In plural GENITIVE form, the stem vowel disappears
 making suffix allomorph -ien, instead of -jen.

*Nouns 5 back examples:*
* *ruuvi:* `ruuvi+N+Sg+Nom` (Eng. # screw)
* *ruuveina:* `ruuvi+N+Pl+Ess`
* *ruuvien:* `ruuvi+N+Pl+Gen`

*Nouns 5F non-standard examples:*
* *ruuvein:* `ruuvi+N+Use/Rare+Pl+Gen`
* *ruuvihin:* `ruuvi+N+Err/Orth+Sg+Ill`
* *ruuvejen:* `ruuvi+N+Err/Orth+Pl+Gen`


*Nouns 5 front examples:*
* *tyyli:* `tyyli+N+Sg+Nom` (Eng. # style)
* *tyyleinä:* `tyyli+N+Pl+Ess`
* *tyylien:* `tyyli+N+Pl+Gen`


*Nouns 5 back k~0 examples:*
* *lokki:* `lokki+N+Sg+Nom` (Eng. # seagull)
* *lokissa:* `lokki+N+Sg+Ine`
* *lokkeina:* `lokki+N+Pl+Ess`
* *lokeista:* `lokki+N+Pl+Ela`
* *lokkien:* `lokki+N+Pl+Gen`


*Nouns 5 front k~0 examples:*
* *häkki:* `häkki+N+Sg+Nom` (Eng. # cage)
* *häkissä:* `häkki+N+Sg+Ine`
* *häkkeinä:* `häkki+N+Pl+Ess`
* *häkeistä:* `häkki+N+Pl+Ela`
* *häkkien:* `häkki+N+Pl+Gen`


*Nouns 5 back p~0 examples:*
* *kuppi:* `kuppi+N+Sg+Nom` (Eng. # cup)
* *kupissa:* `kuppi+N+Sg+Ine`
* *kuppeina:* `kuppi+N+Pl+Ess`
* *kupeista:* `kuppi+N+Pl+Ela`
* *kuppien:* `kuppi+N+Pl+Gen`


*Nouns 5 front p~0 examples:*
* *tyyppi:* `tyyppi+N+Sg+Nom` (Eng. # type)
* *tyypissä:* `tyyppi+N+Sg+Ine`
* *tyyppeinä:* `tyyppi+N+Pl+Ess`
* *tyypeistä:* `tyyppi+N+Pl+Ela`
* *tyyppien:* `tyyppi+N+Pl+Gen`


*Nouns 5 back t~0 examples:*
* *kortti:* `kortti+N+Sg+Nom` (Eng. # card)
* *kortissa:* `kortti+N+Sg+Ine`
* *kortteina:* `kortti+N+Pl+Ess`
* *korteista:* `kortti+N+Pl+Ela`
* *korttien:* `kortti+N+Pl+Gen`




*Nouns 5 front t~0 examples:*
* *skeitti:* `skeitti+N+Sg+Nom` (Eng. # skate)
* *skeitissä:* `skeitti+N+Sg+Ine`
* *skeitteinä:* `skeitti+N+Pl+Ess`
* *skeiteistä:* `skeitti+N+Pl+Ela`
* *skeittien:* `skeitti+N+Pl+Gen`


*Nouns 5 back k~’~0 examples:*
* *laki:* `laki+N+Sg+Nom` (Eng. # law)
* *laissa:* `laki+N+Sg+Ine`
* *la’issa:* `laki+N+Sg+Ine`
* *lakeina:* `laki+N+Pl+Ess`
* *laeista:* `laki+N+Pl+Ela`
* *la’eista:* `laki+N+Pl+Ela`
* *lakien:* `laki+N+Pl+Gen`


*Nouns 5 back p~v examples:*
* *hupi:* `hupi+N+Sg+Nom` (Eng. # fun)
* *huvissa:* `hupi+N+Sg+Ine`
* *hupeina:* `hupi+N+Pl+Ess`
* *huveista:* `hupi+N+Pl+Ela`
* *hupien:* `hupi+N+Pl+Gen`

 There is a gap in i final words for p:v variation and front harmony


*Nouns 5 back t~d examples:*
* *tauti:* `tauti+N+Sg+Nom` (Eng. # disease)
* *taudissa:* `tauti+N+Sg+Ine`
* *tauteina:* `tauti+N+Pl+Ess`
* *taudeista:* `tauti+N+Pl+Ela`
* *tautien:* `tauti+N+Pl+Gen`


*Nouns 5 front t~d examples:*
* *nihti:* `nihti+N+Sg+Nom` (Eng. # knight)
* *nihdissä:* `nihti+N+Sg+Ine`
* *nihteinä:* `nihti+N+Pl+Ess`
* *nihdeistä:* `nihti+N+Pl+Ela`
* *nihtien:* `nihti+N+Pl+Gen`


*Nouns 5 back k~g examples:*
* *vanki:* `vanki+N+Sg+Nom` (Eng. # prisoner)
* *vangissa:* `vanki+N+Sg+Ine`
* *vankeina:* `vanki+N+Pl+Ess`
* *vangeista:* `vanki+N+Pl+Ela`
* *vankien:* `vanki+N+Pl+Gen`


*Nouns 5 front k~g examples:*
* *väylinki:* `väylinki+N+Sg+Nom` (Eng. # ???)
* *väylingissä:* `väylinki+N+Sg+Ine`
* *väylinkeinä:* `väylinki+N+Pl+Ess`
* *väylingeistä:* `väylinki+N+Pl+Ela`
* *väylinkien:* `väylinki+N+Pl+Gen`

 There is a gap in i final paradigm with t:l variation and back vowels.


*Nouns 5 front t~l examples:*
* *pelti:* `pelti+N+Sg+Nom` (Eng. sheet metal)
* *pellissä:* `pelti+N+Sg+Ine`
* *pelteinä:* `pelti+N+Pl+Ess`
* *pelleistä:* `pelti+N+Pl+Ela`
* *peltien:* `pelti+N+Pl+Gen`


*Nouns 5 back t~n examples:*
* *sointi:* `sointi+N+Sg+Nom` (Eng. # sound)
* *soinnissa:* `sointi+N+Sg+Ine`
* *sointeina:* `sointi+N+Pl+Ess`
* *soinneista:* `sointi+N+Pl+Ela`
* *sointien:* `sointi+N+Pl+Gen`


*Nouns 5 front t~n examples:*
* *vienti:* `vienti+N+Sg+Nom` (Eng. # export)
* *viennissä:* `vienti+N+Sg+Ine`
* *vienteinä:* `vienti+N+Pl+Ess`
* *vienneistä:* `vienti+N+Pl+Ela`
* *vientien:* `vienti+N+Pl+Gen`

 New loan words ending in consonant may be inflected as i stem words

*Nouns 5 back no i examples:*
* *punk:* `punk+N+Sg+Nom` (Eng. # punk)
* *punkeina:* `punk+N+Pl+Ess`
* *punkien:* `punk+N+Pl+Gen`


*Nouns 5 front no i examples:*
* *zen:* `zen+N+Sg+Nom` (Eng. # zen)
* *zeneinä:* `zen+N+Pl+Ess`
* *zenien:* `zen+N+Pl+Gen`

 The trisyllabic -i final stems work like their o, u, y. ö counterparts  ;
 they combine the e:i and e:0 variation to the additional allomorphs for
 plural partitive and GENITIVE:


*Nouns 6 back examples:*
* *kanaali:* `kanaali+N+Sg+Nom` (Eng. # canal)
* *kanaaleja:* `kanaali+N+Pl+Par`
* *kanaaleita:* `kanaali+N+Pl+Par`
* *kanaalien:* `kanaali+N+Pl+Gen`
* *kanaaleiden:* `kanaali+N+Pl+Gen`
* *kanaaleitten:* `kanaali+N+Pl+Gen`


*Nouns 6 front examples:*
* *kehveli:* `kehveli+N+Sg+Nom` (Eng. # hoodlum)
* *kehvelejä:* `kehveli+N+Pl+Par`
* *kehveleitä:* `kehveli+N+Pl+Par`
* *kehvelien:* `kehveli+N+Pl+Gen`
* *kehveleiden:* `kehveli+N+Pl+Gen`
* *kehveleitten:* `kehveli+N+Pl+Gen`


*Nouns 6 back no i examples:*
* *stadion:* `stadion+N+Sg+Nom` (Eng. # stadium)
* *stadioneja:* `stadion+N+Pl+Par`
* *stadioneita:* `stadion+N+Pl+Par`
* *stadionien:* `stadion+N+Pl+Gen`
* *stadioneiden:* `stadion+N+Pl+Gen`
* *stadioneitten:* `stadion+N+Pl+Gen`


*Nouns 6 front no i examples:*
* *besserwisser:* `besserwisser+N+Sg+Nom` (Eng. # besserwisser)
* *besserwisserejä:* `besserwisser+N+Pl+Par`
* *besserwissereitä:* `besserwisser+N+Pl+Par`
* *besserwisserien:* `besserwisser+N+Pl+Gen`
* *besserwissereiden:* `besserwisser+N+Pl+Gen`
* *besserwissereitten:* `besserwisser+N+Pl+Gen`

### I-final nominatives with e stems
 Some of the words with i-final nominative forms have i:e variation for
 singular stem, and i:0 for whole plural stems:


*Noun 7 back examples:*
* *onni:* `onni+N+Sg+Nom` (Eng. # happiness)
* *onnena:* `onni+N+Sg+Ess`
* *onnessa:* `onni+N+Sg+Ine`
* *onnissa:* `onni+N+Pl+Ine`
* *onnista:* `onni+N+Pl+Ela`


*Noun 7 front examples:*
* *helmi:* `helmi+N+Sg+Nom` (Eng. # pearl)
* *helmenä:* `helmi+N+Sg+Ess`
* *helmessä:* `helmi+N+Sg+Ine`
* *helmissä:* `helmi+N+Pl+Ine`
* *helmistä:* `helmi+N+Pl+Ela`


*Noun 7 back p~0 examples:*
* *happi:* `happi+N+Sg+Nom` (Eng. # oxygen)
* *hapessa:* `happi+N+Sg+Ine`
* *happina:* `happi+N+Pl+Ess`
* *hapista:* `happi+N+Pl+Ela`
* *happien:* `happi+N+Pl+Gen`


*Noun 7 front p~0 examples:*
* *typpi:* `typpi+N+Sg+Nom` (Eng. # nitrogen)
* *typessä:* `typpi+N+Sg+Ine`
* *typpinä:* `typpi+N+Pl+Ess`
* *typistä:* `typpi+N+Pl+Ela`
* *typpien:* `typpi+N+Pl+Gen`


*Noun 7 back k~0 examples:*
* *noki:* `noki+N+Sg+Nom` (Eng. # coal)
* *noessa:* `noki+N+Sg+Ine`
* *nokina:* `noki+N+Pl+Ess`
* *noista:* `noki+N+Pl+Ela`
* *nokien:* `noki+N+Pl+Gen`


*Noun 7 front k~0 examples:*
* *käki:* `käki+N+Sg+Nom` (Eng. # cuccoo)
* *käessä:* `käki+N+Sg+Ine`
* *käkinä:* `käki+N+Pl+Ess`
* *käistä:* `käki+N+Pl+Ela`
* *käkien:* `käki+N+Pl+Gen`


*Noun 7 back p~v examples:*
* *korpi:* `korpi+N+Sg+Nom` (Eng. # wilderness)
* *korvessa:* `korpi+N+Sg+Ine`
* *korpina:* `korpi+N+Pl+Ess`
* *korvista:* `korpi+N+Pl+Ela`
* *korpien:* `korpi+N+Pl+Gen`


*Noun 7 front p~v examples:*
* *kilpi:* `kilpi+N+Sg+Nom` (Eng. # shield)
* *kilvessä:* `kilpi+N+Sg+Ine`
* *kilpinä:* `kilpi+N+Pl+Ess`
* *kilvistä:* `kilpi+N+Pl+Ela`
* *kilpien:* `kilpi+N+Pl+Gen`


*Noun 7 back t~d examples:*
* *lahti:* `lahti+N+Sg+Nom` (Eng. # peninsula)
* *lahdessa:* `lahti+N+Sg+Ine`
* *lahtina:* `lahti+N+Pl+Ess`
* *lahdista:* `lahti+N+Pl+Ela`
* *lahtien:* `lahti+N+Pl+Gen`


*Noun 7 front t~d examples:*
* *lehti:* `lehti+N+Sg+Nom` (Eng. # leaf)
* *lehdessä:* `lehti+N+Sg+Ine`
* *lehtinä:* `lehti+N+Pl+Ess`
* *lehdistä:* `lehti+N+Pl+Ela`
* *lehtien:* `lehti+N+Pl+Gen`


*Noun 7 back k~g examples:*
* *onki:* `onki+N+Sg+Nom` (Eng. fishing rod)
* *ongessa:* `onki+N+Sg+Ine`
* *onkina:* `onki+N+Pl+Ess`
* *ongista:* `onki+N+Pl+Ela`
* *onkien:* `onki+N+Pl+Gen`


*Noun 7 front k~g examples:*
* *henki:* `henki+N+Sg+Nom` (Eng. # person)
* *hengessä:* `henki+N+Sg+Ine`
* *henkinä:* `henki+N+Pl+Ess`
* *hengistä:* `henki+N+Pl+Ela`
* *henkien:* `henki+N+Pl+Gen`


*Noun 7 back p~m examples:*
* *lampi:* `lampi+N+Sg+Nom` (Eng. # pond)
* *lammessa:* `lampi+N+Sg+Ine`
* *lampina:* `lampi+N+Pl+Ess`
* *lammista:* `lampi+N+Pl+Ela`
* *lampien:* `lampi+N+Pl+Gen`


*Noun 7 front p~m examples:*
* *rimpi:* `rimpi+N+Sg+Nom` (Eng. # ???)
* *rimmessä:* `rimpi+N+Sg+Ine`
* *rimpinä:* `rimpi+N+Pl+Ess`
* *rimmistä:* `rimpi+N+Pl+Ela`
* *rimpien:* `rimpi+N+Pl+Gen`


*Noun 7 back k~j examples:*
* *arki:* `arki+N+Sg+Nom` (Eng. # weekday)
* *arjessa:* `arki+N+Sg+Ine`
* *arkina:* `arki+N+Pl+Ess`
* *arjista:* `arki+N+Pl+Ela`
* *arkien:* `arki+N+Pl+Gen`


*Noun 7 front k~j examples:*
* *järki:* `järki+N+Sg+Nom` (Eng. # sanity)
* *järjessä:* `järki+N+Sg+Ine`
* *järkinä:* `järki+N+Pl+Ess`
* *järjistä:* `järki+N+Pl+Ela`
* *järkien:* `järki+N+Pl+Gen`

### New e-final stems
 The new words with e stem work exactly like the bisyllabic o, u, y, and ö
 stems  ; no stem variation and same allomorph set. This variant can be
 recognised from singular illative then:


*Noun 8 back examples:*
* *nalle:* `nalle+N+Sg+Nom` (Eng. # teddy)
* *nallea:* `nalle+N+Sg+Par`
* *nalleen:* `nalle+N+Sg+Ill`
* *nalleja:* `nalle+N+Pl+Par`
* *nallejen:* `nalle+N+Pl+Gen`
* *nalleihin:* `nalle+N+Pl+Ill`
 Dictionary claims they also have slightly greater probability for archaic
 form of plural:
* *nallein:* `nalle+N+Pl+Gen+Use/Rare`



*Noun 8 front examples:*
* *nisse:* `nisse+N+Sg+Nom` (Eng. # bellhop)
* *nisseä:* `nisse+N+Sg+Par`
* *nisseen:* `nisse+N+Sg+Ill`
* *nissejä:* `nisse+N+Pl+Par`
* *nissejen:* `nisse+N+Pl+Gen`
* *nisseihin:* `nisse+N+Pl+Ill`


*Noun 8 k~0 back examples:*
* *nukke:* `nukke+N+Sg+Nom` (Eng. # doll)
* *nukkea:* `nukke+N+Sg+Par`
* *nukkeen:* `nukke+N+Sg+Ill`
* *nukkeja:* `nukke+N+Pl+Par`
* *nukkejen:* `nukke+N+Pl+Gen`
* *nukkeihin:* `nukke+N+Pl+Ill`



*Noun 8 p~0 front examples:*
* *jeppe:* `jeppe+N+Sg+Nom` (Eng. # chap)
* *jeppeä:* `jeppe+N+Sg+Par`
* *jeppeen:* `jeppe+N+Sg+Ill`
* *jeppejä:* `jeppe+N+Pl+Par`
* *jeppejen:* `jeppe+N+Pl+Gen`
* *jeppeihin:* `jeppe+N+Pl+Ill`


### A-final stems
 The basic variation of a-final stems is a:o in plural forms:


*Noun 9 examples:*
* *kirja:* `kirja+N+Sg+Nom` (Eng. # book)
* *kirjaa:* `kirja+N+Sg+Par`
* *kirjana:* `kirja+N+Sg+Ess`
* *kirjassa:* `kirja+N+Sg+Ine`
* *kirjoina:* `kirja+N+Pl+Ess`
* *kirjojen:* `kirja+N+Pl+Gen`
* *kirjoihin:* `kirja+N+Pl+Ill`


 Notably, the basic a:o paradigm does not support many ä:ö cases.


*Noun 9 k~0 examples:*
* *politiikka:* `politiikka+N+Sg+Nom` (Eng. # politics)
* *politiikkaa:* `politiikka+N+Sg+Par`
* *politiikkana:* `politiikka+N+Sg+Ess`
* *politiikassa:* `politiikka+N+Sg+Ine`
* *politiikkoina:* `politiikka+N+Pl+Ess`
* *politiikkojen:* `politiikka+N+Pl+Gen`
* *politiikkoihin:* `politiikka+N+Pl+Ill`


*Noun 9 p~0 examples:*
* *tippa:* `tippa+N+Sg+Nom` (Eng. # drop)
* *tippaa:* `tippa+N+Sg+Par`
* *tippana:* `tippa+N+Sg+Ess`
* *tipassa:* `tippa+N+Sg+Ine`
* *tippoina:* `tippa+N+Pl+Ess`
* *tippojen:* `tippa+N+Pl+Gen`
* *tippoihin:* `tippa+N+Pl+Ill`


*Noun 9 t~0 examples:*
* *mitta:* `mitta+N+Sg+Nom` (Eng. # measure)
* *mittaa:* `mitta+N+Sg+Par`
* *mittana:* `mitta+N+Sg+Ess`
* *mitassa:* `mitta+N+Sg+Ine`
* *mittoina:* `mitta+N+Pl+Ess`
* *mittojen:* `mitta+N+Pl+Gen`
* *mittoihin:* `mitta+N+Pl+Ill`


*Noun 9 k~’~0 examples:*
* *vika:* `vika+N+Sg+Nom` (Eng. # error)
* *vikana:* `vika+N+Sg+Ess`
* *viassa:* `vika+N+Sg+Ine`
* *vi’assa:* `vika+N+Sg+Ine`
* *vikoina:* `vika+N+Pl+Ess`
* *vikojen:* `vika+N+Pl+Gen`
* *vikoihin:* `vika+N+Pl+Ill`


*Noun 9 k~’ examples:*
* *vaaka:* `vaaka+N+Sg+Nom` (Eng. # scale)
* *vaakana:* `vaaka+N+Sg+Ess`
* *vaa’assa:* `vaaka+N+Sg+Ine`
* *vaaoissa:* `vaaka+N+Pl+Ine`
* *vaa’oissa:* `vaaka+N+Pl+Ine`



*Noun 9 p~v examples:*
* *salpa:* `salpa+N+Sg+Nom` (Eng. # bolt)
* *salpaa:* `salpa+N+Sg+Par`
* *salpana:* `salpa+N+Sg+Ess`
* *salvassa:* `salpa+N+Sg+Ine`
* *salpoina:* `salpa+N+Pl+Ess`
* *salpojen:* `salpa+N+Pl+Gen`
* *salpoihin:* `salpa+N+Pl+Ill`


*Noun 9 t~d examples:*
* *pata:* `pata+N+Sg+Nom` (Eng. # kettle)
* *pataa:* `pata+N+Sg+Par`
* *patana:* `pata+N+Sg+Ess`
* *padassa:* `pata+N+Sg+Ine`
* *patoina:* `pata+N+Pl+Ess`
* *patojen:* `pata+N+Pl+Gen`
* *patoihin:* `pata+N+Pl+Ill`


*Noun 9 k~g examples:*
* *lanka:* `lanka+N+Sg+Nom` (Eng. # yarn)
* *lankaa:* `lanka+N+Sg+Par`
* *lankana:* `lanka+N+Sg+Ess`
* *langassa:* `lanka+N+Sg+Ine`
* *lankoina:* `lanka+N+Pl+Ess`
* *lankojen:* `lanka+N+Pl+Gen`
* *lankoihin:* `lanka+N+Pl+Ill`


*Noun 9 p~m examples:*
* *rampa:* `rampa+N+Sg+Nom` (Eng. # cripple)
* *rampaa:* `rampa+N+Sg+Par`
* *rampana:* `rampa+N+Sg+Ess`
* *rammassa:* `rampa+N+Sg+Ine`
* *rampoina:* `rampa+N+Pl+Ess`
* *rampojen:* `rampa+N+Pl+Gen`
* *rampoihin:* `rampa+N+Pl+Ill`


*Noun 9 t~l examples:*
* *valta:* `valta+N+Sg+Nom` (Eng. # power)
* *valtaa:* `valta+N+Sg+Par`
* *valtana:* `valta+N+Sg+Ess`
* *vallassa:* `valta+N+Sg+Ine`
* *valtoina:* `valta+N+Pl+Ess`
* *valtojen:* `valta+N+Pl+Gen`
* *valtoihin:* `valta+N+Pl+Ill`


*Noun 9 t~n examples:*
* *kutsunta:* `kutsunta+N+Sg+Nom` (Eng. # drafts)
* *kutsuntaa:* `kutsunta+N+Sg+Par`
* *kutsuntana:* `kutsunta+N+Sg+Ess`
* *kutsunnassa:* `kutsunta+N+Sg+Ine`
* *kutsuntoina:* `kutsunta+N+Pl+Ess`
* *kutsuntojen:* `kutsunta+N+Pl+Gen`
* *kutsuntoihin:* `kutsunta+N+Pl+Ill`


*Noun 9 t~n front examples:*
* *kysyntä:* `kysyntä+N+Sg+Nom` (Eng. # demand)
* *kysyntää:* `kysyntä+N+Sg+Par`
* *kysyntänä:* `kysyntä+N+Sg+Ess`
* *kysynnässä:* `kysyntä+N+Sg+Ine`
* *kysyntöinä:* `kysyntä+N+Pl+Ess`
* *kysyntöjen:* `kysyntä+N+Pl+Gen`
* *kysyntöihin:* `kysyntä+N+Pl+Ill`


*Noun 9 t~r examples:*
* *kerta:* `kerta+N+Sg+Nom` (Eng. # pcs)
* *kertaa:* `kerta+N+Sg+Par`
* *kertana:* `kerta+N+Sg+Ess`
* *kerrassa:* `kerta+N+Sg+Ine`
* *kertoina:* `kerta+N+Pl+Ess`
* *kertojen:* `kerta+N+Pl+Gen`
* *kertoihin:* `kerta+N+Pl+Ill`

 Some -A stems do not have the a:o variation, exhibiting a:0 variation for
 plural stems instead. This class notably contains all the -jA suffixed
 deverbal nouns.


*Noun 10 back examples:*
* *soittaja:* `soittaja+N+Sg+Nom` (Eng. # player)
* *soittajaa:* `soittaja+N+Sg+Par`
* *soittajana:* `soittaja+N+Sg+Ess`
* *soittajassa:* `soittaja+N+Sg+Ine`
* *soittajina:* `soittaja+N+Pl+Ess`
* *soittajien:* `soittaja+N+Pl+Gen`
* *soittajiin:* `soittaja+N+Pl+Ill`









*Noun 10 front examples:*
* *hiihtäjä:* `hiihtäjä+N+Sg+Nom` (Eng. # skier)
* *hiihtäjää:* `hiihtäjä+N+Sg+Par`
* *hiihtäjänä:* `hiihtäjä+N+Sg+Ess`
* *hiihtäjässä:* `hiihtäjä+N+Sg+Ine`
* *hiihtäjinä:* `hiihtäjä+N+Pl+Ess`
* *hiihtäjien:* `hiihtäjä+N+Pl+Gen`
* *hiihtäjiin:* `hiihtäjä+N+Pl+Ill`


*Noun 10 back k̃~0 examples:*
* *luokka:* `luokka+N+Sg+Nom` (Eng. # class)
* *luokkaa:* `luokka+N+Sg+Par`
* *luokkana:* `luokka+N+Sg+Ess`
* *luokassa:* `luokka+N+Sg+Ine`
* *luokkina:* `luokka+N+Pl+Ess`
* *luokissa:* `luokka+N+Pl+Ine`
* *luokkien:* `luokka+N+Pl+Gen`


*Noun 10 front k̃~0 examples:*
* *hölkkä:* `hölkkä+N+Sg+Nom` (Eng. # jog)
* *hölkkää:* `hölkkä+N+Sg+Par`
* *hölkkänä:* `hölkkä+N+Sg+Ess`
* *hölkässä:* `hölkkä+N+Sg+Ine`
* *hölkkinä:* `hölkkä+N+Pl+Ess`
* *hölkissä:* `hölkkä+N+Pl+Ine`
* *hölkkien:* `hölkkä+N+Pl+Gen`


*Noun 10 back p~0 examples:*
* *kuoppa:* `kuoppa+N+Sg+Nom` (Eng. # pothole)
* *kuoppaa:* `kuoppa+N+Sg+Par`
* *kuoppana:* `kuoppa+N+Sg+Ess`
* *kuopassa:* `kuoppa+N+Sg+Ine`
* *kuoppina:* `kuoppa+N+Pl+Ess`
* *kuopissa:* `kuoppa+N+Pl+Ine`
* *kuoppien:* `kuoppa+N+Pl+Gen`


*Noun 10 front p~0 examples:*
* *seppä:* `seppä+N+Sg+Nom` (Eng. # blacksmith)
* *seppää:* `seppä+N+Sg+Par`
* *seppänä:* `seppä+N+Sg+Ess`
* *sepässä:* `seppä+N+Sg+Ine`
* *seppinä:* `seppä+N+Pl+Ess`
* *sepissä:* `seppä+N+Pl+Ine`
* *seppien:* `seppä+N+Pl+Gen`


*Noun 10 back t~0 examples:*
* *rotta:* `rotta+N+Sg+Nom` (Eng. # rat)
* *rottaa:* `rotta+N+Sg+Par`
* *rottana:* `rotta+N+Sg+Ess`
* *rotassa:* `rotta+N+Sg+Ine`
* *rottina:* `rotta+N+Pl+Ess`
* *rotissa:* `rotta+N+Pl+Ine`
* *rottien:* `rotta+N+Pl+Gen`


*Noun 10 front t~0 examples:*
* *kenttä:* `kenttä+N+Sg+Nom` (Eng. # field)
* *kenttää:* `kenttä+N+Sg+Par`
* *kenttänä:* `kenttä+N+Sg+Ess`
* *kentässä:* `kenttä+N+Sg+Ine`
* *kenttinä:* `kenttä+N+Pl+Ess`
* *kentissä:* `kenttä+N+Pl+Ine`
* *kenttien:* `kenttä+N+Pl+Gen`


*Noun 10 back k~’~0 examples:*
* *vuoka:* `vuoka+N+Sg+Nom` (Eng. baking tray)
* *vuokaa:* `vuoka+N+Sg+Par`
* *vuokana:* `vuoka+N+Sg+Ess`
* *vuoassa:* `vuoka+N+Sg+Ine`
* *vuo’assa:* `vuoka+N+Sg+Ine`
* *vuokina:* `vuoka+N+Pl+Ess`
* *vuoissa:* `vuoka+N+Pl+Ine`
* *vuo’issa:* `vuoka+N+Pl+Ine`
* *vuokien:* `vuoka+N+Pl+Gen`


*Noun 10 front k~’~0 examples:*
* *ikä:* `ikä+N+Sg+Nom` (Eng. # age)
* *ikää:* `ikä+N+Sg+Par`
* *ikänä:* `ikä+N+Sg+Ess`
* *iässä:* `ikä+N+Sg+Ine`
* *i’issä:* `ikä+N+Pl+Ine`
* *ikiin:* `ikä+N+Pl+Ill`


*Noun 10 back k~’~0 examples:*
* *reikä:* `reikä+N+Sg+Nom` (Eng. # hole)
* *reikää:* `reikä+N+Sg+Par`
* *reikänä:* `reikä+N+Sg+Ess`
* *reiässä:* `reikä+N+Sg+Ine`
* *rei’issä:* `reikä+N+Pl+Ine`
* *reikiin:* `reikä+N+Pl+Ill`


*Noun 10 back p~v examples:*
* *lupa:* `lupa+N+Sg+Nom` (Eng. # permit)
* *lupaa:* `lupa+N+Sg+Par`
* *lupana:* `lupa+N+Sg+Ess`
* *luvassa:* `lupa+N+Sg+Ine`
* *lupina:* `lupa+N+Pl+Ess`
* *luvissa:* `lupa+N+Pl+Ine`
* *lupien:* `lupa+N+Pl+Gen`


*Noun 10 front p~v examples:*
* *leipä:* `leipä+N+Sg+Nom` (Eng. # bread)
* *leipää:* `leipä+N+Sg+Par`
* *leipänä:* `leipä+N+Sg+Ess`
* *leivässä:* `leipä+N+Sg+Ine`
* *leipinä:* `leipä+N+Pl+Ess`
* *leivissä:* `leipä+N+Pl+Ine`
* *leipien:* `leipä+N+Pl+Gen`


*Noun 10 back t~d examples:*
* *sota:* `sota+N+Sg+Nom` (Eng. # war)
* *sotaa:* `sota+N+Sg+Par`
* *sotana:* `sota+N+Sg+Ess`
* *sodassa:* `sota+N+Sg+Ine`
* *sotina:* `sota+N+Pl+Ess`
* *sodissa:* `sota+N+Pl+Ine`
* *sotien:* `sota+N+Pl+Gen`


*Noun 10 front t~d examples:*
* *pöytä:* `pöytä+N+Sg+Nom` (Eng. # table)
* *pöytää:* `pöytä+N+Sg+Par`
* *pöytänä:* `pöytä+N+Sg+Ess`
* *pöydässä:* `pöytä+N+Sg+Ine`
* *pöytinä:* `pöytä+N+Pl+Ess`
* *pöydissä:* `pöytä+N+Pl+Ine`
* *pöytien:* `pöytä+N+Pl+Gen`


*Noun 10 back k~g examples:*
* *honka:* `honka+N+Sg+Nom` (Eng. kind of a tree)
* *honkaa:* `honka+N+Sg+Par`
* *honkana:* `honka+N+Sg+Ess`
* *hongassa:* `honka+N+Sg+Ine`
* *honkina:* `honka+N+Pl+Ess`
* *hongissa:* `honka+N+Pl+Ine`
* *honkien:* `honka+N+Pl+Gen`


*Noun 10 frot k~g examples:*
* *kenkä:* `kenkä+N+Sg+Nom` (Eng. # shoe)
* *kenkää:* `kenkä+N+Sg+Par`
* *kenkänä:* `kenkä+N+Sg+Ess`
* *kengässä:* `kenkä+N+Sg+Ine`
* *kengissä:* `kenkä+N+Pl+Ine`
* *kenkiin:* `kenkä+N+Pl+Ill`



*Noun 10 back p~m examples:*
* *kompa:* `kompa+N+Sg+Nom` (Eng. trick question)
* *kompaa:* `kompa+N+Sg+Par`
* *kompana:* `kompa+N+Sg+Ess`
* *kommassa:* `kompa+N+Sg+Ine`
* *kompina:* `kompa+N+Pl+Ess`
* *kommissa:* `kompa+N+Pl+Ine`
* *kompien:* `kompa+N+Pl+Gen`

 There is a gap for ä-final word with p:m variation


*Noun 10 back t~l examples:*
* *multa:* `multa+N+Sg+Nom` (Eng. # dirt)
* *multaa:* `multa+N+Sg+Par`
* *multana:* `multa+N+Sg+Ess`
* *mullassa:* `multa+N+Sg+Ine`
* *multina:* `multa+N+Pl+Ess`
* *mullissa:* `multa+N+Pl+Ine`
* *multien:* `multa+N+Pl+Gen`


*Noun 10 front t~l examples:*
* *syltä:* `syltä+N+Sg+Nom` (Eng. # ???)
* *syltää:* `syltä+N+Sg+Par`
* *syltänä:* `syltä+N+Sg+Ess`
* *syllässä:* `syltä+N+Sg+Ine`
* *syltinä:* `syltä+N+Pl+Ess`
* *syllissä:* `syltä+N+Pl+Ine`
* *syltien:* `syltä+N+Pl+Gen`


*Noun 10 back t~n examples:*
* *kunta:* `kunta+N+Sg+Nom` (Eng. # municipality)
* *kuntaa:* `kunta+N+Sg+Par`
* *kuntana:* `kunta+N+Sg+Ess`
* *kunnassa:* `kunta+N+Sg+Ine`
* *kuntina:* `kunta+N+Pl+Ess`
* *kunnissa:* `kunta+N+Pl+Ine`
* *kuntien:* `kunta+N+Pl+Gen`


*Noun 10 front t~n examples:*
* *emäntä:* `emäntä+N+Sg+Nom` (Eng. # hostess)
* *emäntää:* `emäntä+N+Sg+Par`
* *emäntänä:* `emäntä+N+Sg+Ess`
* *emännässä:* `emäntä+N+Sg+Ine`
* *emäntinä:* `emäntä+N+Pl+Ess`
* *emännissä:* `emäntä+N+Pl+Ine`
* *emäntien:* `emäntä+N+Pl+Gen`







 There is a gap for k:j gradation in a-final stems.


*Noun 10 front k~j examples:*
* *selkä:* `selkä+N+Sg+Nom` (Eng. # back)
* *selkää:* `selkä+N+Sg+Par`
* *selkänä:* `selkä+N+Sg+Ess`
* *seljässä:* `selkä+N+Sg+Ine`
* *seljissä:* `selkä+N+Pl+Ine`
* *selkiin:* `selkä+N+Pl+Ill`


 Certain trisyllabic stems allow both variations of a:o and a:0 for plural
 forms.

*Noun 11 back examples:*
* *probleema:* `probleema+N+Sg+Nom` (Eng. # problem)
* *probleemia:* `probleema+N+Pl+Par`
* *probleemoita:* `probleema+N+Pl+Par`
* *probleemien:* `probleema+N+Pl+Gen`
* *probleemoitten:* `probleema+N+Pl+Gen`
* *probleemoiden:* `probleema+N+Pl+Gen`
* *probleemiin:* `probleema+N+Pl+Ill`
* *probleemoihin:* `probleema+N+Pl+Ill`
 Also less commonly:
* *probleemain:* `probleema+N+Pl+Gen+Use/Rare`
* *probleemojen:* `probleema+N+Use/Rare+Pl+Gen`
* *probleemoja:* `probleema+N+Use/Rare+Pl+Par`


*Noun 11 front examples:*
* *käpälä:* `käpälä+N+Sg+Nom` (Eng. # paw)
* *käpälää:* `käpälä+N+Sg+Par`
* *käpälänä:* `käpälä+N+Sg+Ess`
* *käpälässä:* `käpälä+N+Sg+Ine`
* *käpälinä:* `käpälä+N+Pl+Ess`
* *käpälien:* `käpälä+N+Pl+Gen`
* *käpäliin:* `käpälä+N+Pl+Ill`

 Some a stems with a:o variation have slightly different set of allomorphs


*Nouns 12 back examples:*
* *makkara:* `makkara+N+Sg+Nom` (Eng. # sausage)
* *makkaraa:* `makkara+N+Sg+Par`
* *makkarana:* `makkara+N+Sg+Ess`
* *makkaroina:* `makkara+N+Pl+Ess`
* *makkaroita:* `makkara+N+Pl+Par`


*Nouns 12 front examples:*
* *häkkyrä:* `häkkyrä+N+Sg+Nom` (Eng. # thingie)
* *häkkyrää:* `häkkyrä+N+Sg+Par`
* *häkkyränä:* `häkkyrä+N+Sg+Ess`
* *häkkyröinä:* `häkkyrä+N+Pl+Ess`
* *häkkyröitä:* `häkkyrä+N+Pl+Par`

 There is a possible class for further variation of a:o
 in the old dictionary dictionary that is worth re-evaluating.

 There’s one more allomorph pattern.


*Nouns 12++ front examples:*
* *mörinä:* `mörinä+N+Sg+Nom` (Eng. # murr)
* *mörinää:* `mörinä+N+Sg+Par`
* *mörinänä:* `mörinä+N+Sg+Ess`
* *mörinässä:* `mörinä+N+Sg+Ine`

 The trisyllabic a-final words with quantitative consonant gradation allow
 same illative variation as o, u, y, and ö finals described earlier.


*Nouns 14 back examples:*
* *lusikka:* `lusikka+N+Sg+Nom` (Eng. # spoon)
* *lusikkoina:* `lusikka+N+Pl+Ess`
* *lusikoissa:* `lusikka+N+Pl+Ine`
* *lusikoihin:* `lusikka+N+Pl+Ill`
* *lusikkoihin:* `lusikka+N+Pl+Ill`


*Nouns 14 front examples:*
* *kämmekkä:* `kämmekkä+N+Sg+Nom` (Eng. # handy)
* *kämmeköihin:* `kämmekkä+N+Pl+Ill`
* *kämmekköihin:* `kämmekkä+N+Pl+Ill`


*Nouns 14 back p̃0 examples:*
* *ulappa:* `ulappa+N+Sg+Nom` (Eng. # horizon)
* *ulappoina:* `ulappa+N+Pl+Ess`
* *ulapoissa:* `ulappa+N+Pl+Ine`
* *ulapoihin:* `ulappa+N+Pl+Ill`
* *ulappoihin:* `ulappa+N+Pl+Ill`


*Nouns 14 back t0 examples:*
* *pohatta:* `pohatta+N+Sg+Nom` (Eng. # shark)
* *pohattoina:* `pohatta+N+Pl+Ess`
* *pohatoissa:* `pohatta+N+Pl+Ine`
* *pohattoihin:* `pohatta+N+Pl+Ill`
* *pohatoihin:* `pohatta+N+Pl+Ill`

 The a-final words ending in long vowels with syllable boundary have a:0
 variation and more allomorphs for plyral GENITIVE or illative.


*Nouns 15 ea examples:*
* *sokea:* `sokea+N+Sg+Nom` (Eng. # blind)
* *sokeana:* `sokea+N+Sg+Ess`
* *sokeina:* `sokea+N+Pl+Ess`
* *sokeissa:* `sokea+N+Pl+Ine`
* *sokeihin:* `sokea+N+Pl+Ill`
* *sokeisiin:* `sokea+N+Pl+Ill`


*Nouns 15 eä examples:*
* *lipeä:* `lipeä+N+Sg+Nom` (Eng. sodium hydroxide)
* *lipeänä:* `lipeä+N+Sg+Ess`
* *lipeinä:* `lipeä+N+Pl+Ess`
* *lipeissä:* `lipeä+N+Pl+Ine`
* *lipeihin:* `lipeä+N+Pl+Ill`
* *lipeisiin:* `lipeä+N+Pl+Ill`

### Lexicalised comparatives
 Lexicalised comparatives have the same special inflection pattern as
 comparatives have: stem final i varies with a, and mp gradates into mm.
 There are  not many comparatives that lexicalise into nouns.


*Nouns 16 examples:*
* *vanhempi:* `vanhempi+N+Sg+Nom` (Eng. # parent)
* *vanhempana:* `vanhempi+N+Sg+Ess`
* *vanhemmassa:* `vanhempi+N+Sg+Ine`
* *vanhempina:* `vanhempi+N+Pl+Ess`
* *vanhemmista:* `vanhempi+N+Pl+Ela`

### Long vowel stems
 The long vowel stems have shortening variation in plural inflection, and
 special singular illatives, partitives. 

 Bisyllabic and longer stems with long vowels have -seen illative suffix.
 This class has some of the old -UU derivations.

*Nouns 17 a examples:*
* *vapaa:* `vapaa+N+Sg+Nom` (Eng. # free)
* *vapaata:* `vapaa+N+Sg+Par`
* *vapaaseen:* `vapaa+N+Sg+Ill`


*Nouns 17 o examples:*
* *tienoo:* `tienoo+N+Sg+Nom` (Eng. # neighborhood)
* *tienoota:* `tienoo+N+Sg+Par`
* *tienooseen:* `tienoo+N+Sg+Ill`


*Nouns 17 u examples:*
* *kerjuu:* `kerjuu+N+Sg+Nom` (Eng. # begging)
* *kerjuuta:* `kerjuu+N+Sg+Par`
* *kerjuuseen:* `kerjuu+N+Sg+Ill`

 Monosyllabic long vowel stems have illative suffixes of form -hVn.


*Nouns 18 a examples:*
* *maa:* `maa+N+Sg+Nom` (Eng. # earth)
* *maata:* `maa+N+Sg+Par`
* *maahan:* `maa+N+Sg+Ill`


*Nouns 18 e examples:*
* *tee:* `tee+N+Sg+Nom` (Eng. # tea)
* *teetä:* `tee+N+Sg+Par`
* *teehen:* `tee+N+Sg+Ill`


*Nouns 18 i back examples:*
* *hai:* `hai+N+Sg+Nom` (Eng. # shark)
* *haita:* `hai+N+Sg+Par`
* *haihin:* `hai+N+Sg+Ill`


*Nouns 18 i examples:*
* *pii:* `pii+N+Sg+Nom` (Eng. # silica)
* *piitä:* `pii+N+Sg+Par`
* *piihin:* `pii+N+Sg+Ill`


*Nouns 18 o examples:*
* *ookoo:* `ookoo+N+Sg+Nom` (Eng. # OK)
* *ookoota:* `ookoo+N+Sg+Par`
* *ookoohon:* `ookoo+N+Sg+Ill`


*Nouns 18 u examples:*
* *puu:* `puu+N+Sg+Nom` (Eng. # tree)
* *puuta:* `puu+N+Sg+Par`
* *puuhun:* `puu+N+Sg+Ill`


*Nouns 18 y examples:*
* *pyy:* `pyy+N+Sg+Nom` (Eng. kind of a bird)
* *pyytä:* `pyy+N+Sg+Par`
* *pyyhyn:* `pyy+N+Sg+Ill`


*Nouns 18 ä examples:*
* *pää:* `pää+N+Sg+Nom` (Eng. # head)
* *päätä:* `pää+N+Sg+Par`
* *päähän:* `pää+N+Sg+Ill`


*Nouns 18 ö examples:*
* *köö:* `köö+N+Sg+Nom` (Eng. pool/billiard stick)
* *köötä:* `köö+N+Sg+Par`
* *kööhön:* `köö+N+Sg+Ill`

### Opening diphthong stems
 In old opening diphthong final words, the dipthong simplifies for plural
 forms by dropping the initial vowel. For new words of this class, no
 stem mutations happen and they are in above mentioned classes, (e.g.
 NOUN_ZOMBIE).


*Nouns 19 ie examples:*
* *tie:* `tie+N+Sg+Nom` (Eng. # road)
* *tietä:* `tie+N+Sg+Par`
* *teitä:* `tie+N+Pl+Par`


*Nouns 19 uo examples:*
* *suo:* `suo+N+Sg+Nom` (Eng. # swamp)
* *suota:* `suo+N+Sg+Par`
* *soita:* `suo+N+Pl+Par`

THIS WAS MISSING 2015-08-23, REDIRECTING Jaska

*Nouns 19 yö examples:*
* *työ:* `työ+N+Sg+Nom` (Eng. # work)
* *työtä:* `työ+N+Sg+Par`
* *töitä:* `työ+N+Pl+Par`

### Newer loan words
 The loan words that end in long vowel, and have been modified to Finnish
 orthography, have combination of long vowel stem’s allomorphs for e.g
 illatives. Sometimes rules for these classes of words are vague.


*Nouns 20 a examples:*
* *nugaa:* `nugaa+N+Sg+Nom` (Eng. # nougat)
* *nugaata:* `nugaa+N+Sg+Par`
* *nugaana:* `nugaa+N+Sg+Ess`
* *nugaahan:* `nugaa+N+Sg+Ill`
* *nugaaseen:* `nugaa+N+Sg+Ill`
* *nugaissa:* `nugaa+N+Pl+Ine`
* *nugaista:* `nugaa+N+Pl+Ela`
* *nugaiden:* `nugaa+N+Pl+Gen`
* *nugaitten:* `nugaa+N+Pl+Gen`
* *nugaisiin:* `nugaa+N+Pl+Ill`
* *nugaihin:* `nugaa+N+Pl+Ill`


*Nouns 20 e examples:*
* *patee:* `patee+N+Sg+Nom` (Eng. # paté)
* *pateeta:* `patee+N+Sg+Par`
* *pateena:* `patee+N+Sg+Ess`
* *pateehen:* `patee+N+Sg+Ill`
* *pateeseen:* `patee+N+Sg+Ill`
* *pateissa:* `patee+N+Pl+Ine`
* *pateista:* `patee+N+Pl+Ela`
* *pateiden:* `patee+N+Pl+Gen`
* *pateitten:* `patee+N+Pl+Gen`
* *pateisiin:* `patee+N+Pl+Ill`
* *pateihin:* `patee+N+Pl+Ill`


*Nouns 20 e front examples:*
* *bidee:* `bidee+N+Sg+Nom` (Eng. # bidet)
* *bideetä:* `bidee+N+Sg+Par`
* *bideenä:* `bidee+N+Sg+Ess`
* *bideehen:* `bidee+N+Sg+Ill`
* *bideeseen:* `bidee+N+Sg+Ill`
* *bideissä:* `bidee+N+Pl+Ine`
* *bideistä:* `bidee+N+Pl+Ela`
* *bideiden:* `bidee+N+Pl+Gen`
* *bideitten:* `bidee+N+Pl+Gen`
* *bideisiin:* `bidee+N+Pl+Ill`
* *bideihin:* `bidee+N+Pl+Ill`

 There’s a gap in -ii final loan stems.


*Nouns 20 o examples:*
* *trikoo:* `trikoo+N+Sg+Nom` (Eng. # tights)
* *trikoota:* `trikoo+N+Sg+Par`
* *trikoona:* `trikoo+N+Sg+Ess`
* *trikoissa:* `trikoo+N+Pl+Ine`
* *trikoista:* `trikoo+N+Pl+Ela`
* *trikooseen:* `trikoo+N+Sg+Ill`
* *trikoiden:* `trikoo+N+Pl+Gen`
* *trikoitten:* `trikoo+N+Pl+Gen`
* *trikoihin:* `trikoo+N+Pl+Ill`
* *trikoisiin:* `trikoo+N+Pl+Ill`


*Nouns 20 u examples:*
* *raguu:* `raguu+N+Sg+Nom` (Eng. # ragout)
* *raguuta:* `raguu+N+Sg+Par`
* *raguuna:* `raguu+N+Sg+Ess`
* *raguussa:* `raguu+N+Sg+Ine`
* *raguista:* `raguu+N+Pl+Ela`
* *raguuseen:* `raguu+N+Sg+Ill`
* *raguiden:* `raguu+N+Pl+Gen`
* *raguiden:* `raguu+N+Pl+Gen`
* *raguisiin:* `raguu+N+Pl+Ill`
* *raguihin:* `raguu+N+Pl+Ill`


*Nouns 20 y examples:*
* *fondyy:* `fondyy+N+Sg+Nom` (Eng. # fondue)
* *fondyytä:* `fondyy+N+Sg+Par`
* *fondyynä:* `fondyy+N+Sg+Ess`
* *fondyyssä:* `fondyy+N+Sg+Ine`
* *fondyistä:* `fondyy+N+Pl+Ela`
* *fonduuseen:* `fondyy+N+Sg+Ill`
* *fondyyhyn:* `fondyy+N+Sg+Ill`
* *fondyiden:* `fondyy+N+Pl+Gen`
* *fondyitten:* `fondyy+N+Pl+Gen`



*Nouns 20 ö examples:*
* *miljöö:* `miljöö+N+Sg+Nom` (Eng. # milieu)
* *miljöötä:* `miljöö+N+Sg+Par`
* *miljöönä:* `miljöö+N+Sg+Ess`
* *miljöössä:* `miljöö+N+Sg+Ine`
* *miljöistä:* `miljöö+N+Pl+Ela`
* *miljööseen:* `miljöö+N+Sg+Ill`
* *miljöiden:* `miljöö+N+Pl+Gen`
* *miljöitten:* `miljöö+N+Pl+Gen`
* *miljöihin:* `miljöö+N+Pl+Ill`
* *miljöisiin:* `miljöö+N+Pl+Ill`

 Some loan words don’t end in long vowel but work like they would. The
 official dictionary says these words should avoid plural GENITIVE itten
 but not iden, in reality they may be in absolutely free variation.
 In general the rules for loan words are vague and do not always seem to
 work.


*Nouns 21 e back examples:*
* *rosé:* `rosé+N+Sg+Nom` (Eng. # rosé)
* *roséta:* `rosé+N+Sg+Par`
* *roséhen:* `rosé+N+Sg+Ill`
* *roséseen:* `rosé+N+Sg+Ill`


*Nouns 21 e front examples:*
* *bébé:* `bébé+N+Sg+Nom` (Eng. # bébé)
* *bébétä:* `bébé+N+Sg+Par`
* *bébéhen:* `bébé+N+Sg+Ill`
* *bébéseen:* `bébé+N+Sg+Ill`

 The rules are even more wonky when the vowel harmony does not follow the
 orthography, or the orthography leaves things open to interpretation.
 Only way to even begin to understand the norm is to look up
 [examples on RILF site](http://www.kotus.fi/index.phtml?i=634&s=2612#faq_634)
 (to me, some of the forms on that normative guide are just bizarre).


*Nouns 21 i back examples:*
* *brasserie:* `brasserie+N+Sg+Nom` (Eng. # brasserie)
* *brasserieta:* `brasserie+N+Sg+Par`
* *brasserietä:* `brasserie+N+Sg+Par`
* *brasseriehen:* `brasserie+N+Sg+Ill`
* *brasseriehin:* `brasserie+N+Sg+Ill`



*Nouns 21 i front examples:*
* *brie:* `brie+N+Sg+Nom` (Eng. # brie)
* *brietä:* `brie+N+Sg+Par`
* *briehen:* `brie+N+Sg+Ill`


*Nouns 21 ye examples:*
* *fondue:* `fondue+N+Sg+Nom` (Eng. # fondue)
* *fondueta:* `fondue+N+Sg+Par`
* *fonduetä:* `fondue+N+Sg+Par`
* *fonduena:* `fondue+N+Sg+Ess`
* *fonduenä:* `fondue+N+Sg+Ess`
* *fonduessa:* `fondue+N+Sg+Ine`
* *fonduessä:* `fondue+N+Sg+Ine`
* *fondueina:* `fondue+N+Pl+Ess`
* *fondueinä:* `fondue+N+Pl+Ess`
* *fondueiden:* `fondue+N+Pl+Gen`
* *fondueitten:* `fondue+N+Pl+Gen`


*Nouns 21 yi examples:*
* *jockey:* `jockey+N+Sg+Nom` (Eng. # jockey)
* *jockeyta:* `jockey+N+Sg+Par`
* *jockeytä:* `jockey+N+Sg+Par`
* *jockeya:* `jockey+N+Sg+Par`


*Nouns 21 iy examples:*
* *cowboy:* `cowboy+N+Sg+Nom` (Eng. # cowboy)
* *cowboytä:* `cowboy+N+Sg+Par`
* *cowboyta:* `cowboy+N+Sg+Par`


*Nouns 21 u back examples:*
* *kungfu:* `kungfu+N+Sg+Nom` (Eng. # kung-fu)
* *kungfuta:* `kungfu+N+Sg+Par`


*Nouns 21 yi examples:*
* *gay:* `gay+N+Sg+Nom` (Eng. # gay)
* *gayhin:* `gay+N+Sg+Ill`
* *gayhyn:* `gay+N+Sg+Ill`




*Nouns 21 a examples:*
* *chachacha:* `chachacha+N+Sg+Nom` (Eng. cha cha cha)
* *chachachata:* `chachacha+N+Sg+Par`




 The loan words that end in consonant when written but vowel when 
 pronounced are inflected with an apostrophe ’. With half-vowels the
 rule is a bit shaky, but officially apostrophe is the only way.


*Nouns 22 a examples:*
* *nougat:* `nougat+N+Sg+Nom` (Eng. # nougat)
* *nougat’ta:* `nougat+N+Sg+Par`
* *nougat’han:* `nougat+N+Sg+Ill`


*Nouns 22 e examples:*
* *parfait:* `parfait+N+Sg+Nom` (Eng. # parfait)
* *parfait’ta:* `parfait+N+Sg+Par`
* *parfait’hen:* `parfait+N+Sg+Ill`


*Nouns 22 e front examples:*
* *beignet:* `beignet+N+Sg+Nom` (Eng. # beignet)
* *beignet’tä:* `beignet+N+Sg+Par`
* *beignet’hen:* `beignet+N+Sg+Ill`


*Nouns 22 o examples:*
* *bordeaux:* `bordeaux+N+Sg+Nom` (Eng. # bordeaux)
* *bordeaux’ta:* `bordeaux+N+Sg+Par`
* *bordeaux’hon:* `bordeaux+N+Sg+Ill`


*Nouns 22 u examples:*
* *show:* `show+N+Sg+Nom` (Eng. # show)
* *show’ta:* `show+N+Sg+Par`
* *show’hun:* `show+N+Sg+Ill`


*Nouns 22 ö examples:*
* *monsieur:* `monsieur+N+Sg+Nom` (Eng. # monsieur)
* *monsieur’tä:* `monsieur+N+Sg+Par`
* *monsieur’hön:* `monsieur+N+Sg+Ill`

### I-final stems (old e stems)
 Some i-final stems have i:e variation in singular forms, as they are
 originated from -e forms, only nominative has -i. They also have
 some consonant stem forms that are archaic for other classes of words.
 The difference between these classes are in the selection of singular
 partitives and plural GENITIVEs (but the boundaries of norm are not
 clear-cut, and most variants are found in the wild):


*Nouns 23 examples:*
* *tuli:* `tuli+N+Sg+Nom` (Eng. # fire)
* *tulessa:* `tuli+N+Sg+Ine`
* *tulta:* `tuli+N+Sg+Par`
* *tulien:* `tuli+N+Pl+Gen`
* *tuliin:* `tuli+N+Pl+Ill`


*Nouns 23++ examples:*
* *vuohi:* `vuohi+N+Sg+Nom` (Eng. # goat)
* *vuohessa:* `vuohi+N+Sg+Ine`
* *vuohea:* `vuohi+N+Sg+Par`
* *vuohta:* `vuohi+N+Sg+Par`
* *vuohien:* `vuohi+N+Pl+Gen`


*Nouns 24 back examples:*
* *ruuhi:* `ruuhi+N+Sg+Nom` (Eng. # boat)
* *ruuhessa:* `ruuhi+N+Sg+Ine`
* *ruuhta:* `ruuhi+N+Sg+Par`
* *ruuhien:* `ruuhi+N+Pl+Gen`
* *ruuhten:* `ruuhi+N+Pl+Gen`


*Nouns 24 front examples:*
* *hiiri:* `hiiri+N+Sg+Nom` (Eng. # mouse)
* *hiiressä:* `hiiri+N+Sg+Ine`
* *hiirtä:* `hiiri+N+Sg+Par`
* *hiirten:* `hiiri+N+Pl+Gen`
* *hiirien:* `hiiri+N+Pl+Gen`

 It is noteworthy of the official dictionary classification, that classes
 with numbers 24 and 26 are identical. The distinction should probably not
 be retained in future versions.


*Nouns 26 back examples:*
* *kaari:* `kaari+N+Sg+Nom` (Eng. # arc)


*Nouns 24 front examples:*
* *mieli:* `mieli+N+Sg+Nom` (Eng. # mind)

 The -mi stems will rarely undego m:n variation for consonant stem forms.


*Nouns 25 back examples:*
* *luomi:* `luomi+N+Sg+Nom` (Eng. # mole)
* *luomea:* `luomi+N+Sg+Par`
* *luonta:* `luomi+N+Sg+Par`
* *luomien:* `luomi+N+Pl+Gen`
* *luonten:* `luomi+N+Pl+Gen`


*Nouns 25 front examples:*
* *liemi:* `liemi+N+Sg+Nom` (Eng. # sauce)
* *liemeä:* `liemi+N+Sg+Par`
* *lientä:* `liemi+N+Sg+Par`
* *liemien:* `liemi+N+Pl+Gen`
* *lienten:* `liemi+N+Pl+Gen`

 The -si words that originate from old -te stems have the consonant
 gradation patterns left in their stems. The si is only in nominative stem
 and this class mainly concerns stems that are old enough to have undergone
 ti>si transformation. 


*Nouns 27 back examples:*
* *kausi:* `kausi+N+Sg+Nom` (Eng. # term)
* *kautena:* `kausi+N+Sg+Ess`
* *kaudessa:* `kausi+N+Sg+Ine`
* *kautta:* `kausi+N+Sg+Par`
* *kausina:* `kausi+N+Pl+Ess`


*Nouns 27 front examples:*
* *köysi:* `köysi+N+Sg+Nom` (Eng. # rope)
* *köytenä:* `köysi+N+Sg+Ess`
* *köydessä:* `köysi+N+Sg+Ine`
* *köyttä:* `köysi+N+Sg+Par`
* *köysinä:* `köysi+N+Pl+Ess`


*Nouns 27 back t~n examples:*
* *ponsi:* `ponsi+N+Sg+Nom` (Eng. # straw)
* *pontena:* `ponsi+N+Sg+Ess`
* *ponnessa:* `ponsi+N+Sg+Ine`
* *pontta:* `ponsi+N+Sg+Par`
* *ponsina:* `ponsi+N+Pl+Ess`


*Nouns 27 front t~n examples:*
* *kynsi:* `kynsi+N+Sg+Nom` (Eng. # nail)
* *kyntenä:* `kynsi+N+Sg+Ess`
* *kynnessä:* `kynsi+N+Sg+Ine`
* *kynttä:* `kynsi+N+Sg+Par`
* *kynsiin:* `kynsi+N+Pl+Ill`


*Nouns 27 back t~r examples:*
* *varsi:* `varsi+N+Sg+Nom` (Eng. # stem)
* *vartena:* `varsi+N+Sg+Ess`
* *varressa:* `varsi+N+Sg+Ine`
* *vartta:* `varsi+N+Sg+Par`
* *varsiin:* `varsi+N+Pl+Ill`


*Nouns 27 front t~r examples:*
* *hirsi:* `hirsi+N+Sg+Nom` (Eng. # timber)
* *hirtenä:* `hirsi+N+Sg+Ess`
* *hirressä:* `hirsi+N+Sg+Ine`
* *hirttä:* `hirsi+N+Sg+Par`
* *hirsiin:* `hirsi+N+Pl+Ill`


*Nouns 27 front t~l examples:*
* *jälsi:* `jälsi+N+Sg+Nom` (Eng. # bark)
* *jältenä:* `jälsi+N+Sg+Ess`
* *jällessä:* `jälsi+N+Sg+Ine`
* *jälttä:* `jälsi+N+Sg+Par`
* *jälsiin:* `jälsi+N+Pl+Ill`

 A few -psi, -ksi, -tsi stems have consonant simplification for
 consonant stems. Other variation with these stems is the selection of
 plural GENITIVE allomorphs.


*Nouns 27 psi examples:*
* *lapsi:* `lapsi+N+Sg+Nom` (Eng. # child)
* *lapsena:* `lapsi+N+Sg+Ess`
* *lasta:* `lapsi+N+Sg+Par`


*Nouns 27 ksi examples:*
* *uksi:* `uksi+N+Sg+Nom` (Eng. # door)
* *uksena:* `uksi+N+Sg+Ess`
* *usta:* `uksi+N+Sg+Par`


*Nouns 27 tsi back examples:*
* *peitsi:* `peitsi+N+Sg+Nom` (Eng. # lance)
* *peistä:* `peitsi+N+Sg+Par`


*Nouns 27 tsi front examples:*
* *veitsi:* `veitsi+N+Sg+Nom` (Eng. # knife)

 The -ksi stem in haaksi includes k:h variation.


*Nouns 27 ksi examples:*
* *haaksi:* `haaksi+N+Sg+Nom` (Eng. # boat)
* *haahtena:* `haaksi+N+Sg+Ess`
* *haahdessa:* `haaksi+N+Sg+Ine`






### Consonant final nouns
 The consonant stems use inverted gradation if applicable, that is, the 
 nominatives have end in consonants and their gradating consonants are in
 weak form. 
 The singular forms of consonant final words have intervening e before
 suffixes. The basic consonant final words have no stem modifications.

*Nouns 32 back examples:*
* *ahven:* `ahven+N+Sg+Nom` (Eng. # berch)
* *ahvenessa:* `ahven+N+Sg+Ine`
* *ahvenina:* `ahven+N+Pl+Ess`


*Nouns 32 back examples:*
* *joutsen:* `joutsen+N+Sg+Nom` (Eng. # swan)


*Nouns 32 front examples:*
* *siemen:* `siemen+N+Sg+Nom` (Eng. # seed)
* *siemenenä:* `siemen+N+Sg+Ess`


*Nouns 32 tar examples:*
* *ajatar:* `ajatar+N+Sg+Nom` (Eng. mythological creature)
* *ajattarena:* `ajatar+N+Sg+Ess`


*Nouns 32 tär examples:*
* *tytär:* `tytär+N+Sg+Nom` (Eng. # daughter)
* *tyttärenä:* `tytär+N+Sg+Ess`


*Nouns 32 ien examples:*
* *ien:* `ien+N+Sg+Nom` (Eng. # gum)
* *ikenenä:* `ien+N+Sg+Ess`

 Some of the n-final stems have n:m variation.


*Nouns 33 back examples:*
* *puhelin:* `puhelin+N+Sg+Nom`
* *puhelimena:* `puhelin+N+Sg+Ess`


*Nouns 33 front examples:*
* *elin:* `elin+N+Sg+Nom`
* *elimenä:* `elin+N+Sg+Ess`


*Nouns 33 back tin examples:*
* *suodatin:* `suodatin+N+Sg+Nom` (Eng. # filter)
* *suodattimena:* `suodatin+N+Sg+Ess`


*Nouns 33 front tin examples:*
* *heitin:* `heitin+N+Sg+Nom` (Eng. # thrower)
* *heittimenä:* `heitin+N+Sg+Ess`


*Nouns 33 back k~in examples:*
* *puin:* `puin+N+Sg+Nom` (Eng. # cloth)
* *pukimena:* `puin+N+Sg+Ess`


*Nouns 33 front k~in examples:*
* *pyyhin:* `pyyhin+N+Sg+Nom` (Eng. # eraser)
* *pyyhkimenä:* `pyyhin+N+Sg+Ess`


*Nouns 33 back vin examples:*
* *raavin:* `raavin+N+Sg+Nom`
* *raapimena:* `raavin+N+Sg+Ess`


*Nouns 33 front vin examples:*
* *särvin:* `särvin+N+Sg+Nom` (Eng. # foodstuff)
* *särpimenä:* `särvin+N+Sg+Ess`


*Nouns 33 back din examples:*
* *vaadin:* `vaadin+N+Sg+Nom` (Eng. female moose)
* *vaatimena:* `vaadin+N+Sg+Ess`


*Nouns 33 back dun examples:*
* *laidun:* `laidun+N+Sg+Nom` (Eng. # meadows)
* *laitumena:* `laidun+N+Sg+Ess`


*Nouns 33 fron din examples:*
* *säädin:* `säädin+N+Sg+Nom` (Eng. # controller)
* *säätimenä:* `säädin+N+Sg+Ess`



*Nouns 33 back lin examples:*
* *askellin:* `askellin+N+Sg+Nom` (Eng. # stepper)
* *askeltimena:* `askellin+N+Sg+Ess`


*Nouns 33 frontlin examples:*
* *sivellin:* `sivellin+N+Sg+Nom` (Eng. # paint-brush)
* *siveltimenä:* `sivellin+N+Sg+Ess`


*Nouns 33 frpnt nin examples:*
* *käännin:* `käännin+N+Sg+Nom` (Eng. # turner)
* *kääntimenä:* `käännin+N+Sg+Ess`


*Nouns 33 back nin examples:*
* *muunnin:* `muunnin+N+Sg+Nom` (Eng. # adapter)
* *muuntimena:* `muunnin+N+Sg+Ess`


*Nouns 33 back rin examples:*
* *kiharrin:* `kiharrin+N+Sg+Nom` (Eng. # curler)
* *kihartimena:* `kiharrin+N+Sg+Ess`


*Nouns 33 back roin examples:*
* *kerroin:* `kerroin+N+Sg+Nom` (Eng. # multiplier)
* *kertoimena:* `kerroin+N+Sg+Ess`


*Nouns 33 fron rin examples:*
* *kierrin:* `kierrin+N+Sg+Nom` (Eng. # turner)
* *kiertimenä:* `kierrin+N+Sg+Ess`


*Nouns 33 back jin examples:*
* *poljin:* `poljin+N+Sg+Nom` (Eng. # pedal)
* *polkimena:* `poljin+N+Sg+Ess`







### -tOn suffixes
 The caritive suffix -ton inflects with A before the singular suffixes.


*Nouns 34 ton examples:*
* *viaton:* `viaton+N+Sg+Nom` (Eng. # innocent)
* *viattomana:* `viaton+N+Sg+Ess`


*Nouns 34 tän examples:*
* *nimetön:* `nimetön+N+Sg+Nom` (Eng. # unnamed)
* *nimettömänä:* `nimetön+N+Sg+Ess`

### Lexicalised superlatives
 The lexicalised superlatives have special inflection pattern.


*Nouns 35 back examples:*
* *kylänvanhin:* `kylänvanhin+N+Sg+Nom` (Eng. # elder)
* *kylänvanhimpana:* `kylänvanhin+N+Sg+Ess`


*Nouns 35 front examples:*
* *lähin:* `lähin+N+Sg+Nom` (Eng. # relative)
* *lähimpänä:* `lähin+N+Sg+Ess`

 Vasen inflects almost like superlative:


*Nouns 35 examples:*
* *vasen:* `vasen+N+Sg+Nom` (Eng. # left)
* *vasempana:* `vasen+N+Sg+Ess`

### -nen suffixed forms
 Number of derivations end in -nen, that has special alternation pattern.


*Nouns 38 back examples:*
* *aakkostaminen:* `aakkostaminen+N+Sg+Nom` (Eng. # alphabetising)
* *aakkostamisena:* `aakkostaminen+N+Sg+Ess`


*Nouns 38 front examples:*
* *kylkiäinen:* `kylkiäinen+N+Sg+Nom` (Eng. # add-on)
* *kylkiäisenä:* `kylkiäinen+N+Sg+Ess`

### -s final words
 The s final words have some variation patterns that are determined 
 lexically.

 The basic variation is s:ks, with e before the singular suffixes.

*Nouns 39 back examples:*
* *vakuutus:* `vakuutus+N+Sg+Nom` (Eng. # insurance)
* *vakuutuksena:* `vakuutus+N+Sg+Ess`


*Nouns 39 front examples:*
* *räjäytys:* `räjäytys+N+Sg+Nom` (Eng. # explosion)
* *räjäytyksenä:* `räjäytys+N+Sg+Ess`

 Some of the s final stems have additional s:t:d variation in singular
 stems. Most notably, the UUs derivations are in this class.


*Nouns 40 back examples:*
* *aakkosellisuus:* `aakkosellisuus+N+Sg+Nom` (Eng. # alphabetificationness)
* *aakkosellisuutena:* `aakkosellisuus+N+Sg+Ess`
* *aakkosellisuudessa:* `aakkosellisuus+N+Sg+Ine`
* *aakkosellisuuksina:* `aakkosellisuus+N+Pl+Ess`


*Nouns 40 front examples:*
* *köyhyys:* `köyhyys+N+Sg+Nom` (Eng. # poverty)
* *köyhyytenä:* `köyhyys+N+Sg+Ess`
* *köyhyydessä:* `köyhyys+N+Sg+Ine`

 Some s final words have special lengthening inflection.


*Nouns 41 as examples:*
* *patsas:* `patsas+N+Sg+Nom` (Eng. # statue)
* *patsaana:* `patsas+N+Sg+Ess`
* *patsaissa:* `patsas+N+Pl+Ine`


*Nouns 41 is examples:*
* *ruumis:* `ruumis+N+Sg+Nom` (Eng. # body)
* *ruumiina:* `ruumis+N+Sg+Ess`
* *ruumiissa:* `ruumis+N+Pl+Ine`




*Nouns 41 is front examples:*
* *ilmatiivis:* `ilmatiivis+N+Sg+Nom` (Eng. # airtight)
* *ilmatiiviinä:* `ilmatiivis+N+Sg+Ess`


*Nouns 41 es back examples:*
* *Aristoteles:* `Aristoteles+N+Prop+Sg+Nom` (Eng. # Aristotle)
* *Aristoteleena:* `Aristoteles+N+Prop+Sg+Ess`


*Nouns 41 es front examples:*
* *kirves:* `kirves+N+Sg+Nom` (Eng. # ax)
* *kirveenä:* `kirves+N+Sg+Ess`


*Nouns 41 äs examples:*
* *äyräs:* `äyräs+N+Sg+Nom` (Eng. # wavetop)
* *äyräänä:* `äyräs+N+Sg+Ess`


*Nouns 41 kas examples:*
* *asukas:* `asukas+N+Sg+Nom` (Eng. # inhabitant)
* *asukkaana:* `asukas+N+Sg+Ess`


*Nouns 41 käs examples:*
* *kärsäkäs:* `kärsäkäs+N+Sg+Nom` (Eng. # elephant)
* *kärsäkkäänä:* `kärsäkäs+N+Sg+Ess`


*Nouns 41 pas examples:*
* *saapas:* `saapas+N+Sg+Nom` (Eng. # boot)
* *saappaana:* `saapas+N+Sg+Ess`


*Nouns 41 päs examples:*
* *rypäs:* `rypäs+N+Sg+Nom` (Eng. # cluster)
* *ryppäänä:* `rypäs+N+Sg+Ess`


*Nouns 41 tas examples:*
* *ratas:* `ratas+N+Sg+Nom` (Eng. # cog)
* *rattaana:* `ratas+N+Sg+Ess`


*Nouns 41 täs examples:*
* *mätäs:* `mätäs+N+Sg+Nom` (Eng. # mound)
* *mättäänä:* `mätäs+N+Sg+Ess`


*Nouns 41 tis examples:*
* *amerikankeltis:* `amerikankeltis+N+Sg+Nom` (Eng. kind of a bird)
* *amerikankelttiinä:* `amerikankeltis+N+Sg+Ess`


*Nouns 41 ies examples:*
* *ies:* `ies+N+Sg+Nom` (Eng. # ???)
* *ikeenä:* `ies+N+Sg+Ess`


*Nouns 41 as examples:*
* *varas:* `varas+N+Sg+Nom` (Eng. # thief)
* *varkaana:* `varas+N+Sg+Ess`


*Nouns 41 is examples:*
* *ruis:* `ruis+N+Sg+Nom` (Eng. # rye)
* *rukiina:* `ruis+N+Sg+Ess`


*Nouns 41 vas examples:*
* *varvas:* `varvas+N+Sg+Nom` (Eng. # toe)
* *varpaana:* `varvas+N+Sg+Ess`


*Nouns 41 väs examples:*
* *seiväs:* `seiväs+N+Sg+Nom` (Eng. # pole)
* *seipäänä:* `seiväs+N+Sg+Ess`


*Nouns 41 das examples:*
* *tehdas:* `tehdas+N+Sg+Nom` (Eng. # factory)
* *tehtaana:* `tehdas+N+Sg+Ess`


*Nouns 41 gas examples:*
* *kangas:* `kangas+N+Sg+Nom` (Eng. # fabric)
* *kankaana:* `kangas+N+Sg+Ess`


*Nouns 41 gäs examples:*
* *köngäs:* `köngäs+N+Sg+Nom` (Eng. # moor)
* *könkäänä:* `köngäs+N+Sg+Ess`


*Nouns 41 mas examples:*
* *hammas:* `hammas+N+Sg+Nom` (Eng. # tooth)
* *hampaana:* `hammas+N+Sg+Ess`


*Nouns 41 las examples:*
* *allas:* `allas+N+Sg+Nom` (Eng. # pool)
* *altaana:* `allas+N+Sg+Ess`


*Nouns 41 nas examples:*
* *kinnas:* `kinnas+N+Sg+Nom` (Eng. # mitten)
* *kintaana:* `kinnas+N+Sg+Ess`


*Nouns 41 näs examples:*
* *rynnäs:* `rynnäs+N+Sg+Nom` (Eng. # breast)
* *ryntäänä:* `rynnäs+N+Sg+Ess`


*Nouns 41 ras examples:*
* *porras:* `porras+N+Sg+Nom` (Eng. # step)
* *portaana:* `porras+N+Sg+Ess`

 The word mies has special s:h variation pattern.


*Nouns 42 examples:*
* *mies:* `mies+N+Sg+Nom` (Eng. # man)
* *miehenä:* `mies+N+Sg+Ess`



### t-final words
 The t-final words have t:0 variation in the stem, and the singular 
 suffixes are as usual joined with e. It is common to see non-standard
 forms of these words.



*Nouns 43 back examples:*
* *olut:* `olut+N+Sg+Nom` (Eng. # beer)
* *oluena:* `olut+N+Sg+Ess`
* *oluen:* `olut+N+Sg+Gen`
* *olueen:* `olut+N+Sg+Ill`


*Nouns 43 front examples:*
* *neitsyt:* `neitsyt+N+Sg+Nom` (Eng. # virgin)
* *neitsyeen:* `neitsyt+N+Sg+Ill`


*Nouns 43 myt examples:*
* *immyt:* `immyt+N+Sg+Nom` (Eng. old wife)
* *impyenä:* `immyt+N+Sg+Ess`
* *impyeen:* `immyt+N+Sg+Ill`

 Few t-final words have lengthening in singular stems


*Nouns 44 examples:*
* *kevät:* `kevät+N+Sg+Nom` (Eng. # spring)
* *keväänä:* `kevät+N+Sg+Ess`

 Nominalised nut participles have special inflection just as well.


*Nouns 47 back examples:*
* *aivokuollut:* `aivokuollut+N+Sg+Nom` (Eng. # braindead)
* *aivokuolleena:* `aivokuollut+N+Sg+Ess`


*Nouns 47 front examples:*
* *sivistynyt:* `sivistynyt+N+Sg+Nom` (Eng. # civilised)
* *sivistyneenä:* `sivistynyt+N+Sg+Ess`

### Old e^ stems
 The e final words that have lost final consonant inflect like consonant
 final words, including the inverse consonant gradation. This class
 includes all deverbal -e suffixed nouns.


*Nouns 48 back examples:*
* *aste:* `aste+N+Sg+Nom` (Eng. # grade)
* *asteena:* `aste+N+Sg+Ess`


*Nouns 48 Front examples:*
* *piste:* `piste+N+Sg+Nom` (Eng. full stop)
* *pisteenä:* `piste+N+Sg+Ess`


*Nouns 48 back ke examples:*
* *kastike:* `kastike+N+Sg+Nom` (Eng. # sauce)
* *kastikkeena:* `kastike+N+Sg+Ess`


*Nouns 48 front ke examples:*
* *lääke:* `lääke+N+Sg+Nom` (Eng. # medicine)
* *lääkkeenä:* `lääke+N+Sg+Ess`


*Nouns 48 back pe examples:*
* *ape:* `ape+N+Sg+Nom` (Eng. # grub)
* *appeena:* `ape+N+Sg+Ess`


*Nouns 48 front pe examples:*
* *ripe:* `ripe+N+Sg+Nom` (Eng. # leftovers)
* *rippeenä:* `ripe+N+Sg+Ess`


*Nouns 48 back te examples:*
* *laite:* `laite+N+Sg+Nom` (Eng. # machine)
* *laitteena:* `laite+N+Sg+Ess`


*Nouns 48 front te examples:*
* *käsite:* `käsite+N+Sg+Nom` (Eng. # concept)
* *käsitteenä:* `käsite+N+Sg+Ess`


*Nouns 48 back 0k examples:*
* *koe:* `koe+N+Sg+Nom` (Eng. # test)
* *kokeena:* `koe+N+Sg+Ess`


*Nouns 48 front 0k examples:*
* *pyyhe:* `pyyhe+N+Sg+Nom` (Eng. # towel)
* *pyyhkeenä:* `pyyhe+N+Sg+Ess`


*Nouns 48 back ve examples:*
* *tarve:* `tarve+N+Sg+Nom` (Eng. # need)
* *tarpeena:* `tarve+N+Sg+Ess`


*Nouns 48 front ve examples:*
* *lieve:* `lieve+N+Sg+Nom` (Eng. tux tail)
* *liepeenä:* `lieve+N+Sg+Ess`


*Nouns 48 front de examples:*
* *kide:* `kide+N+Sg+Nom` (Eng. # crystal)
* *kiteenä:* `kide+N+Sg+Ess`


*Nouns 48 back de examples:*
* *johde:* `johde+N+Sg+Nom` (Eng. # electrode)
* *johteena:* `johde+N+Sg+Ess`


*Nouns 48 back me examples:*
* *lumme:* `lumme+N+Sg+Nom` (Eng. pond leaf)
* *lumpeena:* `lumme+N+Sg+Ess`


*Nouns 48 front le examples:*
* *mielle:* `mielle+N+Sg+Nom` (Eng. # thought)
* *mielteenä:* `mielle+N+Sg+Ess`


*Nouns 48 back le examples:*
* *vuolle:* `vuolle+N+Sg+Nom` (Eng. # woodcut)
* *vuolteena:* `vuolle+N+Sg+Ess`


*Nouns 48 back ne examples:*
* *rakenne:* `rakenne+N+Sg+Nom` (Eng. # construct)
* *rakenteena:* `rakenne+N+Sg+Ess`


*Nouns 48 front ne examples:*
* *kiinne:* `kiinne+N+Sg+Nom` (Eng. # hairspray)
* *kiinteenä:* `kiinne+N+Sg+Ess`


*Nouns 48 back re examples:*
* *aarre:* `aarre+N+Sg+Nom` (Eng. # treasure)
* *aarteena:* `aarre+N+Sg+Ess`


*Nouns 48 fron re examples:*
* *kierre:* `kierre+N+Sg+Nom` (Eng. # spin)
* *kierteenä:* `kierre+N+Sg+Ess`


*Nouns 48 front je examples:*
* *hylje:* `hylje+N+Sg+Nom`
* *hylkeenä:* `hylje+N+Sg+Ess`


*Nouns 48 back je examples:*
* *pohje:* `pohje+N+Sg+Nom` (Eng. # thigh)
* *pohkeena:* `pohje+N+Sg+Ess`

### Dual nominative paradigms
 A handful of words can use two completely distinct inflection patterns
 where a bit of overlapping inflection has been cut out. These words have
 two nominatives, and thus often two dictionary entries: one which is
 regular entry from the e^ class of words (like NOUN_ASTE), and one which
 is consonant final, and may have inverse gradation.


*Nouns 49 ar examples:*
* *askar:* `askar+N+Sg+Nom` (Eng. # chore)
* *askare:* `askar+N+Sg+Nom`
* *askarena:* `askar+N+Sg+Ess`
* *askareena:* `askar+N+Sg+Ess`


*Nouns 49 el examples:*
* *kyynel:* `kyynel+N+Sg+Nom` (Eng. # tear)
* *kyynele:* `kyynel+N+Sg+Nom`
* *kyynelenä:* `kyynel+N+Sg+Ess`
* *kyyneleenä:* `kyynel+N+Sg+Ess`


*Nouns 49 en examples:*
* *säen:* `säen+N+Sg+Nom` (Eng. # spark)
* *säkene:* `säen+N+Sg+Nom`
* *säkenenä:* `säen+N+Sg+Ess`
* *säkeneenä:* `säen+N+Sg+Ess`


*Nouns 49 val examples:*
* *taival:* `taival+N+Sg+Nom` (Eng. # hike)
* *taipale:* `taival+N+Sg+Nom`
* *taipalena:* `taival+N+Sg+Ess`
* *taipaleena:* `taival+N+Sg+Ess`



*Nouns 49 dar examples:*
* *udar:* `udar+N+Sg+Nom` (Eng. # teat)
* *utare:* `udar+N+Sg+Nom`
* *utarena:* `udar+N+Sg+Ess`
* *utareena:* `udar+N+Sg+Ess`


*Nouns 49 ger examples:*
* *penger:* `penger+N+Sg+Nom` (Eng. # bank)
* *penkere:* `penger+N+Sg+Nom`
* *penkerenä:* `penger+N+Sg+Ess`
* *penkereenä:* `penger+N+Sg+Ess`


*Nouns 49 mel examples:*
* *ommel:* `ommel+N+Sg+Nom` (Eng. # stitch)
* *ompele:* `ommel+N+Sg+Nom`
* *ompelena:* `ommel+N+Sg+Ess`
* *ompeleena:* `ommel+N+Sg+Ess`


*Nouns 49 mel examples:*
* *vemmel:* `vemmel+N+Sg+Nom`
* *vempele:* `vemmel+N+Sg+Nom`
* *vempelenä:* `vemmel+N+Sg+Ess`
* *vempeleenä:* `vemmel+N+Sg+Ess`


*Nouns 49 ner examples:*
* *kinner:* `kinner+N+Sg+Nom` (Eng. # heel)
* *kintere:* `kinner+N+Sg+Nom`
* *kinterenä:* `kinner+N+Sg+Ess`
* *kintereenä:* `kinner+N+Sg+Ess`


*Nouns 49 nel examples:*
* *kannel:* `kannel+N+Sg+Nom` (Eng. # kantele)
* *kantele:* `kannel+N+Sg+Nom`
* *kantelena:* `kannel+N+Sg+Ess`
* *kanteleena:* `kannel+N+Sg+Ess`


*Nouns 49 ner examples:*
* *manner:* `manner+N+Sg+Nom` (Eng. # continent)
* *mantere:* `manner+N+Sg+Nom`
* *manterena:* `manner+N+Sg+Ess`
* *mantereena:* `manner+N+Sg+Ess`


*Nouns 49 nar examples:*
* *piennar:* `piennar+N+Sg+Nom` (Eng. # curb)
* *pientare:* `piennar+N+Sg+Nom`
* *pientarena:* `piennar+N+Sg+Ess`
* *pientareena:* `piennar+N+Sg+Ess`


*Nouns 49 auer examples:*
* *auer:* `auer+N+Sg+Nom` (Eng. # morning mist)
* *autere:* `auer+N+Sg+Nom`
* *auterena:* `auer+N+Sg+Ess`
* *autereena:* `auer+N+Sg+Ess`

### Exceptions to dictionary inflection
 There are few cases where dictionaries traditionally have never indicated
 correct inflection by classification. In computational implementation we
 need to assign some classes or exceptional paths to them, and they are
 described here.

 Two words have exceptions in their vowel harmony patterns:

*Nouns veri examples:*
* *veri:* `veri+N+Sg+Nom` (Eng. # blood)
* *verta:* `veri+N+Sg+Par`
* ★*vertä:* `veri+N+Sg+Par` (is not standard language)


*Nouns meri examples:*
* *meri:* `meri+N+Sg+Nom` (Eng. # sea)
* *merta:* `meri+N+Sg+Par`
* ★*mertä:* `meri+N+Sg+Par` (is not standard language)



*Noun uros examples:*
* *uros:* `uros+N+Sg+Nom` (Eng. # male)
* *uroksen:* `uros+N+Sg+Gen`
* *uroon:* `uros+N+Sg+Gen`

 It is not noted anywhere that the common inflection pattern for veli is
 exceptional:


*Noun veli examples:*
* *veli:* `veli+N+Sg+Nom` (Eng. # brother)
* *veljeä:* `veli+N+Sg+Par`
* *veljen:* `veli+N+Sg+Gen`

 A few of ika-final nouns, but not all, have the shifting half vowel
 written as j in normative orthography.


*Nouns aika examples:*
* *aika:* `aika+N+Sg+Nom` (Eng. # time)
* *ajassa:* `aika+N+Sg+Ine`


*Nouns poika examples:*
* *poika:* `poika+N+Sg+Nom` (Eng. # boy)
* *pojassa:* `poika+N+Sg+Ine`

### Noun forms of numerals with special inflection
 The numerals are not really nouns in this morphology, for details see
 [numeral-stems], but some of their compounds are nouns, and the following
 classes are for those that have special stem or suffix patterns not
 available with other nouns.
 The numerals 1 and 2 are in paradigm that is currently left with one other
 noun, _haaksi_, so nominals with 2 go to that class but 1 gets a new class
 for being front vowelled. 3 Has its own paradigm, 4 is like koira, 5
 like hiisi and 6 like kausi.
 The numerals 7, 8, 9 have their own paradigm, which, other than the
 nominative having extra n at the end, is same as the tri-syllabic
 a, ä stems, similarly 10 is quite like sisar with the extra en in nominative.


*Nouns from numerals yksi examples:*
* *aamuyksi:* `aamuyksi+N+Sg+Nom` (Eng. 1 a.m.)
* *aamuyhtä:* `aamuyksi+N+Sg+Par`
* *aamuyhtenä:* `aamuyksi+N+Sg+Ess`


*Nouns from numerals kolme examples:*
* *aamukolme:* `aamukolme+N+Sg+Nom` (Eng. 3 a.m.)
* *aamukolmea:* `aamukolme+N+Sg+Par`
* *aamukolmeen:* `aamukolme+N+Sg+Ill`
* *aamukolmia:* `aamukolme+N+Pl+Par`
* *aamukolmien:* `aamukolme+N+Pl+Gen`
* *aamukolmiin:* `aamukolme+N+Pl+Ill`



*Nouns from numerals 8 examples:*
* *aamukahdeksan:* `aamukahdeksan+N+Sg+Nom` (Eng. 8 a.m.)
* *aamukahdeksaa:* `aamukahdeksan+N+Sg+Par`
* *aamukahdeksana:* `aamukahdeksan+N+Sg+Ess`
* *aamukahdeksassa:* `aamukahdeksan+N+Sg+Ine`
* *aamukahdeksina:* `aamukahdeksan+N+Pl+Ess`
* *aamukahdeksien:* `aamukahdeksan+N+Pl+Gen`
* *aamukahdeksiin:* `aamukahdeksan+N+Pl+Ill`


*Nouns from numerals 9 examples:*
* *aamuyhdeksän:* `aamuyhdeksän+N+Sg+Nom` (Eng. 9 a.m.)
* *aamuyhdeksää:* `aamuyhdeksän+N+Sg+Par`
* *aamuyhdeksänä:* `aamuyhdeksän+N+Sg+Ess`
* *aamuyhdeksässä:* `aamuyhdeksän+N+Sg+Ine`
* *aamuyhdeksinä:* `aamuyhdeksän+N+Pl+Ess`
* *aamuyhdeksien:* `aamuyhdeksän+N+Pl+Gen`
* *aamuyhdeksiin:* `aamuyhdeksän+N+Pl+Ill`



*Nouns from numerals 10 examples:*
* *aamukymmenen:* `aamukymmenen+N+Sg+Nom` (Eng. 10 a.m.)


*Nouns from numerals 1000 examples:*
* *vuosituhat:* `vuosituhat+N+Sg+Nom` (Eng. # millennium)
* *vuosituhantena:* `vuosituhat+N+Sg+Ess`




















































































































### Plurales tantum
 For some words, the singular forms are rare, odd, or even deemed
 ungrammatical, these words have separate classes for them. In Finnish
 words to commonly be in this class are events like häät (wedding),
 juhlat (party), etc. then all things that are semantically coupled,
 like clothes with two somethings (as with English): farkut (jeans),
 housut (pants). It is noteworthy, that sometimes dictionaries classify
 common words as plurale tantum for semantic reasons: joukot (troops)
 versus joukko (group). We don’t need that at this point.
 The compounds of plurale tantum words are made from singular forms:
 farkkukangas (jean fabric), hääjuhla (wedding party).


*Noun 1o plurt examples:*
* *aivot:* `aivot+N+Pl+Nom` (Eng. # brains)
* ★*aivo:* `aivot+N+Sg+Nom` (is not standard language)


*Noun 1y plurt examples:*
* *pöksyt:* `pöksyt+N+Pl+Nom` (Eng. # pants)
* ★*pöksy:* `pöksyt+N+Sg+Nom` (is not standard language)


*Noun 1u plurt examples:*
* *housut:* `housut+N+Pl+Nom` (Eng. # pants)
* ★*housu:* `housut+N+Sg+Nom` (is not standard language)


*Noun 1ko plurt examples:*
* *huoltojoukot:* `huolto#joukot+N+Pl+Nom` (Eng. # troops)
* ★*huoltojoukko:* `huolto#joukot+N+Sg+Nom` (is not standard language)


*Noun 1ku plurt examples:*
* *farkut:* `farkut+N+Pl+Nom` (Eng. # jeans)
* ★*farkku:* `farkut+N+Sg+Nom` (is not standard language)



*Noun 1dot plurt examples:*
* *perustiedot:* `perus#tiedot+N+Pl+Nom` (Eng. knowledge (in school grades))
* ★*perustieto:* `perus#tiedot+N+Sg+Nom` (is not standard language)


*Noun 1töt plurt examples:*
* *kaksostytöt:* `kaksos#tytöt+N+Pl+Nom` (Eng. girls ?)
* ★*kaksostyttö:* `kaksos#tytöt+N+Sg+Nom` (is not standard language)


*Noun 1ot plurt examples:*
* *pidot:* `pidot+N+Pl+Nom` (Eng. # bacchanalia)
* ★*pito:* `pidot+N+Sg+Nom` (is not standard language)


*Noun 1vut plurt examples:*
* *ravut:* `ravut+N+Pl+Nom` (Eng. crabs ??)
* ★*rapu:* `ravut+N+Sg+Nom` (is not standard language)


*Noun 1ut plurt examples:*
* *urut:* `urut+N+Pl+Nom` (Eng. # organs)
* ★*urku:* `urut+N+Sg+Nom` (is not standard language)


*Noun 1put plurt examples:*
* *raput:* `raput+N+Pl+Nom` (Eng. # stairs)
* ★*rappu:* `raput+N+Sg+Nom` (is not standard language)


*Noun 1dyt plurt examples:*
* *käädyt:* `käädyt+N+Pl+Nom` (Eng. # necklace)
* ★*kääty:* `käädyt+N+Sg+Nom` (is not standard language)


*Noun 1lot plurt examples:*
* *aallot:* `aallot+N+Pl+Nom` (Eng. # waves)
* ★*aalto:* `aallot+N+Sg+Nom` (is not standard language)


*Noun 1not plurt examples:*
* *opinnot:* `opinnot+N+Pl+Nom` (Eng. # studies)
* ★*opinto:* `opinnot+N+Sg+Nom` (is not standard language)


*Noun 2iot plurt examples:*
* *rauniot:* `rauniot+N+Pl+Nom` (Eng. # ruins)
* ★*raunio:* `rauniot+N+Sg+Nom` (is not standard language)



*Noun 3iot plurt examples:*
* *pippalot:* `pippalot+N+Pl+Nom` (Eng. # party)
* ★*pippalo:* `pippalot+N+Sg+Nom` (is not standard language)


*Noun 3lut plurt examples:*
* *neuvottelut:* `neuvottelut+N+Pl+Nom` (Eng. # negotiation)
* ★*neuvottelu:* `neuvottelut+N+Sg+Nom` (is not standard language)


*Noun 1nöt plurt examples:*
* *säännöt:* `säännöt+N+Pl+Nom` (Eng. # rules)
* ★*sääntö:* `säännöt+N+Sg+Nom` (is not standard language)


*Noun 7 back t~n plurt examples:*
* *tunnit:* `tunnit+N+Pl+Nom` (Eng. # classes)
* ★*tunti:* `tunnit+N+Sg+Nom` (is not standard language)


*Noun xxx a front k0 plurt examples:*
* *sukat:* `sukat+N+Pl+Nom` (Eng. # socks)
* ★*sukka:* `sukat+N+Sg+Nom` (is not standard language)


*Noun xxx a front kg plurt examples:*
* *kengät:* `kengät+N+Pl+Nom` (Eng. # shoes)
* ★*kenkä:* `kengät+N+Sg+Nom` (is not standard language)


*Noun 8 plurt examples:*
* *ravet:* `ravet+N+Pl+Nom` (Eng. # rave)
* ★*rave:* `ravet+N+Sg+Nom` (is not standard language)


*Noun 8  front  plurt examples:*
* *bänet:* `bänet+N+Pl+Nom` (Eng. # breakup)
* ★*bäne:* `bänet+N+Sg+Nom` (is not standard language)


*Noun xxx a front plurt examples:*
* *markkinat:* `markkinat+N+Pl+Nom` (Eng. # market)
* ★*markkina:* `markkinat+N+Sg+Nom` (is not standard language)


*Noun xxx a front pp plurt examples:*
* *hipat:* `hipat+N+Pl+Nom` (Eng. # party)
* ★*hippa:* `hipat+N+Sg+Nom` (is not standard language)


*Noun xxx a back p plurt examples:*
* *kuopat:* `kuopat+N+Pl+Nom` (Eng. holes ?)
* ★*kuoppa:* `kuopat+N+Sg+Nom` (is not standard language)


*Noun xxx dat plurt examples:*
* *raudat:* `raudat+N+Pl+Nom` (Eng. # irons)
* ★*rauta:* `raudat+Sg+Nom` (is not standard language)


*Noun xxx vat plurt examples:*
* *tavat:* `tavat+N+Pl+Nom` (Eng. # manners)
* ★*tapa:* `tavat+N+Sg+Nom` (is not standard language)


*Noun xxx lat plurt examples:*
* ★*vallat:* `vallat+N+Pl+Nom` (is not standard language # states)
* *valta:* `vallat+N+Sg+Nom`


*Noun xxx rrt plurt examples:*
* *varat:* `varat+N+Pl+Nom` (Eng. # budget)
* ★*vara:* `varat+N+Sg+Nom` (is not standard language)


*Noun xxx t plurt examples:*
* *juhlat:* `juhlat+N+Pl+Nom` (Eng. # party)
* ★*juhla:* `juhlat+N+Sg+Nom` (is not standard language)


*Noun xxxihi t plurt examples:*
* *kiharat:* `kiharat+N+Pl+Nom` (Eng. # curls)
* ★*kihara:* `kiharat+N+Sg+Nom` (is not standard language)

* *käräjät:* `käräjät+N+Pl+Nom` (Eng. # court)

* *käräjät:* `käräjät+N+Pl+Nom` (Eng. # court)


*Nounäjäm xxx t plurt examples:*
* ★*käräjä:* `käräjät+N+Sg+Nom` (is not standard language)


*Noun xxx kAt plurt examples:*
* *paikat:* `paikat+N+Pl+Nom` (Eng. body parts)
* ★*paikka:* `paikat+N+Sg+Nom` (is not standard language)


*Noun 14 kat plurt examples:*
* *silakat:* `silakat+N+Pl+Nom` (Eng. kinds of a fish)
* ★*silakka:* `silakat+N+Sg+Nom` (is not standard language)


*Noun 14 kat2 plurt examples:*
* *rintsikat:* `rintsikat+N+Pl+Nom` (Eng. # bra)
* ★*rintsikka:* `rintsikat+N+Sg+Nom` (is not standard language)


*Noun xxx teet plurt examples:*
* *tieteet:* `tieteet+N+Pl+Nom` (Eng. # sciences)
* ★*tiede:* `tieteet+N+Sg+Nom` (is not standard language)

*Noun xxx t plurt examples:*


*Noun xxx it plurt examples:*
* *farmarit:* `farmarit+N+Pl+Nom` (Eng. # jeans)
* ★*farmari:* `farmarit+N+Sg+Nom` (is not standard language)


*Noun xxx tit plurt examples:*
* *kastanjetit:* `kastanjetit+N+Pl+Nom` (Eng. kind of an instrumemt)
* ★*kastanjetti:* `kastanjetit+N+Sg+Nom` (is not standard language)


*Noun xxx dit plurt examples:*
* *taudit:* `taudit+N+Pl+Nom` (Eng. diseases)
* ★*tauti:* `taudit+N+Sg+Nom` (is not standard language)


*Noun xxx dit front plurt examples:*
* *pihdit:* `pihdit+N+Pl+Nom` (Eng. # jaws)
* ★*pihti:* `pihdit+N+Sg+Nom` (is not standard language)


*Noun xxx git plurt examples:*
* *tongit:* `tongit+N+Pl+Nom` (Eng. # pliers)
* ★*tonki:* `tongit+N+Sg+Nom` (is not standard language)


*Noun xxx git fornt plurt examples:*
* *syömingit:* `syömingit+N+Pl+Nom` (Eng. eating party)
* ★*syöminki:* `syöminki+N+Sg+Nom` (is not standard language)


*Noun xxx get plurt examples:*
* *länget:* `länget+N+Pl+Nom` (Eng. some cowboy accessory thing)
* ★*länki:* `länget+N+Sg+Nom` (is not standard language)


*Noun xxx sit plurt examples:*
* *lasit:* `lasit+N+Pl+Nom` (Eng. # glasses)
* ★*lasi:* `lasit+N+Sg+Nom` (is not standard language)



*Noun xxx nit plurt examples:*
* *bikinit:* `bikinit+N+Pl+Nom` (Eng. # bikini)
* ★*bikini:* `bikinit+N+Sg+Nom` (is not standard language)


*Noun xxx iiiit plurt examples:*
* *liivit:* `liivit+N+Pl+Nom` (Eng. # vest)
* ★*liivi:* `liivit+N+Sg+Nom` (is not standard language)


*Noun xxx eat plurt examples:*
* *hopeat:* `hopeat+N+Pl+Nom` (Eng. cutlery (silvers))
* ★*hopea:* `hopeat+N+Sg+Nom` (is not standard language)


*Noun 15 tiet plurt examples:*
* *tiet:* `tiet+N+Pl+Nom` (Eng. # roads)
* ★*tie:* `tiet+N+Sg+Nom` (is not standard language)


*Noun xxx työt plurt examples:*
* *työt:* `työt+N+Pl+Nom` (Eng. # employment)
* ★*työ:* `työt+N+Sg+Nom` (is not standard language)


*Noun xxx matt plurt examples:*
* *vanhemmat:* `vanhemmat+N+Pl+Nom` (Eng. # parents)
* ★*vanhempi:* `vanhemmat+N+Sg+Nom` (is not standard language)


*Noun xxx maat plurt examples:*
* *itämaat:* `itämaat+N+Pl+Nom` (Eng. near east)
* ★*itämaa:* `itämaat+N+Sg+Nom` (is not standard language)





*Noun xx puu t plurt examples:*
* *puut:* `puut+N+Pl+Nom` (Eng. # trees)
* ★*puu:* `puut+N+Sg+Nom` (is not standard language)


*Noun xxx äät plurt examples:*
* *häät:* `häät+N+Pl+Nom` (Eng. # wedding)
* ★*hää:* `häät+N+Sg+Nom` (is not standard language)


*Noun xxx kooot plurt examples:*
* *talkoot:* `talkoot+N+Pl+Nom` (Eng. house party)
* ★*talkoo:* `talkoot+N+Sg+Nom` (is not standard language)


*Noun xxx sakset plurt examples:*
* *sakset:* `sakset+N+Pl+Nom` (Eng. # scissors)
* ★*saksi:* `sakset+N+Sg+Nom` (is not standard language)


*Noun xxx ripset plurt examples:*
* *ripset:* `ripset+N+Pl+Nom` (Eng. # eyebrows)
* ★*ripsi:* `ripset+N+Sg+Nom` (is not standard language)


*Noun xxx nnet plurt examples:*
* *kannet:* `kannet+N+Pl+Nom` (Eng. # covers)
* ★*kansi:* `kannet+N+Sg+Nom` (is not standard language)


*Noun 27 det plurt examples:*
* *vuodet:* `vuodet+N+Pl+Nom` (Eng. # years)
* ★*vuosi:* `vuodet+N+Sg+Nom` (is not standard language)


*Noun 25 t plurt examples:*
* *toimet:* `toimet+N+Pl+Nom` (Eng. # actions)
* ★*toimi:* `toimet+N+Sg+Nom` (is not standard language)


*Noun 32 t plurt examples:*
* *höyhenet:* `höyhen+N+Pl+Nom` (Eng. # feathers)
* ★*höyhen:* `höyhenet+N+Sg+Nom` (is not standard language)


*Noun 31 t plurt examples:*
* *aterimet:* `aterimet+N+Pl+Nom` (Eng. # cutlery)
* ★*aterin:* `aterimet+N+Sg+Nom` (is not standard language)


*Noun 31 t fr plurt examples:*
* *keritsimet:* `keritsimet+N+Pl+Nom` (Eng. sheep razor thing)
* ★*keritsin:* `keritsimet+N+Sg+Nom` (is not standard language)



*Noun 31 nnin plurt examples:*
* *antimet:* `antimet+N+Pl+Nom` (Eng. # offerings)
* ★*annin:* `antimet+N+Sg+Nom` (is not standard language)


*Noun 31 tin plurt examples:*
* *hoksottimet:* `hoksottimet+N+Pl+Nom` (Eng. # smarts)
* ★*hoksotin:* `hoksottimet+N+Sg+Nom` (is not standard language)


*Noun 31 tin front plurt examples:*
* *synnyttimet:* `synnyttimet+N+Pl+Nom` (Eng. # ovaries)
* ★*synnytin:* `synnyttimet+N+Sg+Nom` (is not standard language)


*Noun 31 puin plurt examples:*
* *pukimet:* `pukimet+N+Pl+Nom` (Eng. # clothes)
* ★*puin:* `pukimet+N+Sg+Nom` (is not standard language)


*Noun 31 ien plurt examples:*
* *ikenet:* `ikenet+N+Pl+Nom` (Eng. # gums)
* ★*ien:* `ikenet+N+Sg+Nom` (is not standard language)


*Noun  nut plurt examples:*
* *liittoutuneet:* `liittoutuneet+N+Pl+Nom` (Eng. # allied)
* ★*liittoutunut:* `liittoutunut+N+Sg+Nom` (is not standard language)


*Noun 38 nen plurt examples:*
* *rappuset:* `rappuset+N+Pl+Nom` (Eng. # stairs)
* ★*rappunen:* `rappuset+N+Sg+Nom` (is not standard language)


*Noun 38 nen fornt plurt examples:*
* *vihkiäiset:* `vihkiäiset+N+Pl+Nom` (Eng. # engagement)
* ★*vihkiäinen:* `vihkiäiset+N+Sg+Nom` (is not standard language)


*Noun  jaat plurt examples:*
* *valjaat:* `valjaat+N+Pl+Nom` (Eng. # strap)
* ★*valjas:* `valjaat+N+Sg+Nom` (is not standard language)


*Noun  kaat plurt examples:*
* *tikkaat:* `tikkaat+N+Pl+Nom` (Eng. # ladders)
* ★*tikas:* `tikkaat+N+Sg+Nom` (is not standard language)


*Noun  taat plurt examples:*
* *rattaat:* `rattaat+N+Pl+Nom` (Eng. # stroller)
* ★*ratas:* `rattaat+N+Sg+Nom` (is not standard language)


*Noun  gaat plurt examples:*
* *renkaat:* `renkaat+N+Pl+Nom` (Eng. # tires)
* ★*rengas:* `renkaat+N+Sg+Nom` (is not standard language)


*Noun  ntääaat plurt examples:*
* *ryntäät:* `ryntäät+N+Pl+Nom` (Eng. # breasts)
* ★*rynnäs:* `ryntäät+N+Sg+Nom` (is not standard language)









*Noun  ntyyaat plurt examples:*
* *hynttyyt:* `hynttyyt+N+Pl+Nom` (Eng. # clothes)
* ★*hyntys:* `hynttyyt+N+Sg+Nom` (is not standard language)



*Noun  ntyyt plurt examples:*
* *rynttyyt:* `rynttyyt+N+Pl+Nom` (Eng. # rags)
* ★*ryntys:* `rynttyyt+N+Sg+Nom` (is not standard language)


*Noun  ltaat plurt examples:*
* *maltaat:* `maltaat+N+Pl+Nom` (Eng. # hops)
* ★*mallas:* `maltaat+N+Sg+Nom` (is not standard language)


*Noun  rtaat plurt examples:*
* *portaat:* `portaat+N+Pl+Nom` (Eng. # stairs)
* ★*porras:* `portaat+N+Sg+Nom` (is not standard language)


*Noun  kset plurt examples:*
* *serkukset:* `serkukset+N+Pl+Nom` (Eng. # cousins)
* ★*serkus:* `serkukset+N+Sg+Nom` (is not standard language)


*Noun  kset fr plurt examples:*
* *ystävykset:* `ystävykset+N+Pl+Nom` (Eng. # friends)
* ★*ystävys:* `ystävykset+N+Sg+Nom` (is not standard language)


*Noun  sdet plurt examples:*
* *oikeudet:* `oikeudet+N+Pl+Nom` (Eng. # rights)
* ★*oikeus:* `oikeudet+N+Sg+Nom` (is not standard language)


*Noun  mppt plurt examples:*
* *kamppeet:* `kamppeet+N+Pl+Nom` (Eng. # equipment)
* ★*kampe:* `kamppeet+N+Sg+Nom` (is not standard language)


*Noun  ntteet plurt examples:*
* *hyntteet:* `hyntteet+N+Pl+Nom` (Eng. # clothes)
* ★*hynte:* `hyntteet+N+Sg+Nom` (is not standard language)


*Noun  ukeet plurt examples:*
* *pukeet:* `pukeet+N+Pl+Nom` (Eng. # clothes)
* ★*pue:* `pukeet+N+Sg+Nom` (is not standard language)


*Noun  kkkeeet plurt examples:*
* *kuulokkeet:* `kuulokkeet+N+Pl+Nom` (Eng. # earphones)
* ★*kuuloke:* `kuulokkeet+N+Sg+Nom` (is not standard language)


*Noun  teeeeet plurt examples:*
* *vaatteet:* `vaatteet+N+Pl+Nom` (Eng. # clothes)
* ★*vaate:* `vaatteet+N+Sg+Nom` (is not standard language)


*Noun  taateee plurt examples:*
* *peitteet:* `peitteet+N+Pl+Nom` (Eng. # covers)
* ★*peite:* `peitteet+N+Sg+Nom` (is not standard language)


*Noun  taad plurt examples:*
* *suhteet:* `suhteet+N+Pl+Nom` (Eng. # relations)
* ★*suhde:* `suhteet+N+Sg+Nom` (is not standard language)


*Noun  daat plurt examples:*
* *lauteet:* `lauteet+N+Pl+Nom` (Eng. sitting bench in sauna)
* ★*laude:* `lauteet+N+Sg+Nom` (is not standard language)


*Noun  daat plurt examples:*
* *säteet:* `säteet+N+Pl+Nom` (Eng. # rays)
* ★*säde:* `säteet+N+Sg+Nom` (is not standard language)


*Noun  trate plurt examples:*
* *siteet:* `siteet+N+Pl+Nom` (Eng. # restraints)
* ★*side:* `siteet+N+Sg+Nom` (is not standard language)


*Noun  veat plurt examples:*
* *tarpeet:* `tarpeet+N+Pl+Nom` (Eng. # necessities)
* ★*tarve:* `tarpeet+N+Sg+Nom` (is not standard language)


*Noun  leat plurt examples:*
* *hiutaleet:* `hiutaleet+N+Pl+Nom` (Eng. # flakes)
* ★*hiutale:* `hiutaleet+N+Sg+Nom` (is not standard language)


*Noun  jeet plurt examples:*
* *alkeet:* `alkeet+N+Pl+Nom` (Eng. # basics)
* ★*alje:* `alkeet+N+Sg+Nom` (is not standard language)


*Noun  jeat plurt examples:*
* *perkeet:* `perkeet+N+Pl+Nom` (Eng. fish scales and guts)
* ★*perje:* `perkeet+N+Sg+Nom` (is not standard language)


*Noun pojat plurt examples:*
* *pojat:* `pojat+N+Pl+Nom` (Eng. # boys)
* ★*poika:* `pojat+N+Sg+Nom` (is not standard language)










































































































































































































## Adjective-initial Compounds with Agreeing Inflection
 The words in dictionary paradigm class ⁵¹ refer to old closed class of
 adjective initial compounds, which follow agreeing compound pattern same
 as numbers. The amount of these words is relatively small, so they have
 been spelled out here in full form rather than using more complex methods
 of agreeing compounding, 
 Further reading: [VISK § 420](http://scripta.kotus.fi/visk/sisalto.php?p=420)
 FIXME: some are still missing


*Old A+N compounds 51 examples:*
* *aavameri:* `aavameri+N+Sg+Nom` (Eng. # wide open ocean)
* *aavanmeren:* `aavameri+N+Sg+Gen`
* *aavoissamerissä:* `aavameri+N+Pl+Ine`
* *Iso-Britannia:* `Iso-Britannia+N+Prop+Sg+Nom` (Eng. # Great Britain)
* *Ison-Britannian:* `Iso-Britannia+N+Prop+Sg+Gen`
* *Isoissa-Britannioissa:* `Iso-Britannia+N+Prop+Pl+Ine`

























































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































Nouns and other nominals inflect in number, cases, possessives and with
clitics, in that order. Combinations that this regular inflection can form
is approximately 2×15×5×26=4900, so we do not show all variants in test cases
and examples, but just central ones that are interesting and potential to
break.





## Nominatives
Singular nominative is the dictionary reference form for most of the words.


*Noun nominative back examples:*
* *talo:* `talo+N+Sg+Nom` (Eng. # a house)
* *taloko:* `talo+N+Sg+Nom+Qst`
* *talohan:* `talo+N+Sg+Nom+Foc/han`
* ★*talohän:* `talo+N+Sg+Nom+Foc/han` (is not standard language)


*Noun nominative front examples:*
* *pöly:* `pöly+N+Sg+Nom` (Eng. # dust)

The plural nominative attaches to singular stem. For plural words it is also
the form that is used for dictionary lookups:


*Noun nominative plural back examples:*
* *talot:* `talo+N+Pl+Nom`
* *sakset:* `sakset+N+Pl+Nom` (Eng. # scissors)
* *saksetpa:* `sakset+N+Pl+Nom+Foc/pa`
* ★*saksetpä:* `sakset+N+Pl+Nom+Foc/pa` (is not standard language)


*Noun nominative plural front examples:*
* *pölyt:* `pöly+N+Pl+Nom`
* *häät:* `häät+N+Pl+Nom` (Eng. # wedding)

## Singular case inflection
The nouns can inflect in 15 regular cases. Most of the cases have one or two
case endings, with only varying part being the harmony vowel. For nouns with
direct consonant gradation, the most of the singular case suffixes attach
to the weak singular stem:


*Noun basic singular back examples:*
* *pata:* `pata+N+Sg+Nom` (Eng. # pot)
* *padan:* `pata+N+Sg+Gen`
* *padassa:* `pata+N+Sg+Ine`
* *padasta:* `pata+N+Sg+Ela`
* *padalla:* `pata+N+Sg+Ade`
* *padalta:* `pata+N+Sg+Abl`
* *padalle:* `pata+N+Sg+All`
* *padaksi:* `pata+N+Sg+Tra`
* *padatta:* `pata+N+Sg+Abe`


*Noun basic singular front examples:*
* *näky:* `näky+N+Sg+Nom` (Eng. # vision)
* *näyn:* `näky+N+Sg+Gen`
* *näyssä:* `näky+N+Sg+Ine`
* *näystä:* `näky+N+Sg+Ela`
* *näyllä:* `näky+N+Sg+Ade`
* *näyltä:* `näky+N+Sg+Abl`
* *näylle:* `näky+N+Sg+All`
* *näyksi:* `näky+N+Sg+Tra`
* *näyttä:* `näky+N+Sg+Abe`

For words with direct gradation the only forms that attach to strong singular
stem may be essive and possessive's of nominative or genitive:


*Noun basic singular strong back examples:*
* *patana:* `pata+N+Sg+Ess`
* *patani:* `pata+N+Sg+Nom+PxSg1`
* *patasi:* `pata+N+Sg+Gen+PxSg2`


*Noun basic singular strong front examples:*
* *näkynä:* `näky+N+Sg+Ess`
* *näkyni:* `näky+N+Sg+Nom+PxSg1`
* *näkysi:* `näky+N+Sg+Gen+PxSg2`

## Plural inflection


*Noun basic plural back examples:*
* *padoilta:* `pata+N+Pl+Abl`
* *padoilla:* `pata+N+Pl+Ade`
* *padoille:* `pata+N+Pl+All`
* *padoista:* `pata+N+Pl+Ela`
* *padoissa:* `pata+N+Pl+Ine`
* *padoiksi:* `pata+N+Pl+Tra`
* *padoitta:* `pata+N+Pl+Abe`
* *padoin:* `pata+N+Pl+Ins`


*Noun basic plural front examples:*
* *näyillä:* `näky+N+Pl+Ade`
* *näyiltä:* `näky+N+Pl+Abl`
* *näyille:* `näky+N+Pl+All`
* *näyissä:* `näky+N+Pl+Ine`
* *näyistä:* `näky+N+Pl+Ela`
* *näyiksi:* `näky+N+Pl+Tra`
* *näyittä:* `näky+N+Pl+Abe`
* *näyin:* `näky+N+Pl+Ins`

The strong plural stem of words with direct gradation contains only essive
and comitative:


*Noun basic plural back strong examples:*
* *patoina:* `pata+N+Pl+Ess`
* *patoine:* `pata+N+Com`


*Noun basic plural front strong examples:*
* *näkyinä:* `näky+N+Pl+Ess`
* *näkyine:* `näky+N+Com`

## Cases with allomorphic variation
The nominal cases which can take several different suffixes are singular
and plural partitives, singular and plural illatives and plural genitives.
After stem variation the selection of these allomorphs is the main factor
of morphological classification of nouns.

The reconstructed historical suffix for partitives of Finnish is ða, ðä, the
current variants according to that theory would be the different
realisations of extinct ð.

For basic vowel stems the partitive suffix is a, ä.

*Noun singular partitive a examples:*
* *politiikka:* `politiikka+N+Sg+Nom` (Eng. # politics)
* *politiikkaa:* `politiikka+N+Sg+Par`


*Noun singular partitive ä examples:*
* *kysyntä:* `kysyntä+N+Sg+Nom` (Eng. # demand)
* *kysyntää:* `kysyntä+N+Sg+Par`

In stems other than -a, -ä stems, the 3rd possessive suffix may appear in
-an, -än form after the partitive suffix.

*Noun singular partitive a poss aan examples:*
* *pato:* `pato+N+Sg+Nom` (Eng. # dam)
* *patoa:* `pato+N+Sg+Par`
* *patoaan:* `pato+N+Sg+Par+PxSg3`


*Noun singular partitive ä poss ään examples:*
* *näkyä:* `näky+N+Sg+Par`
* *näkyään:* `näky+N+Sg+Par+PxSg3`

The consonant stems and long vowel stems regularly take -ta, -tä suffix
for singular partitives. It is up to interpretation whether the partitive
suffix of the -e^ stem is considered to be -tta, -ttä, or just -ta, -tä.
In principle the consonant that disappeared from -e^ stems could be from
that.


*Noun singular partitive ta examples:*
* *taimen:* `taimen+N+Sg+Nom` (Eng. # trout)
* *taimenta:* `taimen+N+Sg+Par`


*Noun singular partitive tä examples:*
* *tie:* `tie+N+Sg+Nom` (Eng. # road)
* *tietä:* `tie+N+Sg+Par`
* *vene:* `vene+N+Sg+Nom` (Eng. # boat)
* *venettä:* `vene+N+Sg+Par`

The singular illatives have more variants.

The variant with intervening h attaches to long vowel stems:

*Noun singular illative han examples:*
* *maa:* `maa+N+Sg+Nom` (Eng. # earth)
* *maahan:* `maa+N+Sg+Ill`
* *maahansa:* `maa+N+Sg+Ill+PxSg3`


*Noun singular illative hen back examples:*
* *toffee:* `toffee+N+Sg+Nom` (Eng. # taffy)
* *toffeehen:* `toffee+N+Sg+Ill`


*Noun singular illative hen front examples:*
* *tiehen:* `tie+N+Sg+Ill`


*Noun singular illative hin back examples:*
* *hai:* `hai+N+Sg+Nom` (Eng. # shark)
* *haihin:* `hai+N+Sg+Ill`


*Noun singular illative hin front examples:*
* *pii:* `pii+N+Sg+Nom` (Eng. # silica)
* *piihin:* `pii+N+Sg+Ill`


*Noun singular illative hon examples:*
* *suo:* `suo+N+Sg+Nom` (Eng. # swamp)
* *suohon:* `suo+N+Sg+Ill`


*Noun singular illative hun examples:*
* *puu:* `puu+N+Sg+Nom` (Eng. # tree)
* *puuhun:* `puu+N+Sg+Ill`


*Noun singular illative hyn examples:*
* *pyy:* `pyy+N+Sg+Nom` (Eng. # kind of a bird)
* *pyyhyn:* `pyy+N+Sg+Ill`


*Noun singular illative hän examples:*
* *pää:* `pää+N+Sg+Nom` (Eng. # head)
* *päähän:* `pää+N+Sg+Ill`


*Noun singular illative hän examples:*
* *työ:* `työ+N+Sg+Nom` (Eng. # job)
* *työhön:* `työ+N+Sg+Ill`

The stems with short vowel do not have the intervening h.


*Noun singular illative an examples:*
* *kirja:* `kirja+N+Sg+Nom` (Eng. # book)
* *kirjaan:* `kirja+N+Sg+Ill`


*Noun singular illative en ba<ck examples:*
* *tuli:* `tuli+N+Sg+Nom` (Eng. # fire)
* *tuleen:* `tuli+N+Sg+Ill`


*Noun singular illative en front examples:*
* *hiiri:* `hiiri+N+Sg+Nom` (Eng. # mouse)
* *hiireen:* `hiiri+N+Sg+Ill`


*Noun singular illative in back examples:*
* *ruuvi:* `ruuvi+N+Sg+Nom` (Eng. # screw)
* *ruuviin:* `ruuvi+N+Sg+Ill`


*Noun singular illative in front examples:*
* *tyyli:* `tyyli+N+Sg+Nom` (Eng. # style)
* *tyyliin:* `tyyli+N+Sg+Ill`


*Noun singular illative on examples:*
* *taloon:* `talo+N+Sg+Ill`


*Noun singular illative un examples:*
* *nielu:* `nielu+N+Sg+Nom` (Eng. # throat)
* *nieluun:* `nielu+N+Sg+Ill`


*Noun singular illative yn examples:*
* *näkyyn:* `näky+N+Sg+Ill`


*Noun singular illative än examples:*
* *räkä:* `räkä+N+Sg+Nom` (Eng. # booger)
* *räkään:* `räkä+N+Sg+Ill`


*Noun singular illative ön examples:*
* *tönö:* `tönö+N+Sg+Nom` (Eng. # shack)
* *tönöön:* `tönö+N+Sg+Ill`

Bisyllabic long vowel stems have illative suffix of -seen.

*Noun singular illative seen back examples:*
* *tienoo:* `tienoo+N+Sg+Nom` (Eng. # neighbourhood)
* *tienooseen:* `tienoo+N+Sg+Ill`


*Noun singular illative seen front examples:*
* *miljöö:* `miljöö+N+Sg+Nom` (Eng. # millieu)
* *miljööseen:* `miljöö+N+Sg+Ill`

Plural genitives have the most variants. Especially, many words have more or
less free variation between handful of choices.

*Noun plural genitive iden back examples:*
* *aavikko:* `aavikko+N+Sg+Nom`
* *aavikoiden:* `aavikko+N+Pl+Gen`


*Noun plural genitive iden front examples:*
* *kännykkä:* `kännykkä+N+Sg+Nom` (Eng. # mobile phone)
* *kännyköiden:* `kännykkä+N+Pl+Gen`


*Noun plural genitive itten back examples:*
* *tienoitten:* `tienoo+N+Pl+Gen`


*Noun plural genitive itten front examples:*
* *teitten:* `tie+N+Pl+Gen`


*Noun plural genitive ien back examples:*
* *nainen:* `nainen+N+Sg+Nom` (Eng. # woman)
* *naisien:* `nainen+N+Pl+Gen`


*Noun plural genitive ien back examples:*
* *murha:* `murha+N+Sg+Nom` (Eng. # murder)
* *murhien:* `murha+N+Pl+Gen`


*Noun plural genitive jen back examples:*
* *alivalikkojen:* `alivalikko+N+Pl+Gen`


*Noun plural genitive jen front examples:*
* *pyhäkkö:* `pyhäkkö+N+Sg+Nom` (Eng. # sanctum)
* *pyhäkköjen:* `pyhäkkö+N+Pl+Gen`


*Noun plural genitive ten back examples:*
* *sisar:* `sisar+N+Sg+Nom` (Eng. # sister)
* *sisarten:* `sisar+N+Pl+Gen`


*Noun plural genitive ten front examples:*
* *väännin:* `väännin+N+Sg+Nom` (Eng. # turner)
* *väänninten:* `väännin+N+Pl+Gen`

The -in suffix for plural genitive that goes with singular stem is always
markedly archaic. Most commonly it appears in compound words.


*Noun plural genitive rare in back examples:*
* *patain:* `pata+N+Pl+Gen+Use/Rare`


*Noun plural genitive rare in front examples:*
* *tyrmä:* `tyrmä+N+Sg+Nom` (Eng. # prison)
* *tyrmäin:* `tyrmä+N+Pl+Gen+Use/Rare`

Plural partitive has a few variants:


*Noun plural partitive ia examples:*
* *koiria:* `koira+N+Pl+Par`


*Noun plural partitive iä examples:*
* *kärsä:* `kärsä+N+Sg+Nom` (Eng. # trunk)
* *kärsiä:* `kärsä+N+Pl+Par`


*Noun plural partitive ita examples:*
* *tienoita:* `tienoo+N+Pl+Par`


*Noun plural partitive itä examples:*
* *teitä:* `tie+N+Pl+Par`


*Noun plural partitive ja examples:*
* *lepakkoja:* `lepakko+N+Pl+Par`


*Noun plural partitive jä examples:*
* *pyhäkköjä:* `pyhäkkö+N+Pl+Par`

And plural illative has few variants:


*Noun plural illative ihin back examples:*
* *taloihin:* `talo+N+Pl+Ill`


*Noun plural illative ihin front examples:*
* *näkyihin:* `näky+N+Pl+Ill`


*Noun plural illative iin back examples:*
* *koiriin:* `koira+N+Pl+Ill`


*Noun plural illative iin front examples:*
* *sieni:* `sieni+N+Sg+Nom` (Eng. # mushroom)
* *sieniin:* `sieni+N+Pl+Ill`


*Noun plural illative isiin back examples:*
* *sokea:* `sokea+N+Sg+Nom`
* *sokeisiin:* `sokea+N+Pl+Ill`




*Noun plural illative isiin front examples:*
* *kevät:* `kevät+N+Sg+Nom` (Eng. # spring)
* *keväisiin:* `kevät+N+Pl+Ill`



## Possessive suffixes
Possessives come optionally after the case suffixes. For consonant final
cases the possessives assimilate or eat the final part of the case ending
or stem.

*Noun possessive back examples:*
* *taloni:* `talo+N+Sg+Nom+PxSg1`
* *talosi:* `talo+N+Sg+Nom+PxSg2`
* *talonsa:* `talo+N+Sg+Nom+PxSg3`
* *talomme:* `talo+N+Sg+Nom+PxPl1`
* *talonne:* `talo+N+Sg+Nom+PxPl2`


*Noun possessive front examples:*
* *tönöni:* `tönö+N+Sg+Nom+PxSg1`
* *tönösi:* `tönö+N+Sg+Nom+PxSg2`
* *tönönsä:* `tönö+N+Sg+Nom+PxSg3`
* *tönömme:* `tönö+N+Sg+Nom+PxPl1`
* *tönönne:* `tönö+N+Sg+Nom+PxPl2`

The possessive suffix of form -an, -än, attaches to some long vowel
stems:


*Noun possessive an examples:*
* *taloaan:* `talo+N+Sg+Par+PxPl3`


*Noun possessive back en examples:*
* *tienookseen:* `tienoo+N+Sg+Tra+PxSg3`


*Noun possessive front en examples:*
* *tiekseen:* `tie+N+Sg+Tra+PxSg3`


*Noun possessive än examples:*
* *tönöään:* `tönö+N+Sg+Par+PxPl3`

## Noun clitics
Clitics can attach to any word-form, including one that already has a clitic.
Clitics do not modify the form they attach to and are simply concatenated to
the end.


*Noun clitic back examples:*
* *talokaan:* `talo+N+Sg+Nom+Foc/kaan`


*Noun clitic front examples:*
* *tönökään:* `tönö+N+Sg+Nom+Foc/kaan`

















## Noun compounding
Nouns form compounds productively. The non-final parts of regular compounds
are singular nominatives, or singular or plural genitives of nouns. The final
parts are nominals and inflect regularly.


* examples:*
* *talojuttu:* `talo+N+Sg+Nom#juttu+N+Sg+Nom`


* examples:*
* *talonjuttu:* `talo+N+Sg+Gen#juttu+N+Sg+Nom`


* examples:*
* *naisienjuttu:* `nainen+N+Pl+Gen#juttu+N+Sg+Nom`


* examples:*
* *lepakoidenjuttu:* `lepakko+N+Pl+Gen#juttu+N+Sg+Nom`


* examples:*
* *vanhempainjuttu:* `vanhempi+N+Pl+Gen#juttu+N+Sg+Nom`


* examples:*
* *lepakoittenjuttu:* `lepakko+N+Pl+Gen#juttu+N+Sg+Nom`


* examples:*
* *talojenjuttu:* `talo+N+Pl+Gen#juttu+N+Sg+Nom`


* examples:*
* *miestenjuttu:* `mies+N+Pl+Gen#juttu+N+Sg+Nom`



* examples:*
* *aakkostamisjuttu:* `aakkostaminen+N+Der/s#juttu+N+Sg+Nom`
















































































































































































































































































































































































































































# Numeral inflection
Numeral inflection is like nominal, except that numerals compound in all
forms which requires great amount of care in the inflection patterns.








 * **LEXICON ARABICCASES**  adds +Arab

 * **LEXICON ARABICCASE**  adds +Arab

 * **LEXICON ARABICCASE0**  adds +Arab


 * **LEXICON DIGITCASES**  to distinguish between 0 and oblique

 * **LEXICON DIGITCASE0**


 * **LEXICON DIGITCASE**






 * **LEXICON ROMNUMTAGOBL**








# Original file


*Numeral nominative back examples:*
* *kaksi:* `kaksi+Num+Card+Sg+Nom` (Eng. # two)


*Numeral nominative front examples:*
* *yksi:* `yksi+Num+Card+Sg+Nom` (Eng. # one)


*Numeral nominative plural back examples:*
* *kahdet:* `kaksi+Num+Card+Pl+Nom`


*Numeral nominative plural front examples:*
* *yhdet:* `yksi+Num+Card+Pl+Nom`


*Numeral weak singular back examples:*
* *kahden:* `kaksi+Num+Card+Sg+Gen`
* *kahdella:* `kaksi+Num+Card+Sg+Ade`
* *kahdelta:* `kaksi+Num+Card+Sg+Abl`
* *kahdelle:* `kaksi+Num+Card+Sg+All`
* *kahdessa:* `kaksi+Num+Card+Sg+Ine`
* *kahdesta:* `kaksi+Num+Card+Sg+Ela`
* *kahdeksi:* `kaksi+Num+Card+Sg+Tra`
* *kahdetta:* `kaksi+Num+Card+Sg+Abe`


*Numeral weak singular front examples:*
* *yhden:* `yksi+Num+Card+Sg+Gen`
* *yhdellä:* `yksi+Num+Card+Sg+Ade`
* *yhdeltä:* `yksi+Num+Card+Sg+Abl`
* *yhdelle:* `yksi+Num+Card+Sg+All`
* *yhdessä:* `yksi+Num+Card+Sg+Ine`
* *yhdestä:* `yksi+Num+Card+Sg+Ela`
* *yhdeksi:* `yksi+Num+Card+Sg+Tra`
* *yhdettä:* `yksi+Num+Card+Sg+Abe`


*Numeral strong singular back examples:*
* *kahtena:* `kaksi+Num+Card+Sg+Ess`


*Numeral strong singular front examples:*
* *yhtenä:* `yksi+Num+Card+Sg+Ess`


*Numeral weak plural back examples:*
* *kaksilla:* `kaksi+Num+Card+Pl+Ade`
* *kaksilta:* `kaksi+Num+Card+Pl+Abl`
* *kaksille:* `kaksi+Num+Card+Pl+All`
* *kaksissa:* `kaksi+Num+Card+Pl+Ine`
* *kaksista:* `kaksi+Num+Card+Pl+Ela`
* *kaksiksi:* `kaksi+Num+Card+Pl+Tra`
* *kaksitta:* `kaksi+Num+Card+Pl+Abe`


*Numeral weak plural front examples:*
* *yksillä:* `yksi+Num+Card+Pl+Ade`
* *yksiltä:* `yksi+Num+Card+Pl+Abl`
* *yksille:* `yksi+Num+Card+Pl+All`
* *yksissä:* `yksi+Num+Card+Pl+Ine`
* *yksistä:* `yksi+Num+Card+Pl+Ela`
* *yksiksi:* `yksi+Num+Card+Pl+Tra`
* *yksittä:* `yksi+Num+Card+Pl+Abe`


*Numeral weak plural back strong examples:*
* *kaksina:* `kaksi+Num+Card+Pl+Ess`
* *kaksine:* `kaksi+Num+Card+Com`


*Numeral weak plural front strong examples:*
* *yksinä:* `yksi+Num+Card+Pl+Ess`
* *yksine:* `yksi+Num+Card+Com`



*Numeral singular partitive a examples:*
* *kahdeksaa:* `kahdeksan+Num+Card+Sg+Par` (Eng. # eight)


*Numeral singular partitive ä examples:*
* *neljää:* `neljä+Num+Card+Sg+Par` (Eng. # four)


*Numeral singular partitive a poss aan examples:*
* *kolmea:* `kolme+Num+Card+Sg+Par` (Eng. # three)


*Numeral singular partitive ta examples:*
* *kuutta:* `kuusi+Num+Card+Sg+Par` (Eng. # six)


*Numeral singular partitive tä examples:*
* *viittä:* `viisi+Num+Card+Sg+Par` (Eng. # five)


*Numeral singular illative an examples:*
* *kahdeksaan:* `kahdeksan+Num+Card+Sg+Ill`


*Numeral singular illative en back examples:*
* *kolmeen:* `kolme+Num+Card+Sg+Ill`


*Numeral singular illative en front examples:*
* *viiteen:* `viisi+Num+Card+Sg+Ill`


*Numeral singular illative in back examples:*
* *miljardiin:* `miljardi+Num+Card+Sg+Ill` (Eng. # billion)


*Numeral singular illative än examples:*
* *neljään:* `neljä+Num+Card+Sg+Ill`


*Numeral plural partitive ia examples:*
* *kaksia:* `kaksi+Num+Card+Pl+Par`


*Numeral plural partitive iä examples:*
* *neljiä:* `neljä+Num+Card+Pl+Par`


*Numeral plural partitive ja examples:*
* *miljardeja:* `miljardi+Num+Card+Pl+Par`


*Numeral plural genitive ien back examples:*
* *kaksien:* `kaksi+Num+Card+Pl+Gen` (Eng. NUM_BACK_CLIT_OPT)


*Numeral plural genitive ien front examples:*
* *yksien:* `yksi+Num+Card+Pl+Gen` (Eng. NUM_BACK_CLIT_OPT)


*Numeral plural genitive jen back examples:*
* *satojen:* `sata+Num+Card+Pl+Gen` (Eng. # hundred NUM_BACK_CLIT_OPT)


*Numeral plural genitive ten back examples:*
* *kuutten:* `kuusi+Num+Card+Pl+Gen` (Eng. NUM_BACK_CLIT_OPT)


*Numeral plural genitive ten front examples:*
* *viitten:* `viisi+Num+Card+Pl+Gen` (Eng. NUM_BACK_CLIT_OPT)


*Numeral plural genitive in back examples:*
* *yhdeksäin:* `yhdeksän+Num+Card+Pl+Gen+Use/Rare` (Eng. NUM_BACK_CLIT_OPT)


*Numeral plural genitive in front examples:*
* *neljäin:* `neljä+Num+Card+Pl+Gen+Use/Rare` (Eng. NUM_BACK_CLIT_OPT)


*Numeral plural illaive ihin bavk examples:*
* *miljardeihin:* `miljardi+Num+Card+Pl+Ill`


*Numeral plural illaive iin back examples:*
* *kaksiin:* `kaksi+Num+Card+Pl+Ill`


*Numeral plural illaive iin front examples:*
* *yksiin:* `yksi+Num+Card+Pl+Ill`



*Numeral possessive back examples:*
* *kahteni:* `kaksi+Num+Card+Sg+Nom+PxSg1`


*Numeral possessive front examples:*
* *yhteni:* `yksi+Num+Card+Sg+Nom+PxSg1`


*Numeral possessive back aan examples:*
* *kolmeaan:* `kolme+Num+Card+Sg+Par+PxSg3`


*Numeral possessive back eenback examples:*
* *kahdekseen:* `kaksi+Num+Card+Sg+Tra+PxSg3`


*Numeral possessive back een front examples:*
* *neljäkseen:* `neljä+Num+Card+Sg+Tra+PxSg3`


*Numeral possessive back ään examples:*
* *viittään:* `viisi+Num+Card+Sg+Par+PxSg3`


*Numeral clitic back examples:*
* *kaksihan:* `kaksi+Num+Card+Sg+Nom+Foc/han`


*Numeral clitic front examples:*
* *yksihän:* `yksi+Num+Card+Sg+Nom+Foc/han`

















# Finnish Numerals







 Numerals have been split in three sections, the compounding parts
 of cardinals and ordinals, and the non-compounding ones:

*Numeral examples:*
* *kaksikymmentäkolmetuhatta:* `kaksi+Num+Card+Sg+Nom#kymmenen+Num+Card+Sg+Par#kolme+Num+Card+Sg+Nom#tuhat+Num+Card+Sg+Par` (Eng. # 23,000)
* *kahdessadasneljäs:* `kahdes+Num+Ord+Sg+Nom#sadas+Num+Ord+Sg+Nom#neljäs+Num+Ord+Sg+Nom` (Eng. # 204rd)
* *viitisenkymmentä:* `viitisen+Num#kymmentä` (Eng. # 50-ish)

 The compounding parts of cardinals are the number multiplier words.

*cardinal examples:*
* *yksi:* `yksi+Num+Card+Sg+Nom` (Eng. # one)
* *viidelle:* `viisi+Num+Card+Sg+All` (Eng. # five)
* *tuhatta:* `tuhat+Num+Card+Sg+Par` (Eng. # thousand)

 The suffixes only appear after cardinal multipliers

*Cardinal multiplicants examples:*
* *viisikymmentä:* `viisi+Num+Card+Sg+Nom#kymmentä`
* *neljäsataatuhatta:* `neljä+Num+Card+Sg+Nom#sata+Num+Card+Sg+Par#tuhatta`

 The compounding parts of ordinals are the number multiplier words.

*Ordinal numerals examples:*
* *neljäs:* `neljäs+A+Ord+Sg+Nom`
* *viidennelle:* `viides+A+Ord+Sg+All`
* *tuhannetta:* `tuhannes+A+Ord+Sg+Par`

 The suffixes only appear after cardinal multipliers

*Ordinal multiplicants examples:*
* *viideskymmenes:* `viides+A+Ord+Sg+Nom#kymmenes`
* *neljässadastuhannes:* `neljäs+A+Ord+Sg+Nom#sadas+A+Ord+Sg+Nom#tuhannes`

 There is a set of numbers or corresponding expressions that work like them,
 but are not basic cardinals or ordinals:

*Numeral others examples:*
* *viitisenkymmentä:* `viitisen+Num#kymmentä`
* *puolikymmentä:* `puolikymmentä+Num+Sg+Nom`

## Numeral stem variation
 Numerals follow the same stem variation patterns as nouns, some of these
 being very rare to extinct for nouns. 

*Numerals 31 examples:*
* *yksi:* `yksi+Num+Card+Sg+Nom`
* *yhteen:* `yksi+Num+Card+Sg+Ill`
* *yhtenä:* `yksi+Num+Card+Sg+Ess`
* *yhdessä:* `yksi+Num+Card+Sg+Ine`
* *yhtä:* `yksi+Num+Card+Sg+Par`
* *yksiä:* `yksi+Num+Card+Pl+Par`
* *yksien:* `yksi+Num+Card+Pl+Gen`
* *yksiin:* `yksi+Num+Card+Pl+Ill`
* *yksinä:* `yksi+Num+Card+Pl+Ess`
* *yksissä:* `yksi+Num+Card+Pl+Ine`


*Numerals 31 back§ examples:*
* *kaksi:* `kaksi+Num+Card+Sg+Nom`
* *kahteen:* `kaksi+Num+Card+Sg+Ill`
* *kahtena:* `kaksi+Num+Card+Sg+Ess`
* *kahdessa:* `kaksi+Num+Card+Sg+Ine`
* *kahta:* `kaksi+Num+Card+Sg+Par`
* *kaksia:* `kaksi+Num+Card+Pl+Par`
* *kaksien:* `kaksi+Num+Card+Pl+Gen`
* *kaksiin:* `kaksi+Num+Card+Pl+Ill`
* *kaksina:* `kaksi+Num+Card+Pl+Ess`
* *kaksissa:* `kaksi+Num+Card+Pl+Ine`


*Numerals 8~5 examples:*
* *kolme:* `kolme+Num+Card+Sg+Nom`
* *kolmeen:* `kolme+Num+Card+Sg+Ill`
* *kolmena:* `kolme+Num+Card+Sg+Ess`
* *kolmessa:* `kolme+Num+Card+Sg+Ine`
* *kolmea:* `kolme+Num+Card+Sg+Par`
* *kolmia:* `kolme+Num+Card+Pl+Par`
* *kolmien:* `kolme+Num+Card+Pl+Gen`
* *kolmiin:* `kolme+Num+Card+Pl+Ill`
* *kolmina:* `kolme+Num+Card+Pl+Ess`
* *kolmissa:* `kolme+Num+Card+Pl+Ine`


*Numerals 10 examples:*
* *neljä:* `neljä+Num+Card+Sg+Nom`
* *neljää:* `neljä+Num+Card+Sg+Par`
* *neljään:* `neljä+Num+Card+Sg+Ill`
* *neljänä:* `neljä+Num+Card+Sg+Ess`
* *neljässä:* `neljä+Num+Card+Sg+Ine`
* *neljiä:* `neljä+Num+Card+Pl+Par`
* *neljien:* `neljä+Num+Card+Pl+Gen`
* *neljiin:* `neljä+Num+Card+Pl+Ill`
* *neljinä:* `neljä+Num+Card+Pl+Ess`
* *neljissä:* `neljä+Num+Card+Pl+Ine`
* *neljäin:* `neljä+Num+Card+Pl+Gen+Use/Rare`


*Numerals 27 front examples:*
* *viisi:* `viisi+Num+Card+Sg+Nom`
* *viiteen:* `viisi+Num+Card+Sg+Ill`
* *viitenä:* `viisi+Num+Card+Sg+Ess`
* *viidessä:* `viisi+Num+Card+Sg+Ine`
* *viisinä:* `viisi+Num+Card+Pl+Ess`
* *viisissä:* `viisi+Num+Card+Pl+Ine`
* *viittä:* `viisi+Num+Card+Sg+Par`
* *viitten:* `viisi+Num+Card+Pl+Gen`
* *viisiä:* `viisi+Num+Card+Pl+Par`
* *viisien:* `viisi+Num+Card+Pl+Gen`
* *viisiin:* `viisi+Num+Card+Pl+Ill`


*Numerals 27 back examples:*
* *kuusi:* `kuusi+Num+Card+Sg+Nom`
* *kuutena:* `kuusi+Num+Card+Sg+Ess`
* *kuudessa:* `kuusi+Num+Card+Sg+Ine`
* *kuusina:* `kuusi+Num+Card+Pl+Ess`
* *kuusissa:* `kuusi+Num+Card+Pl+Ine`
* *kuutta:* `kuusi+Num+Card+Sg+Par`
* *kuutten:* `kuusi+Num+Card+Pl+Gen`
* *kuusia:* `kuusi+Num+Card+Pl+Par`
* *kuusien:* `kuusi+Num+Card+Pl+Gen`
* *kuusiin:* `kuusi+Num+Card+Pl+Ill`



*Numerals 10n examples:*
* *kahdeksan:* `kahdeksan+Num+Card+Sg+Nom`
* *kahdeksaa:* `kahdeksan+Num+Card+Sg+Par`
* *kahdeksaan:* `kahdeksan+Num+Card+Sg+Ill`
* *kahdeksassa:* `kahdeksan+Num+Card+Sg+Ine`
* *kahdeksana:* `kahdeksan+Num+Card+Sg+Ess`
* *kahdeksia:* `kahdeksan+Num+Card+Pl+Par`
* *kahdeksien:* `kahdeksan+Num+Card+Pl+Gen`
* *kahdeksiin:* `kahdeksan+Num+Card+Pl+Ill`
* *kahdeksissa:* `kahdeksan+Num+Card+Pl+Ine`
* *kahdeksina:* `kahdeksan+Num+Card+Pl+Ess`
* *kahdeksain:* `kahdeksan+Num+Card+Pl+Gen+Use/Rare`


*Numerals 10n front examples:*
* *yhdeksän:* `yhdeksän+Num+Card+Sg+Nom`
* *yhdeksää:* `yhdeksän+Num+Card+Sg+Par`
* *yhdeksään:* `yhdeksän+Num+Card+Sg+Ill`
* *yhdeksässä:* `yhdeksän+Num+Card+Sg+Ine`
* *yhdeksänä:* `yhdeksän+Num+Card+Sg+Ess`
* *yhdeksiä:* `yhdeksän+Num+Card+Pl+Par`
* *yhdeksien:* `yhdeksän+Num+Card+Pl+Gen`
* *yhdeksissä:* `yhdeksän+Num+Card+Pl+Ine`
* *yhdeksinä:* `yhdeksän+Num+Card+Pl+Ess`
* *yhdeksäin:* `yhdeksän+Num+Card+Pl+Gen+Use/Rare`


*Numerals 32 examples:*
* *kymmenen:* `kymmenen+Num+Card+Sg+Nom`
* *kymmeneen:* `kymmenen+Num+Card+Sg+Ill`
* *kymmenenä:* `kymmenen+Num+Card+Sg+Ess`
* *kymmenessä:* `kymmenen+Num+Card+Sg+Ine`
* *kymmentä:* `kymmenen+Num+Card+Sg+Par`
* *kymmenten:* `kymmenen+Num+Card+Pl+Gen`
* *kymmenien:* `kymmenen+Num+Card+Pl+Gen`
* *kymmeniin:* `kymmenen+Num+Card+Pl+Ill`
* *kymmenissä:* `kymmenen+Num+Card+Pl+Ine`
* *kymmeninä:* `kymmenen+Num+Card+Pl+Ess`


*Numerals 9 examples:*
* *sata:* `sata+Num+Card+Sg+Nom`
* *satana:* `sata+Num+Card+Sg+Ess`
* *sadassa:* `sata+Num+Card+Sg+Ine`
* *sataan:* `sata+Num+Card+Sg+Ill`
* *sataa:* `sata+Num+Card+Sg+Par`
* *satojen:* `sata+Num+Card+Pl+Gen`
* *satoihin:* `sata+Num+Card+Pl+Ill`
* *sadoissa:* `sata+Num+Card+Pl+Ine`
* *satoina:* `sata+Num+Card+Pl+Ess`


*Numerals 46 examples:*
* *tuhat:* `tuhat+Num+Card+Sg+Nom`
* *tuhanteen:* `tuhat+Num+Card+Sg+Ill`
* *tuhantena:* `tuhat+Num+Card+Sg+Ess`
* *tuhannessa:* `tuhat+Num+Card+Sg+Ine`
* *tuhansien:* `tuhat+Num+Card+Pl+Gen`
* *tuhatta:* `tuhat+Num+Card+Sg+Par`
* *tuhansia:* `tuhat+Num+Card+Pl+Par`
* *tuhansien:* `tuhat+Num+Card+Pl+Gen`
* *tuhansiin:* `tuhat+Num+Card+Pl+Ill`
* *tuhansina:* `tuhat+Num+Card+Pl+Ess`
* *tuhansissa:* `tuhat+Num+Card+Pl+Ine`


*Numerals 10 examples:*
* *miljoona:* `miljoona+Num+Card+Sg+Nom`
* *miljoonana:* `miljoona+Num+Card+Sg+Ess`
* *miljoonassa:* `miljoona+Num+Card+Sg+Ine`
* *miljoonaa:* `miljoona+Num+Card+Sg+Par`
* *miljoonaan:* `miljoona+Num+Card+Sg+Ill`
* *miljoonia:* `miljoona+Num+Card+Pl+Par`
* *miljoonien:* `miljoona+Num+Card+Pl+Gen`
* *miljooniin:* `miljoona+Num+Card+Pl+Ill`
* *miljoonissa:* `miljoona+Num+Card+Pl+Ine`
* *miljoonina:* `miljoona+Num+Card+Pl+Ess`



*Numerals 5 examples:*
* *miljardi:* `miljardi+Num+Card+Sg+Nom`
* *miljardina:* `miljardi+Num+Card+Sg+Ess`
* *miljardissa:* `miljardi+Num+Card+Sg+Ine`
* *miljardeina:* `miljardi+Num+Card+Pl+Ess`
* *miljardeissa:* `miljardi+Num+Card+Pl+Ine`
* *miljardia:* `miljardi+Num+Card+Sg+Par`
* *miljardiin:* `miljardi+Num+Card+Sg+Ill`
* *miljardeja:* `miljardi+Num+Card+Pl+Par`
* *miljardien:* `miljardi+Num+Card+Pl+Gen`
* *miljardeihin:* `miljardi+Num+Card+Pl+Ill`



*Numerals 5 more examples:*
* *Googol:* `Googol+Num+Card+Sg+Nom`


*Numerals 5 moremore examples:*
* *pari:* `pari+Num+Card+Sg+Nom`
* *parina:* `pari+Num+Card+Sg+Ess`
* *parissa:* `pari+Num+Card+Sg+Ine`
* *pareissa:* `pari+Num+Card+Pl+Ine`
* *pareina:* `pari+Num+Card+Pl+Ess`
* *paria:* `pari+Num+Card+Sg+Par`
* *pareja:* `pari+Num+Card+Pl+Par`
* *parien:* `pari+Num+Card+Pl+Gen`
* *pareihin:* `pari+Num+Card+Pl+Ill`
* *pariin:* `pari+Num+Card+Sg+Ill`



*Numerals 38 examples:*
* *ensimmäinen:* `ensimmäinen+A+Ord+Sg+Nom`
* *ensimmäisenä:* `ensimmäinen+A+Ord+Sg+Ess`
* *ensimmäisessä:* `ensimmäinen+A+Ord+Sg+Ine`
* *ensimmäistä:* `ensimmäinen+A+Ord+Sg+Par`
* *ensimmäisten:* `ensimmäinen+A+Ord+Pl+Gen`
* *ensimmäisiä:* `ensimmäinen+A+Ord+Pl+Par`
* *ensimmäisiin:* `ensimmäinen+A+Ord+Pl+Ill`
* *ensimmäisinä:* `ensimmäinen+A+Ord+Pl+Ess`
* *ensimmäisissä:* `ensimmäinen+A+Ord+Pl+Ine`


*Numerals 38 back examples:*
* *toinen:* `toinen+A+Ord+Sg+Nom`
* *toiseen:* `toinen+A+Ord+Sg+Ill`
* *toista:* `toinen+A+Ord+Sg+Par`
* *toisten:* `toinen+A+Ord+Pl+Gen`
* *toisien:* `toinen+A+Ord+Pl+Gen`
* *toisia:* `toinen+A+Ord+Pl+Par`
* *toisiin:* `toinen+A+Ord+Pl+Ill`
* *toisena:* `toinen+A+Ord+Sg+Ess`
* *toisessa:* `toinen+A+Ord+Sg+Ine`
* *toisissa:* `toinen+A+Ord+Pl+Ine`
* *toisina:* `toinen+A+Ord+Pl+Ess`


*Numerals 45 examples:*
* *kolmas:* `kolmas+A+Ord+Sg+Nom`
* *kolmantena:* `kolmas+A+Ord+Sg+Ess`
* *kolmannessa:* `kolmas+A+Ord+Sg+Ine`
* *kolmanteen:* `kolmas+A+Ord+Sg+Ill`
* *kolmatta:* `kolmas+A+Ord+Sg+Par`
* *kolmansia:* `kolmas+A+Ord+Pl+Par`
* *kolmansien:* `kolmas+A+Ord+Pl+Gen`
* *kolmansissa:* `kolmas+A+Ord+Pl+Ine`
* *kolmansina:* `kolmas+A+Ord+Pl+Ess`


*Numerals 45 fron examples:*
* *neljäs:* `neljäs+A+Ord+Sg+Nom`
* *neljänteen:* `neljäs+A+Ord+Sg+Ill`
* *neljäntenä:* `neljäs+A+Ord+Sg+Ess`
* *neljännessä:* `neljäs+A+Ord+Sg+Ine`
* *neljättä:* `neljäs+A+Ord+Sg+Par`
* *neljänsiä:* `neljäs+A+Ord+Pl+Par`
* *neljänsien:* `neljäs+A+Ord+Pl+Gen`
* *neljänsiin:* `neljäs+A+Ord+Pl+Ill`
* *neljänsissä:* `neljäs+A+Ord+Pl+Ine`
* *neljänsinä:* `neljäs+A+Ord+Pl+Ess`






































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































# Particles
 The particles are all words that do not inflect at all. For compatibility
 reasons subsets of particles have been set off to classes like conjunctions,
 adverbs, adpositions, and interjections. The ones that are *not* in those classes are left here as
 particles.

 Examples:

*Particles examples:*
* *ahaa:* `ahaa+Pcle`
* *edes:* `edes+Pcle`
















































































# Adpositions
 Adpositions are morphologically nominals that have defective inflection
 patterns. Some of them come from forms of nominals that are no longer
 used. The adpositions are classified along whether they take possessives
 clitics, or not. They also have slight syntactic and semantic differences,
 the syntactic differences are coded in the analyses to be compatible with
 other languages, but for most intents and purposes all adpositions can
 appear in both syntactic positions, after and before the head word.

 Examples:

*Adpositions examples:*
* *alta:* `alta+Po`
* *alleen:* `alle+Po+PxSg3`
* *irti:* `irti+Pr`
* *irtikö:* `irti+Pr+Qst`

# Prefixes
Prefixes are bound morphs that can appear in beginning of the compounds,
mostly forms of nominals. Finnish does not have almost any real prefix 
morphemes.


*Prefixes examples:*
* *alabanaani:* `ala+N+Pref#banaani+N+Sg+Nom` (Eng. # lower banana)
* *erikoistyyppi:* `erikois+N+Pref#tyyppi+N+Sg+Nom` (Eng. # special guy)
# Pronouns
Pronouns are a closed special sub class of nouns. Morphologically pronouns
have often defective, heteroclitic or otherwise irregular inflectional
patterns, and certain pronouns have an morphophonologically distinct
accusative case, extinct from other noun classes 
Further reading: [VISK §§ 
100|http://scripta.kotus.fi/visk/sisalto.php?p=100] – 104, Semantics ...
[VISK § 7XX](http://scripta.kotus.fi/visk/sisalto.php?p=7XX)

Pronouns are subdivided into categories by semantic and syntactic means.
Semantic categories delimit the type of referents (humane, sentient,
object), qualification and quantification. (interrogative, quantor).
Morphosyntactically distinct is class of proadjectives, that inflect and
act like adjectives.
There are six personal pronouns for the six deictic references used; first,
second and third singular and plural. The personal pronouns have separate
accusative cases marked by t suffix. The pronouns in standard literary
Finnish are *minä* (I), *sinä* (you), *hän* (he), *me* (we), *te* (you),
*he* (they). 
Further reading: [VISK § 100](http://scripta.kotus.fi/visk/sisalto.php?p=100)

*Personal pronouns examples:*
* *minä:* `minä+Pron+Pers+Sg+Nom` (Eng. # me)
* *sinut:* `sinä+Pron+Pers+Sg+Acc`
* *heille:* `he+Pron+Pers+Pl+All`


The personal pronouns are among the most dialectally varied words of the
Finnish language. The pronouns forms are one of the factors separating
eastern dialects from the western ones. The personal pronouns of eastern
dialects are *mie*, *sie*, (*hää*, *hiä*), *myö*, *työ*, *hyö* resp.;
The third singular being rare in modern use. |citation-needed|

In the western dialects the pronouns are *mää*, *sää* for first and second
singular, and more variedly *meitti*, *teitti*, *heitti* for plurals.

In standard spoken Finnish, and in many cases even in written form, the
words *mä* and *sä* are more common and preferred to longer minä and sinä
for first and second singular respectively. In practice the distinction is
much like between Estonian corresponding pronouns, but official norm still
recommends only the long forms.
For third singular the nominative form is *hän* as in standard written 
language, however the inflection is without intervening *-ne-* part.
In old literary Finnish and poetic language the forms *ma* and *sa* are
still used.

There are six demonstrative pronouns for six non-personal references. In
standard written Finnish these are *tämä* (this), *tuo* (that), *se* (it),
*nämä* (these), *nuo* (those), *ne* (those).

Further reading: [VISK § 101](http://scripta.kotus.fi/visk/sisalto.php?p=101)

*Demonstrative pronouns examples:*
* *tämä:* `tämä+Pron+Dem+Sg+Nom`
* *tuolle:* `tuo+Pron+Dem+Sg+All`

In standard spoken Finnish the demonstrative pronouns are commonly
*tää*, *toi*, *nää*, *noi* instead of *tämä*, *tuo*, *nämä*, *nuo*.

Interrogative pronouns are used in question clauses. The basic
interrogatives in standard written Finnish are *kuka* (who), *mikä* (what),
*kumpi* (which); *millainen* (what kind of), *kuinka* (how),
*miksi* (what for). 
Further reading: [VISK §734](http://scripta.kotus.fi/visk/sisalto.php?p=734)

*Interrogative pronouns examples:*
* *kuka:* `kuka+Pron+Interr+Sg+Nom` (Eng. # who)
* *ketä:* `kuka+Pron+Interr+Sg+Par`
* *kenet:* `kuka+Pron+Interr+Sg+Acc`

The stem of *kuka* is shortened by from *kene* to *ke* in spoken language.

Few forms of *kuka* based on *ken* stem and *ku* stem have become archaic.
Fuhrer reading: [VISK §102](http://scripta.kotus.fi/visk/sisalto.php?p=102)
Also the short form of *mi* is archaic and limited to poetic
use. |citation-needed|

Relative pronouns are *kuka*, *joka* and *mikä* (which, whose). VISK §735|
They are morphologically indistinct from corresponding interrogative
pronouns.

*Relative pronouns examples:*
* *kuka:* `kuka+Pron+Rel+Sg+Nom`




Quantor pronouns correspond to existential and universal quantifiers and
their negations. The generic quantors are *joku* (someone),
*jokin* (something), *jokainen* (everyone), *kaikki* (everything),
*kukin* (each one), *kukaan* (no one), *mikään* (nothing), *jokunen*,
*muutama*, *harva* (a few), *moni* (many) and *useampi* (more). The
dual quantors, quantifying over set of two objects are *jompikumpi* (either
or), *kumpikin*, *molemmat* (both), *kumpikaan* (neither). 
[VISK §740](http://scripta.kotus.fi/visk/sisalto.php?p=740)
The quantor pronouns subsume the class of indefinite pronouns used in
older grammar defintions. 
[VISK §742](http://scripta.kotus.fi/visk/sisalto.php?p=742)
The indefinite quantifiers are classified as
indefinite quantors for the sake of compatibility. This covers *joku*,
*jokin*, *jompikumpi*, as well as specific
*eräs*, *muuan* (some), *yksi* (one).  
Further reading [VISK §746](http://scripta.kotus.fi/visk/sisalto.php?p=746)
– 749.

*Quantor pronouns examples:*
* *joku:* `joku+Pron+Qu+Indef+Sg+Nom`
* *jotkut:* `joku+Pron+Qu+Indef+Pl+Nom`




Reflexive pronoun is the word *itse* refering to self, usually but not
always coupled with possessive suffix to denote the referent. 
Further reading: [VISK §729](http://scripta.kotus.fi/visk/sisalto.php?p=729)

*Reflexive pronouns examples:*
* *itse:* `itse+Pron+Refl+Sg+Nom`

Reciprocal pronoun is *toinen* refering to each other. It uses possessive
suffix to delimit the reciprocal group. 
Further reading: [VISK §732](http://scripta.kotus.fi/visk/sisalto.php?p=732)

*Reciprocal pronouns examples:*
* *toisiamme:* `toinen+Pron+Recipr+Pl+Par+PxPl1`

Proadjectives are pronouns that act in place of adjectives syntactically.
They are formed by compounds (or derivations) of pronoun and *lainen* or
*moinen* (such as). 
Further reading: [VISK §715](http://scripta.kotus.fi/visk/sisalto.php?p=715)

*Proadjectives examples:*
* *jollainen:* `jollainen+A+Rel+Sg+Nom`

Proadverbs are the pronouns that have lexicalised into adverbs by their
syntax and semantics. 
Further reading: [VISK §715](http://scripta.kotus.fi/visk/sisalto.php?p=715)

*Proadverbs examples:*
* *missä:* `missä+Adv+Interr+Ine`

forms of *jompi* may not exist as free morphs. The marginal forms of
*monias* are extinct. Oddly enough, the semireduplicative intensifier
monituinen is nowhere to be found in VISK either.

Marginally in the pro word category are nouns, adjectives and adverbs
refering to equivalence in comparative context since they are
also otherwise lacking meaning like other pro words. This
fgroup includes words *sama* (same), *eri* (different), *muu* (other),
*toinen* (another), and their derivations. 
Further reading: [VISK §766](http://scripta.kotus.fi/visk/sisalto.php?p=766)

In spoken language the supposedly non-inflecting *eri* has common inflected
forms.




































































































































































































































































































































































































































































































































































































































































































































































# Proper nouns
 Proper nouns are morphologically indistinct subset of nouns. They have
 some orthographical differences, required capitalisations and compounding
 with hyphens. The derivations may lowercase. They may be classified 
 semantically to match other giellatekno things in the future.


*Proper nouns examples:*
* *Kalle:* `Kalle+N+Prop+Sg+Nom`
* *Joensuu:* `Joensuu+N+Prop+Sg+Nom`


 details see [noun-stems.html].
 The proper nouns are classified and inflected along noun patterns, for 



Many of Proper nouns inflect like nouns... however, compound differently





































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































*Nouns 20 ä examples:*
* *Hyvinkää:* `Hyvinkää+N+Prop+Sg+Nom` (Eng. # Hyvinkää)
* *Hyvinkäätä:* `Hyvinkää+N+Prop+Sg+Par`
* *Hyvinkäähän:* `Hyvinkää+N+Prop+Sg+Ill`
* *Hyvinkääseen:* `Hyvinkää+N+Prop+Sg+Ill`

# Other symbols
Punctuation characters detailed here are the characters that appear commonly
in Finnish texts, but are not part of words or linguistic content. The
punctuations control clause and sentence level annotations, and range
from full stops and commas to brackets. While punctuation symbols might
have limited use as inflecting units, the ones described here refer to
punctuation symbols as used in their primary purpose, in isolation. The
part of language norms controlling punctuation are from orthography and
the references of punctuation in good language use are not from the grammar
but issues of Kielikello journal on good Finnish language use. The most
current issue on punctuation was [Kielikello 
2/2006|http://arkisto.kielikello.fi/index.php?mid=2&pid=12&maid=110] (N.B.
you may need to buy subscription or route through university servers).


The primary punctuation marks are sentence final punctuation, they mark
the end of a sentence. The most typical of these is full stop symbol, which
ends neutral sentences. The exclamation mark and question mark end
exclamative and questioning sentences respectively. An elliptic or unfinished
sentence is ended with three successive full stops. In sloppy writing style
it is common to use two, four or more full stops to mark an elliptic
sentence. The Unicode compatibility character ellipsis has never been used
for Finnish language and must not be used. Same applies for other
combinations of sentence ending punctuation marks, the most common of these
have separate analyses.

*Punctuation and symbols examples:*
* *.:* `.+Punct` (Eng. # full stop)

The clause level punctuation marks are used in clause boundaries. The most
typical of these is comma. The colon and semicolon are too. The clause
boundaries do not have separate semantics needed in applications so they only
have analyses for clause boundaries.


The brackets are used to offset portions of text in opening and closing
pairs. The most common pair is round brackets. Others used in Finnish are
square, curly and angle brackets, in somewhat decreasing order of commonness.
The angle brackets are commonly replaced by lower than symbol for opening and
greater than symbol for closing bracket.
The bracketed question mark is used to indicate uncertainty and bracketed
exclamation mark to indicate surprise, both of these annotations are used
within sentence as other bracketed constructions.

The quotation marks are used to offset quotations. The typical ones in
Finnish are the 9-shaped double quotation marks and apostrophes. Angle
quotation marks can also be used, primarily in books and newspapers.
It is possible to replace curly quotation marks with neutral typewriter
ones where technology limits. It is also common to see foreign quotation
marks or accent marks in place of quotation marks in sloppy writing style.

There are two different dashes in Finnish. The hyphen is used for mainly
word internally and won't appear as itself. The dash is used to offset
some sentences or mark elision. The dash symbol can be either of unicode
dash symbols or replaced with dash offset by spaces. In sloppy writing,
two hyphens are often used in place of dashes.

The space is used to separate words. For most applications the space has
separate meaning so it rarely gets used as a symbol in applications of

Less used symbols that appear in the Finnish texts; these do not have 
special analyses. A slash can be used as a replacement of the meaning 'or',
as a division slash or as a separator of verses in poem. 
Backslash is used only in computer systems.
Underscore is used only in computer systems.
The pipe is used in dictionaries as morhpeme boundary, and computer systems.
At sign is used only in computer systems.
An ampersand can be used as a replacement of the meaning 'and'.
Percent symbol is used after numeric expressions meaning 0.01 multiplier.
Permille symbol is used after numeric expressions meaning 0.001 multiplier.
§ sign is used for numbering sections etc.
Degree sign is used with measurements. The second and minute signs can be
used in conjunction with degree sign.
The plus sign, specific minus sign and plus-minus signs are used in
numeric expressions. The multiplication sign is used in numeric expressions.
The equals sign is used in formulae. The asterisk is used as a marker
for ungrammatical constructions and computer and other expressions.
Registered and trademark symbols are rarely used. Copyright symbol is
rarely used. Hash sign is used in phones and computer systems.
The doubled § sign has been used as chapter range sign. The pilcrow
sign can be used to mark chapters.
The currency signs for euro, dollar, pound sterling, cent and yen can be
used. I don't think anyone uses the currency sign ¤ ever.
The lines below this one are not from any referenced source


























































































































































































































































































































































































# Suffixes
Suffixes are bound morphs that come after nominals in compounds. Finnish 
doesn't quite have real suffixes, these are mostly compound parts.

Examples:

*Suffixes examples:*
* *banaaniaaltoinen:* `banaani+N+Sg+Nom#aaltoinen+A+Sg+Nom` (Eng. # banana waveful)



# Symbol affixes





 Verbs are the words that inflect in tense, mood, personal suffixes, and
 clitics, but verbs also have s.c. infinite inflection pattern which is
 basically nominal derivations. The dictionary entries of verbs are
 A-infinitive forms, there are no verbs in dictionary that do not end in
 a or ä. Verbs are very distinct from other classes, their classification
 is not difficult.
 The key to find unique class for a verb is to pick stems and suffixes from:
 indicative non-past 1st singular and 3rd singular, indicative past 1st 
 singular, ...


The auxiliary verbs require infinintive verbal phrase objects.
Infinitives usually: *aion tappaa*, *joudun kuolemaan*


 The verbs are classified along the stem mutations suffix assimilation,
 and harmony:

*Verb examples:*
* *kutoo:* `kutoa+V+Act+Ind+Prs+Sg3` (Eng. # knit)
* *kudotaan:* `kutoa+V+Pss+Ind+Prs+Pe4`
* *tietää:* `tietää+V+Act+Ind+Prs+Sg3` (Eng. # know)
* *tiesit:* `tietää+V+Act+Ind+Prt+Sg2`
* *nähnyt:* `nähdä+V+Act+PrfPrc+Sg+Nom` (Eng. # see)
* *juossut:* `juosta+V+Act+PrfPrc+Sg+Nom` (Eng. # run)







# Verb inflection and derivation

The verbs' conjugation includes voice (in Finnish grammars also verbal 
genus),  tense/mood (tempus/modus), personal endings and negation marker
The verbs also have productive derivations to defective nouns as
infinitives, and to adjectives as participles, which are considered to be 
part of inflection and thus included in most versions 
([VISK § 105](http://scripta.kotus.fi/visk/sisalto.php?p?105)).
The morphology of participles and historical 4th infinitive is further
detailed in section Deverbal nouns'  morphology.
The analysis strings of verb are not as systematic as nouns,
as many categories collapse together in forms, e.g. the tense and mood
are only distinct with indicative past and non-past, otherwise mood implies
tense in semantic sense.



## Verb stem variation
 Verbs have no allomorphic variation per se, except for some assimilation
 and variation of ð forms, but the stem variation is the same
 as in nouns. The examples for verb stems are given for each class:
 A infinitive's lative, e infinitive's inessive, indicative present 1st 
 singular, indicative present 3rd singular, indicative past 1st singular, 
 indicative past 3rd singular, conditional past 3rd singular, 
 imperative 2nd plural, potential 1st singular, present passive, 
 past passive, nut participle passive

### Verb stems without stem variation

 The u stems have no stem variation:

*Verbs 52u examples:*
* *kaunistua:* `kaunistua+V+Act+InfA+Sg+Lat` (Eng. # beautify)
* *kaunistuessa:* `kaunistua+V+Act+InfE+Sg+Ine`
* *kaunistun:* `kaunistua+V+Act+Ind+Prs+Sg1`
* *kaunistuu:* `kaunistua+V+Act+Ind+Prs+Sg3`
* *kaunistuin:* `kaunistua+V+Act+Ind+Prt+Sg1`
* *kaunistui:* `kaunistua+V+Act+Ind+Prt+Sg3`
* *kaunistuisi:* `kaunistua+V+Act+Cond+Sg3`
* *kaunistukaa:* `kaunistua+V+Act+Imprt+Pl2`
* *kaunistunen:* `kaunistua+V+Act+Pot+Sg1`
* *kaunistutaan:* `kaunistua+V+Pss+Ind+Prs+Pe4`
* *kaunistuttiin:* `kaunistua+V+Pss+Ind+Prt+Pe4`
* *kaunistuttu:* `kaunistua+V+Pss+PrfPrc+Sg+Nom`
* *kaunistuminen:* `kaunistua+V+Der/minen+Sg+Nom`

 The o stems have no stem variation:

*Verbs 52o examples:*
* *punoa:* `punoa+V+Act+InfA+Sg+Lat` (Eng. # bundle?)
* *punoessa:* `punoa+V+Act+InfE+Sg+Ine`
* *punon:* `punoa+V+Act+Ind+Prs+Sg1`
* *punoo:* `punoa+V+Act+Ind+Prs+Sg3`
* *punoin:* `punoa+V+Act+Ind+Prt+Sg1`
* *punoi:* `punoa+V+Act+Ind+Prt+Sg3`
* *punoisi:* `punoa+V+Act+Cond+Sg3`
* *punokaa:* `punoa+V+Act+Imprt+Pl2`
* *punonen:* `punoa+V+Act+Pot+Sg1`
* *punotaan:* `punoa+V+Pss+Ind+Prs+Pe4`
* *punottiin:* `punoa+V+Pss+Ind+Prt+Pe4`
* *punottu:* `punoa+V+Pss+PrfPrc+Sg+Nom`
* *punominen:* `punoa+V+Der/minen+Sg+Nom`

 The ö stems have no stem variation:

*Verbs 52ö examples:*
* *säilöä:* `säilöä+V+Act+InfA+Sg+Lat` (Eng. # store)
* *säilöessä:* `säilöä+V+Act+InfE+Sg+Ine`
* *säilön:* `säilöä+V+Act+Ind+Prs+Sg1`
* *säilöö:* `säilöä+V+Act+Ind+Prs+Sg3`
* *säilöin:* `säilöä+V+Act+Ind+Prt+Sg1`
* *säilöi:* `säilöä+V+Act+Ind+Prt+Sg3`
* *säilöisi:* `säilöä+V+Act+Cond+Sg3`
* *säilökää:* `säilöä+V+Act+Imprt+Pl2`
* *säilönen:* `säilöä+V+Act+Pot+Sg1`
* *säilötään:* `säilöä+V+Pss+Ind+Prs+Pe4`
* *säilöttiin:* `säilöä+V+Pss+Ind+Prt+Pe4`
* *säilötty:* `säilöä+V+Pss+PrfPrc+Sg+Nom`
* *säilöminen:* `säilöä+V+Der/minen+Sg+Nom`

 The y stems have no stem variation:

*Verbs 52y examples:*
* *ällistyä:* `ällistyä+V+Act+InfA+Sg+Lat` (Eng. # amaze)
* *ällistyessä:* `ällistyä+V+Act+InfE+Sg+Ine`
* *ällistyn:* `ällistyä+V+Act+Ind+Prs+Sg1`
* *ällistyy:* `ällistyä+V+Act+Ind+Prs+Sg3`
* *ällistyin:* `ällistyä+V+Act+Ind+Prt+Sg1`
* *ällistyi:* `ällistyä+V+Act+Ind+Prt+Sg3`
* *ällistyisi:* `ällistyä+V+Act+Cond+Sg3`
* *ällistykää:* `ällistyä+V+Act+Imprt+Pl2`
* *ällistynen:* `ällistyä+V+Act+Pot+Sg1`
* *ällistytään:* `ällistyä+V+Pss+Ind+Prs+Pe4`
* *ällistyttiin:* `ällistyä+V+Pss+Ind+Prt+Pe4`
* *ällistytty:* `ällistyä+V+Pss+PrfPrc+Sg+Nom`

### Verb stems with only gradation


*Verbs 52 k~0 o examples:*
* *haukkoa:* `haukkoa+V+Act+InfA+Sg+Lat` (Eng. # gasp)


*Verbs 52 k~0 u examples:*
* *nuokkua:* `nuokkua+V+Act+InfA+Sg+Lat` (Eng. # sleep)


*Verbs 52 k~0 y examples:*
* *kärkkyä:* `kärkkyä+V+Act+InfA+Sg+Lat` (Eng. # steal a base)


*Verbs 52 p~0 o examples:*
* *harppoa:* `harppoa+V+Act+InfA+Sg+Lat` (Eng. # walk)


*Verbs 52 p~0 u examples:*
* *loppua:* `loppua+V+Act+InfA+Sg+Lat` (Eng. # end)


*Verbs 52 p~0 y examples:*
* *leppyä:* `leppyä+V+Act+InfA+Sg+Lat` (Eng. # settle)


*Verbs 52 t~0 y examples:*
* *kivettyä:* `kivettyä+V+Act+InfA+Sg+Lat` (Eng. # stone)


*Verbs 52 t~0 o examples:*
* *viittoa:* `viittoa+V+Act+InfA+Sg+Lat` (Eng. # sign)


*Verbs 52 t~0 u examples:*
* *hermottua:* `hermottua+V+Act+InfA+Sg+Lat` (Eng. # nerve)


*Verbs 52 k~0’ y examples:*
* *mäikyä:* `mäikyä+V+Act+InfA+Sg+Lat` (Eng. # bong)


*Verbs 52 k~0’ o examples:*
* *takoa:* `takoa+V+Act+InfA+Sg+Lat` (Eng. # hammer)


*Verbs 52 k~0’ u examples:*
* *maukua:* `maukua+V+Act+InfA+Sg+Lat` (Eng. # gong)


*Verbs 52 p~v y examples:*
* *elpyä:* `elpyä+V+Act+InfA+Sg+Lat` (Eng. # rescuscitate)


*Verbs 52 p~v u examples:*
* *hiipua:* `hiipua+V+Act+InfA+Sg+Lat` (Eng. # fizzle)


*Verbs 52 p~v o examples:*
* *silpoa:* `silpoa+V+Act+InfA+Sg+Lat` (Eng. # slash)


*Verbs 52 t~d o examples:*
* *kietoa:* `kietoa+V+Act+InfA+Sg+Lat` (Eng. # entangle)


*Verbs 52 t~d y examples:*
* *siliytyä:* `siliytyä+V+Act+InfA+Sg+Lat` (Eng. # iron)


*Verbs 52 t~d u examples:*
* *rohtua:* `rohtua+V+Act+InfA+Sg+Lat` (Eng. # scar)


*Verbs 52 k~g u examples:*
* *vinkua:* `vinkua+V+Act+InfA+Sg+Lat` (Eng. # squeal)


*Verbs 52 k~g o examples:*
* *penkoa:* `penkoa+V+Act+InfA+Sg+Lat` (Eng. # dig)


*Verbs 52 p~m o examples:*
* *tempoa:* `tempoa+V+Act+InfA+Sg+Lat` (Eng. # tuck)


*Verbs 52 p~m u examples:*
* *ampua:* `ampua+V+Act+InfA+Sg+Lat` (Eng. # shoot)


*Verbs 52 t~l y examples:*
* *mieltyä:* `mieltyä+V+Act+InfA+Sg+Lat` (Eng. # like)


*Verbs 52 t~l u examples:*
* *humaltua:* `humaltua+V+Act+InfA+Sg+Lat` (Eng. # intoxicate)


*Verbs 52 t~n u examples:*
* *vakaantua:* `vakaantua+V+Act+InfA+Sg+Lat` (Eng. # stabilise)


*Verbs 52 t~n y examples:*
* *tyhjentyä:* `tyhjentyä+V+Act+InfA+Sg+Lat` (Eng. # empty)


*Verbs 52 t~r y examples:*
* *pyörtyä:* `pyörtyä+V+Act+InfA+Sg+Lat` (Eng. # faint)


*Verbs 52 t~r o examples:*
* *vartoa:* `vartoa+V+Act+InfA+Sg+Lat` (Eng. # wait)


*Verbs 52 t~r u examples:*
* *pusertua:* `pusertua+V+Act+InfA+Sg+Lat` (Eng. # squeeze)

### Verbs with -a stems
 


*Verbs 53 back examples:*
* *mutristaa:* `mutristaa+V+Act+InfA+Sg+Lat` (Eng. # ?)


*Verbs 53 front examples:*
* *kivistää:* `kivistää+V+Act+InfA+Sg+Lat` (Eng. # hurt)


*Verbs 53 front t~0 examples:*
* *räpsyttää:* `räpsyttää+V+Act+InfA+Sg+Lat` (Eng. # blink)


*Verbs 53 back t~0 examples:*
* *vieroittaa:* `vieroittaa+V+Act+InfA+Sg+Lat`



*Verbs 53 back k~0 examples:*
* *purkaa:* `purkaa+V+Act+InfA+Sg+Lat` (Eng. # tear)


*Verbs 53 front t~d examples:*
* *kärähtää:* `kärähtää+V+Act+InfA+Sg+Lat` (Eng. # fry)


*Verbs 53 back t~d examples:*
* *mojahtaa:* `mojahtaa+V+Act+InfA+Sg+Lat` (Eng. # smack)


*Verbs 53 front t~n examples:*
* *kyntää:* `kyntää+V+Act+InfA+Sg+Lat` (Eng. # plow)


*Verbs 53 back t~r examples:*
* *sortaa:* `sortaa+V+Act+InfA+Sg+Lat` (Eng. # crush)

 Some of the a stems have t:s in past stems by ti>si variation.


*Verbs 54 back examples:*
* *huutaa:* `huutaa+V+Act+InfA+Sg+Lat` (Eng. # yell)


*Verbs 54 front examples:*
* *pyytää:* `pyytää+V+Act+InfA+Sg+Lat` (Eng. # aks)


*Verbs 54 back t~l examples:*
* *sivaltaa:* `sivaltaa+V+Act+InfA+Sg+Lat` (Eng. # slash)


*Verbs 54 front t~l examples:*
* *viheltää:* `viheltää+V+Act+InfA+Sg+Lat` (Eng. # whistle)


*Verbs 54 front t~n examples:*
* *hiventää:* `hiventää+V+Act+InfA+Sg+Lat`


*Verbs 54 back t~n examples:*
* *huonontaa:* `huonontaa+V+Act+InfA+Sg+Lat` (Eng. # worsen)


*Verbs 54 back t~r examples:*
* *kuhertaa:* `kuhertaa+V+Act+InfA+Sg+Lat` (Eng. # snuggle)


*Verbs 54 front t~r examples:*
* *näpertää:* `näpertää+V+Act+InfA+Sg+Lat` (Eng. # twiddle)

 In some cases t:s variation is optionally alongside the regular gradation:



*Verbs 55 front examples:*
* *kiitää:* `kiitää+V+Act+InfA+Sg+Lat` (Eng. # rush)


*Verbs 55 back examples:*
* *joutaa:* `joutaa+V+Act+InfA+Sg+Lat`


*Verbs 55 front t~l examples:*
* *yltää:* `yltää+V+Act+InfA+Sg+Lat` (Eng. # reach)


*Verbs 55 front t~n examples:*
* *entää:* `entää+V+Act+InfA+Sg+Lat`

 Other a stems undergo a:o variation


*Verbs 56 back examples:*
* *kasvaa:* `kasvaa+V+Act+InfA+Sg+Lat` (Eng. # grow)


*Verbs 56 back k~0 examples:*
* *jakaa:* `jakaa+V+Act+InfA+Sg+Lat` (Eng. # share)


*Verbs 56 back p~0 examples:*
* *tappaa:* `tappaa+V+Act+InfA+Sg+Lat` (Eng. # kill)


*Verbs 56 back t~0 examples:*
* *auttaa:* `auttaa+V+Act+InfA+Sg+Lat` (Eng. # help)


*Verbs 56 back t~d examples:*
* *sataa:* `sataa+V+Act+InfA+Sg+Lat` (Eng. # rain)


*Verbs 56 back t~n examples:*
* *kantaa:* `kantaa+V+Act+InfA+Sg+Lat` (Eng. # carry)

 In some of the a:o variations the t:s variant is also possible.


*Verbs 57 back examples:*
* *kaataa:* `kaataa+V+Act+InfA+Sg+Lat` (Eng. # pour)


*Verbs 57 back t~r examples:*
* *saartaa:* `saartaa+V+Act+InfA+Sg+Lat` (Eng. # surround)

### Verbs with e stems


*Verbs 58 front examples:*
* *kytkeä:* `kytkeä+V+Act+InfA+Sg+Lat` (Eng. # switch)


*Verbs 58 back examples:*
* *sotkea:* `sotkea+V+Act+InfA+Sg+Lat` (Eng. # mess)


*Verbs 58 back k~0 examples:*
* *pukea:* `pukea+V+Act+InfA+Sg+Lat` (Eng. # dress)


*Verbs 58 front p~v examples:*
* *rypeä:* `rypeä+V+Act+InfA+Sg+Lat` (Eng. # bathe)


*Verbs 58 front t~d examples:*
* *päteä:* `päteä+V+Act+InfA+Sg+Lat` (Eng. # nitpick)


*Verbs 58 back t~d examples:*
* *kutea:* `kutea+V+Act+InfA+Sg+Lat` (Eng. # breed)


*Verbs 58 back k~g examples:*
* *tunkea:* `tunkea+V+Act+InfA+Sg+Lat` (Eng. # push)


*Verbs 58 back k~j examples:*
* *polkea:* `polkea+V+Act+InfA+Sg+Lat` (Eng. # stomp)


*Verbs 58 front k~j examples:*
* *särkeä:* `särkeä+V+Act+InfA+Sg+Lat` (Eng. # break)

 Some of the e stems allow for optional t:s variation in past form.


*Verbs 59 examples:*
* *tuntea:* `tuntea+V+Act+InfA+Sg+Lat` (Eng. # feel)

 The rare ht:ks kind of variation is also possible.


*Verbs 60 examples:*
* *lähteä:* `lähteä+V+Act+InfA+Sg+Lat` (Eng. # go)

### Verbs with i stems


*Verbs 61 back examples:*
* *kosia:* `kosia+V+Act+InfA+Sg+Lat` (Eng. # propose)


*Verbs 61 front examples:*
* *ryskiä:* `ryskiä+V+Act+InfA+Sg+Lat` (Eng. # crash)


*Verbs 61 back k~0 examples:*
* *kukkia:* `kukkia+V+Act+InfA+Sg+Lat` (Eng. # bloom)


*Verbs 61 front k~0 examples:*
* *sörkkiä:* `sörkkiä+V+Act+InfA+Sg+Lat` (Eng. # meddle)


*Verbs 61 back p~0 examples:*
* *kalppia:* `kalppia+V+Act+InfA+Sg+Lat`


*Verbs 61 front p~0 examples:*
* *hyppiä:* `hyppiä+V+Act+InfA+Sg+Lat` (Eng. # jump)


*Verbs 61 back t~0 examples:*
* *moittia:* `moittia+V+Act+InfA+Sg+Lat`


*Verbs 61 front t~0 examples:*
* *miettiä:* `miettiä+V+Act+InfA+Sg+Lat` (Eng. # ponder)


*Verbs 61 front p~v examples:*
* *riipiä:* `riipiä+V+Act+InfA+Sg+Lat` (Eng. # pull)


*Verbs 61 back p~v examples:*
* *raapia:* `raapia+V+Act+InfA+Sg+Lat` (Eng. # scratch)


*Verbs 61 back t~d examples:*
* *ahnehtia:* `ahnehtia+V+Act+InfA+Sg+Lat` (Eng. # greed)


*Verbs 61 front t~d examples:*
* *ehtiä:* `ehtiä+V+Act+InfA+Sg+Lat`


*Verbs 61 front k~g examples:*
* *mönkiä:* `mönkiä+V+Act+InfA+Sg+Lat` (Eng. # crawl)


*Verbs 61 back k~g examples:*
* *onkia:* `onkia+V+Act+InfA+Sg+Lat` (Eng. # fish)


*Verbs 61 frontp~m examples:*
* *tympiä:* `tympiä+V+Act+InfA+Sg+Lat` (Eng. # bore)


*Verbs 61 back t~n examples:*
* *kontia:* `kontia+V+Act+InfA+Sg+Lat` (Eng. # mole)


*Verbs 61 back k~j examples:*
* *hylkiä:* `hylkiä+V+Act+InfA+Sg+Lat` (Eng. # reject)

### Verbs with long vowel stem
 These verbs also have da variant of a infinitive forms.


*Verbs 62 back examples:*
* *kopioida:* `kopioida+V+Act+InfA+Sg+Lat` (Eng. # copy)


*Verbs 62 front examples:*
* *öykkäröidä:* `öykkäröidä+V+Act+InfA+Sg+Lat` (Eng. # bully)


*Verbs 63 a examples:*
* *saada:* `saada+V+Act+InfA+Sg+Lat` (Eng. # get)


*Verbs 63 y examples:*
* *myydä:* `myydä+V+Act+InfA+Sg+Lat` (Eng. # sell)


*Verbs 63 ä examples:*
* *jäädä:* `jäädä+V+Act+InfA+Sg+Lat` (Eng. # stay)

### Monosyllabic verbs with widening diphthong
 Widening diphthongs are simplified before past and conditional suffix i's
 by removal of first component. 


*Verbs 64 ie examples:*
* *viedä:* `viedä+V+Act+InfA+Sg+Lat` (Eng. # take)


*Verbs 64 uo examples:*
* *tuoda:* `tuoda+V+Act+InfA+Sg+Lat` (Eng. # bring)


*Verbs 64 yö examples:*
* *syödä:* `syödä+V+Act+InfA+Sg+Lat` (Eng. # eat)

 In past and conditional forms of käydä, the glide before suffix is marked
 even in normative orthography.


*Verbs 65 examples:*
* *käydä:* `käydä+V+Act+InfA+Sg+Lat` (Eng. # visit, go for)

### Verbs with consonant stems

 Verbs with momentane derivation are common consonant stems.


*Verbs 66 back examples:*
* *marista:* `marista+V+Act+InfA+Sg+Lat` (Eng. # whine)


*Verbs 66 front examples:*
* *äristä:* `äristä+V+Act+InfA+Sg+Lat` (Eng. # arr)



*Verbs 66 front v p examples:*
* *häväistä:* `häväistä+V+Act+InfA+Sg+Lat` (Eng. # sacrilege)


*Verbs 66 back v~p examples:*
* *vavista:* `vavista+V+Act+InfA+Sg+Lat` (Eng. # shake)



*Verbs 66 back k~g examples:*
* *rangaista:* `rangaista+V+Act+InfA+Sg+Lat` (Eng. # punish)

### Verbs with n, r, l, s stems
 Notably, the a infinitive forms d assimilates to preceding consonant.


*Verbs 67 back r examples:*
* *surra:* `surra+V+Act+InfA+Sg+Lat` (Eng. # meh)




*Verbs 67 front r examples:*
* *pierrä:* `pierrä+V+Act+InfA+Sg+Lat` (Eng. # fart)


*Verbs 67 back n examples:*
* *panna:* `panna+V+Act+InfA+Sg+Lat` (Eng. # put)


*Verbs 67 front n examples:*
* *mennä:* `mennä+V+Act+InfA+Sg+Lat` (Eng. # go)

 Frequentative derivations are most common source of l stemmed verbs.


*Verbs 67 back l examples:*
* *vastailla:* `vastailla+V+Act+InfA+Sg+Lat` (Eng. # answer)


*Verbs 67 frot l examples:*
* *äksyillä:* `äksyillä+V+Act+InfA+Sg+Lat`


*Verbs 67 front l 0k examples:*
* *leikellä:* `leikellä+V+Act+InfA+Sg+Lat` (Eng. # cut)


*Verbs 67 back l 0k examples:*
* *nakella:* `nakella+V+Act+InfA+Sg+Lat` (Eng. # throw)


*Verbs 67 back l 0p examples:*
* *tapella:* `tapella+V+Act+InfA+Sg+Lat` (Eng. # fight)


*Verbs 67 front l 0p examples:*
* *hypellä:* `hypellä+V+Act+InfA+Sg+Lat` (Eng. # jump)


*Verbs 67 back l 0t examples:*
* *sulatella:* `sulatella+V+Act+InfA+Sg+Lat` (Eng. # melt)


*Verbs 67 front l 0t examples:*
* *herätellä:* `herätellä+V+Act+InfA+Sg+Lat` (Eng. # wake)


*Verbs 67 back l d~t examples:*
* *tipahdella:* `tipahdella+V+Act+InfA+Sg+Lat` (Eng. # drop)


*Verbs 67 front l d~t examples:*
* *säpsähdellä:* `säpsähdellä+V+Act+InfA+Sg+Lat` (Eng. # wake)


*Verbs 67 back l m~p examples:*
* *ommella:* `ommella+V+Act+InfA+Sg+Lat` (Eng. # sow)


*Verbs 67 back l l~t examples:*
* *vaellella:* `vaellella+V+Act+InfA+Sg+Lat` (Eng. # wander)


*Verbs 67 frpnt l l~t examples:*
* *kiillellä:* `kiillellä+V+Act+InfA+Sg+Lat` (Eng. # sparkle)


*Verbs 67 back l n~t examples:*
* *komennella:* `komennella+V+Act+InfA+Sg+Lat` (Eng. # boss)


*Verbs 67 front l n~t examples:*
* *käännellä:* `käännellä+V+Act+InfA+Sg+Lat` (Eng. # turn)


*Verbs 67 back l r~t examples:*
* *nakerrella:* `nakerrella+V+Act+InfA+Sg+Lat` (Eng. # bite)


*Verbs 67 front l r~t examples:*
* *kiherrellä:* `kiherrellä+V+Act+InfA+Sg+Lat` (Eng. # giggle)

### tse stuff
 Some verbs have possible optional heteroclitic indicative stems:


*Verbs 68 front examples:*
* *isännöidä:* `isännöidä+V+Act+InfA+Sg+Lat` (Eng. # host)


*Verbs 68 back examples:*
* *mellakoida:* `mellakoida+V+Act+InfA+Sg+Lat` (Eng. # riot)

 In these stems the tse formed stem is only one.


*Verbs 69 back examples:*
* *palkita:* `palkita+V+Act+InfA+Sg+Lat` (Eng. # reward)


*Verbs 69 front examples:*
* *merkitä:* `merkitä+V+Act+InfA+Sg+Lat` (Eng. # mark)

 Few words have special consonant cluster simplification for ks forms.


*Verbs 70 back examples:*
* *juosta:* `juosta+V+Act+InfA+Sg+Lat` (Eng. # run)


*Verbs 70 front examples:*
* *piestä:* `piestä+V+Act+InfA+Sg+Lat` (Eng. # spank)


 nähdä has special h:k variation.


*Verbs 71 examples:*
* *nähdä:* `nähdä+V+Act+InfA+Sg+Lat` (Eng. # see)

### Verbs with -ne- stems


*Verbs 72 back examples:*
* *karheta:* `karheta+V+Act+InfA+Sg+Lat`


*Verbs 72 rfont examples:*
* *vähetä:* `vähetä+V+Act+InfA+Sg+Lat` (Eng. # lessen)


*Verbs 72 back 0~k examples:*
* *niuketa:* `niuketa+V+Act+InfA+Sg+Lat` (Eng. # tighten)


*Verbs 72 front 0~k examples:*
* *jyrketä:* `jyrketä+V+Act+InfA+Sg+Lat`


*Verbs 72 back 0~p examples:*
* *hapata:* `hapata+V+Act+InfA+Sg+Lat` (Eng. # ensour)


*Verbs 72 front 0~p examples:*
* *supeta:* `supeta+V+Act+InfA+Sg+Lat` (Eng. # tighten)


*Verbs 72 front 0~p examples:*
* *tylpetä:* `tylpetä+V+Act+InfA+Sg+Lat` (Eng. # dull)


*Verbs 72 back 0~p o examples:*
* *helpota:* `helpota+V+Act+InfA+Sg+Lat` (Eng. # ease)


*Verbs 72 back 0~t examples:*
* *loitota:* `loitota+V+Act+InfA+Sg+Lat` (Eng. # distance)


*Verbs 72 back 0~k o examples:*
* *ulota:* `ulota+V+Act+InfA+Sg+Lat` (Eng. # outen)


*Verbs 72 back v~p examples:*
* *kaveta:* `kaveta+V+Act+InfA+Sg+Lat` (Eng. # tighten)


*Verbs 72 front d~t examples:*
* *mädätä:* `mädätä+V+Act+InfA+Sg+Lat` (Eng. # rot)


*Verbs 72 back d~t examples:*
* *leudota:* `leudota+V+Act+InfA+Sg+Lat`


*Verbs 72 front d~t e examples:*
* *pidetä:* `pidetä+V+Act+InfA+Sg+Lat` (Eng. # lengthen)


*Verbs 72 front m~p  examples:*
* *lämmetä:* `lämmetä+V+Act+InfA+Sg+Lat` (Eng. # warm)


*Verbs 72 front n~t examples:*
* *kiinnetä:* `kiinnetä+V+Act+InfA+Sg+Lat` (Eng. # firm)


*Verbs 72 back j~k examples:*
* *juljeta:* `juljeta+V+Act+InfA+Sg+Lat` (Eng. # dare)

### Vowel lengthening(?) stems


*Verbs 73 back examples:*
* *arvata:* `arvata+V+Act+InfA+Sg+Lat` (Eng. # guess)


*Verbs 73 front examples:*
* *ynnätä:* `ynnätä+V+Act+InfA+Sg+Lat` (Eng. # add)


*Verbs 73 back 0~k examples:*
* *morkata:* `morkata+V+Act+InfA+Sg+Lat` (Eng. # blame)


*Verbs 73 front 0~k examples:*
* *yökätä:* `yökätä+V+Act+InfA+Sg+Lat` (Eng. # yuck)


*Verbs 73 back 0~p examples:*
* *siepata:* `siepata+V+Act+InfA+Sg+Lat` (Eng. # kidnap)


*Verbs 73 frot 0~p examples:*
* *välpätä:* `välpätä+V+Act+InfA+Sg+Lat`


*Verbs 73 back 0~t examples:*
* *luntata:* `luntata+V+Act+InfA+Sg+Lat` (Eng. # cheat)


*Verbs 73 front 0~t examples:*
* *läntätä:* `läntätä+V+Act+InfA+Sg+Lat` (Eng. # splat)


*Verbs 73 back v~p examples:*
* *kaivata:* `kaivata+V+Act+InfA+Sg+Lat` (Eng. # long)


*Verbs 73 front v~p examples:*
* *levätä:* `levätä+V+Act+InfA+Sg+Lat` (Eng. # rest)


*Verbs 73 back d~r examples:*
* *jahdata:* `jahdata+V+Act+InfA+Sg+Lat` (Eng. # hunt)


*Verbs 73 front d~t examples:*
* *tähdätä:* `tähdätä+V+Act+InfA+Sg+Lat` (Eng. # aim)


*Verbs 73 back g~k examples:*
* *vongata:* `vongata+V+Act+InfA+Sg+Lat` (Eng. # beg)




*Verbs 73 front g~k examples:*
* *vängätä:* `vängätä+V+Act+InfA+Sg+Lat` (Eng. # argue)


*Verbs 73 back m~p examples:*
* *temmata:* `temmata+V+Act+InfA+Sg+Lat` (Eng. # pull)


*Verbs 73 back l~t examples:*
* *mullata:* `mullata+V+Act+InfA+Sg+Lat` (Eng. # dirt)


*Verbs 73 back n~t examples:*
* *suunnata:* `suunnata+V+Act+InfA+Sg+Lat` (Eng. # go)


*Verbs 73 front n~t examples:*
* *rynnätä:* `rynnätä+V+Act+InfA+Sg+Lat` (Eng. # rush)


*Verbs 73 back r~t examples:*
* *virrata:* `virrata+V+Act+InfA+Sg+Lat` (Eng. # stream)


*Verbs 73 front j~k examples:*
* *hyljätä:* `hyljätä+V+Act+InfA+Sg+Lat` (Eng. # abandon)


*Verbs 73 back 0~g examples:*
* *digata:* `digata+V+Act+InfA+Sg+Lat` (Eng. # dig)


*Verbs 73 back 0~b examples:*
* *lobata:* `lobata+V+Act+InfA+Sg+Lat` (Eng. # lobby)


*Verbs 74 back  examples:*
* *karhuta:* `karhuta+V+Act+InfA+Sg+Lat` (Eng. # tax)


*Verbs 74 front  examples:*
* *tähytä:* `tähytä+V+Act+InfA+Sg+Lat` (Eng. # periscope)


*Verbs 74 back 0~k  examples:*
* *kaikota:* `kaikota+V+Act+InfA+Sg+Lat` (Eng. # disappear)


*Verbs 74 back 0~k e  examples:*
* *poiketa:* `poiketa+V+Act+InfA+Sg+Lat`


*Verbs 74 front 0~k e  examples:*
* *keretä:* `keretä+V+Act+InfA+Sg+Lat`


*Verbs 74 front   0~k u  examples:*
* *koukuta:* `koukuta+V+Act+InfA+Sg+Lat` (Eng. # hook)


*Verbs 74 back 0~p  examples:*
* *pulputa:* `pulputa+V+Act+InfA+Sg+Lat` (Eng. # pulp)


*Verbs 74 back 0~p o  examples:*
* *upota:* `upota+V+Act+InfA+Sg+Lat` (Eng. # sink)


*Verbs 74 back 0~to examples:*
* *lotota:* `lotota+V+Act+InfA+Sg+Lat` (Eng. # lotto)


*Verbs 74 back 0~tu examples:*
* *luututa:* `luututa+V+Act+InfA+Sg+Lat` (Eng. # mop)


*Verbs 74 back v~p  examples:*
* *kivuta:* `kivuta+V+Act+InfA+Sg+Lat` (Eng. # climb)


*Verbs 74 back v~pe examples:*
* *korveta:* `korveta+V+Act+InfA+Sg+Lat`


*Verbs 74 front v~pe examples:*
* *kiivetä:* `kiivetä+V+Act+InfA+Sg+Lat` (Eng. # climb)


*Verbs 74 back v~po examples:*
* *kirvota:* `kirvota+V+Act+InfA+Sg+Lat` (Eng. # loose)


*Verbs 74 back d~t  examples:*
* *kadota:* `kadota+V+Act+InfA+Sg+Lat` (Eng. # disappear)


*Verbs 74 back t~de examples:*
* *todeta:* `todeta+V+Act+InfA+Sg+Lat` (Eng. # note)


*Verbs 74 front t~de examples:*
* *vyyhdetä:* `vyyhdetä+V+Act+InfA+Sg+Lat` (Eng. # entangle)


*Verbs 74 back d~tu examples:*
* *liiduta:* `liiduta+V+Act+InfA+Sg+Lat` (Eng. # chalk)


*Verbs 74 back g~ke examples:*
* *tungeta:* `tungeta+V+Act+InfA+Sg+Lat` (Eng. # push)


*Verbs 74 back g~ko examples:*
* *pingota:* `pingota+V+Act+InfA+Sg+Lat` (Eng. # ping)


*Verbs 74 front g~ke  examples:*
* *ängetä:* `ängetä+V+Act+InfA+Sg+Lat` (Eng. # push)


*Verbs 74 back m~p  examples:*
* *kummuta:* `kummuta+V+Act+InfA+Sg+Lat`


*Verbs 74 back p~mu examples:*
* *kammeta:* `kammeta+V+Act+InfA+Sg+Lat` (Eng. # lever)


*Verbs 74 back p~mu examples:*
* *sammota:* `sammota+V+Act+InfA+Sg+Lat`


*Verbs 74 back t~no examples:*
* *innota:* `innota+V+Act+InfA+Sg+Lat` (Eng. # eager)


*Verbs 74 back t~to examples:*
* *irrota:* `irrota+V+Act+InfA+Sg+Lat` (Eng. # tear)


*Verbs 74 back j~ke examples:*
* *haljeta:* `haljeta+V+Act+InfA+Sg+Lat` (Eng. # split)


*Verbs 74 front j~k  examples:*
* *iljetä:* `iljetä+V+Act+InfA+Sg+Lat` (Eng. # dare)


*Verbs 75 front examples:*
* *myrskytä:* `myrskytä+V+Act+InfA+Sg+Lat` (Eng. # storm)


*Verbs 75 back examples:*
* *lassota:* `lassota+V+Act+InfA+Sg+Lat` (Eng. # lasso)


*Verbs 75 itä examples:*
* *selvitä:* `selvitä+V+Act+InfA+Sg+Lat` (Eng. # manage)


*Verbs 75 pytä examples:*
* *ryöpytä:* `ryöpytä+V+Act+InfA+Sg+Lat` (Eng. # flood)


*Verbs 75 tota examples:*
* *peitota:* `peitota+V+Act+InfA+Sg+Lat` (Eng. # beat)


*Verbs 75 itä examples:*
* *keritä:* `keritä+V+Act+InfA+Sg+Lat`


*Verbs 75 dota examples:*
* *muodota:* `muodota+V+Act+InfA+Sg+Lat` (Eng. # form)


*Verbs 75 mitä examples:*
* *lämmitä:* `lämmitä+V+Act+InfA+Sg+Lat` (Eng. # heat)


*Verbs 75 lota examples:*
* *aallota:* `aallota+V+Act+InfA+Sg+Lat` (Eng. # wave)


*Verbs 75 litä examples:*
* *hellitä:* `hellitä+V+Act+InfA+Sg+Lat` (Eng. # loose)

### Vowel stems with t:s variation


*Verbs 76 back examples:*
* *taitaa:* `taitaa+V+Act+InfA+Sg+Lat` (Eng. # can)


*Verbs 76 front examples:*
* *tietää:* `tietää+V+Act+InfA+Sg+Lat` (Eng. # know)

### Verbs with defective paradigms
 For some verbs, the normative inflection does not allow full set of forms:


*Verbs 77 back  examples:*
* *vipajaa:* `vipajaa+V+Act+InfA+Sg+Lat` (Eng. # thump)


*Verbs 77 front examples:*
* *heläjää:* `heläjää+V+Act+InfA+Sg+Lat` (Eng. # chime)


*Verbs 78 back examples:*
* *raikaa:* `raikaa+V+Act+InfA+Sg+Lat` (Eng. # baum)


*Verbs 78 front examples:*
* *ähkää:* `ähkää+V+Act+InfA+Sg+Lat` (Eng. # meh)

### Verbs with exceptional inflection patterns
 There is a handful of verbs that does not fit to the patterns of old
 dictionaries.

 The verb olla has very peculiar and heteroclitic inflection with lot of
 common short forms in standard spoken Finnish:

*Verb olla examples:*
* *olla:* `olla+V+Act+InfA+Sg+Lat` (Eng. # be)
* *lienen:* `olla+V+Act+Pot+Sg1`
* *lie:* `olla+V+Act+Pot+ConNeg`
* *olija:* `olla+V+Der/ja+Sg+Nom`







































































































































































































































































































































































## Verb inflection proper


### Present vowel stems

* *kutoa:* `kutoa+V+Act+InfA+Sg+Lat` (Eng. # knit)
* *kudon:* `kutoa+V+Act+Ind+Prs+Sg1`

* *yltää:* `yltää+V+Act+InfA+Sg+Lat` (Eng. # reach)
* *yllän:* `yltää+V+Act+Ind+Prs+Sg1`

The strong form of present indicative endings in strong stems and ma 
infinitive, and maisilla derivation (nee infinitive).

*Verb strong present back examples:*
* *kutovat:* `kutoa+V+Act+Ind+Prs+Pl3`
* *kutomatta:* `kutoa+V+Act+InfMa+Sg+Abe`


*Verb strong present front examples:*
* *yltävät:* `yltää+V+Act+Ind+Prs+Pl3`
* *yltämättä:* `yltää+V+Act+InfMa+Sg+Abe`

### Verb 3rd singular forms
The third singular form of present tense has few allomorphs according to
preceding vowel context, either lengthening or zero after long vowel stem:


*Verb 3rd singular a examples:*
* *rakentaa:* `rakentaa+V+Act+Ind+Prs+Sg3` (Eng. # builds)


*Verb 3rd singular e back examples:*
* *tulee:* `tulla+V+Act+Ind+Prs+Sg3` (Eng. # comes)


*Verb 3rd singular e front examples:*
* *menee:* `mennä+V+Act+Ind+Prs+Sg3` (Eng. # goes)


*Verb 3rd singular i back examples:*
* *munii:* `munia+V+Act+Ind+Prs+Sg3` (Eng. # lays eggs)


*Verb 3rd singular e front examples:*
* *rämpii:* `rämpiä+V+Act+Ind+Prs+Sg3` (Eng. # hikes)


*Verb 3rd singular o back examples:*
* *kutoo:* `kutoa+V+Act+Ind+Prs+Sg3`


*Verb 3rd singular u back examples:*
* *viruu:* `virua+V+Act+Ind+Prs+Sg3` (Eng. # stretches)


*Verb 3rd singular y examples:*
* *kääntyy:* `kääntyä+V+Act+Ind+Prs+Sg3` (Eng. # turns)


*Verb 3rd singular ö examples:*
* *säilöö:* `säilöä+V+Act+Ind+Prs+Sg3` (Eng. # conserves)


*Verb 3rd singular ä examples:*
* *vääntää:* `vääntää+V+Act+Ind+Prs+Sg3` (Eng. # twist)

### Past forms


*Verb past back examples:*
* *kudoin:* `kutoa+V+Act+Ind+Prt+Sg1`


*Verb past front examples:*
* *ylsin:* `yltää+V+Act+Ind+Prt+Sg1`


*Verb past back strong examples:*
* *kutoivat:* `kutoa+V+Act+Ind+Prt+Pl3`


*Verb past front strong examples:*
* *ylsivät:* `yltää+V+Act+Ind+Prt+Pl3`

### Imperatives


*Verb imperative back examples:*
* *kutokaa:* `kutoa+V+Act+Imprt+Pl2`


*Verb imperative front examples:*
* *yltäkää:* `yltää+V+Act+Imprt+Pl2`

### Conditionals


*Verb conditional back examples:*
* *kutoisin:* `kutoa+V+Act+Cond+Sg1`


*Verb conditional front examples:*
* *yltäisin:* `yltää+V+Act+Cond+Sg1`

### Potentials


*Verb potential n back examples:*
* *kutonen:* `kutoa+V+Act+Pot+Sg1`


*Verb potential n front examples:*
* *yltänen:* `yltää+V+Act+Pot+Sg1`


*Verb potential s back examples:*
* *juossen:* `juosta+V+Act+Pot+Sg1` (Eng. # I might run)


*Verb potential s front examples:*
* *päässen:* `päästä+V+Act+Pot+Sg1` (Eng. # I might be able to)


*Verb potential l back examples:*
* *tullen:* `tulla+V+Act+Pot+Sg1`


*Verb potential l front examples:*
* *kävellen:* `kävellä+V+Act+Pot+Sg1` (Eng. # I might walk)


*Verb potential r back examples:*
* *surren:* `surra+V+Act+Pot+Sg1` (Eng. # I might be sad)


*Verb potential r front examples:*
* *pierren:* `pierrä+V+Act+Pot+Sg1` (Eng. # I might fart)

## Passive forms
The passive forms usually contain -ta-, -tä-, -da-, -dä-, element in them.
The variation between the realisations is one key factor of determining the
classification of the verb roots.


*Verb passive back examples:*
* *kudottiin:* `kutoa+V+Pss+Ind+Prt+Pe4`
* *juostiin:* `juosta+V+Pss+Ind+Prt+Pe4`


*Verb passive front examples:*
* *nähtiin:* `nähdä+V+Pss+Ind+Prt+Pe4`


The form of present passive assimilates leftwards, varying between
-ta, -tä, -da, -dä, -la, -lä, -ra, -rä, -na, -nä.


*Verb passive back t examples:*
* *juostaan:* `juosta+V+Pss+Ind+Prs+Pe4`


*Verb passive front t examples:*
* *ylletään:* `yltää+V+Pss+Ind+Prs+Pe4`


*Verb passive back d examples:*
* *kopioidaan:* `kopioida+V+Pss+Ind+Prs+Pe4` (Eng. # copy-passive)


*Verb passive front d examples:*
* *nähdään:* `nähdä+V+Pss+Ind+Prs+Pe4`


*Verb passive back l examples:*
* *vuollaan:* `vuolla+V+Pss+Ind+Prs+Pe4` (Eng. # carve wood-passive)


*Verb passive front l examples:*
* *niellään:* `niellä+V+Pss+Ind+Prs+Pe4` (Eng. # swallow-passive)


*Verb passive back r examples:*
* *surraan:* `surra+V+Pss+Ind+Prs+Pe4` (Eng. # sorrow-passive)


*Verb passive front r examples:*
* *pierrään:* `pierrä+V+Pss+Ind+Prs+Pe4` (Eng. # fart-passive)


*Verb passive back n examples:*
* *pannaan:* `panna+V+Pss+Ind+Prs+Pe4` (Eng. # put-passive)


*Verb passive front n examples:*
* *mennään:* `mennä+V+Pss+Ind+Prs+Pe4`


## Infinite verb forms


*Verb a infinitive examples:*
* *juoda:* `juoda+V+Act+InfA+Sg+Lat` (Eng. # drink)


*Verb ä infinitive examples:*
* *nähdä:* `nähdä+V+Act+InfA+Sg+Lat`


*Verb E infinitive back examples:*
* *kutoessa:* `kutoa+V+Act+InfE+Sg+Ine`


*Verb E infinitive front examples:*
* *yltäessä:* `yltää+V+Act+InfE+Sg+Ine`

### Participles


*Verb nut participle passive tu examples:*
* *valkaistu:* `valkaista+V+Pss+PrfPrc+Sg+Nom` (Eng. # bleached)


*Verb nut participle passive ty examples:*
* *häpäisty:* `häpäistä+V+Pss+PrfPrc+Sg+Nom` (Eng. # sacrileged)


*Verb nut participle passive tu~u examples:*
* *huudettu:* `huutaa+V+Pss+PrfPrc+Sg+Nom` (Eng. # yelled)


*Verb nut participle passive ty~y examples:*
* *ylennetty:* `ylentää+V+Pss+PrfPrc+Sg+Nom` (Eng. # promoted)


*Verb nut participle passive tu~du examples:*
* *viipaloitu:* `viipaloida+V+Pss+PrfPrc+Sg+Nom` (Eng. # sliced)


*Verb nut participle passive ty~dy examples:*
* *yksilöity:* `yksilöidä+V+Pss+PrfPrc+Sg+Nom` (Eng. # singled out)


*Verb nut participle passive tu~lu examples:*
* *paranneltu:* `parannella+V+Pss+PrfPrc+Sg+Nom` (Eng. # embettered)


*Verb nut participle passive ty~ly examples:*
* *vähätelty:* `vähätellä+V+Pss+PrfPrc+Sg+Nom` (Eng. # belittled)


*Verb nut participle passive tu~nu examples:*
* *pantu:* `panna+V+Pss+PrfPrc+Sg+Nom` (Eng. # put)


*Verb nut participle passive ty~ny examples:*
* *menty:* `mennä+V+Pss+PrfPrc+Sg+Nom` (Eng. # went)


*Verb nut participle passive tu~ru examples:*
* *purtu:* `purra+V+Pss+PrfPrc+Sg+Nom` (Eng. # bitten)


*Verb nut participle passive ty~ry examples:*
* *pierty:* `pierrä+V+Pss+PrfPrc+Sg+Nom`

### Possessives for infinite verb forms

*Verb possessive front examples:*
* *kutoakseni:* `kutoa+V+Act+InfA+Sg+Tra+PxSg1`
* *kutoessasi:* `kutoa+V+Act+InfE+Sg+Ine+PxSg2`


*Verb possessive back examples:*
* *nähdäkseni:* `nähdä+V+Act+InfA+Sg+Tra+PxSg1`
* *nähdessäsi:* `nähdä+V+Act+InfE+Sg+Ine+PxSg2`


*Verb possessive back an examples:*
* *kutomattaan:* `kutoa+V+Act+InfMa+Sg+Abe+PxSg3`


*Verb possessive back en examples:*
* *kutoakseen:* `kutoa+V+Act+InfA+Sg+Tra+PxPl3`


*Verb possessive front en examples:*
* *nähdäkseen:* `nähdä+V+Act+InfA+Sg+Tra+PxPl3`


*Verb possessive back än examples:*
* *näkemättään:* `nähdä+V+Act+InfMa+Sg+Abe+PxSg3`

### Verb clitics


*Verbs clitic back examples:*
* *kudonhan:* `kutoa+V+Act+Ind+Prs+Sg1+Foc/han`
* *kudotko:* `kutoa+V+Act+Ind+Prs+Sg2+Qst`


*Verbs clitic front examples:*
* *näenhän:* `nähdä+V+Act+Ind+Prs+Sg1+Foc/han`
* *näetkö:* `nähdä+V+Act+Ind+Prs+Sg2+Qst`











## Deverbal derivations
Part of the deverbal derivation system in Finnish is so regular that it has
been included as part of inflectional morphology in many traditional systems.
These derivations are treated as inflection in our system as well.

### -minen, "Fourth infinitive"



### Participles


*Verbs participle back examples:*
* *kutomani:* `kutoa+V+AgPrc+Sg+Nom+PxSg1`
* *kutomaton:* `kutoa+V+NegPrc+Sg+Nom`
* *kutova:* `kutoa+V+Act+PrsPrc+Sg+Nom`


*Verbs participle front examples:*
* *näkemäni:* `nähdä+V+AgPrc+Sg+Nom+PxSg1`
* *näkemätön:* `nähdä+V+NegPrc+Sg+Nom`
* *näkevä:* `nähdä+V+Act+PrsPrc+Sg+Nom`


















































## Number transcriptions
Transcribing numbers to words in Finnish is not completely trivial, one
reason is that numbers in Finnish are written as compounds, regardless of
length:
123456 is *satakaksikymmentäkolmetuhattaneljäsataaviisikymmentäkuusi*.
Another limitation is that inflections can be unmarked in running text,
that is digit expression is assumed to agree the case of the phrase it is in,
e.g.
27 is *kaksikymmentäseitsemän*, and
27:lle *kahdellekymmenelleseitsemälle*
but in a phrase: "tarjosin 27 osanottajalle" 27 assumes the allative case
without marking and it is preferred grammatical form in good writing.

### Flag diacritics
Flag diacritics in number transcribing are used to control case agreement:
in Finnish numeral compounds all words agree in case except in nominative
singular where 10's exponential multipliers are in singular partitive.
 * `@U.CASE.SGNOM@ ` for singular nominative agreement
 * `@U.CASE.SGALL@ ` for singular allative agreement


### Morphotactics of digit strings
The morphotactics related to numbers and their transcriptions is that we
need to know the whole digit string to know how the length of whole digit
string to know what to start reading, and zeroes are not read out but
have an effect to readout.
The numerals are systematic and perfectly compositional:
the implementation of 100 000–999 999 is almost
exactly same as 100 000 000–999 000 000 and everything afterwads with the
change of word *tuhat*~*tuhatta*, *miljoona*~*miljoonaa*, *miljardia*,
*biljoonaa*, *biljardia* and so forth–that is along the long scale British
(French) system where American billion = milliard etc.
The numbers are built from ~single
word length blocks in decreasing order with the exception of zig-zagging
over numbers 11–19 where the second digit comes before first.
The rest of this documentation describes the morphotactic implementation
by the lexicon structure in descending order of magnitude with examples.

*Digits of all magnitudes examples:*
* *1:* `yksi`
* *21:* `kaksikymmentäyksi`
* *321:* `kolmesataakaksikymmentäyksi`
* *4321:* `neljätuhattakolmesataakaksikymmentäyksi`
* *54321:* `viisikymmentäneljätuhattakolmesataakaksikymmentäyksi`
* *654321:* `kuusisataaviisikymmentäneljätuhattakolmesataakaksikymmentäyksi`
* *7654321:* `seitsemänmiljoonaakuusisataaviisikymmentäneljätuhattakolmesataakaksikymmentäyksi`

Lexicon `HUNDREDSMRD` contains numbers 2-9 that need to be followed by exactly
11 digits: 200 000 000 000–999 999 999 999
this is to implement *N*sataa...miljardia...

Lexicon `CUODIMRD` contains numbers 2-9 that need to be followed by exactly
this is to implement N*sataa*...miljardia...

*hundreds of milliards examples:*
* *20000000000:* `kaksisataamiljardia`

Lexicon `HUNDREDMRD` is for numbers in range: 100 000 000 000–199 000 000 000
this is to implement *sata*...miljardia...

*hundred milliards examples:*
* *1000000000:* `satamiljardia`


Lexicon `TEENSMRD` is for numbers with 11 000 000 000–19 000 000 000
this is to implement ...N*toista*...miljardia...

Lexicon `TEENMRD` is for numbers with 11 000 000 000–19 000 000 000
this is to implement ...*N*toista...miljardia...

*teen milliards examples:*
* *1200000000:* `kaksitoistailjardia`


Lexicon `TENSMRD` is for numbers with 20 000 000 000–90 000 000 000
this is to implement ...*N*kymmentä...miljardia...

Lexicon `TENMRD` is for numbers with 10 000 000 000–10 999 999 999
this is to implement ...*kymmenen*miljardia...

*ten milliards examples:*
* *1000000000:* `kymmenenmiljardia`

Lexicon `LÅGEVMRD` is for numbers with 20 000 000 000–90 000 000 000
this is to implement ...N*kymmentä*...miljardia...

*tens of milliards examples:*
* *20000000000:* `kaksikymmentämiljardia`

Lexicon `ONESMRD` is for numbers with 1 000 000 000–9 000 000 000
this is to implement ...*N*miljardia...

Lexicon `MILJARD` is for numbers with 1 000 000 000–9 000 000 000
this is to implement ...N*miljardia*...

*milliards examples:*
* *2000000000:* `kaksimiljardia`


Lexicon `OVERMILLIONS` is for the millions *part* of numbers greater than 1 milliard


Lexicon `HUNDREDSM` contains numbers 2-9 that need to be followed by exactly
8 digits: 200 000 000–999 999 999
this is to implement *N*sataa...miljoonaa...

Lexicon `CUODIM` contains numbers 2-9 that need to be followed by exactly
this is to implement N*sataa*...miljoonaa...

*Hundreds of millions examples:*
* *200000000:* `kaksisataamiljoonaa`

Lexicon `HUNDREDM` is for numbers in range: 100 000 000–199 000 000
this is to implement *sata*...miljoonaa...


Lexicon `TEENSM` is for numbers with 11 000 000–19 000 000
this is to implement ...N*toista*...miljoonaa...

Lexicon `TEENM` is for numbers with 11 000 000–19 000 000
this is to implement ...*N*toista...miljoonaa...

*Teen millions examples:*
* *1200000:* `kaksitoistamiljoonaa`


Lexicon `TENSM` is for numbers with 20 000 000–90 000 000
this is to implement ...*N*kymmentä...miljoonaa...

Lexicon `TENM` is for numbers with 10 000 000–10 999 999
this is to implement ...*kymmenen*miljoonaa...

*Ten millions examples:*
* *2000000:* `kymmenenmiljoonaa`

Lexicon `LÅGEVM` is for numbers with 20 000 000–90 000 000
this is to implement ...N*kymmentä*...miljoonaa..

*Tens of millions examples:*
* *2000000:* `kaksikymmentämiljoonaa`

Lexicon `ONESM` is for numbers with 1 000 000–9 000 000
this is to implement ...*N*miljoonaa...

Lexicon `MILJON` is for numbers with 1 000 000–9 000 000
this is to implement ...N*miljoonaa*...

*Millions examples:*
* *200000:* `kaksisataamiljoonaa`


Lexicon `UNDERMILLION` is for numbers with 100 000–900 000 after milliards

Lexicon `OVERTHOUSANDS` is for the thousands *part* of numbers greater than 1 million

Lexicon `HUNDREDST` contains numbers 2-9 that need to be followed by exactly
5 digits: 200 000–999 999
this is to implement *N*sataa...tuhatta...

Lexicon `CUODIT` contains numbers 2-9 that need to be followed by exactly
this is to implement N*sataa*...tuhatta...

*Hundreds of thousands examples:*
* *20000:* `kaksisataatuhatta`

Lexicon `HUNDREDT` is for numbers in range: 100 000–199 000
this is to implement *sata*...tuhatta...

Lexicon `TEENST` is for numbers with 11 000–19 000
this is to implement ...N*toista*...tuhatta...


Lexicon `TEENT` is for numbers with 11 000–19 000
this is to implement ...*N*toista...tuhatta...

*Teens of thousands examples:*
* *12000:* `kaksitoistatuhatta`


Lexicon `TENST` is for numbers with 20 000–90 000
this is to implement ...*N*kymmentä...tuhatta...


Lexicon `TENT` is for numbers with 10 000 000–10 999 999
this is to implement ...*kymmenen*tuhatta...

*Ten thousands examples:*
* *10000:* `kymmenentuhatta`

Lexicon `LÅGEVT` is for numbers with 20 000–90 000
this is to implement ...N*kymmentä*...tuhatta..

*Tens of thousands examples:*
* *20000:* `kaksikymmentätuhatta`

Lexicon `ONEST` is for numbers with 1 000–9 000
this is to implement ...*N*tuhatta...

Lexicon `THOUSANDS` is for numbers with 1 000–9 000
this is to implement ...N*tuhatta*...

*Thousands examples:*
* *2000:* `kaksituhatta`
* *3456:* `kolmetuhattaneljäsataaviisikymmentäkuusi`

Lexicon `THOUSAND` is for the ones-tens-hundreds of numbers greater than thousand


Lexicon `UNDERTHOUSAND` is for numbers with 100–900 after thousands

Lexicon `HUNDREDS` contains numbers 2-9 that need to be followed by exactly
2 digits: 200–999
this is to implement *N*sataa...

Lexicon `CUODI` contains numbers 2-9 that need to be followed by exactly
this is to implement N*sataa*...

*Hundreds examples:*
* *200:* `kaksisataa`
* *345:* `kolmesataaneljäkymmentäviisi`

Lexicon `HUNDRED` is for numbers in range: 100–999

Lexicon `TEENS` is for numbers with 11–19
this is to implement ...N*toista*

Lexicon `TEEN` is for numbers with 11–19
this is to implement ...*N*toista

*Teens examples:*
* *11:* `yksitoista`
* *12:* `kaksitoista`
* *13:* `kolmetoista`



Lexicon `TENS` is for numbers with 20–90
this is to implement ...*N*kymmentä...

Lexicon `LÅGEV` is for numbers with 20–90
this is to implement ...N*kymmentä*...

*Tens examples:*
* *20:* `kaksikymmentä`
* *34:* `kolmekymmentäneljä`

Lexicon `JUSTTEN` is for number 10
this is to implement ...*kymmenen*

*Ten examples:*
* *10:* `kymmenen`

Lexicon `ONES` is for numbers with 1–9
this is to implement yksi, kaksi, kolme..., yhdeksän

*Ones examples:*
* *1:* `yksi`
* *2:* `kaksi`
* *3:* `kolme`

Lexicon `ZERO` is for number 0
nolla

*Zero examples:*
* *0:* `nolla`


Lexicon `LOPPU` is to implement potential case inflection with a colon.

*Digits with explicit cases examples:*
* *1\:lle:* `yhdelle`
*Note:* accepting or rejecting case inflected digit strings without explicit
suffix can be changed here.










 * "Force hyphen between vowels"   

