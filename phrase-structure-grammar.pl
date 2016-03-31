
%allows for omitting the empty list
s(Parse, Sentence) :- s(Parse, Sentence, []).

%sentences
%declarative
s(s(Subject, VP)) --> np(Subject, Person, Number, nominative), vp(VP, VerbForm), {phi(Person, Number, VerbForm)}.	
%predicate nominative
s(s(Subject, PN)) --> np(Subject, Person, Number, nominative), pn(PN, Person, Number, _).					
%predicate adjective 
s(s(Subject, PA)) --> np(Subject, Person, Number, nominative), pa(PA, Person, Number, _).

%interrogative
%wh- object movement
s(q(WH, AUX, Subject, VP, trace(WH))) --> wh(WH), aux(AUX, VerbForm), np(Subject, Person, Number, nominative), qvp(VP, vb), {phi(Person, Number, VerbForm)}.	
qvp(vp(VTrans), vb) --> vt(VTrans, vb).			%question verb phrase for object wh- movement
%wh- subject movement
s(q(WH, trace(WH), VP)) --> wh(WH), vp(VP, VerbForm), {phi(_, _, VerbForm)}.
%wh- predicative nominative/adjective movement
s(q(WH, trace(WH), PN)) --> wh(WH), pn(PN, _, _, _).		%not working properly - accepts "who is you"
s(q(WH, trace(WH), PA)) --> wh(WH), pa(PA, _, _, _).		%not working properly - why isn't "who are available" working?

%raising constructions
s(s(Subject, VRP)) --> np(Subject, Person, Number, nominative), vrp(VRP, VerbForm, Subject, Person, Number), {phi(Person, Number, VerbForm)}.
%with an expletive
s(s(Exp, VRP)) --> exp(Exp, 3, Number), vrp_exp(VRP, VerbForm, _, Person, Number), {phi(Person, Number, VerbForm)}.
%verb raising phrase
vrp(vp(VR, SBar), VerbForm, Subject, Person, Number) --> vr(VR, VerbForm), sbar(SBar, Subject, Person, Number).			
%with an expletive	
vrp_exp(vp(VR, SBar), VerbForm, Subject, Person, Number) --> vr(VR, VerbForm), sbar_exp(SBar, Subject, Person, Number).			
%embedded clause rules
sbar(sbar(trace(Subject), inf(to), VP), Subject, _, _) --> [to], vp(VP, vb).									%nonfinite
sbar(sbar(trace(Subject), inf(to), PN), Subject, Person, Number) --> [to], pn(PN, Person, Number, vb).			%predicate nominative
sbar(sbar(trace(Subject), inf(to), PA), Subject, _, _) --> [to], pa(PA, _, _, vb).								%predicate adjective
%with an expletive
sbar_exp(sbar(inf(to), v(be), np(Subject), adj(PredAdj)), Subject, Person, Number) --> [to], [be], np(Subject, Person, Number, nominative), adj(PredAdj).

%noun phrases
np(np(NNP), 3, singular, _) --> nnp(NNP, singular).										%nnp = proper noun
np(np(DET, NN), 3, Number, _) --> det(DET, Number), nn(NN, Number).						%nn = common noun
np(np(QUANT, NN), 3, Number, _) --> quant(QUANT, Number), nn(NN, Number).				%nn and quantifiers
np(np(NN), 3, plural, _) --> nn(NN, plural).											%for plural common nouns
np(np(PRO), Person, Number, Case) --> pro(PRO, Person, Number, Case).					%pronouns
%np(np(NP, PP), Person, Number) --> np(NP, Person, Number), pp(PP).						%prepositions -- left recursive problem

%verb phrases
vp(vp(VTrans, Object), VerbForm) --> vt(VTrans, VerbForm), np(Object, _, _, objective).																		%transitive  
vp(vp(VInt), VerbForm) --> vi(VInt, VerbForm).																												%intransitive
vp(vp(VDi, IO, DO), VerbForm) --> vd(VDi, VerbForm), np(IO, _, _, objective), np(DO, _, _, objective).														%ditransitive
pn(pn(VBe, PredNom), Person, Number, VerbForm) --> be(VBe, Person, Number, VerbForm), np(PredNom, _, Number, nominative), {phi(Person, Number, VerbForm)}.	%predicate nominative
pa(pa(VBe, PredAdj), Person, Number, VerbForm) --> be(VBe, _, _, VerbForm), adj(PredAdj), {phi(Person, Number, VerbForm)}.									%predicate adjective
%vp(vp(VP, PP), VerbForm) --> vp(VP, VerbForm), pp(PP).																										%prepositions -- left recursive problem

%prepositional phrase
%pp(pp(P, NP)) --> prep(P), np(NP, _, _).													%preposition rule -- will cause left recursion problems

%%%%%%%%%%%lexicon%%%%%%%%%%%%

%wh-words
wh(wh(WH)) --> [WH], {wh_words(List), member(WH, List)}.
wh_words([who]).

%expletives
exp(exp(there), 3, singular) --> [there].
exp(exp(there), 3, plural) --> [there].

%determiners
det(det(the), _) --> [the].
det(det(a), singular) --> [a].

%prepositions
%pp(prep(with)) --> [with].

%common nouns
%singular
nn(nn(NN), singular) --> [NN], {nn_singular(List), member(NN, List)}.
nn_singular([book, boy, man, girl, ball, pig]).
%plural
nn(nn(NN), plural) --> [NN], {nn_plural(List), member(NN, List)}.
nn_plural([boys, books, men, girls, balls, pigs]).

%proper nouns
nnp(nnp(NNP), singular) --> [NNP], {nnp(List), member(NNP, List)}.
nnp([john, mary, bill, michael, sue, erika]).

%pronouns - subject
pro(i, 1, singular, nominative) --> [i].
pro(you, 2, _, _) --> [you].
pro(he, 3, singular, nominative) --> [he].
pro(she, 3, singular, nominative) --> [she].
pro(it, 3, singular, _) --> [it].
pro(we, 1, plural, nominative) --> [we].
pro(they, 3, plural, nominative) --> [they].

%pronouns - object
pro(me, 1, singular, objective) --> [me].
%pro(you, 2, _, objective) --> [you].
pro(him, 3, singular, objective) --> [him].
pro(her, 3, singular, objective) --> [her].
%pro(it, 3, singular, objective) --> [it].
pro(us, 1, plural, objective) --> [us].
pro(them, 3, plural, objective) --> [them].

%quantifiers
%singular
quant(quant(QUANT), singular) --> [QUANT], {quant_singular(List), member(QUANT, List)}.
quant_singular([one]).
%plural
quant(quant(QUANT), plural) --> [QUANT], {quant_plural(List), member(QUANT, List)}.
quant_plural([some, many, several]).

%adjectives
adj(adj(ADJ)) --> [ADJ], {adjectives(List), member(ADJ, List)}.
adjectives([available, yellow, blue, green, ugly, hungry]).

%verbs
%transitive
%verb forms of kick
vt(vb(kick), vb) --> [kick].			%vb = base
vt(vbp(kick), vbp) --> [kick].			%vbp = present
vt(vbz(kicks), vbz) --> [kicks].		%vbz = 3rd person singular present
vt(vbd(kicked), vbd) --> [kicked].		%vbd = past tense
vt(vbg(kicking), vbg) --> [kicking].	%vbg = present participle
vt(vbn(kicked), vbn) --> [kicked]. 		%vbn = past participle

%verb forms of see
vt(vb(see), vb) --> [see].				%vb = base
vt(vbp(see), vbp) --> [see].			%vbp = present
vt(vbz(sees), vbz) --> [sees].			%vbz = 3rd person singular present
vt(vbd(saw), vbd) --> [saw].			%vbd = past tense
vt(vbg(seeing), vbg) --> [seeing].		%vbg = present participle
vt(vbn(seen), vbn) --> [seen]. 			%vbn = past participle

%intransitive
%verb forms of sleep
vi(vb(sleep), vb) --> [sleep].			%vb = base
vi(vbp(sleep), vbp) --> [sleep].		%vbp = present
vi(vbz(sleeps), vbz) --> [sleeps].		%vbz = 3rd person singular present
vi(vbd(slept), vbd) --> [slept].		%vbd = past tense
vi(vbg(sleeping), vbg) --> [sleeping].	%vbg = present participle
vi(vbn(slept), vbn) --> [slept]. 		%vbn = past participle

%ditransitive
%verb forms of give
vd(vb(give), vb) --> [give].			%vb = base
vd(vbp(give), vbp) --> [give].			%vbp = present
vd(vbz(gives), vbz) --> [gives].		%vbz = 3rd person singular present
vd(vbd(gave), vbd) --> [gave].			%vbd = past tense
vd(vbg(giving), vbg) --> [giving].		%vbg = present participle
vd(vbn(given), vbn) --> [given]. 		%vbn = past participle

%verb to be
be(be(be), _, _, vb) --> [be].			%vb = base
be(be(being), _, _, vbg) --> [being].	%vbg = present participle
be(be(been), _, _, vbn) --> [been].		%vbn = past participle
be(be(am), 1, singular, vbp) --> [am].
be(be(are), 2, _, vbp) --> [are].
be(be(is), 3, singular, vbz) --> [is].
be(be(are), _, plural, vbp) --> [are].
%be(be(are), 2, plural, vbp) --> [are].
%be(be(are), 3, plural, vbp) --> [are].
be(be(was), 1, singular, vbd) --> [was].
be(be(were), 2, singualr, vbd) --> [were].
be(be(was), 3, singular, vbd) --> [was].
be(be(were), _, plural, vbd) --> [were].

%auxiliaries
%verb forms of do
aux(aux(do), vb) --> [do].				%vb = base
aux(aux(do), vbp) --> [do].				%vbp = present
aux(aux(does), vbz) --> [does].			%vbz = 3rd person singular present
aux(aux(did), vbd) --> [did].			%vbd = past tense
aux(aux(doing), vbg) --> [doing].		%vbg = present participle
aux(aux(done), vbn) --> [done]. 		%vbn = past participle

%raising verbs
%verb forms of seem
%vr(vb(seem), vb) --> [seem].			%vb = be 					%commented out because it was letting incorrect use of vbz/vbp of verb raisings through
vr(vbp(seem), vbp) --> [seem].			%vbp = present
vr(vbz(seems), vbz) --> [seems].		%vbz = 3rd person singular present
vr(vbd(seemed), vbd) --> [seemed].		%vbd = past tense
vr(vbg(seeming), vbg) --> [seeming].	%vbg = present participle
vr(vbn(seemed), vbn) --> [seemed]. 		%vbn = past participle

%verb forms of is_likely
%vr(vb(be_likely), vb) --> be(_, _, _, vb), [likely].				%commented out because it was letting incorrect use of vbz/vbp of verb raisings through
vr(vbp(am_likely), vbp) --> be(_, 1, singular, vbp), [likely].
vr(vbp(are_likely), vbp) --> be(_, 2, _, vbp), [likely].
vr(vbz(is_likely), vbz) --> be(_, 3, singular, vbz), [likely].
vr(vbp(are_likely), vbp) --> be(_, _, plural, vbp), [likely].
%vr(vbp(are_likely), vbp) --> be(_, 2, plural, vbp), [likely].
%vr(vbp(are_likely), vbp) --> be(_, 3, plural, vbp), [likely].
vr(vbd(was_likely), vbd) --> be(_, 1, singular, vbd), [likely].
vr(vbd(were_likely), vbd) --> be(_, 2, singular, vbd), [likely].
vr(vbd(was_likely), vbd) --> be(_, 3, singular, vbd), [likely].
vr(vbd(were_likely), vbd) --> be(_, _, plural, vbd), [likely].

%verb form of is_happy

%phi agreement
phi(3, singular, vbz).			%only case for present
phi(_, _, vbd).					%every case works for past
phi(_, _, vb).					%every case works for infinitive
phi(Person, Number, vbp) :- \+	%every case EXCEPT ... works for present/base
	(Person = 3,
	Number = singular).

