%determiners and entailment

%calls wordnet files
:- ensure_loaded([wordnet]).

%calls porter stemmer
:- use_module(library(porter_stem)).
:- ensure_loaded(library(porter_stem)).


%creates enatailment relations
hypernymSynset(S1, S2) :-				%S1 entails S2 iff hyp(S1, S2).
	hyp(S1, S2).

%recursive statement
hypernymSynset(S1, S2) :-				%S1 entails S2 iff S3 entails S2 and hyp(S1, S3)
	hyp(S1, S3),
	hypernymSynset(S3, S2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%intermediate predicates

%takes words and uses synset_id to determine entailment
entail(Word1, Word2) :-
	s(Synset_Id1, _, Word1, PoS, _, _),
	hypernymSynset(Synset_Id1, Synset_Id2),
	s(Synset_Id2, _, Word2, PoS, _, _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%predicates for the four possible relations

mono_left_downward(Statement1, Statement2) :-
%Statement = [noun, verb]
%no([fish, fly], [salmon, fly]
	Statement1 = [H1|T1],
	Statement2 = [H2|T2],
	entail(H2, H1),
	T1 = T2.	

mono_left_upward(Statement1, Statement2) :-
%Statement = [noun, verb]
%some ([animal, walk], [animal, move])
	Statement1 = [H1|T1],
	Statement2 = [H2|T2],
	entail(H1, H2),
	T1 = T2.	

mono_right_downward(Statement1, Statement2) :-
%Statement = [noun, verb]
%no ([rock, move], [rock, walk])
	Statement1 = [H1|_],
	Statement2 = [H2|_],
	nth1(2, Statement1, T1),
	nth1(2, Statement2, T2),
	entail(T2, T1),
	H1 = H2.	

mono_right_upward(Statement1, Statement2) :-
%Statement = [noun, verb]
%all([cats, walk], [cats, move])
	Statement1 = [H1|_],
	Statement2 = [H2|_],
	nth1(2, Statement1, T1),
	nth1(2, Statement2, T2),
	entail(T1, T2),
	H1 = H2.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%quantifier lexicon
quantifier(all, mono_left_downward).
quantifier(all, mono_right_upward).
quantifier(some, mono_left_upward).
quantifier(some, mono_right_upward).
quantifier(no, mono_left_downward).
quantifier(no, mono_right_downward).
quantifier(every, mono_left_downward).
quantifier(every, mono_right_upward).
quantifier(no_more_than, mono_left_downward).
quantifier(no_more_than, mono_right_downward).
quantifier(few, mono_left_downward).
quantifier(few, mono_right_downward).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%predicate to determine validity of relation
quantifier_mono(Quantifier, Statement1, Statement2) :-
%Quantifier = word
%Statements = [noun, verb]	
	quantifier(Quantifier, Direction),
	functor(Y, Direction, 2),
	arg(1, Y, Statement1),
	arg(2, Y, Statement2),
	Y.
