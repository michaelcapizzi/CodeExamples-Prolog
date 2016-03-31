/* tokenizer  */

:- ensure_loaded([parsing_grammar]).

%tokenize - takes user input and turns it into a list of ASCII
tokenize(X) :-						
	write('Give me only one sentence.  Please put it inside single quotes then finish with another PERIOD after your end quotes:'),
	nl,
	read(X),
	name(X, L),				%turns user input (X) into a list of ASCII
	separate(L, [], [], _).		%calls separate/4 

%space
separate([H|T], Acc, List, Final_List) :-	
	H = 32,					%uses space to separate words
	atom_codes(Word, Acc),
	append(List, [Word], New_List), !,
	separate(T, [], New_List, Final_List).
	
%comma
separate([H|T], Acc, List, Final_List) :-
	H = 44,					%handles a comma
	atom_codes(Word, Acc),
	append(List, [Word], New_List), !,
	atom_codes(Comma, [',']),
	append(New_List, [Comma], Newer_List), !,
	T = [TH | RT],
	(TH = 32 -> separate(RT, [], Newer_List, Final_List); separate(T, [], Newer_List, Final_List)).	

%semicolon
separate([H|T], Acc, List, Final_List) :-
	H = 59,					%handles a comma
	atom_codes(Word, Acc),
	append(List, [Word], New_List), !,
	atom_codes(Comma, [';']),
	append(New_List, [Comma], Newer_List), !,
	T = [TH | RT],
	(TH = 32 -> separate(RT, [], Newer_List, Final_List); separate(T, [], Newer_List, Final_List)).	
	
%more than one sentence		%if written entry is more than one sentence
separate([H|T], _, _, _) :-
	H = 46,
	T \= [],
	nl, nl,
	write('It appears that you have provided me with more than one sentence.'),
	nl,
	tokenize(_).

%period at end of sentence
separate([H|_], Acc, List, Final_List) :-	
	H = 46,									%if there is a period at the end
	atom_codes(Word, Acc),					%ignoes period and calls last word only
	append(List, [Word], Final_List), !,
	write(Final_List),
	nl, nl,
	s(Parse, Final_List, []), !,			%calls sentence from [parsing_grammar]
	write(Parse).

%end of sentence
separate([], Acc, List, Final_List) :-
	atom_codes(Word, Acc),
	append(List, [Word], Final_List), !,
	write(Final_List),
	nl, nl,
	s(Parse, Final_List, []), !,		%calls sentence from [parsing_grammar]
	write(Parse).

separate([H|T], Acc, List, Final_List) :-			%general case
	append(Acc, [H], Acc_Word), !,
	separate(T, Acc_Word, List, Final_List).
	
