%NFSA implementation

:- dynamic arc/3.				%allows assert to work


%can use loops (1,x,1) or empty transitions (1,'#', 0)
change_directory :-
	working_directory(_, "/home/mcapizzi/Google_Drive_Arizona/Programming/Prolog/Formal_FSA/").

%should I change this to be asserted in input?
initial(0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%only requires the symbol list
recognizer(String) :- 
	initial(Current_State),
	recognizer_intermediate(Current_State, String).
	
%recognizes and generates FSA
recognizer_intermediate(Current_State, []) :-								%base case
	final(Current_State).													%succeeds if Current_State is the final node

%can handle non-deterministic or deterministic in set form
recognizer_intermediate(Current_State, String) :-			%set form deterministic				%recursive case
	String = [H | T],
	(arc(Current_State, H, Next_State);			
		arc(Current_State, Z, Next_State),
		member(H, Z)),
	recognizer_intermediate(Next_State, T).

recognizer_intermediate(Current_State, String) :-			%non-deterministic with EPSILON
	arc(Current_State, '#', Next_State),
	recognizer_intermediate(Next_State, String).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
string_to_test :-
	nl, nl,
	write('Now that you\'ve entered the arguments for the FSA, enter the string you\'d like to test.'), nl,
	write('Enter it as a list in the following format:'), nl,
	write('[a,b,c,d]'), nl,
%can't generate all possible strings - why not?
%	write('Or if you\'d like to see all possible strings that match the FSA, type Y'), nl,
	read(X),
	recognizer(X).

%takes user input for arcs
input_arguments :-
	%trace,
	nl,nl,nl,nl,nl,
	write('Now provide the FSA paths in the following format:'), nl,
	write('arc([beginning node], [symbol], [ending_node])'), nl,
	write('The first entry should beging with the starting node of 0'), nl,
	write('example: arc(0,h,1)'), nl,
	write('The last entry should be the final node, written as final(0)'), nl,
	write('example: final(6)'), nl,
	write('When you are finished, push CTRL-D'), nl,
	read(X),
	(X \== end_of_file 
		-> 
		assert(X),
		input_arguments
		;	
		string_to_test).
