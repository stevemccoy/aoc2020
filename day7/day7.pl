%
% Advent of Code 2020 - Day 7.
%

:- working_directory(_, 'C:/Users/stephen.mccoy/github/aoc2020/day7/').

% Parse input using the DCG.

read_input_line(Stream, List) :- 
	read_line_to_string(Stream, String),
	!,
	String \== end_of_file,
	string_chars(String, Chars),
	phrase(rule(Head, Tail), Chars, []),
    findall(bag_has(Head, X), (member(X, Tail), assertz(bag_has(Head, X))), List).

read_input_lines(Stream, [X | Tail]) :-
	read_input_line(Stream, X),
	!,
	read_input_lines(Stream, Tail).
read_input_lines(_, []).

read_input_data(FileName, List) :-
	open(FileName, read, Stream),
	read_input_lines(Stream, List),
	close(Stream).

% DCG rules.

identifier_char(Cout) -->
	[Cin],
	{	char_type(Cin, alpha),
		downcase_atom(Cin, Cout)
	}.

identifier_chars([HChar | TChars]) -->
	identifier_char(HChar),
	identifier_chars(TChars).
identifier_chars([Char]) -->
	identifier_char(Char).

word(AtomName) -->
	identifier_chars(IDChars),
	{ atom_chars(AtomName, IDChars)
	}.

integer(I) -->
        digit(D0),
        digits(D),
        { number_codes(I, [D0|D])
        }.

digits([D|T]) -->
        digit(D), !,
        digits(T).
digits([]) -->
        [].

digit(D) -->
        [D],
        { code_type(D, digit)
        }.

% Reserved words.
bag --> [b,a,g].
bags --> bag, [s].
contain --> [c,o,n,t,a,i,n].
no_other --> [n,o,' ',o,t,h,e,r].
space --> [' '].
stop --> ['.'].

bag_or_bags -->
	bag.
bag_or_bags -->
	bags.

head(Head) --> 
    word(A1), space, word(A2), space, bags,
    {   atomic_list_concat([A1,A2], '_', Head)
    }.

quantity(Q) -->
	integer(Q).

bag_spec(Item) -->
	quantity(Q), space, word(A1), space, word(A2), space, bag_or_bags,
	{	atomic_list_concat([A1,A2], '_', Name),
		Item =.. [Name, Q]
	}.

body([]) --> no_other, space, bags.
body([Item | Tail]) -->
	bag_spec(Item), [','], space, body(Tail).
body([Item]) -->
	bag_spec(Item).

rule(Head, Tail) --> 
    head(Head), space, contain, space, body(Tail), stop.

% Solutions.

contains_shiny_gold_bag(C) :-
	bag_has(C, D),
	contains_shiny_gold_bag(D).
contains_shiny_gold_bag(C) :- 
	bag_has(C, shiny_gold(_)).

day7part1(List) :-
	retractall(bag_has(_,_)),
	read_input_data("input7.txt", List),
	findall(X, contains_shiny_gold_bag(X), BagList),
	length(BagList, Count),
	format('Day 7, Part 1: ~d bags eventually contain a shiny gold bag.', [Count]).

