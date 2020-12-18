%
% Advent of Code 2020 - Day 7.
%

% :- working_directory(_, 'C:/Users/stephen.mccoy/github/aoc2020/day1/').

read_input_line(Stream, X) :- 
	read_line_to_string(Stream, String),
	!,
	String \== end_of_file,
	number_string(X, String).

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
	{	char_type(Cin, alphanumeric),
		downcase_atom(Cin, Cout)
	}.

identifier_chars([HChar | TChars]) -->
	identifier_char(HChar),
	identifier_chars(TChars).
identifier_chars([Char]) -->
	identifier_char(Char).

% ID of a body - sequence of alphanumeric characters.
word(AtomName) -->
	identifier_chars(IDChars),
	{ atom_chars(AtomName, IDChars)
	}.

head(Head) --> 
    word(A1), [' '], word(A2),
    {   atom_list_concat([A1,A2], '_', Head)
    }.

rule(Head, Tail) --> 
    head(Head), "contain", body(Tail).

head(Head) -->
    word(First), " ", word(Second).
