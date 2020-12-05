/*
	Two entries in the expense report - find the ones that sum to 2020, 
	multiply them and return the product.
*/

% :- working_directory(_, 'C:/Users/Steve/github/aoc2020/day1/').

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
	retractall(entry(_)),
	open(FileName, read, Stream),
	read_input_lines(Stream, List),
	close(Stream).

day1part1([A,B,Ret]) :-
	read_input_data('input1.txt', List),
	member(A, List),
	member(B, List),
	A \== B,
	2020 is A + B,
	Ret is A * B.

day1part2([A,B,C,Ret]) :-
	read_input_data('input1.txt', List),
	member(A, List),
	member(B, List),
	member(C, List),
	A \== B,
	B \== C,
	A \== C,
	2020 is A + B + C,
	Ret is A * B * C.
