%
% ADVENT OF CODE 2020 - DAY 3
% 

:- working_directory(_, 'C:/Users/stephen.mccoy/github/aoc2020/day3/').

bits_string([0 | BTail], ['.' | STail]) :-
	bits_string(BTail, STail).
bits_string([1 | BTail], ['#' | STail]) :-
	bits_string(BTail, STail).
bits_string([], []).

% Get zero-indexed item in a list.
get_item(0, [Item | _], Item).
get_item(N, [_ | Tail], Item) :-
	N > 0,
	M is N - 1,
	get_item(M, Tail, Item).

read_input_line(Stream, List) :- 
	read_line_to_string(Stream, String),
	!,
	String \== end_of_file,
	string_chars(String, Chars),
	bits_string(List, Chars).

skip_lines(Stream, N) :-
	N > 0,
	read_line_to_string(Stream, String),
	String \== end_of_file,
	!,
	M is N - 1,
	skip_lines(Stream, M).
skip_lines(_, N) :-
	N >= 0.

% Input: Pos1, Count1.
% Read line.
% Add item(Pos1) to Count1.

read_input_lines(Stream, Rows, Delta, InPos, InCount, OutPos, OutCount) :-
	read_input_line(Stream, List),
	!,
	get_item(InPos, List, Item),
	Count2 is InCount + Item,
	TmpPos is InPos + Delta,
	Pos2 is TmpPos mod 31,
	Skip is Rows - 1,
	skip_lines(Stream, Skip),
	read_input_lines(Stream, Rows, Delta, Pos2, Count2, OutPos, OutCount).
read_input_lines(_, _, _, InPos, InCount, InPos, InCount).

read_input_lines(Stream, Rows, Cols, FinalCount) :-
	read_input_lines(Stream, Rows, Cols, 0, 0, _, FinalCount).

day3part1(Count) :-
	open('input3.txt', read, Stream),
	read_input_lines(Stream, 1, 3, Count),
	format('Part 1 - All done, tree count = ~d', [Count]),
	close(Stream).

day3part2(R, C, Count) :-
	open('input3.txt', read, Stream),
	read_input_lines(Stream, R, C, Count),
	format('Part 2 - Rows = ~d, Cols = ~d, Tree count = ~d', [R, C, Count]),
	nl,
	close(Stream).

day3part2(Product) :-
	findall(T, (
		member([R,C], [[1,1],[1,3],[1,5],[1,7],[2,1]]),
		day3part2(R, C, T)
	), [T1, T2, T3, T4, T5]),
	P1 is T1 * T2,
	P2 is T3 * T4,
	P3 is P1 * P2,
	Product is P3 * T5,
	format('Part 2 - Product: ~d * ~d * ~d * ~d * ~d = ~d', [T1, T2, T3, T4, T5, Product]).


