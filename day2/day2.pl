%
% Advent of Code 2020 - Day 2.
%

:- working_directory(_, 'C:/Users/stephen.mccoy/github/aoc2020/day2/').
:- dynamic input/1.

read_input_lines(List) :-
	findall(X, input(X), List).

read_input(List) :-
	retractall(input(_)),
	load_files(['input.pl'], []),
	!,
	read_input_lines(List).

% Count number of occurrences in a list.
count_matches(X, [X | Tail], N) :-
	!,
	count_matches(X, Tail, M),
	N is M + 1.
count_matches(X, [_ | Tail], N) :-
	!,
	count_matches(X, Tail, N).
count_matches(_, [], 0).

% Get indexed item in a list.
get_item(1, [Item | _], Item).
get_item(N, [_ | Tail], Item) :-
	N > 1,
	M is N -1,
	get_item(M, Tail, Item).


% Verify the given example against the rule.
verify_part1([Min,Max,Item], Example) :-
	atom_chars(Example, Atoms),
	count_matches(Item, Atoms, Count),
	Count =< Max,
	Count >= Min.

verify_part2([Pos1, Pos2, Item], Example) :-
	atom_chars(Example, Atoms),
	findall(X, 
		(	member(X,[Pos1,Pos2]), 
			get_item(X, Atoms, Item)
		), L),
	length(L, 1).

day2part1(Good) :-
	read_input(InputList),
	findall(Example, 
		(	member([Min,Max,Item,Example], InputList), 
			verify_part1([Min,Max,Item],Example)), 
		Good),
	length(Good, GoodCount),
	format('In Part 1, ~d examples were good.', [GoodCount]).

day2part2(Good) :-
	read_input(InputList),
	findall(Example, 
		(	member([Min,Max,Item,Example], InputList), 
			verify_part2([Min,Max,Item],Example)
		), 
		Good),
	length(Good, GoodCount),
	format('In Part 2, ~d examples were good.', [GoodCount]).


