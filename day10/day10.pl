/*
    Advent of Code 2020 - Day 10, Part 2.
*/

:- working_directory(_, 'C:/Users/stephen.mccoy/github/aoc2020/day10/').
:- set_prolog_flag(stack_limit, 4_194_304_000).

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

% Utility Functions.

% List concatenation.
conc([], L, L).
conc([H | Tail], L1, [H | L2]) :-
	conc(Tail, L1, L2).

% Used for quicksort partition below.
compare_terms(X, Y, Delta) :-
	Delta is Y - X.

% Partition list of numbers based on given pivot element.
% partition(InputList, Pivot, LowPart, HighPart).

partition([], _, [], []).

partition([X | Tail], Pivot, [X | LowPart], HighPart) :-
	compare_terms(X, Pivot, Delta),
	Delta > 0,
	!,
	partition(Tail, Pivot, LowPart, HighPart).

partition([X | Tail], Pivot, LowPart, [X | HighPart]) :-
	partition(Tail, Pivot, LowPart, HighPart).	
	
% quicksort(List, SortedList) - Quicksort algorithm.
quicksort([], []).
quicksort([X | Tail], Sorted) :-
	partition(Tail, X, Small, Big),
	quicksort(Small, SortedSmall),
	quicksort(Big, SortedBig),
	conc(SortedSmall, [X | SortedBig], Sorted).

% dividelist(List, List1, List2) - Decompose List into List1 and List2.	

dividelist([], [], []).
dividelist([X | Tail], [X | Tail1], List2) :-
	dividelist(Tail, Tail1, List2).
dividelist([X | Tail], List1, [X | Tail2]) :-
	dividelist(Tail, List1, Tail2).

% Delete an item from a list.
del(Item, [Item | List], List).
del(Item, [First | List1], [First | List2]) :-
	del(Item, List1, List2).

sequence(_, _, []).
sequence(From1, Jolts1, [Head | Tail]) :-
	del(Head, From1, From2),
	Head > Jolts1,
	member(Delta, [1,2,3]),
	Head is Jolts1 + Delta,
	sequence(From2, Head, Tail).



day10part2(Count) :-
	read_input_data('input10.txt', InList),
	quicksort(InList, List),
	aggregate(count, S, sequence(List, 0, S), Count),
	format('~d sequences found.', [Count]).
