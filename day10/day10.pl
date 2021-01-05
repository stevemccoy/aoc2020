/*
    Advent of Code 2020 - Day 10, Part 2.
*/

:- working_directory(_, 'C:/Users/stephen.mccoy/github/aoc2020/day10/').
:- set_prolog_flag(stack_limit, 4_194_304_000).
:- dynamic entry/1.
:- dynamic entry/2.

read_input_line(Stream, X) :- 
	read_line_to_string(Stream, String),
	!,
	String \== end_of_file,
	number_string(X, String).

read_input_lines(Stream, [X | Tail]) :-
	read_input_line(Stream, X),
	assertz(entry(X)),
	!,
	read_input_lines(Stream, Tail).
read_input_lines(_, []).

read_input_data(FileName, List) :-
	retractall(entry(_)),
	open(FileName, read, Stream),
	read_input_lines(Stream, List),
	close(Stream).

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

% Delete an item from a list.
del(Item, [Item | List], List).
del(Item, [First | List1], [First | List2]) :-
	del(Item, List1, List2).

% State space definition for depth-first search below (succ/2 and goal/1).

succ(State1, State2) :-
	entry(State1),
	member(Delta, [1,2,3]),
	State2 is State1 - Delta,
	entry(State2).

goal(State) :-
	State =< 3.

% depthfirst(Path, Node, Solution)
%	extending the path [Node | Path] to a goal gives Solution
depthfirst(Path, Node, [Node | Path]) :-
	goal(Node).
depthfirst(Path, Node, Solution) :-
	succ(Node, Node1),
	not(member(Node1, Path)),	% Prevent cycles.
	depthfirst([Node | Path], Node1, Solution).


sum(X, Y, Z) :-
	Z is X + Y.

% Aggregate elements of a list successively by a named function(X,Y,Z), 
% producing a scalar result.
my_aggregate_list([A], _, A).
my_aggregate_list([X, Y | InTail], Function, Result) :-
	Goal =.. [Function, X, Y, Z],
	Goal,
	my_aggregate_list([Z | InTail], Function, Result).

% Count the number of solutions from the given starting node to a goal node, making 
% use of previously calculated entries.
% count_solutions(Node, Count)
count_solutions(Node, Count) :-
	entry(Node, Count),
	!.
count_solutions(Node, Count) :-
	findall(X, (succ(Node, N1), count_solutions(N1, X)), CL1),
	my_aggregate_list(CL1, sum, Count).

% Count the number of solutions (paths from start position to a goal node), for 
% each starting node in the given list, recording these counts in entry/2 facts.
count_solutions_list([]).
count_solutions_list([H | Tail]) :-
	goal(H),
	!,
	findall(X, depthfirst([], H, X), XL),
	length(XL, Count),
	assertz(entry(H, Count)),
	count_solutions_list(Tail).
count_solutions_list([H | Tail]) :-
	count_solutions(H, Count),
	!,
	assertz(entry(H, Count)),
	count_solutions_list(Tail).

% Read input and count solution paths (configurations of adapters).
do_stuff(InputFile, MaxCount) :-
	read_input_data(InputFile, InList),
	quicksort(InList, List),
	conc(_, [MaxNode], List),
	% Count and stash solution counts, for increasing lengths of path.
	retractall(entry(_,_)),
	count_solutions_list(List),
	!,
	% Retrieve the count for full length path.
	entry(MaxNode, MaxCount),
	format('~d sequences found.', [MaxCount]).

day10part2_test1(Count) :-	do_stuff('example10a.txt', Count).

day10part2_test2(Count) :-	do_stuff('example10b.txt', Count).

day10part2(Count) :-		do_stuff('input10.txt', Count).
