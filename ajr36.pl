%% ajr36 - cm20214 semester 2 coursework

% NOTE: I use the length() builtin predicate to make the program easier to examine,
% it does not provide any functionality for the program.

%------------------------------------------------------------------------------
%
% X and Y are integers.
%
% 1 < X < Y (note: X != Y)
%
% X + Y <= 100
%
% S = X + Y
%
% P = X * Y
%
% (a) P: I do not know the two numbers.
% (b) S: I knew you didn’t know. I don’t know either.
% (c) P: Now I know the two numbers.
% (d) S: Now I know the two numbers.
%


/*
%------------------------------------------------------------------------------

1. 1747 solutions remaining

s1 consists of all quadruples with non-unique P values

I formed s1 by removing all quadruples with unique P values

%------------------------------------------------------------------------------

2. 145 solutions remaining

I formed s2 by removing every quadruple from s0 (the original pool) which 
contains an S value which appears in the list of 'unique' product quadruples 
removed in s1

%------------------------------------------------------------------------------

3. 86 solutions remaining

s3 consists of the members of s2 which have a unique P value, as Mr.P now
knows X and Y

I formed s3 by selecting members of s2 which had a unique P value at this stage

%------------------------------------------------------------------------------

4. 1 solution remaining

s4 consists of the members of s3 which have a unique S value, as Mr.S now
knows X and Y

I formed s4 by selecting members of s3 which had a unique S value at this stage


%------------------------------------------------------------------------------
5. still only 1 solution with 500 sum limit

I had to optimise the program to cut the inferences to a low enough number to 
run the program without a stack overflow. Optimisations included removing 
duplicates from badsums before removing each bad sum during s2, and changing
from quicksort to mergesort.

*/




%------------------------------------------------------------------------------
%
% (1) - P: I do not know the two numbers
%
%------------------------------------------------------------------------------

% X and Y are not both primes, if they were Mr P would be able to guess them
%
%
%
% To complete s1, I need a list containing all solutions with non-unique P 
% values
%
% S cannot be lower than 7 because 6 is semiprime, 5 is prime, and 4 is both 
% semiprime and invalid due to X and Y having to be distinct and greater than 1

%------------------------------------------------------------------------------

%% add_to_i(-Answers:list, +I:integer, [B:Count])
add_to_i(Answers, I) :-

	% Ensure tuples are unique by starting after halfway
	% this also ensures a and b are not equal
	Start is (I // 2) + 1,
	add_to_i(Answers, I, Start).

% base case, minimum number should be 2
add_to_i([], I, Y) :- 
	Y is I - 1,
	!.

% recursive case
add_to_i([(X, Y, S, P) | Rest], I, Y) :-
	X is I - Y, % X + Y = I
	Count is Y + 1, % count + 1
	S is X + Y,
	P is X * Y,
	add_to_i(Rest, I, Count).

%------------------------------------------------------------------------------


%% concat_lists(+Input:list, +Append:list, -Output:list) - concatenates lists

% base case
concat_lists([], List, List).

% recursive case
concat_lists([Elem | List1], List2, [Elem | List3]) :-
	concat_lists(List1, List2, List3).

%------------------------------------------------------------------------------


%% s0(+I:integer, -Set:list, [Count:integer]) - 2350 solutions
%
% Aggregates all of the possible solutions from sum 5 to I

% error handling - the sum cannot be lower than 5
s0(_, I) :-
	I < 5,
	write('Minimum possible sum at this stage is 5'),
	!.
% start
s0(Set, I) :- 
	s0(Set, I, 7),
	length(Set, L),
	nl,
	write('s0: '),
	write(L),
	write(' solutions').

% base case - when I == Count then we are at our upper bound
s0(Set, I, I) :- add_to_i(Set, I), !.

% recursive
s0(Set, I, Count) :-
	add_to_i(Tuples, Count),
	concat_lists(Tuples, MoreTuples, Set),
	Next is Count + 1,
	s0(MoreTuples, I, Next).


%------------------------------------------------------------------------------


%% merge_sort(+Input:list, -Output:list, +Criteria:atom)

% merge sorts the input list according to sum or product
% 
% set criteria to s for sum or p for product
%
%
%

% base case - empty list sorts to empty list
merge_sort([], [], _). 

% base case - single item list sorts to itself
merge_sort([Item], [Item], _).

% recursive case - 2 or more item lists split and sort
merge_sort([First, Second | Rest], Output, Criteria) :-
  split([First, Second | Rest], Left, Right),
  merge_sort(Left, SortedLeft, Criteria),
  merge_sort(Right, SortedRight, Criteria),
  merge_lists(SortedLeft, SortedRight, Output, Criteria).


%------------------------------------------------------------------------------

%% split(+Input:list, -Left:list, -Right:list) 
%
% divides the elements of Input into two lists

% base case - empty list splits to empty list
split([], [], []).

% base case - one item splits into itself and empty
split([First], [First], []).

% Recursive case - splits lists with two or more items into two lists, then 
% 				   splits their tails
split([First, Second | Rest], [First | Left], [Second | Right]) :-
	split(Rest, Left, Right).

%------------------------------------------------------------------------------

%% merge_lists(+List1:list, +List2:list, -Merged:list, +Criteria)
%
% recursively sorts List1 and List2 according to Criteria

% base case - merging with the empty list does not change the input
merge_lists(Element, [], Element, _).
merge_lists([], Element, Element, _).

% recursive case - when first is smaller/equal to second, put it as the first
% element of the merged output
merge_lists([(X1,Y1,S1,P1) | Rest1], [(X2,Y2,S2,P2) | Rest2],
		 [(X1,Y1,S1,P1) | Merged], s):-
  S1 =< S2,
  !,
  merge_lists(Rest1, [(X2,Y2,S2,P2) | Rest2], Merged, s).

% recursive case - first is larger than second, put second as the first element
% of the merged output
merge_lists([(X1,Y1,S1,P1) | Rest1], [(X2,Y2,S2,P2) | Rest2], 
		 [(X2,Y2,S2,P2) | Merged], s) :-
  merge_lists([(X1,Y1,S1,P1) | Rest1], Rest2, Merged, s).

% recursive case - when first is smaller/equal to second, put it as the first
% element of the merged output
merge_lists([(X1,Y1,S1,P1) | Rest1], [(X2,Y2,S2,P2) | Rest2],
		 [(X1,Y1,S1,P1) | Merged], p):-
  P1 =< P2,
  !,
  merge_lists(Rest1, [(X2,Y2,S2,P2) | Rest2], Merged, p).

% recursive case - first is larger than second, put second as the first element
% of the merged output
merge_lists([(X1,Y1,S1,P1) | Rest1], [(X2,Y2,S2,P2) | Rest2], 
		 [(X2,Y2,S2,P2) | Merged], p) :-
  merge_lists([(X1,Y1,S1,P1) | Rest1], Rest2, Merged, p).


%------------------------------------------------------------------------------

%% select_non_unique_products(Input:list, Output:list, Removed:list)
%
% Starts a recursive separation of the input list into terms producing a unique
% number and those which do not.

% start - first 2 are dupes
select_non_unique_products([(X1,Y1,S1,P),(X2,Y2,S2,P)|Rest],
						  Output,
						  [(X1,Y1,S1,P)|Removed]) :-
	!,
	% The third soln in this case will be the middle one in the next
	% minimum 2 solns in next recursion
	remove_non_unique_products([(X1,Y1,S1,P),(X2,Y2,S2,P)|Rest],
							   Output,
							   Removed).

% start - first 2 are distinct
select_non_unique_products([(X1,Y1,S1,P1),(X2,Y2,S2,P2)|Rest],
						[(X1,Y1,S1,P1)|Output],
						Removed) :-
	!,
	% The third soln in this case will be the middle one in the next
	% minimum 2 solns in next recursion
	remove_non_unique_products([(X1,Y1,S1,P1),(X2,Y2,S2,P2)|Rest],
							   Output,
							   Removed).

%% remove_non_unique_products(+Input:list, -Output:list, -Removed:list)
%
% Removes solutions which do not produce a unique product
%
% Input must be a list of quadruples ordered in ascending values of P


% base case - last two products are non-uniques
remove_non_unique_products([(_,_,_,P),(X2,Y2,S2,P)|[]],
						  [],
						  [(X2,Y2,S2,P)]) :-
	!.

% base case - last two products are distinct
remove_non_unique_products([(_,_,_,_),(X2,Y2,S2,P2)|[]],
						  [(X2,Y2,S2,P2)],
						  []).


% recursive case - middle product is a non-unique with next soln
remove_non_unique_products([(_,_,_,_),(X2,Y2,S2,P2),(X3,Y3,S3,P3)|Rest],
						  Output,
						  [(X2,Y2,S2,P2)|Removed]) :-
	P2 == P3, !,

	% The third soln in this case will be the middle one in the next
	% minimum 2 solns in next recursion
	remove_non_unique_products([(X2,Y2,S2,P2),(X3,Y3,S3,P3)|Rest],
							   Output,
							   Removed).

% recursive case - middle product is a non-unique with prev soln
remove_non_unique_products([(_,_,_,P1),(X2,Y2,S2,P2),(X3,Y3,S3,P3)|Rest],
						  Output,
						  [(X2,Y2,S2,P2)|Removed]) :-
	P1 == P2, !,

	% The third soln in this case will be the middle one in the next
	% minimum 2 solns in next recursion
	remove_non_unique_products([(X2,Y2,S2,P2),(X3,Y3,S3,P3)|Rest],
							   Output,
							   Removed).

% recursive case - middle product is unique
remove_non_unique_products([(_,_,_,_),(X2,Y2,S2,P2),(X3,Y3,S3,P3)|Rest],
						  [(X2,Y2,S2,P2)|Output],
						  Removed) :-

	% The third soln in this case will be the middle one in the next
	% minimum 2 solns in next recursion
	remove_non_unique_products([(X2,Y2,S2,P2),(X3,Y3,S3,P3)|Rest],
							   Output,
							   Removed).


%------------------------------------------------------------------------------

%% s1(-Q:list, +I:integer)
%
% Mr P: "I do not know the two numbers" 
%
% Matches Q with all possible solutions after sentence 1 - solutions from S =
% 7 to S = 100 excluding those with (prime, prime, _, _)
%

s1(Q, I) :-
	time(s0(W, I)), % find all solutions which sum from 7 to 100
	time(merge_sort(W, E, p)), % sort them by their product
	select_non_unique_products(E, _, R), % select all non-unique products
	time(merge_sort(R, Q, s)), % sort these non-unique products by their sum
	%write(Sums),
	length(Q, L),
	nl,
	write('s1: '),
	write(L),
	write(' solutions'), % 1747 solutions
	!.


% ?- time(s1(Q,100)).
% % 136,609 inferences, 0.081 CPU in 0.088 seconds (92% CPU, 1688094 Lips)
% Q = [ (3, 4, 7, 12), (2, 6, 8, 12), (4, 5, 9, 20), (3, 6, 9, 18), (4, 6, 
% 10, 24), (3, 8, 11, 24), (4, 7, ..., ...), (2, ..., ...), (..., ...)|...].




%------------------------------------------------------------------------------
%
% (2) - S: I knew you didn’t know. I don’t know either.
%
%------------------------------------------------------------------------------

% "I knew you didn't know" suggests that S is a number that can only be
% produced by pairs containing at least one composite number, because if S
% could be produced by a pair of primes, then Mr.S would not have been sure
% Mr.P didn't know X and Y.
%
% S cannot be even, as according to Goldbach's conjecture, all even numbers can
% be expressed as a sum of two primes
%
% 
%
% "I don't know either" implies that there are multiple pairs of integers in
% obeying the rules which can make up S, this rules out 2 + 3 = 5 (although
% this would already have been ruled out because they are both primes) and 2 +
% 4 = 6.
%
% To complete s2 I need to be able to generate a list of solutions in the same
% range as in s1, however, for every S which contains a 'prime solution', I
% need to exclude all solutions containing that S



%------------------------------------------------------------------------------



%% listmember(+Item:atom, +List:list) - returns true if Item is in List

% base case - item matches head of list
listmember(Item,[Item|_]) :- !.

% recursive case - item does not match head but matches an item in the tail
listmember(Item,[_|Rest]) :- listmember(Item,Rest).


%------------------------------------------------------------------------------


%% remove_sums(+Solns:list of tuples, +Sums:list of tuples, -Output:list)
%
% Removes every solution adding up to each sum in Sums list, outputs in Output
%
% Solns should be in sum order
%
% e.g. remove_sums([(3,4,7,12), (2,5,7,10), (3,5,8,15), (2,6,8,12)], [7,8], [])
%

% base case - no input solns
remove_sums([], _, []) :- !.

% base case - bad sum
remove_sums([(_,_,S,_)|[]], Sums, []) :-

	% The first tuple has a bad sum if it is in list Sums
	listmember((_,_,S,_), Sums),
	!.

% base case - good sum
remove_sums([(X,Y,S,P)|[]], _, [(X,Y,S,P)]).

% recursive case - bad sum
remove_sums([(_,_,S,_)|MoreSolns], Sums, Output) :-
	% The first tuple has a bad sum if it is in list Sums
	listmember((_,_,S,_), Sums),
	!,

	% Remove any more tuples with this sum
	purge_dupes([(_,_,S,_)| MoreSolns], RestofSolns),
	% feed output back in for the other sums

	remove_sums(RestofSolns, Sums, Output). 

% recursive case - good sum
remove_sums([(X,Y,S,P)|MoreSolns], Sums, [(X,Y,S,P)|Output]) :-

	% feed output back in for the other sums
	remove_sums(MoreSolns, Sums, Output). 


%------------------------------------------------------------------------------


%% purge_dupes(+Input:list, -Output:list)
purge_dupes([(_,_,S,_),(_,_,S,_)|Rest], Output) :-
	purge_dupes([(_,_,S,_)|Rest], Output),
	!.
purge_dupes([_|Output], Output).


%------------------------------------------------------------------------------


%% remove_duplicate_sums(+Input:list, -Output:list)
%
% Returns the input list with only one of each sum remaining
%

remove_duplicate_sums([], []).

% base case - sums are duplicates
remove_duplicate_sums([(_,_,S,_),(_,_,S,_)|[]], []):-!.

% base case - last solution
remove_duplicate_sums([(X,Y,S,P)|[]], [(X,Y,S,P)]).

% recursive case - sums are duplicates, add first to output, remove rest
remove_duplicate_sums([(X,Y,S,P),(_,_,S,_)|Rest], [(X,Y,S,P)|Output]) :-
	!,
	purge_dupes([(_,_,S,_)|Rest], Purged),
	remove_duplicate_sums(Purged, Output).

% recursive case - sum is unique
remove_duplicate_sums([(X1,Y1,S1,P1),(X2,Y2,S2,P2)|Rest],
	                      [(X1,Y1,S1,P1)|Output]) :-
	remove_duplicate_sums([(X2,Y2,S2,P2)|Rest], Output).


%------------------------------------------------------------------------------


%% s2(-Q:list, +I:integer)

% To complete s2 I need to be able to generate a list of solutions in the same
% range as in s1, however, for every S which contains a 'prime solution', I
% need to exclude all solutions containing that S

% NOTE: Sum <= 55 is now impossible because every number above 55 inclusive can
% be broken into X + 53, which will always yield a unique product within our 
% constraints, seeing as Mr.S knew Mr.P did nor know, this is proved.

% ANOTHER NOTE: whilst we only care about sum <= 55 , entering 55 as the I 
% value restricts some solutions, messing up the program - 35 and 37 are
% treated as bad sums because they have X and Y values which create a unique
% product with this restriction - to solve this, use 100 for the stack to i 
% part and then for the rest of s2 put the 55 cap on it

s2(Q, I) :-
	time(s0(S0s, I)),
	time(merge_sort(S0s, S0p, p)), % sort them by their product

	% separate unique and non-unique products
	time(select_non_unique_products(S0p, Uniques, Dupes)),

	% sort separated lists by sum
	time(merge_sort(Uniques, Badsums, s)),
	time(merge_sort(Dupes, SortedDupes, s)),

	% duplicate badsums slow down the program
	time(remove_duplicate_sums(Badsums, PureBadsums)),

	% remove any sum associated with a unique product
	time(remove_sums(SortedDupes, PureBadsums, Nomorebadsums)),

	% sort the solutions pool by product
	time(merge_sort(Nomorebadsums, Q, p)),
	length(Q, L),
	nl,
	write('s2: '),
	write(L),
	write(' solutions'),!.
	
% ?- time(s2(Q,100)).
% 145 solutions
% 173,335 inferences, 0.115 CPU in 0.123 seconds (94% CPU, 1500892 Lips)
% Q = [ (2, 9, 11, 18), (3, 8, 11, 24), (4, 7, 11, 28), (2, 15, 17, 30), (5, 6,
% 11, 30), (3, 14, 17, 42), (2, 21, ..., ...), (2, ..., ...), (..., ...)|...].






%------------------------------------------------------------------------------
%
% (3) - P: Now I know the two numbers.
%
%------------------------------------------------------------------------------

% The fact that Mr.S knew Mr.P didn't know X and Y initially means that all
% solutions to S produce non-semiprime numbers, this narrowed it down
% sufficiently leaving Mr.P with 1 solution.
%
% To complete s3 I need to filter the results from s2 to show all solutions 
% which now produce a unique P values
%
% That is to say P must be produced by various X and Y integer values, 
% however, only one set consists of two numbers that do not add up to a sum of 
% two distinct primes
% e.g. if P = 52, (4 x 13) and (2 x 26) are both possible solutions after step
% 1, but after step two, (2 x 26) has been removed leaving one solution for Mr.
% P.


%------------------------------------------------------------------------------


s3(Q, I) :- % I = 55 - this causes bugs and i dont know why
	
	time(s2(S2p, I)),

	% Remove all products which cannot be uniquely made from one soln left in
	% the pool
	select_non_unique_products(S2p, S3p, _),

	% Sort these solutions by sum
	merge_sort(S3p, Q,s),

	length(Q, L),
	nl,
	write('s3: '),
	write(L),
	write(' solutions'),
	!.

% ?- time(s3(Q, 100)).
% 173,335 inferences, 0.115 CPU in 0.123 seconds (94% CPU, 1500892 Lips)
% Q = [ (2, 9, 11, 18), (3, 8, 11, 24), (4, 7, 11, 28), (2, 15, 17, 30), (5, 6,
% 11, 30), (3, 14, 17, 42), (2, 21, ..., ...), (2, ..., ...), (..., ...)|...].





%------------------------------------------------------------------------------
%
% (4) - S: Now I know the two numbers
%
%------------------------------------------------------------------------------

% Out of the set of solutions with add to S, only one could uniquely produce 
% P
%
% e.g. (2,15),(3,14),(4,13),(5,12),(6,11),(7,10),(8,9) can all add to 17, but
% only (4,13) produce a number which has all but one solution currently ruled
% out, and therefore this must be the solution which Mr. P has just concluded
% is correct
%
%



%% select_non_unique_sums(Input:list, Output:list, Removed:list)
%
% Starts a recursive separation of the input list into terms producing a unique
% number and those which do not.

% start - first 2 are dupes
select_non_unique_sums([(X1,Y1,S,P1),(X2,Y2,S,P2)|Rest],
						  Output,
						  [(X1,Y1,S,P1)|Removed]) :-
	!,
	% The third soln in this case will be the middle one in the next
	% minimum 2 solns in next recursion
	remove_non_unique_sums([(X1,Y1,S,P1),(X2,Y2,S,P2)|Rest],
							   Output,
							   Removed).

% start - first 2 are distinct
select_non_unique_sums([(X1,Y1,S1,P1),(X2,Y2,S2,P2)|Rest],
						[(X1,Y1,S1,P1)|Output],
						Removed) :-
	!,
	% The third soln in this case will be the middle one in the next
	% minimum 2 solns in next recursion
	remove_non_unique_sums([(X1,Y1,S1,P1),(X2,Y2,S2,P2)|Rest],
							   Output,
							   Removed).

%% remove_non_unique_sums(+Input:list, -Output:list, -Removed:list)
%
% Removes solutions which do not produce a unique sum
%
% Input must be a list of quadruples ordered in ascending values of P


% base case - last two sums are non-uniques
remove_non_unique_sums([(_,_,S,_),(X2,Y2,S,P2)|[]],
						  [],
						  [(X2,Y2,S,P2)]) :-
	!.

%FINE
% base case - last two sums are distinct
remove_non_unique_sums([(_,_,_,_),(X2,Y2,S2,P2)|[]],
						  [(X2,Y2,S2,P2)],
						  []).


% recursive case - middle sum is a non-unique with next soln
remove_non_unique_sums([(_,_,_,_),(X2,Y2,S2,P2),(X3,Y3,S3,P3)|Rest],
						  Output,
						  [(X2,Y2,S2,P2)|Removed]) :-
	S2 == S3, !,

	% The third soln in this case will be the middle one in the next
	% minimum 2 solns in next recursion
	remove_non_unique_sums([(X2,Y2,S2,P2),(X3,Y3,S3,P3)|Rest],
							   Output,
							   Removed).

% recursive case - middle sum is a non-unique with prev soln
remove_non_unique_sums([(_,_,S1,_),(X2,Y2,S2,P2),(X3,Y3,S3,P3)|Rest],
						  Output,
						  [(X2,Y2,S2,P2)|Removed]) :-
	S1 == S2, !,

	% The third soln in this case will be the middle one in the next
	% minimum 2 solns in next recursion
	remove_non_unique_sums([(X2,Y2,S2,P2),(X3,Y3,S3,P3)|Rest],
							   Output,
							   Removed).

% recursive case - middle sum is unique
remove_non_unique_sums([(_,_,_,_),(X2,Y2,S2,P2),(X3,Y3,S3,P3)|Rest],
						  [(X2,Y2,S2,P2)|Output],
						  Removed) :-

	% The third soln in this case will be the middle one in the next
	% minimum 2 solns in next recursion
	remove_non_unique_sums([(X2,Y2,S2,P2),(X3,Y3,S3,P3)|Rest],
							   Output,
							   Removed).


%------------------------------------------------------------------------------

s4(Q, I) :-
	time(s3(S3s, I)),
	% Remove all non-unique sums
	select_non_unique_sums(S3s, Q,_),

	length(Q, L),
	nl,
	write('s4: '),
	write(L),
	write(' solutions'),
	!.

% ?- time(s4(Q, 100)).
% s4: 1 solutions
% 175,268 inferences, 0.093 CPU in 0.099 seconds (94% CPU, 1875768 Lips)
% Q = [ (4, 13, 17, 52)].

% ?-time(s4(Q, 500)).
% s4: 1 solutions
% 7,432,365 inferences, 5.873 CPU in 6.252 seconds (94% CPU, 1265529 Lips)
% Q = [ (4, 13, 17, 52)].


%------------------------------------------------------------------------------
%
% Results
%
%------------------------------------------------------------------------------


% ?- [ajr36].
% ajr36 compiled 0.00 sec, 1 clauses
% true.

% ?- time(s1(Q,100)).
% % 136,609 inferences, 0.081 CPU in 0.088 seconds (92% CPU, 1688094 Lips)
% Q = [ (3, 4, 7, 12), (2, 6, 8, 12), (4, 5, 9, 20), (3, 6, 9, 18), (4, 6, 
% 10, 24), (3, 8, 11, 24), (4, 7, ..., ...), (2, ..., ...), (..., ...)|...].

% ?- time(s2(Q,100)).
% 145 solutions
% 173,335 inferences, 0.115 CPU in 0.123 seconds (94% CPU, 1500892 Lips)
% Q = [ (2, 9, 11, 18), (3, 8, 11, 24), (4, 7, 11, 28), (2, 15, 17, 30), (5, 6,
% 11, 30), (3, 14, 17, 42), (2, 21, ..., ...), (2, ..., ...), (..., ...)|...].

% ?- time(s3(Q, 100)).
% 173,335 inferences, 0.115 CPU in 0.123 seconds (94% CPU, 1500892 Lips)
% Q = [ (2, 9, 11, 18), (3, 8, 11, 24), (4, 7, 11, 28), (2, 15, 17, 30), (5, 6,
% 11, 30), (3, 14, 17, 42), (2, 21, ..., ...), (2, ..., ...), (..., ...)|...].

% ?- time(s4(Q, 100)).
% s4: 1 solutions
% 175,268 inferences, 0.093 CPU in 0.099 seconds (94% CPU, 1875768 Lips)
% Q = [ (4, 13, 17, 52)].

% ?-time(s4(Q, 500)).
% s4: 1 solutions
% 7,432,365 inferences, 5.873 CPU in 6.252 seconds (94% CPU, 1265529 Lips)
% Q = [ (4, 13, 17, 52)].















