% ********** UTILITIES **********
greater(s(_), zero).
greater(s(N), s(M)) :- greater(N, M).



% ********** PART 1 **********

% search (Elem , List )
search (X , cons (X , _)).
search (X , cons (_ , Xs )) :- search (X , Xs ).


% search2 (Elem , List )
% looks for two consecutive occurrences of Elem
search2 (X , cons (X , cons (X , _)) ).
search2 (X , cons (_ , Xs ) ) :- search2 (X , Xs ) .


% search_two (Elem , List )
% looks for two occurrences of Elem with any element in between !
search_two(X, cons(X, cons(_, cons(X, _)))).
search_two(X, cons(_, Xs)) :- search_two(X, Xs).


% search_anytwo (Elem , List )
% looks for any Elem that occurs two times , anywhere
search_anytwo(X, cons(X, Xs)) :- search(X, Xs).
search_anytwo(X, cons(_, Xs)) :- search_anytwo(X, Xs).



% ********** PART 2 **********

%%% size (List , Size )
% Size will contain the number of elements in List , written using notation zero , s( zero ), s(s( zero ))..
size(nil, zero).
size(cons(_, T), s(S)) :- size(T, S).

%max(List , Max)
% Max is the biggest element in List
% Suppose the list has at least one element
max(nil, TempMax, TempMax).
max(cons(H, T), H, Max) :- max(T, H, Max).
max(cons(H, T), TempMax, Max) :- greater(TempMax, H), max(T, TempMax, Max).
max(cons(H, T), TempMax, Max) :- greater(H, TempMax), max(T, H, Max).

max(cons(H, T), Max) :- max(T, H, Max).


% min-max(List,Min,Max)
% Min is the smallest element in List
% Max is the biggest element in List
% Suppose the list has at least one element
min(nil, TempMin, TempMin).
min(cons(H, T), H, Min) :- min(T, H, Min).
min(cons(H, T), TempMin, Min) :- greater(TempMin, H), min(T, H, Min).
min(cons(H, T), TempMin, Min) :- greater(H, TempMin), min(T, TempMin, Min).

min(cons(H, T), Min) :- min(T, H, Min).

min-max(L, Min, Max) :- min(L, Min), max(L, Max).



% ********** PART 3 **********

% same(List1 ,List2)
% are the two lists exactly the same?
same(nil, nil).
same(cons(H, T), cons(H, T)).


% all_bigger(List1,List2)
% all elements in List1 are bigger than those in List2, 1 by 1
all_bigger(cons(H1, nil), cons(H2, nil)) :- greater(H1, H2).
all_bigger(cons(H1, T1), cons(H2, T2)) :- greater(H1, H2), all_bigger(T1, T2).


% sublist(List1 ,List2)
% List1 should contain elements all also in List2
sublist(nil, _).
sublist(cons(H1, T1), L) :- search(H1, L), sublist(T1, L).

% MY VERSION OF SUBLIST (I think that a sublist should preserve 
% order and not be interleaved with other elements).
% sublist_started represents a sublist where matching of at
% least one element is already started
%sublist_started(nil, _).
%sublist_started(cons(H, T1), cons(H, T2)) :- sublist_started(T1, T2).
%sublist(cons(H1, T1), cons(H2, T2)) :- sublist(cons(H1, T1), T2).
%sublist(cons(H1, T1), cons(H1, T2)) :- sublist_started(T1, T2).



% ********** PART 4 **********

% seq(N,E,List) --> List is [E,E,...,E] with size N
% example: seq(s(s(s(zero))), a, cons(a,cons(a,cons(a, nil)))).
seq(zero, _, nil).
seq(s(N), E, cons(E,T)) :- seq(N, E, T).


% seqR(N,List)
seqR(zero, cons(zero, nil)).
seqR(s(N), cons(s(N), L)) :- seqR(N, L).


% seqR2(N,List) --> is [0,1,...,N-1]
last(nil, E, cons(E, nil)).
last(cons(H, T), E, cons(H, L)) :- last(T, E, L).

seqR2(zero, Acc, Acc).
seqR2(s(N), Acc, L) :- seqR2(N, Acc, R), last(R, N, L).
seqR2(s(N), L) :- seqR2(s(N), nil, L).
seqR2(zero, nil).

% MY VERSION without using last
%seqR2(zero, Acc, Acc).
%seqR2(s(N), Acc, L) :- seqR2(N, cons(N, Acc), L).
%seqR2(s(N), L) :- seqR2(s(N), nil, L).
%seqR2(zero, nil).



% ********** PART 5 **********

% last(List, E) where E is the last element of List (in a linked list that's the first element that was inserted)
% Usage: last(cons(a, cons(b, nil)), b).		(Works the same with peano numbers)
last(cons(E, nil), E).
last(cons(H, T), E) :- last(T, E).


% mapPlusOne(List, MappedList) where MappedList is List where each element was incremented by 1
% Usage: mapPlusOne(cons(zero, cons(s(zero), nil)), cons(s(zero), cons(s(s(zero)), nil)))
mapPlusOne(nil, nil).
% QUESTION: Which one of these two ways is better?
% 1. simpler but allows for non-peano numbers
mapPlusOne(cons(N, T), cons(s(N), L)) :- mapPlusOne(T, L).
% 2. complex but allows only peano numbers
mapPlusOne(cons(zero, T), cons(s(zero), L)) :- mapPlusOne(T, L).
mapPlusOne(cons(s(N), T), cons(s(s(N)), L)) :- mapPlusOne(T, L).


% filterPositive(List, FilteredList) where FilteredList is List withou non-positive elements
% Usage: filterPositive(cons(zero, nil), nil)
% Usage: filterPositive(cons(s(zero), nil), cons(s(zero), nil))
% Usage: filterPositive(cons(zero, cons(s(zero), nil)), cons(s(zero), nil))
filterPositive(nil, nil).
filterPositive(cons(zero, T), FL) :- filterPositive(T, FL).
filterPositive(cons(s(N), T), cons(s(N), FL)) :- filterPositive(T, FL).


% countPositive(List, Count) where Count is the number of positive elements in List
% Usage: countPositive(cons(zero, nil), zero)
% Usage: countPositive(cons(s(zero), nil), s(zero))
% Usage: countPositive(cons(s(zero), cons(zero, nil)), s(zero))
% Usage: countPositive(cons(s(zero), cons(s(zero), nil)), s(s(zero)))
countPositive(nil, zero).
countPositive(cons(zero, T), C) :- countPositive(T, C).
countPositive(cons(s(N), T), s(C)) :- countPositive(T, C). 


% findPositive(List, E) where E is the first positive element found in List
% Usage: findPositive(cons(s(zero), nil), s(zero))
% Usage: findPositive(cons(zero, cons(s(zero), cons(s(s(zero)), nil))), s(zero))
findPositive(cons(s(N), T), s(N)).
findPositive(cons(zero, T), E) :- findPositive(T, E).


% dropRight(List, N, ListOut) where ListOut is List without the last N elements
% Usage: dropRight(cons(a, cons(b, cons(c, nil))), s(zero), cons(a, cons(b, nil)))
% Usage: dropRight(cons(a, cons(b, cons(c, nil))), s(s(zero)), cons(a, nil))
% Usage: dropRight(cons(a, cons(b, cons(c, nil))), zero, cons(a, cons(b, cons(c, nil))))
dropRightOne(cons(_, nil), nil).
dropRightOne(cons(E, T), cons(E, L)) :- dropRightOne(T, L).

dropRight(L, zero, L).
dropRight(L, s(N), LO) :- dropRightOne(L, D), dropRight(D, N, LO).


% dropWhilePositive(List, DroppedList) where DroppedList is List without the longest prefix of positive elements
% Usage: dropWhilePositive(cons(s(zero), nil), nil)
% Usage: dropWhilePositive(cons(zero, nil), cons(zero, nil))
% Usage: dropWhilePositive(cons(zero, cons(s(zero), nil)), cons(zero, cons(s(zero), nil)))
% Usage: dropWhilePositive(cons(zero, cons(s(zero), cons(s(s(zero)), nil))), cons(zero, cons(s(zero), cons(s(s(zero)), nil))))
% Usage: dropWhilePositive(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), cons(zero, cons(s(s(zero)), nil)))
dropWhilePositive(nil, nil).
dropWhilePositive(cons(zero, T), cons(zero, T)).
dropWhilePositive(cons(s(N), T), L) :- dropWhilePositive(T, L).


% partitionPositive(List, Partition1, Partition2) where Partition1 contains all positive elemtns of List and Partition2 all others
% Usage: partitionPositive(nil, nil, nil)
% Usage: partitionPositive(cons(zero, nil), nil, cons(zero, nil))
% Usage: partitionPositive(cons(s(zero), nil), cons(s(zero), nil), nil)
% Usage: partitionPositive(cons(s(zero), cons(zero, cons(s(s(zero)), cons(zero, nil)))), cons(s(zero), cons(s(s(zero)), nil)), cons(zero, cons(zero, nil)))
partitionPositive(nil, nil, nil).
partitionPositive(cons(zero, T), P1, cons(zero, P2)) :- partitionPositive(T, P1, P2).
partitionPositive(cons(s(N), T), cons(s(N), P1), P2) :- partitionPositive(T, P1, P2).


% reversed(List, ReversedList) where ReversedList is List with reversed order of elements
% Usage: reversed(nil, nil)
% Usage: reversed(cons(a, cons(b, nil)), cons(b, cons(a, nil)))
reversed(nil, nil).
reversed(cons(H, T), RL) :- reversed(T, R), last(R, H, RL).

% drop(List, N, DroppedList) where DroppedList is List without the first N elements
% Usage: drop(nil, zero, nil)
% Usage: drop(cons(a, nil), zero, cons(a, nil))
% Usage: drop(cons(a, cons(b, nil)), s(zero), cons(b, nil))
drop(L, zero, L).
drop(cons(H, T), s(N), DL) :- drop(T, N, DL).

% take(List, N, TakenList) where TakenList is a list of the first N elements of List
% Usage: take(cons(a, nil), zero, nil)
% Usage: take(cons(a, cons(b, cons(c, nil))), s(s(zero)), cons(a, cons(b, nil)))
take(_, zero, nil).
take(cons(H, T), s(N), cons(H, L)) :- take(T, N, L).

% zip(List, List2, ZippedLists) where ZippedLists is a list where each element is composed of the two elements at the same index of List1 and List2
zip(_, nil, nil).
zip(nil, _, nil).
zip(cons(H1, T1), cons(H2, T2), cons((H1, H2), ZL)) :- zip(T1, T2, ZL).