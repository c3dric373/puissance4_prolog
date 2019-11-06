

% replacing element at index I in L
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, I1 is I-1,replace(T, I1, X, R).


% Get the max
max(X,Y,Max):- X>=Y, Max is X.
max(X,Y,Max):- X<Y, Max is Y.

min(X, Y, Min):-X>=Y, Min =Y.
min(X,Y,Min):-X< Y, Min=X.

% Given a List it returns S the sublist L from M to N
sublist([],0,0,_).
sublist(S,M,N,[_A|B]):- M>0, M<N, sublist(S,M-1,N-1,B).
sublist(S,M,N,[A|B]):- 0 is M, M<N, 
N2 is N-1, S=[A|D],sublist(D,0,N2,B).

