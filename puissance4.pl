:-dynamic(board/1).
 board([['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-']]).

:-dynamic(michto/2).
michto('jeanne', 'frank').
 michto('xxxxjeanne', 'frank').
 michto('jeanneyyyyy', 'frankiii').


display:-write(' 1 2 3 4 5 6 7'), nl, draw_table. 
draw_table:-get_position(0).

% Iterate over the number of rows to draw each column
get_position(6):-nl.
get_position(N):-draw_column_pos(N,0),NS is  N+1, get_position(NS).

% Draw the pos of each column 
draw_column_pos(N,7):-nl. 
draw_column_pos(N,C):-board(X),nth0(C, X, R), nth0(N,R, R1), write(' '), write(R1), CS is C+1, draw_column_pos(N,CS).
start:-display,play('X').

chooseMove('X', N):- \+cannotPlay(N).
chooseMove('X',N):-cannotPlay(N), write('cannotplay').

askColumn(N):-write(J), write( ':Dans quelle colonne'),read(N).

cannotPlay(N):- \+integer(N);N>7;N<1;Ns is N-1, \+ get_free_index_column(Ns,6,'s',INDEX_LIBRE).


play('X'):-write('X: Dans quelle colonne'), read(N), Ns is N-1, make_move(Ns,'X'), play('O').
play('O'):-write('O: Dans quelle colonne'), read(N), Ns is N-1, make_move(Ns, 'O'), play('X').


playIa:-random_between(0,6,R)


% Insert j in nth column with current board b resulting in r
make_move(N_COL, J):-board(B),nth0(N_COL, B, COL),get_free_index_column(COL,6,'s',INDEX_LIBRE),replace(COL,INDEX_LIBRE, J,COL_RES),replace(B,N_COL, COL_RES, R),retract(board(_)),assert(board(R)),display.


% replacing element at index I in L 
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, I1 is I-1, replace(T, I1, X, R).

% finding first index of list which is '-' 
get_free_index_column(COL, INDEX, '-' , INDEX).
get_free_index_column(COL, INDEX, VALEUR , INDEX_LIBRE):-INDEX1 is INDEX -1, nth0(INDEX1, COL, VALEUR1), get_free_index_column(COL,INDEX1,VALEUR1,INDEX_LIBRE).  

reset:-retract(board(_)), assert(board([['-','-','-','-','-','-'],
            ['-','-','-','-','-','-'],
            ['-','-','-','-','-','-'],
            ['-','-','-','-','-','-'],
            ['-','-','-','-','-','-'],
            ['-','-','-','-','-','-'],
            ['-','-','-','-','-','-']])).
