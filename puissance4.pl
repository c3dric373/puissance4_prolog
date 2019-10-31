:-dynamic(board/1).
 board([['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-']]).

display:-write(' 1 2 3 4 5 6 7'), nl, draw_table.
draw_table:-get_position(0).

% Iterate over the number of rows to draw each column
get_position(6):-nl.
get_position(N):-draw_column_pos(N,0),NS is  N+1, get_position(NS).

% Draw the pos of each column
draw_column_pos(_,7):-nl.
draw_column_pos(N,C):-board(X),nth0(C, X, R), nth0(N,R, R1), write(' '), write(R1), CS is C+1, draw_column_pos(N,CS).
start:-reset,display,play('X').

% Ici on va voir si le coup est possible, s'il ne l'est pas on redemande
chooseMove('X', N, N_ok):- \+cannotPlay(N), N_ok is N.
chooseMove('X', N, N_ok):- cannotPlay(N),call_column('X', N_ok).
chooseMove('O', N, N_ok):- \+cannotPlay(N), N_ok is N.
chooseMove('O', N, N_ok):- cannotPlay(N),call_column('O', N_ok).

cannotPlay(N):- \+integer(N).
cannotPlay(N):- N>7.
cannotPlay(N):- N<1.
cannotPlay(N):- Ns is N-1, board(B),nth0(Ns, B, COL), \+get_free_index_column(COL,6,'s',_).

call_column('X', N_ok) :- write('X: Dans quelle colonne : '), read(N), chooseMove('X', N, N_ok).
call_column('O', N_ok) :- write('O: Dans quelle colonne : '), read(N), chooseMove('O', N, N_ok).
%fin du choix de la colonne jouer

play('X'):-call_column('X', N_ok), Ns is N_ok-1, make_move(Ns,'X',_), play('O').
play('O'):-call_column('O', N_ok), Ns is N_ok-1, make_move(Ns,'O',_), play('X').

% Insert j in nth column with current board b resulting in r
make_move(N_COL, J,INDEX_LIBRE):-board(B),nth0(N_COL, B, COL),get_free_index_column(COL,6,'s',INDEX_LIBRE),replace(COL,INDEX_LIBRE, J,COL_RES),replace(B,N_COL, COL_RES, R),retract(board(_)),assert(board(R)),display.


% replacing element at index I in L
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, I1 is I-1, replace(T, I1, X, R).

% finding first index of list which is '-'
get_free_index_column(_, INDEX, '-' , INDEX):-!.
get_free_index_column(COL, INDEX, _, INDEX_LIBRE):-INDEX1 is INDEX -1, nth0(INDEX1, COL, VALEUR1), get_free_index_column(COL,INDEX1,VALEUR1,INDEX_LIBRE).

% Get the number of aligned pieces if we play at the provided column, then come back to the original board
get_nb_aligned_pieces(NB_COL,Player, Nb_pieces_aligned):- board(Current_board), make_move(NB_COL,Player,INDEX_LIBRE),count_vertical_pieces(Player,NB_COL,INDEX_LIBRE,1, Nb_pieces_vertical),count_horizontal_pieces(Player,NB_COL, INDEX_LIBRE,Nb_pieces_horizontal),max(Nb_pieces_vertical,Nb_pieces_horizontal,Nb_pieces_aligned),retract(board(_)),assert(board(Current_board)).

% Count the number of similar pieces that are under the last piece that we have put in the column
	count_vertical_pieces(Player,_,_,4, _). % win

	% Stop when it is the end of the list
	count_vertical_pieces(Player,_,5,Last_count, Last_count):-!. 

	count_vertical_pieces(Player,NB_COL,INDEX_LIBRE,Last_count, Real_Count):- board(B), nth0(NB_COL, B, COL),Next_piece is INDEX_LIBRE+1, nth0(Next_piece,COL,Player), New_Count is Last_count+1,count_vertical_pieces(Player,NB_COL,Next_piece,New_Count, Real_Count).

	% Stop when the next piece is different from the current piece
	count_vertical_pieces(Player,NB_COL,INDEX_LIBRE,Last_count, Real_Count):- board(B), nth0(NB_COL, B, COL),Next_piece is INDEX_LIBRE+1, \+nth0(Next_piece,COL,Player), Real_Count is Last_count.

% get a Line
	getLine(NB_Line, Line,7,Line):-!.
	getLine(NB_Line, Line,Index,Total_line):-board(B), nth0(Index, B, COL), nth0(NB_Line, COL, Element),append(Line,[Element],New_line), Index1 is Index+1,getLine(NB_Line, New_line,Index1,Total_line).

% count the number of horizontal aligned pieces when we put a piece in the selected column
% NB_Line correspond to the INDEX_Libre (index in the column where we have put the piece)
	count_horizontal_pieces(Player,NB_COL, NB_Line,Count):- count_left_horizontal_pieces(Player,NB_COL,NB_Line,1, Count_left),count_right_horizontal_pieces(Player,NB_COL,NB_Line,0, Count_right), Count is Count_left+Count_right.

	% Count the number of similar pieces at the left of the last added piece
	count_left_horizontal_pieces(Player,0,_,Last_count, Last_count):-!.
	% get the line of the last added piece and compare it to the piece next to it 
	count_left_horizontal_pieces(Player,NB_COL,NB_Line,Last_count, Real_Count):-getLine(NB_Line,[],0,Line), Next_piece is NB_COL-1, nth0(Next_piece,Line,Player),New_Count is Last_count+1,count_left_horizontal_pieces(Player,Next_piece,NB_Line,New_Count, Real_Count).
	count_left_horizontal_pieces(Player,NB_COL,NB_Line,Last_count, Real_Count):-getLine(NB_Line,[],0,Line), Next_piece is NB_COL-1, \+nth0(Next_piece,Line,Player),Real_Count is Last_count.

	% Count the number of similar pieces at the right of the last added piece
	count_right_horizontal_pieces(Player,6,NB_Line,Last_count, Last_count):-!.
	% get the line of the last added piece and compare it to the piece next to it 
	count_right_horizontal_pieces(Player,NB_COL,NB_Line,Last_count, Real_Count):-getLine(NB_Line,[],0,Line), Next_piece is NB_COL+1, nth0(Next_piece,Line,Player),New_Count is Last_count+1,count_right_horizontal_pieces(Player,Next_piece,NB_Line,New_Count, Real_Count).
	count_right_horizontal_pieces(Player,NB_COL,NB_Line,Last_count, Real_Count):-getLine(NB_Line,[],0,Line), Next_piece is NB_COL+1, \+nth0(Next_piece,Line,Player),Real_Count is Last_count.

% Get the max
max(X,Y,Max):- X>=Y, Max is X.
max(X,Y,Max):- X<Y, Max is Y.
 


reset:-retract(board(_)), assert(board([['-','-','-','-','-','-'],
            ['-','-','-','-','-','-'],
            ['-','-','-','-','-','-'],
            ['-','-','-','-','-','-'],
            ['-','-','-','-','-','-'],
            ['-','-','-','-','-','-'],
            ['-','-','-','-','-','-']])).
