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
start:-reset,display,play('O').

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

play('X'):-call_column('X', N_ok), Ns is N_ok-1, make_move(Ns,'X',Index),get_nb_aligned_pieces(Ns,'X',Index, Nb_pieces_aligned),check_victory('X',Nb_pieces_aligned).
play('O'):-call_column('O', N_ok), Ns is N_ok-1, make_move(Ns,'O',Index),get_nb_aligned_pieces(Ns,'O',Index, Nb_pieces_aligned),check_victory('O',Nb_pieces_aligned).

check_victory('O',Nb_pieces_aligned):-Nb_pieces_aligned>=4,display, write('you win'),reset.
check_victory('O',Nb_pieces_aligned):-Nb_pieces_aligned<4,display, playIA_heur1('X').
check_victory('X',Nb_pieces_aligned):-Nb_pieces_aligned>=4,display, write('IA win'), reset.
check_victory('X',Nb_pieces_aligned):-Nb_pieces_aligned<4,display,play('O').

% Insert j in nth column with current board b resulting in r
make_move(N_COL, J,INDEX_LIBRE):-board(B),nth0(N_COL, B, COL),get_free_index_column(COL,6,'s',INDEX_LIBRE),replace(COL,INDEX_LIBRE, J,COL_RES),replace(B,N_COL, COL_RES, R),retract(board(_)),assert(board(R)).%,display.


% replacing element at index I in L
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, I1 is I-1, replace(T, I1, X, R).

% finding first index of list which is '-'
get_free_index_column(_, INDEX, '-' , INDEX):-!.
get_free_index_column(COL, INDEX, _, INDEX_LIBRE):-INDEX1 is INDEX -1, nth0(INDEX1, COL, VALEUR1), get_free_index_column(COL,INDEX1,VALEUR1,INDEX_LIBRE).

% get a Line (Needed for the pieces' counting) 
	getLine(NB_Line, Line,7,Line):-!.
	getLine(NB_Line, Line,Index,Total_line):-board(B), nth0(Index, B, COL), nth0(NB_Line, COL, Element),append(Line,[Element],New_line), Index1 is Index+1,getLine(NB_Line, New_line,Index1,Total_line).

% Get the max
	max(X,Y,Max):- X>=Y, Max is X.
	max(X,Y,Max):- X<Y, Max is Y.

%---------------------------------------------------------------------
% COUNT THE NUMBER OF ALIGNED PIECE IF WE PLAY AT THE SELECTED COLUMN
%---------------------------------------------------------------------

% Get the number of aligned pieces if we play at the provided column
get_nb_aligned_pieces(NB_COL,Player,INDEX_LIBRE, Nb_pieces_aligned):-count_vertical_pieces(Player,NB_COL,INDEX_LIBRE,1, Nb_pieces_vertical),count_horizontal_pieces(Player,NB_COL, INDEX_LIBRE,Nb_pieces_horizontal),count_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,Nb_pieces_diagonal),max(Nb_pieces_vertical,Nb_pieces_horizontal,Max_vert_hor),max(Max_vert_hor,Nb_pieces_diagonal,Nb_pieces_aligned).

% Count the number of similar pieces that are under the last piece that we have put in the column (vertical count)

	% Stop when it is the end of the list
	count_vertical_pieces(Player,_,5,Last_count, Last_count):-!. 

	count_vertical_pieces(Player,NB_COL,INDEX_LIBRE,Last_count, Real_Count):- board(B), nth0(NB_COL, B, COL),Next_piece is INDEX_LIBRE+1, nth0(Next_piece,COL,Player), New_Count is Last_count+1,count_vertical_pieces(Player,NB_COL,Next_piece,New_Count, Real_Count).

	% Stop when the next piece is different from the current piece
	count_vertical_pieces(Player,NB_COL,INDEX_LIBRE,Last_count, Real_Count):- board(B), nth0(NB_COL, B, COL),Next_piece is INDEX_LIBRE+1, \+nth0(Next_piece,COL,Player), Real_Count is Last_count.

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

% count the number of diagonal aligned pieces when we put a piece in the selected column
% INDEX_LIBRE is the index where we have put the piece
% Get the max of the right-aligned-diagonal, and the left-aligned-diagonal
count_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,Count):- count_right_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,Right_count), count_left_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,Left_count), max(Right_count,Left_count,Count).

	count_right_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,Count):- count_bottom_left_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,1, Left_Count),count_top_right_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,0, Right_Count), Count is Left_Count+Right_Count.
		% Count the number of piece on the right diagonal under the last put piece
		count_bottom_left_diagonal_pieces(Player,0,_,Last_count, Last_count):-!.
		count_bottom_left_diagonal_pieces(Player,_,5,Last_count, Last_count):-!.
		count_bottom_left_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,Last_count, Real_Count):-board(B), New_NB_COL is NB_COL-1,nth0(New_NB_COL, B, COL), Next_piece is INDEX_LIBRE+1,nth0(Next_piece,COL,Player), New_Count is Last_count+1,count_bottom_left_diagonal_pieces(Player,New_NB_COL,Next_piece,New_Count, Real_Count).
		count_bottom_left_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,Last_count, Real_Count):-board(B), New_NB_COL is NB_COL-1,nth0(New_NB_COL, B, COL), Next_piece is INDEX_LIBRE+1,\+nth0(Next_piece,COL,Player),Real_Count is Last_count.

		% Count the number of piece on the right diagonal above the last put piece
		count_top_right_diagonal_pieces(Player,6,_,Last_count, Last_count):-!.
		count_top_right_diagonal_pieces(Player,_,0,Last_count, Last_count):-!.
		count_top_right_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,Last_count, Real_Count):-board(B), New_NB_COL is NB_COL+1,nth0(New_NB_COL, B, COL), Next_piece is INDEX_LIBRE-1,nth0(Next_piece,COL,Player), New_Count is Last_count+1,count_top_right_diagonal_pieces(Player,New_NB_COL,Next_piece,New_Count, Real_Count).
		count_top_right_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,Last_count, Real_Count):-board(B), New_NB_COL is NB_COL+1,nth0(New_NB_COL, B, COL), Next_piece is INDEX_LIBRE-1,\+nth0(Next_piece,COL,Player),Real_Count is Last_count.

	count_left_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,Count):- count_bottom_right_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,1, Right_Count),count_top_left_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,0, Left_Count), Count is Left_Count+Right_Count.
		% Count the number of piece on the left diagonal under the last put piece
		count_bottom_right_diagonal_pieces(Player,6,_,Last_count, Last_count):-!.
		count_bottom_right_diagonal_pieces(Player,_,5,Last_count, Last_count):-!.
		count_bottom_right_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,Last_count, Real_Count):-board(B), New_NB_COL is NB_COL+1,nth0(New_NB_COL, B, COL), Next_piece is INDEX_LIBRE+1,nth0(Next_piece,COL,Player), New_Count is Last_count+1,count_bottom_right_diagonal_pieces(Player,New_NB_COL,Next_piece,New_Count, Real_Count).
		count_bottom_right_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,Last_count, Real_Count):-board(B), New_NB_COL is NB_COL+1,nth0(New_NB_COL, B, COL), Next_piece is INDEX_LIBRE+1,\+nth0(Next_piece,COL,Player),Real_Count is Last_count.

		% Count the number of piece on the left diagonal above the last put piece
		count_top_left_diagonal_pieces(Player,0,_,Last_count, Last_count):-!.
		count_top_left_diagonal_pieces(Player,_,0,Last_count, Last_count):-!.
		count_top_left_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,Last_count, Real_Count):-board(B), New_NB_COL is NB_COL-1,nth0(New_NB_COL, B, COL), Next_piece is INDEX_LIBRE-1,nth0(Next_piece,COL,Player), New_Count is Last_count+1,count_top_left_diagonal_pieces(Player,New_NB_COL,Next_piece,New_Count, Real_Count).
		count_top_left_diagonal_pieces(Player,NB_COL,INDEX_LIBRE,Last_count, Real_Count):-board(B), New_NB_COL is NB_COL-1,nth0(New_NB_COL, B, COL), Next_piece is INDEX_LIBRE-1,\+nth0(Next_piece,COL,Player),Real_Count is Last_count.

%------------ End of the pieces'counting --------------------------------


%--------------------------------------------------------------------------------------
% First Heuristic : Moves that align the more pieces and that disturb the other player
%--------------------------------------------------------------------------------------

next_player('X',P):- P='O'.
next_player('O',P):- P='X'.


% Give a score to each column 
	% Simulate a move to know how many pieces we can align by playing in a column and how many pieces the opponent will align
	% simulate a move, get the number of pieces aligned, get the number of pieces that the opponent will align, get back to the original board
	get_list_of_scores_by_column(7, Player,List,List):-!.
	get_list_of_scores_by_column(Nb_col,Player,List, Final_list):- board(Current_board), make_move(Nb_col,Player,INDEX_LIBRE), get_nb_aligned_pieces(Nb_col,Player,INDEX_LIBRE, Nb_pieces_aligned),next_player(Player,Next_player),get_max_score_next_player(0,Next_player,-5, Max_next_player),give_a_score(Nb_pieces_aligned,Max_next_player,Score),append(List,[Score],New_List),New_Nb_col is Nb_col+1,retract(board(_)), assert(board(Current_board)), get_list_of_scores_by_column(New_Nb_col,Player,New_List, Final_list).
	% If the column is already full we enter -1000 to be sure that it will never be chosen and we continue 
	get_list_of_scores_by_column(Nb_col,Player,List, Final_list):- N is Nb_col+1, cannotPlay(N), append(List,[-1000],New_List),New_Nb_col is Nb_col+1,get_list_of_scores_by_column(New_Nb_col,Player,New_List, Final_list). 

% give a score for each move 
	% if after we play, the opponent align 4 pieces, we must not play there, score=-10
	give_a_score(Nb_pieces_aligned,Max_next_player,Score):- Max_next_player>=4, Nb_pieces_aligned<4, Score is -10.
	% if we aligned pieces before the opponent, we must play there : score =10 
	give_a_score(Nb_pieces_aligned,Max_next_player,Score):- Nb_pieces_aligned>=4, Score is 10.
	% else score= Nb_pieces_aligned-Max_next_player
	give_a_score(Nb_pieces_aligned,Max_next_player,Score):- Max_next_player<4, Nb_pieces_aligned<4, Score is Nb_pieces_aligned - Max_next_player.

% Get the max score that will have the next player, if he plays after us
	% simulate every moves that the opponent can make and get the maximal score that he can make, then go back to the original board
	get_max_score_next_player(7, Player,Curr_max, Curr_max):-!.
	get_max_score_next_player(Nb_col, Player,Curr_max, Real_max):- board(Current_board), make_move(Nb_col,Player,INDEX_LIBRE), get_nb_aligned_pieces(Nb_col,Player,INDEX_LIBRE, Nb_pieces_aligned), max(Curr_max,Nb_pieces_aligned, New_max),New_Nb_col is Nb_col+1, retract(board(_)), assert(board(Current_board)),get_max_score_next_player(New_Nb_col, Player,New_max, Real_max).
	% If the column is already full we look at the other columns
	get_max_score_next_player(Nb_col, Player,Curr_max, Real_max):- N is Nb_col+1, cannotPlay(N),New_Nb_col is Nb_col+1,get_max_score_next_player(New_Nb_col, Player,Curr_max, Real_max).

max_from_list(List,7,Max,Max):-!.
max_from_list(List,Index,Max,Real_max):-nth0(Index, List, Value), max(Value,Max, New_max),New_index is Index+1,max_from_list(List,New_index,New_max,Real_max).

%find index(plural) of the max of the list
	get_index_of_max(_,_,List_index,7,List_index):-!.
	% Add to the index_list if the index correspond to the max
	get_index_of_max(Max,List_value,List_index,Curr_index,Final_index_list):- nth0(Curr_index, List_value, Max), append(List_index,[Curr_index],New_list), New_index is Curr_index+1,get_index_of_max(Max,List_value,New_list,New_index,Final_index_list).
	% else do not add the index to the index_list
	get_index_of_max(Max,List_value,List_index,Curr_index,Final_index_list):- \+nth0(Curr_index, List_value, Max),New_index is Curr_index+1,get_index_of_max(Max,List_value,List_index,New_index,Final_index_list).

% Choose the move with the best score
	% if there is just one best move, choose it
	choose_move_IA_Heur1(Nb_col,Player,List,Max,List_index):-get_list_of_scores_by_column(0,Player,[],List),max_from_list(List,0,-20,Max),get_index_of_max(Max,List,[],0,List_index),length(List_index,1),nth0(0, List_index, Nb_col).
	% else choose trandom move among the best ones -- A Ameliorer
	choose_move_IA_Heur1(Nb_col,Player,List,Max,List_index):-get_list_of_scores_by_column(0,Player,[],List),max_from_list(List,0,-20,Max),get_index_of_max(Max,List,[],0,List_index), \+length(List_index,1),length(List_index,Size), Index_max is Size-1,random_between(0,Index_max,Index),nth0(Index, List_index, Nb_col).

% play
playIA_heur1(Player):-choose_move_IA_Heur1(Nb_col,Player,List,Max,List_index),make_move(Nb_col,Player,Index),get_nb_aligned_pieces(Nb_col,Player,Index, Nb_pieces_aligned),check_victory(Player,Nb_pieces_aligned).

%----------------- End of First heuristic



%----------------- Heuristic 3: Each coin will have a score equal to the number of neighbors of the same color, the score of the player will be the sum of all scores of his coins. 


get_score_heur3(Board,'X',Score):-get_score_heur3_col(0,Board,'X',0,Res), Score is -Res.
get_score_heur3(Board,'O',Score):-get_score_heur3_col(0,Board,'O',0,Score),write('tst').

get_score_heur3_col(0, Board, Player, 0, Res):-Factor is 6,
nth0(0, Board, COL),
score_list(COL, Player, 0, SlotsInCol), 
Sum is SlotsInCol * Factor,
N_Col is 1, 
get_score_heur3_col(N_Col, Board,Player, Sum,Res).

get_score_heur3_col(6, Board, Player, Sum, Res):-Res is  Sum,!.

get_score_heur3_col(N_Col, Board, Player, Sum, Res):-Factor is 6-N_Col,
 nth0(N_Col, Board, COL),
score_list(COL, Player, 0, SlotsInCol),
N_Sum is SlotsInCol * Factor + Sum,
Ns_Col is N_Col+1,
get_score_heur3_col(Ns_Col, Board,Player, N_Sum,Res).






% Checks how many slots are equal to Player in List Column
score_list([], Player, Sum,Res):-Res is Sum.
score_list([H|T], Player, Sum,Res):-H==Player, NSum is Sum +1, score_list(T,Player,NSum,Res).
score_list([H|T], Player, Sum,Res):-H\=Player,score_list(T,Player,Sum,Res).

% Checks whether a slot belongs to a certain player
is_from_player(Board, N_Col, N_Row, Player):-get_slot(Board, N_col, N_row, Slot), Slot==Player.


%Get the slot at coordinate point N_COL, N_ROW is free 0,0 being the bottom left
get_slot(Board, N_COL, N_ROW,Slot):-nth0(N_COL, Board, COL),N is 5-N_ROW,nth0(N, COL,Slot).


sublist(S,M,N,[_A|B]):- M>0, M<N, sublist(S,M-1,N-1,B).
sublist(S,M,N,[A|B]):- 0 is M, M<N, N2 is N-1, S=[A|D], sublist(D,0,N2,B).
sublist([],0,0,_).
%---------------- End Heuristic 3


get_valid_moves([],L,Res):-append([],L,Res).
get_valid_moves([H|T],L,Res):-get_free_index_column(H,6,'s',NTM),length(T,X), I is 6-X,append(L,[I],L_new),get_valid_moves(T,L_new,Res).
get_valid_moves([H|T],L,Res):- \+get_free_index_column(H,6,'s',NTM),get_valid_moves(T,L,Res).



simulate_move(Board, N_COL, J,INDEX_LIBRE,R):-nth0(N_COL, Board, COL),get_free_index_column(COL,6,'s',INDEX_LIBRE)
,replace(COL,INDEX_LIBRE, J,COL_RES)
,replace(Board,N_COL, COL_RES, R).




%----------------- Start MinMax Algorithm 
minmax(Board,0, MaximizingPlayer, Eval_score):- \+MaximizingPlayer,get_score_heur3(Board,'X', Score),Eval_score is Score, write('test').
minmax(Board,0, MaximizingPlayer, Eval_score):-MaximizingPlayer,get_score_heur3(Board,'O', Score), write('test2').

minmax(Board, Depth, MaximizingPlayer, Eval_score):- Depth \= 0,MaximizingPlayer,MaxEval is -10000,get_valid_moves(Board,L,Valid_moves), minmax_childs(Board, Depth, Valid_moves, MaximizingPlayer, Max_Eval, Res_Max_Eval), Eval_Score is Res_Max_Eval. 

minmax(Board, Depth, MaximizingPlayer, Eval_score):- Depth \= 0, \+MaximizingPlayer,MinEval is 10000,    get_valid_moves(Board,L,Valid_moves), minmax_childs(Board, Depth, Valid_moves,               MaximizingPlayer, Min_Eval, Res_Min_Eval), Eval_Score is Res_Min_Eval.
 

minmax_childs(Board, Depth, [], MaximizingPlayer, Max_Eval, Max_Eval):-!.
minmax_childs(Board, Depth,  [H|T], MaximizingPlayer, Max_Eval, Res_Max_Eval):-MaximizingPlayer, simulate_move(Board,H,'O', _,Res_Board),N_Depth is Depth -1, minmax(Res_Board,N_Depth, false, Eval_score),max(Eval_score, Max_Eval, New_Max_Eval), minmax_childs(Board, Depth, [T], MaximizingPlayer,New_Max_Eval, Res_Max_Eval). 

minmax_childs(Board, Depth,  [H|T], MaximizingPlayer, Min_Eval, Res_Min_Eval):- \+MaximizingPlayer, simulate_move(Board,H,'X', _,Res_Board),N_Depth is Depth -1,               minmax(Res_Board,N_Depth, true, Eval_score),min(Eval_score, Min_Eval, New_Min_Eval), minmax_childs(Board, Depth, [T], MaximizingPlayer,New_Min_Eval, Res_Min_Eval).





%----------------- End MinMax Algorithm 






reset:-retract(board(_)), assert(board([['-','-','-','-','-','-'],
            ['-','-','-','-','-','-'],
            ['-','-','-','-','-','-'],
            ['-','-','-','-','-','-'],
            ['-','-','-','-','-','-'],
            ['-','-','-','-','-','-'],
            ['-','-','-','-','-','-']])).
