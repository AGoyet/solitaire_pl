:- use_module(library(clpfd)).
:- use_module(library(tty)).
:- use_module(library(readutil)).
:- use_module(library(ansi_term)).
    
save_file("solitaire_save.txt").
stats_file("solitaire_stats.txt").
prompt(_,"").

%% What a terrible failure.
wtf:-
    writeln("wtf"),
    abort.

fd_length(L, N) :-
   N #>= 0,
   fd_length(L, N, 0).

fd_length([], N, N0) :-
   N #= N0.
fd_length([_|L], N, N0) :-
   N1 is N0+1,
   N #>= N1,
   fd_length(L, N, N1).

list_string([], "").
list_string([X|L], S):-
    (  string(X)
    -> S2 = X
    ;  term_string(X, S2)
    ),
    list_string(L, SL),
    string_concat(S2, SL, S).

card( [Suit, Rank] ) :-
    Suit in 0..2,
    Rank in 0..9.

isfamily([_, 0]).
notfamily([_, Rank]):-
    Rank in 1..9.

char_of_suit(Suit, Char):-
    nth0(Suit, ['♦','♥','♣','♠'], Char).
    
write_card(Card) :-
    [Suit, Rank]= Card,
    char_of_suit(Suit, SChar),
    (  isfamily(Card)
    -> RChar= SChar
    ;  term_string(Rank, RChar)
    ),
    string_concat(RChar, SChar, S),
    %string_concat(RChar, " ", S),
    nth0(Suit, [[], [fg(red)], [fg(green)]], Options),
    ansi_format(Options, S,[]).

write_card_bw(Card) :-
    [Suit, Rank]= Card,
    ( isfamily(Card) ->
      write_suit(Suit)
    ;
    write(Rank)
    ),
    write_suit(Suit).

write_stack([]).
write_stack(Stack):-
    append(Rest, [Card], Stack),
    write_card(Card), nl,
    write_stack(Rest).
    
deck(Deck) :-
    findall(Rank, (Rank in 0..9, label([Rank])), Ranks),
    append([0,0,0], Ranks, Ranks_full),
    findall(Suit, (Suit in 0..2, label([Suit])), Suits),
    findall(C, ( card(C), C=[Suit, Rank], member(Rank, Ranks_full), member(Suit, Suits)), Deck).

empty([]).

top_left([]).
top_left([C]):- card(C).


top_right([]).
top_right(L):-
    maplist(card, L),
    maplist(not_family, L).

column(Column):-
    maplist(card, Column).

board(Board):-
    [Top_lefts, Top_rights, Columns]= Board,
    length(Top_lefts, 3),
    maplist(top_left, Top_lefts),
    length(Top_rights, 3),
    maplist(top_right, Top_rights),
    length(Columns, 8),
    maplist(column, Columns).

% Top of a stack (displayed at the bottom of the board): first element of the list.
write_top_card([]):- write([]).
write_top_card([C|_]):- write_card(C).

write_top_card_cols([Col|Cols]):-
    write_top_card(Col),
    (  Cols = []
    ;  write("  "),
       write_top_card_cols(Cols)
    ).

max_len(L, N, Max):-
    length(L, Len),
    Max is max(N, Len).

max_len_lists(LL, Max):-
    foldl(max_len, LL, 0, Max).

write_line_column(Column, MaxN, N):-
    length(Column, Len),
    NN is N - (MaxN + 1 - Len),
    (
        NN < 0  ->
        write("  ")
    ;
    nth0(NN, Column, C),
    write_card(C)
    ).

write_line_columns([Column|Tail], MaxN, N):-
    write_line_column(Column, MaxN, N),
    (  Tail = []
    ;  write("  "),
       write_line_columns(Tail, MaxN, N)
    ).

write_lines_columns_sub(_, _, -1).
write_lines_columns_sub(Columns, MaxN, N):-
    write_line_columns(Columns, MaxN, N),
    write("\n"),
    M is N - 1,
    write_lines_columns_sub(Columns, MaxN, M).

write_lines_columns(Columns):-
    max_len_lists(Columns, Len),
    MaxN is Len - 1,
    write_lines_columns_sub(Columns, MaxN, MaxN).

reset_screen:-
    %write("\033c").
    (  tty_clear
    ;  true
    ).

write_horz:-
    writeln("――――――――――――――――――――――――――――――").
%    writeln("------------------------------"),

write_board(Board):-
    reset_screen,
    write("01  02  03          06        \n"),
    write_horz,
    [Top_lefts, Top_rights, Columns]= Board,
    write_top_card_cols(Top_lefts),
    write("          "),
    write_top_card_cols(Top_rights),
    write("\n"),
    write_horz,
    write_lines_columns(Columns),
    write("\n"),
    write_horz,
    write("1   2   3   4   5   6   7   8\n"),
    !.


empty_board([Top_lefts, Top_rights, Columns]):-
    length(Columns, 8),
    maplist(empty, Columns),
    length(Top_lefts, 3),
    maplist(empty, Top_lefts),
    length(Top_rights, 3),
    maplist(empty, Top_rights).

% Assumes that Top_rights is ordered (as if obtained by following valid moves).
solved_board([_, _, [[], [], [], [], [], [], [], []]]).

replace_in_l(L, I, X, X_old, L_new):-
    fd_length(L, N),
    LenB is N - I - 1,
    fd_length(A, I),
    fd_length(B, LenB),
    append([A, [X_old], B], L),
    append([A, [X], B], L_new).

% No checks done
place_card(Board, Card, [H,I], Board_new):-
    nth0(H, Board, Cols),
    replace_in_l(Cols, I, [Card|Col], Col, Cols_new),
    replace_in_l(Board, H, Cols_new, _, Board_new).

deal_sub([], Board, _, Board).
deal_sub(Deck, Board, I, Board_new):-
    fd_length(Deck, N),
    N > 0,
    random(0, N, J),
    nth0(J, Deck, Card, Deck_new),
    place_card(Board, Card, [2, I], Board_temp),
    ( I is 7 ->
      I3 is 0
    ;
    I3 is I + 1
    ),
    deal_sub(Deck_new, Board_temp, I3, Board_new).

deal(Board):-
    deck(Deck),
    empty_board(B),
    deal_sub(Deck, B, 0, Board).

isstack([_]).
isstack([Card1|[Card2|Tail]]):-
    notfamily(Card1),
    [S1,R1]= Card1,
    [S2,R2]= Card2,
    R2 #= R1 + 1,
    S1 #\= S2,
    isstack([Card2|Tail]).

valid_grab_stack(Board, [H,I,J], Stack):-
    H in 0..2,
    nth0(H, Board, Cols),
    (  H #= 0 ->
       J #= 0,
       I in 0..2,
       nth0(I, Cols, [Card]),
       Stack= [Card]
    ;  H #= 2,
       I in 0..7,
       J in 0..8,
       nth0(I, Cols, Column),
       fd_length(Column, Len),
       Len #> J,
       Stack_len #= J + 1,
       fd_length(Stack, Stack_len),
       append(Stack, _, Column),
       isstack(Stack)
    ).

remove_stack(Board, [H,I,J], Board_new):-
    H in 0..2,
    nth0(H, Board, Cols),
    (  H #= 0 ->
       I in 0..2,
       J #= 0,
       replace_in_l(Cols, I, [], _, Cols_new)
    ;  ( H #= 2 ->
         I in 0..7,
         J in 0..8,
         nth0(I, Cols, Column),
         fd_length(Column, N),
         LenB #= N - J - 1,
         fd_length(B, LenB),
         append(_, B, Column),
         replace_in_l(Cols, I, B, _, Cols_new)
       ;
       fail
       )
    ),
    replace_in_l(Board, H, Cols_new, _, Board_new).

place_stack(Board, Stack, [H,I], Board_new):-
    H in 0..2,
    I in 0..7,
    nth0(H, Board, Cols),
    nth0(I, Cols, Col),
    (  H #= 2
    -> (  Col = [Card2|_]
       -> last(Stack, Card),
          notfamily(Card2),
          [S1,R1]= Card,
          [S2,R2]= Card2,
          R2 #= R1 + 1,
          S1 #\= S2
       ;  true
       )
    ;  (  H #= 0
       -> ( Col = [],
            Stack = [ _ ]
          )
       ; % H #= 1
         I #= S1,
         [Card] = Stack,
         notfamily(Card),
         [S1,R1]= Card,
         (  Col = [Card2|_]
         -> [S1,R1]= Card,
            [S2,R2]= Card2,
            R1 #= R2 + 1,
            S1 #= S2
         ;  % Col = []
            [_, Rank] = Card,
            Rank #= 1
         )
       )
    ),
    append(Stack, Col, Col_new),
    replace_in_l(Cols, I, Col_new, _, Cols_new),
    replace_in_l(Board, H, Cols_new, _, Board_new).


longest_stack_sub(Col_sure, [], Len):-
    length(Col_sure, Len).
longest_stack_sub(Col_sure, [Card| Col_rest], Len):-
    append(Col_sure, [Card], Col_sure2),
    (  isstack(Col_sure2)
    -> longest_stack_sub(Col_sure2, Col_rest, Len)
    ;  length(Col_sure, Len)
    ).

find_longest_stack(Board, I, J):-
    [_, _, Columns]= Board,
    nth0(I, Columns, Column),
    longest_stack_sub([], Column, Len),
    J is Len -1.

count_visible(_, [], 0).
count_visible(Card, [Col| Cols], N):-
    count_visible(Card, Cols, N2),
    (  Col = [Card | _]
    -> N is N2 + 1
    ;  N is N2
    ).

remove_visible(_, [], []).
remove_visible(Card, [Col|Cols], [Col_new|Cols_new]):-
    (  [Card|Rest] = Col
    -> Col_new= Rest
    ;  Col_new= Col
    ),
    remove_visible(Card, Cols, Cols_new).

kill_this_family(Board, Card, Board_new):-
    isfamily(Card),
    [Top_lefts, Top_rights, Columns]= Board,
    count_visible(Card, Top_lefts, Nl),
    Nl > 0,
    count_visible(Card, Columns, Nc),
    Nc is 4 - Nl,
    remove_visible(Card, Top_lefts, Top_lefts_new),
    remove_visible(Card, Columns, Columns_new),
    Board2= [Top_lefts_new, Top_rights, Columns_new],
    place_first_topleft(Board2, Card, Board_new).

% Kills one, fails if kills zero.
kill_one_family_sub(Board, [Col|L], Board_new):-
    (  (  [Card]= Col, kill_this_family(Board, Card, Board_new)  )
    ;  kill_one_family_sub(Board, L, Board_new)
    ).

kill_one_family(Board, Board_new):-
    [Top_lefts, _, _]= Board,
    kill_one_family_sub(Board, Top_lefts, Board_new).

% Kills all, fails if kills zero.
kill_families(Board, Board_new):-
    (  kill_one_family(Board, Board2) % This one needs to succed
    -> (  kill_families(Board2, Board_new) % If this one fails, it's ok.
       ;  Board_new= Board2
       )
    ).

kill_zero_or_more_families(Board3, Board4):-
    (  kill_families(Board3, Board4)
    ;  Board4= Board3
    ).
    
place_first_topleft(Board, Card, Board_new):-
    [Top_lefts, Top_rights, Columns]= Board,
    nth0(I, Top_lefts, []),
    replace_in_l(Top_lefts, I, [Card], [], Top_lefts_new),
    [Top_lefts_new, Top_rights, Columns]= Board_new.

do_moves(Board3, [], Board4):-
    kill_zero_or_more_families(Board3, Board4).
do_moves(Board, [Move|Moves], Board_new):-
    fd_length(Moves, _),
    % Move = [[I, J], I_new]
    [HIJ, HI_new]= Move,
    %integer(I), integer(J), integer(I_new), % To help the solver?
    valid_grab_stack(Board, HIJ, Stack),
    remove_stack(Board, HIJ, Board2),
    place_stack(Board2, Stack, HI_new, Board3),
    kill_zero_or_more_families(Board3, Board4),
    do_moves(Board4, Moves, Board_new).


automoves(Board0, Moves, Board_new):-
    kill_zero_or_more_families(Board0, Board),
    (  ( H in 0..2, H #\= 1, 
         %M= [[H,I,0],[1,I_new]],
         M= [[H,I,0],[1,_]],
         % Do not automatically do "unsafe" move:
         % If the top right contains a [0,2] and a [1,5], the board contains a [0,4],
         % which might need to be placed on the [1,5] as it cannot be placed in the top right.
         valid_grab_stack(Board, [H,I,0], [[_, Rank]]),
         [_, Top_rights, _]= Board,
         maplist(rank_or_zero, Top_rights, Ranks),
         min_list(Ranks, Min),
         % +2 here because having a 2 and a 4 is ok. Replace +2 by +1 for a more aestethic
         % automoving.
         Rank #=< (Min + 2),
         do_moves(Board, [M], Board2)
       )
    -> automoves(Board2, Moves2, Board_new),
       Moves= [M| Moves2]
    ;  Board_new= Board,
       Moves= []
    ).

count(_, [], 0).
count(Goal, [X|L], N):-
    count(Goal, L, N2),
    (  call(Goal, X)
    -> N is N2+1
    ;  N is N2
    ).

isempty(L):-
    L = [].

free_spaces(Board, N):-
    [Top_lefts, _, Columns]= Board,
    count(isempty, Top_lefts, N1),
    count(isempty, Columns, N2),
    N is N1+N2.

solver_do_moves(Board3, [], Board4):-
    kill_zero_or_more_families(Board3, Board4).
solver_do_moves(Board, [Moves|Movess], Board_new):-
    fd_length(Movess, _),
    Moves= [Move|Moves_auto],
    % Move = [[I, J], I_new]
    [HIJ, HI_new]= Move,
    valid_grab_stack(Board, HIJ, Stack),
    remove_stack(Board, HIJ, Board2),
    place_stack(Board2, Stack, HI_new, Board3),
    automoves(Board3, Moves_auto, Board5),
    solver_do_moves(Board5, Movess, Board_new).

search_sub(Board, Depth_max, Board2, Movess, Depth_exact):-
    writeln(["Depth", Depth_exact]),
    (  (  fd_length(Movess, Depth_exact),
          solver_do_moves(Board, Movess, Board2)
       )
    ;  Depth_exact < Depth_max,
       Depth_exact2 is Depth_exact+1,
       search_sub(Board, Depth_max, Board2, Movess, Depth_exact2)
    ).

search(Board, Depth_max, Board2, Movess):-
    search_sub(Board, Depth_max, Board2, Movess, 0).


write_moves(Board, [], Board).
write_moves(Board, [Move|Moves], Board_new):-
    do_moves(Board, [Move], Board2),
    write_board(Board2),
    write_moves(Board2, Moves, Board_new).

write_movess(Board, [], Board).
write_movess(Board, [Moves|Movess], Board_new):-
    write_moves(Board, Moves, Board2),
    write_movess(Board2, Movess, Board_new).
    
%
%
% Everything pas this point is only useful when playing as a human.
%
%


% The game saves the board and the "movess", which are lists of lists of moves.
% each sublist corresponds to a single undo step. Automoves are added to the last
% sublist. Player moves add a new sublist.
do_movess(Board, [], Board).
do_movess(Board, [Moves|Movess], Board_new):-
    do_moves(Board, Moves, Board2),
    do_movess(Board2, Movess, Board_new).

uindex(Is, [Hp, Ip]):-
    string(Is),
    (  (  (  nth0(I2s, ["01","02","03","06"], Is)
          -> term_string(I2, I2s),
             ( I2 >= 3 ->
               Hp is 1,
               Ip is I2 - 3
             ;
             Hp is 0,
             Ip is I2
             )
          )
       ;  term_string(I, Is),
          integer(I),
          I < 9,
          Hp is 2,
          Ip is I - 1
       )
    ;  !,
       write("invalid position index: "), writeln(Is),
       fail
    ),
    !.

umove(Mu, Mp):-
    [[I,J], I_new]= Mu,
    uindex(I, [Hp,Ip]),
    uindex(I_new, [Hp_new, Ip_new]),
    Mp= [[Hp, Ip, J], [Hp_new, Ip_new]].

rank_or_zero([[_,Rank]|_], Rank).
rank_or_zero([], 0).

solitaire_input(Input):-
    read_line_to_string(user_input, Input2),
    (  Input2 = "0"
    -> read_line_to_string(user_input, Input3),
       string_concat(Input2, Input3, Input)
    ;  Input= Input2
    ).
    
scroll_text_list([]).
scroll_text_list([Text|Rest]):-
    writeln(Text),
    writeln("(Enter q to stop reading,\nanything else to see more)"),
    solitaire_input(Input),
    (  Input == "q"
    -> true
    ;  scroll_text_list(Rest)
    )
.

solitaire_help:-
    scroll_text_list([
"How to play:
Lower columns are numbered 
1 to 8, top left 01 to 03,
 top right is 06.

Other commands:
q or quit
h or help
r or restart
u or undo
n or new (new game)",

"You can press 9 (or anything
 not above) to \"cancel\" the
 current move.

A save file and stats file are
 created in the launch
 directory.  Delete (or
 rename) either to reset.",

"Rules:

* The goal of the game is to
  empty the lower columns.

* Picking a column will grab
the card displayed at the
bottom, or several cards at
the same time if they are well
'stacked'. Cards are stacked
if the numbers increase from
the bottom up, with
alternating colors.

* A card or stack can then be
  placed anywhere it stacks.

* If you can only place part
of a stack, the game will
automatically detect how many
cards you wanted to pick,
leaving the rest in its inital
position.  (So only the bottom
part of the stack will move).
",

"* The 3 top left spots can
hold a single card, and can be
used as temporary storage for
any card. They are displayed
as [] when empty.

* The 3 top right columns hold
only numbered cards, in
increasing number.  Only the
highest number is shown, or []
if the column is empty. So a
card numbered 1 can go on [],
a 2 can go on a 3 etc. Each
color has its own column, so
there is no choice between
column when placing there.

* Cards cannot be picked back
  from the top right column.

* Cards are automatically
moved to the top right when it
is safe to do so.  ", "* Each
color has 4 cards with no
number. Those form a 'family',
and do not stack.

* The family is automatically
'reunited' when at least one
of its member is in a top left
spot, and all others can
currently be picked. After
that they only take one spot
in the top left.

* In some rare situations, you
might want to pick only part
of a stack to put it in an
empty column. You can do that
by typing for example '[5,2]'
to pick 2 cards from the
column 5.

* Restarts and undos used in a
  game that was won are
  tracked in the stats."  ]).

%% To print the moves AND print family killings, we need to duplicate what
%%  do_moves does. There are two passes: convert moves to "pseudo moves",
%%  where "[]" means kill families, then write those pseudo moves.

% Includes family killing (not technically a move)
pseudo_moves(Board, Moves, Moves_pseudo):-
    (  kill_one_family(Board, Board_family)
    -> Moves_pseudo= [[]|Rest_pseudo],
       pseudo_moves(Board_family, Moves, Rest_pseudo)
    ;  (  Moves = [Move|Rest]
       -> [HIJ, HI_new]= Move,
          valid_grab_stack(Board, HIJ, Stack),
          remove_stack(Board, HIJ, Board2),
          place_stack(Board2, Stack, HI_new, Board3),
          [Move|Rest_pseudo]= Moves_pseudo,
          pseudo_moves(Board3, Rest, Rest_pseudo)
       ;  % no moves left, no family to kill
          Moves_pseudo= []
       )
    ).
    
% Prints the state after each move except the last one.
write_pseudo_moves(_, []).
write_pseudo_moves(Board, [Move|Moves]):-
    (  [HIJ, HI_new]= Move
    -> valid_grab_stack(Board, HIJ, Stack),
       remove_stack(Board, HIJ, Board2),
       place_stack(Board2, Stack, HI_new, Board3)
    ;  (Move = [] ; wtf), % sanity check
       kill_one_family(Board, Board3),
       (Board \= Board3 ; wtf) % sanity check
    ),
    (  Moves = [_|_]  % Only print if this isn't the last move
    -> write_board(Board3),
       sleep(0.25)
    ;  true
    ),
    write_pseudo_moves(Board3, Moves).

play_automoves(Board, Movess, Board_new, Movess_new):-
    (  (  automoves(Board, Auto_Moves, Board_new),
          pseudo_moves(Board, Auto_Moves, Moves_pseudo),
          Moves_pseudo \= []
       )
    -> write_board(Board),
       ansi_format([fg(green)], "Automoves:\n",[]),
       sleep(0.5),
       write_pseudo_moves(Board, Moves_pseudo),
       (  append(Rest, [Moves], Movess)
       -> append(Moves, Auto_Moves, Moves_new),
          append(Rest, [Moves_new], Movess_new)
       ;  (  Movess = [] ; wtf), % sanity check
          Movess_new= [Auto_Moves]
       )
    ;  Board_new= Board,
       Movess_new= Movess
    ).

play(Board_initial, Board, Movess):-
    play(Board_initial, Board, Movess, "").

menu_inputs(Board_initial, Board, Movess, Input):-
    ( (Input = "q" ; Input = "quit")
    -> writeln("Press q again to confirm quit,\nanything else to cancel"),
       read_line_to_string(user_input, Input_confirm),
       (  (Input_confirm = "q" ; Input_confirm = "quit")
       -> abort
       ;  play(Board_initial, Board, Movess),
          abort
       )
    ;  ( (Input = "h" ; Input = "help")
       -> solitaire_help,
          play(Board_initial, Board, Movess)
       )
    ;  ( (Input = "r" ; Input = "restart")
       -> writeln("Restart the game? Press r to\nconfirm, anything else to cancel"),
          read_line_to_string(user_input, Input_confirm),
          (  (Input_confirm = "r" ; Input_confirm = "restart")
          -> add_ru_stats(1,0),
             play(Board_initial),
             abort
          ;  play(Board_initial, Board, Movess),
             abort
          )
       )
    ;  ( (Input = "u" ; Input = "undo")
       -> (  Movess = []
          -> play(Board_initial, Board_initial, [])
          ;  add_ru_stats(0,1),
             append(Rest, [_], Movess),
             do_movess(Board_initial, Rest, Board2),
             save(Board_initial, Rest),
             play(Board_initial, Board2, Rest),
             abort
          )
       )
    ;  ( (Input = "n" ; Input = "new")
       -> (  solved_board(Board)
          -> Win is 1
          ;  writeln("Start a new
game? This will count as a
loss. The undos and restarts
will not count.  Press n to
confirm, anything else to
cancel."),
             read_line_to_string(user_input, Input_confirm),
             (  (Input_confirm = "n" ; Input_confirm = "new")
             -> Win is 0
             ;  play(Board_initial, Board, Movess)
             )
          ),
          % If Win is 1, stats should already have updated when the solve message is displayed.
          % But this should be safe to redo, and might future proof.
          update_stats(Board_initial, Win),
          play_new,
          abort
       )
    ).

play(Board_initial, Board, Movess, Message):-
    write_board(Board),
    writeln(Message),
    (  do_movess(Board_initial, Movess, Board) ; wtf ), % Sanity check
    (  solved_board(Board)
    -> (  ansi_format([fg(green)], "SOLVED!\n",[]), 
          update_stats(Board_initial, 1),
          (  Message = ""
          -> stats_string(S),
             writeln(S)
          ;  true % Assume stats have already been displayed in message
          ),
          writeln("Press n for new game or q to
quit (You can still use other
commands)")
       )
    ; true
    ),
    !,
    (  (   write("pick a column:\n"),
           solitaire_input(Input1),
           (  (  term_string([Is,JJ], Input1),
                 string(Is), integer(JJ),
                 J is JJ-1
              )
           -> uindex(Is,[Hp,Ip])
           ;  (  menu_inputs(Board_initial, Board, Movess, Input1)
              ;  uindex(Input1, [Hp,Ip]),
                 (  Hp = 0
                 -> J= 0
                 ;  find_longest_stack(Board, Ip, J)
                 )
              )
           ),
           HIJ= [Hp, Ip, J],
           valid_grab_stack(Board, HIJ, Stack),
           remove_stack(Board, HIJ, Board2),
           write("picked:\n"),
           write_stack(Stack),
           write("pick a destination\n"),
           solitaire_input(Input2),
           (  menu_inputs(Board_initial, Board, Movess, Input2)
           ;  (  umove([[Input1, J], Input2], [HIJ, [H_dest, I_dest]]),
                 (  (  (  H_dest = 1
                       -> [Card]= Stack,
                          [Suit,_]= Card,
                          HI_new= [1, Suit]
                       ;  HI_new= [H_dest, I_dest]
                       ),
                       Move= [HIJ, HI_new],
                       place_stack(Board2, Stack, HI_new, Board3),
                       Move2= Move
                    )
                 ;  term_string(IJu, Input1),
                    integer(IJu),
                    % Try to automatically guess what the user meant (size of stack)
                    (  J_guess in 0..8,
                       valid_grab_stack(Board, [Hp, Ip, J_guess], Stack_guess),
                       (  H_dest = 1
                       -> [Card]= Stack_guess,
                          [Suit,_]= Card,
                          HI_new= [1, Suit]
                       ;  HI_new= [H_dest, I_dest]
                       ),
                       remove_stack(Board, [Hp, Ip, J_guess], Board2_guess),
                       place_stack(Board2_guess, Stack_guess, HI_new, Board3)
                    ),
                    Move2= [[Hp, Ip, J_guess], HI_new]
                 ),
                 Board_new= Board3,
                 append(Movess, [[Move2]], Movess_new)
              )
           )
       )
    ;   ansi_format([fg(red)], "Incorrect move\n\n",[]),
        sleep(0.5),
        play(Board_initial, Board, Movess)
    ),
    !,
    play_automoves(Board_new, Movess_new, Board_new2, Movess_new2),
    save(Board_initial, Movess_new2),
    play(Board_initial, Board_new2, Movess_new2).


play(Board_initial):-
    play(Board_initial, "").
    
play(Board_initial, Message):-
    play_automoves(Board_initial, [], Board, Movess),
    save(Board_initial, Movess),
    play(Board_initial, Board, Movess, Message).

play_new:-
    play_new("").
play_new(Message):-
    deal(Board_initial),
    stats_string(S),
    list_string([Message, "\n", S, "\n", "Dealt new game."], Message2),
    
    play(Board_initial, Message2).

play:-
    Message= "Welcome to solitaire!
Enter 'h' for help.",
    (  load(Board_initial, Movess, Board, Message_load)
    ;  play_new(Message)
    ),
    !,
    stats_string(S),
    list_string([Message, "\n", Message_load, "\n", S], Message2),
    play(Board_initial, Board, Movess, Message2).

save_data(File, Data):-
    (  (  open(File, write, Out),
          write(Out, Data),
          write(Out, "."),
          close(Out)
       )
    ;  write("Cannot write to file "),
       writeln(File),
       abort
    ),
    !.

load_data(File, Data):-
    (  exists_file(File)
    -> (  open(File, read, In)
       -> (  read(In, Data)
          ;  write("Invalid data in "),
             writeln(File),
             writeln("Rename or delete that file to fix the problem."),
             abort
          )
       ;  write("Cannot open file "),
          writeln(File),
          abort
       )
    ),
    !.        

save(Board_initial, Movess):-
    save_file(File),
    save_data(File, [Board_initial, Movess]).

load(Board_initial, Movess, Board, Message):-
    save_file(File),
    load_data(File, [Board_initial, Movess]),
    (  do_movess(Board_initial, Movess, Board)
    -> string_concat("loaded file ", File, Message)
    ;  write("Invalid save data in\n"),
       writeln(File),
       writeln("Rename or delete that file\nto fix the problem."),
       Message="",
       abort
    ),
    !.

save_stats(Stats):-
    stats_file(File),
    save_data(File, Stats).

load_stats(Stats):-
    stats_file(File),
    load_data(File, Stats),
    (  (  [B|Rest]= Stats,
          length(Rest, 8),
          ( B = [] ; board(B) ),
          maplist(integer, Rest)
       )
    ;  write("Invalid stats data in\n"),
       writeln(File),
       writeln("Rename or delete that file\nto fix the problem."),
       abort
    ),
    !.

stats_string(S):-
    (  (  load_stats(Stats),
          [_, Win_nb, Loss_nb, Streak_curr, Streak_max, Restarts, Undos, _, _]= Stats,
          N is (Win_nb +Loss_nb),
          (  N = 0
          -> S = ""
          ;  round((Win_nb / N)*10000, X),
             Win_percent is X / 100,
             round((Restarts / N)*100, XR),
             Restarts_avg is XR / 100,
             round((Undos / N)*100, XU),
             Undos_avg is XU / 100,
             L= ["Stats: W/L: ",
                 Win_nb, "/", Loss_nb,
                 " : ", Win_percent, "%\n",
                 "Streak: ", Streak_curr, " Max Streak: ", Streak_max,
                 "\n",
                 "R/U used: ", Restarts, "/", Undos,
                 "\nR/U avg: ", Restarts_avg, "/", Undos_avg
                ],
             list_string(L, S)
          )
       )
    ; S= ""
    ).

% Win= 1 or 0
update_stats(Board_initial, Win):-
    (  load_stats(Stats)
    ;  Stats= [[], 0,0,0,0,0,0,0,0]
    ),
    [Board_stats, Win_nb, Loss_nb,
     Streak_curr, Streak_max,
     Restarts, Undos,
     Restarts_curr, Undos_curr]= Stats,
    (  Board_stats \= Board_initial
    -> (  Win = 1
       -> Win_nb_new is Win_nb+1,
          Loss_nb_new is Loss_nb,
          Streak_curr_new is Streak_curr+1,
          Streak_max_new is max(Streak_curr_new, Streak_max),
          Restarts_new is Restarts_curr + Restarts,
          Undos_new is Undos_curr + Undos
       ;  Win_nb_new is Win_nb,
          Loss_nb_new is Loss_nb+1,
          Streak_curr_new is 0,
          Streak_max_new is Streak_max,
          Restarts_new is Restarts,
          Undos_new is Undos
       ),
       Stats_new= [Board_initial, Win_nb_new, Loss_nb_new,
                   Streak_curr_new, Streak_max_new,
                   Restarts_new, Undos_new, 0, 0],
       save_stats(Stats_new)
    ;  true % Do nothing if already done for this board.
    ).

add_ru_stats(Restarts, Undos):-
    (  load_stats(Stats)
    ;  Stats= [[], 0,0,0,0,0,0,0,0]
    ),
    nth0(7, Stats, Restarts_tot),
    Restarts_tot_new is Restarts_tot + Restarts,
    replace_in_l(Stats, 7, Restarts_tot_new, _, Stats2),
    nth0(8, Stats, Undos_tot),
    Undos_tot_new is Undos_tot + Undos,
    replace_in_l(Stats2, 8, Undos_tot_new, _, Stats_new),
    save_stats(Stats_new).    
    
main:-
    current_prolog_flag(argv, Argv),
    (  ( member('--summary', Argv) ; member('-s', Argv) )
    -> load(_Board_initial, _Movess, Board, _Message),
       write_board(Board),
       stats_string(S),
       writeln(S),
       abort
    ; ( (  (nth0(K, Argv, '--seed'), K1 is K+1, nth0(K1, Argv, Seed_a),
            atom_number(Seed_a, Seed))
        -> set_random(seed(Seed))
        ; true
        ),
        play
      ;  abort
      )
    ).
