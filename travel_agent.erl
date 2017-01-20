-module(travel_agent).
-export([run_time/0, run_test/0]).

-record(board, {size :: integer(),
		board = [] :: [integer() | undefined]}). %naive, but just for now

-record(die_face, {value = undefined :: integer() | undefined,
		   squares_visited = [] :: [integer()]}).
-record(oriented_die, {
                         top     = #die_face{}
                       , bottom  = #die_face{}
                       , north   = #die_face{}
                       , south   = #die_face{}
                       , east    = #die_face{}
                       , west    = #die_face{}
                       }).

move_die(north, Die=#oriented_die{top=T, north=N, bottom=B, south=S}) ->
	Die#oriented_die{top=S, north=T, bottom=N, south=B};
move_die(east, Die=#oriented_die{top=T, east=E, bottom=B, west=W}) ->
	Die#oriented_die{top=W, east=T, bottom=E, west=B};
move_die(south, Die) -> %CanI not nest these in one line?
	D1 = move_die(north, Die),
	D2 = move_die(north, D1),
	move_die(north, D2);
move_die(west, Die) ->
	D1 = move_die(east, Die),
	D2 = move_die(east, D1),
	move_die(east, D2).
		
make_move(Move, Board=#board{size=Size}, CurrentPosition, Die) ->
	NewDie=#oriented_die{top=#die_face{value=CurrentDieVal, squares_visited=Visited}} = move_die(Move, Die),
	NewPos = next_position(Move, CurrentPosition, Size),
	NewDieVal = case CurrentDieVal of
		undefined ->  get_board_value(Board, NewPos);
		_ -> CurrentDieVal %We already checked its a valid move
	end,
	NewDie#oriented_die{top=#die_face{value=NewDieVal, squares_visited=[NewPos|Visited]}}.
	
valid_moves(Board, CurrentPosition, Die) ->
	FilterFun = fun(Direction) -> is_valid_move(Direction, Board, CurrentPosition, Die) end,
	lists:filter(FilterFun, [north, south, east, west]).

is_valid_move(Direction, Board=#board{size=Size}, CurrentPos, Die) ->
	case next_pos_if_on_board(Direction, CurrentPos, Board, Size) of
		not_on_board -> false;
		NextPos -> case value_if_not_visited(NextPos, Board, Die) of
			visited -> false;
			BoardValue -> die_face_matches(BoardValue, Die, Direction)
		end
	end.
%1 index list as grid, e.g.
%[ 1, 2, 3,
%  4, 5, 6,
%  7, 8, 9]
%First failure cases
next_pos_if_on_board(north, CurrentPos, _Board, {Wide, _Down}) when CurrentPos =< Wide -> not_on_board;
next_pos_if_on_board(south, CurrentPos, _Board, {Wide, Down}) when CurrentPos > Wide * (Down - 1) -> not_on_board;
next_pos_if_on_board(east, CurrentPos, _Board, {Wide, _Down}) when CurrentPos rem Wide =:= 0 -> not_on_board;
next_pos_if_on_board(west, CurrentPos, _Board, {Wide, _Down}) when CurrentPos rem Wide =:= 1 -> not_on_board;
	
next_pos_if_on_board(Direction, CurrentPos, _Board, {Wide, Down}) ->
	next_position(Direction, CurrentPos, {Wide, Down}).

value_if_not_visited(PotentialNextPos, Board, Die) ->
	Visited = squares_visited(Die),
	case lists:member(PotentialNextPos, Visited) of
		true -> visited;
		false -> get_board_value(Board, PotentialNextPos)
	end.

squares_visited(#oriented_die{top=T, bottom=B, north=N, south=S, east=E, west=W}) ->
	V = lists:map(fun(#die_face{squares_visited=L}) -> L end, [N,S,E,W,T,B]),
	lists:append(V).

next_position(north, CurrentPos, {Wide, _Down}) ->
	CurrentPos - Wide;
next_position(south, CurrentPos, {Wide, _Down}) ->
	CurrentPos + Wide;
next_position(east, CurrentPos,_Size) ->
	CurrentPos + 1;
next_position(west, CurrentPos, _Size) ->
	CurrentPos -1 .

%1 indexed! but so is nth!
get_board_value(#board{board=B}, N) ->
	lists:nth(N, B).

die_face_matches(_BoardValue=undefined, _Die, _Direction) -> true;
die_face_matches(BoardValue, Die, Direction) ->
	case get_top(move_die(Direction, Die)) of
		undefined -> true;
		BoardValue -> true;
		_OtherVal -> false
	end.
	
get_top(#oriented_die{top=#die_face{value=V}}) -> V.

%make sure recursion is right. should return [#solutions]
solve(#board{size={Wide, Down}}, CurrentPosition, Die) when CurrentPosition =:= Wide * Down ->
	travel_agent_solution:process_solution(Die);
solve(Board=#board{size=Size}, CurrentPosition, Die) ->
	LegalMoves = valid_moves(Board, CurrentPosition, Die),
	MapFunc = fun(D) -> 
			NextPos = next_position(D, CurrentPosition, Size),
			NextDie = make_move(D, Board, CurrentPosition, Die),
			solve(Board, NextPos, NextDie)  end,
	lists:map(MapFunc, LegalMoves).

test_board() -> #board{size = {5,5}, board =
	[3,4,1,7,5,
	 1,2,4,3,5,
	 2,4,3,6,2,
	 9,5,7,2,3,
	 5,8,3,4,1]}.
test_die() -> #oriented_die{top = #die_face{value = 3, squares_visited=[1]} }.

initial_die() -> #oriented_die{top = #die_face{value = 1, squares_visited=[1]}}.
real_board() ->#board{size={12,12}, board=
[1, 5, 4, 4, 6, 1, 1, 4, 1, 3, 7, 5,
 3,undefined,undefined,undefined,undefined,undefined,undefined,undefined,undefined,undefined,undefined, 1,
 4,undefined, 6, 4, 1, 8, 1, 4, 2, 1,undefined, 3,
 7,undefined, 1,undefined,undefined,undefined,undefined,undefined,undefined, 1,undefined, 2,
 1,undefined, 1,undefined, 6, 1, 6, 2,undefined, 2,undefined, 1,
 8,undefined, 4,undefined, 1,undefined,undefined, 8,undefined, 3,undefined, 5,
 4,undefined, 2,undefined, 5,undefined,undefined, 3,undefined, 5,undefined, 2,
 8,undefined, 5,undefined, 1, 1, 2, 3,undefined, 4,undefined, 6,
 6,undefined, 1,undefined,undefined,undefined,undefined,undefined,undefined, 3,undefined, 6,
 3,undefined, 6, 3, 6, 5, 4, 3, 4, 5,undefined, 1,
 6,undefined,undefined,undefined,undefined,undefined,undefined,undefined,undefined,undefined,undefined, 3,
 2, 1, 6, 6, 4, 5, 2, 1, 1, 1, 7, 1]}.

run_time() ->
	io:format("Time start: ~p~n", [erlang:time()]),
	travel_agent_solution:start_link(),
	run(),
	travel_agent_solution:finished(),
	io:format("Time stop: ~p~n", [erlang:time()]).

run() -> solve(real_board(), 1, initial_die()).

run_test() ->
	travel_agent_solution:start_link(),
	solve(test_board(), 1, test_die()),
	timer:sleep(1000),
	travel_agent_solution:finished().
