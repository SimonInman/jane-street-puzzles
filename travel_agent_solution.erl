-module(travel_agent_solution).

-behaviour(gen_server).
-export([code_change/3, start_link/0,handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-export([
          process_solution/1
		, finished/0
	]).

-record(state, {max_so_far, path}).
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

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
init([]) ->
	{ok, #state{max_so_far=0, path=[]}}.

process_solution(Die) ->
	gen_server:cast({global, ?MODULE}, {solution, Die}).

finished() -> gen_server:call({global, ?MODULE}, {finished}).

handle_cast({solution, Die}, State=#state{max_so_far=MaxSoFar}) ->
	{Prod, Path} = do_process_solution(Die),
	NewState = case Prod > MaxSoFar of
		true -> #state{max_so_far=Prod, path=Path};
		false -> State
	end,
	{noreply, NewState}.

handle_call({finished}, _From, State=#state{max_so_far=Max, path=Path}) ->
	{reply, {Max, Path}, State}.

do_process_solution(Die=#oriented_die{top=T, bottom=B, north=N, south=S, east=E, west=W}) ->
	io:format("Solution Die ~p~n", [Die]),
	ValsVisitTimes = lists:append(lists:map(fun(#die_face{value=Val, squares_visited=L}) -> lists:duplicate(length(L), case Val of undefined -> 9; _ -> Val end) end, [N,S,E,W,T,B])),
	Prod = lists:foldl(fun(X, Prod) -> X * Prod end, 1, ValsVisitTimes),
	
	V = lists:map(fun(#die_face{squares_visited=L}) -> L end, [N,S,E,W,T,B]),
	Path = lists:append(V),
	io:format("Found solution ~p~n", [{Prod, Path}]),
	{Prod, Path}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_, _State) ->
	ok.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
