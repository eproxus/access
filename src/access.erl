-module(access).

% API
-export([get/2]).
-export([find/2]).
-export([update/3]).
-export([get_and_update/3]).
-export([remove/2]).
-export([merge/1]).
-export([merge/2]).
% Selectors
-export([all/0]).

% -compile([{inline, [retrieve/5, selector/1, map_get/1, list_get/1]}]).

%--- API -----------------------------------------------------------------------

get(Path, Data) ->
    % traverse(get, Path, Data, fun(V) -> V end, fun badvalue/2).
    retrieve(get, Path, Data, fun(V) -> V end, fun badvalue/2).

find(Path, Data) ->
    retrieve(get, Path, Data, fun(V) -> {ok, V} end, fun(_V, _P) -> error end).

update(Path, Value, Data) ->
    {_, New} = retrieve(
        get_and_update,
        Path,
        Data,
        fun(Current) -> {Current, Value} end,
        fun badvalue/2
    ),
    New.

get_and_update(Path, Value, Data) ->
    retrieve(
        get_and_update,
        Path,
        Data,
        fun(Current) -> {Current, Value} end,
        fun badvalue/2
    ).

remove(Path, Data) ->
    {_, New} = retrieve(
        get_and_update,
        Path,
        Data,
        fun(_Current) -> pop end,
        fun badvalue/2
    ),
    New.

merge(Data) -> merge(Data, #{}).

merge([Data | Rest], Opts) ->
    lists:foldl(fun(Right, Left) -> merge(Left, Right, Opts) end, Data, Rest).

merge(Left, Right, Opts) ->
    Default = #{strategy => #{}},
    traverse(Left, Right, fun(X) -> {todo, X} end, maps:merge(Default, Opts)).

%--- Selectors -----------------------------------------------------------------

map_get(Key) ->
    fun
        (get, #{Key := Value}, Fun) ->
            {ok, Fun(Value)};
        (get, _, _Fun) ->
            % Either the key is not found or the data is not a map
            error;
        (get_and_update, #{Key := Value} = Map, Fun) ->
            case Fun(Value) of
                {Old, New} -> {Old, maps:put(Key, New, Map)};
                pop -> {Value, maps:remove(Key, Map)};
                _Else -> error({badfun, _Else, Fun})
            end;
        (get_and_update, _Map, _Fun) ->
            error
    end.

map_merge(default, Left, Right, Fun) ->
    map_merge(merge, Left, Right, Fun);
map_merge(merge, Left, Right, Fun) when is_map(Left), is_map(Right) ->
    maps:merge_with(Fun, Left, Right);
map_merge(intersect, Left, Right, Fun) when is_map(Left), is_map(Right) ->
    maps:intersect_with(Fun, Left, Right);
map_merge(_Strategy, _Left, Right, _Fun) ->
    Right.

list_get(Index) when is_integer(Index) ->
    fun
        (get, List, Fun) when is_list(List), Index =< length(List) ->
            {ok, Fun(lists:nth(Index, List))};
        (get, _Data, _Fun) ->
            % Either the index is out of bounds or the data is not a list
            error;
        (get_and_update, List, Fun) when is_list(List), abs(Index) > 0, abs(Index) < length(List) ->
            Value = list_get(Index, List),
            case Fun(Value) of
                {Old, New} -> {Old, list_replace(Index, New, List)};
                pop -> {Value, list_remove(Index, List)};
                _Wat -> error({badfun, _Wat, Fun})
            end;
        (get_and_update, _List, _Fun) ->
            error
    end.

list_merge(default, Left, Right, Fun) ->
    list_merge(append, Left, Right, Fun);
list_merge(Strategy, Left, Right, _Fun) when Strategy == merge; Strategy == append ->
    lists:append(Left, Right);
list_merge(intersect, Left, Right, _Fun) ->
    sets:to_list(sets:intersection(sets:from_list(Left), sets:from_list(Right)));
list_merge(_Strategy, _Left, Right, _Fun) ->
    Right.

all() ->
    fun
        (get, List, Next) when is_list(List) ->
            {ok, lists:map(Next, List)};
        (get, Map, Next) when is_map(Map) ->
            {ok, lists:map(Next, maps:values(Map))};
        (get, Value, Next) ->
            {ok, Next(Value)};
        (get_and_update, List, Next) when is_list(List) ->
            {
                List,
                lists:filtermap(
                    fun(Value) ->
                        case Next(Value) of
                            {_Old, New} -> {true, New};
                            pop -> false
                        end
                    end,
                    List
                )
            };
        (get_and_update, Map, Next) when is_map(Map) ->
            {
                maps:values(Map),
                maps:filtermap(
                    fun(_Key, Value) ->
                        case Next(Value) of
                            {_Old, New} -> {true, New};
                            pop -> false
                        end
                    end,
                    Map
                )
            }
    end.

%--- Internal ------------------------------------------------------------------

retrieve(Op, Path, Data, Present, Missing) ->
    retrieve(Op, Path, Data, Present, Missing, []).

retrieve(_Op, [], Data, Present, _Missing, _Acc) ->
    Present(Data);
retrieve(Op, [Key | Path], Data, Present, Missing, Acc) ->
    Fun = selector(Key),
    Next = fun(V) -> retrieve(Op, Path, V, Present, Missing, [Key | Acc]) end,
    case {Op, Fun(Op, Data, Next)} of
        {get, {ok, Value}} -> Value;
        {get_and_update, {Old, Value}} -> {Old, Value};
        {_, error} -> Missing(Data, lists:reverse([Key | Acc]))
    end;
retrieve(_Op, Path, Data, _Present, _Missing, _Acc) ->
    error({badpath, Path, Data}).

traverse(Left, Right, Fun, Opts) -> traverse(Left, Right, Fun, [], Opts).

traverse(Left, Right, Fun, Path, Opts) ->
    Merger = merger(Path, Left, Opts),
    Traverse = fun(K, L, R) ->
        traverse(L, R, Fun, [K | Path], Opts)
    end,
    Merger(Left, Right, Traverse).

selector(Key) when is_function(Key, 3) -> Key;
selector({index, Index}) when is_integer(Index) -> list_get(Index);
selector(Index) when is_integer(Index) -> list_get(Index);
selector({key, Key}) -> map_get(Key);
% The default is map access
selector(Key) -> map_get(Key).

merger(Path, Term, #{strategy := Strategies}) ->
    case maps:get(Path, Strategies, default) of
        Strategy when is_function(Strategy, 3) ->
            Strategy;
        Strategy when is_atom(Strategy) ->
            case Term of
                _ when is_map(Term) ->
                    fun(Left, Right, Fun) -> map_merge(Strategy, Left, Right, Fun) end;
                _ when is_list(Term) ->
                    fun(Left, Right, Fun) -> list_merge(Strategy, Left, Right, Fun) end;
                _ ->
                    fun(_Left, Right, _Fun) -> Right end
            end
    end.

badvalue(V, P) -> error({badvalue, P, V}).

list_get(Index, List) when Index > 0 ->
    lists:nth(Index, List).

list_replace(Index, Value, List) -> list_modify(replace, Index, 1, Value, List).

list_remove(Index, List) -> list_modify(remove, Index, 1, [], List).

list_modify(replace, Index, Index, Value, [_Head | Tail]) ->
    [Value | Tail];
list_modify(remove, Index, Index, _Value, [_Head | Tail]) ->
    Tail;
list_modify(Op, Index, Pos, Value, [Head | Tail]) ->
    [Head | list_modify(Op, Index, Pos + 1, Value, Tail)].
