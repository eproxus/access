-module(access).

% API
-export([get/2]).
-export([find/2]).
-export([update/3]).
-export([remove/2]).
% Selectors
-export([all/0]).

-compile([{inline, [traverse/5, selector/1, key/1, index/1]}]).

%--- API -----------------------------------------------------------------------

get(Path, Data) ->
    % traverse(get, Path, Data, fun(V) -> V end, fun badvalue/2).
    traverse(get, Path, Data, fun(V) -> V end, fun badvalue/2).

find(Path, Data) ->
    traverse(get, Path, Data, fun(V) -> {ok, V} end, fun(_V, _P) -> error end).

update(Path, Value, Data) ->
    {_, New} = traverse(
        get_and_update,
        Path,
        Data,
        fun(Current) -> {Current, Value} end,
        fun badvalue/2
    ),
    New.

remove(Path, Data) ->
    {_, New} = traverse(
        get_and_update,
        Path,
        Data,
        fun(_Current) -> pop end,
        fun badvalue/2
    ),
    New.

%--- Selectors -----------------------------------------------------------------

key(Key) ->
    fun
        (get, #{Key := Value}, Present) ->
            {ok, Present(Value)};
        (get, _, _Fun) ->
            % Either the key is not found or the data is not a map
            error;
        (get_and_update, #{Key := Value} = Map, Present) ->
            case Present(Value) of
                {Value, New} -> {Map, maps:put(Key, New, Map)};
                pop -> {Map, maps:remove(Key, Map)};
                _Else -> error({badfun, _Else, Present})
            end;
        (get_and_update, _Map, _Fun) ->
            error;
        (_Else, _Data, _Next) ->
            error
    end.

index(Index) when is_integer(Index) ->
    fun
        (get, List, Present) when is_list(List), Index =< length(List) ->
            {ok, Present(lists:nth(Index, List))};
        (get, _Data, _Fun) ->
            % Either the index is out of bounds or the data is not a list
            error;
        (get_and_update, List, Present) when is_list(List) ->
            Value = lists:nth(Index, List),
            case Present(Value) of
                {Value, New} -> {List, list_replace(Index, New, List)};
                pop -> {List, list_remove(Index, List)};
                _Wat -> error({badfun, _Wat, Present})
            end
    end.

all() ->
    fun
        (get, List, Next) when is_list(List) ->
            {ok, lists:map(Next, List)};
        (get, Map, Next) when is_map(Map) ->
            {ok, lists:map(Next, maps:values(Map))};
        (get_and_update, List, Next) when is_list(List) ->
            {List,
                lists:filtermap(
                    fun(Value) ->
                        case Next(Value) of
                            {Value, New} -> {true, New};
                            pop -> false
                        end
                    end,
                    List
                )};
        (get_and_update, Map, Next) when is_map(Map) ->
            {Map,
                maps:filtermap(
                    fun(_Key, Value) ->
                        case Next(Value) of
                            {Value, New} -> {true, New};
                            pop -> false
                        end
                    end,
                    Map
                )}
    end.

%--- Internal ------------------------------------------------------------------

traverse(Op, Path, Data, Present, Missing) ->
    traverse(Op, Path, Data, Present, Missing, []).

traverse(_Op, [], Data, Present, _Missing, _Acc) ->
    Present(Data);
traverse(Op, [Key | Path], Data, Present, Missing, Acc) ->
    Fun = selector(Key),
    case
        {Op,
            Fun(Op, Data, fun(V) ->
                traverse(Op, Path, V, Present, Missing, [Key | Acc])
            end)}
    of
        {get, {ok, Value}} -> Value;
        {get_and_update, {Old, Value}} -> {Old, Value};
        {_, error} -> Missing(Data, lists:reverse([Key | Acc]))
    end;
traverse(_Op, Path, Data, _Present, _Missing, _Acc) ->
    error({badpath, Path, Data}).

selector(Key) when is_function(Key, 3) -> Key;
selector({index, Index}) when is_integer(Index) -> index(Index);
selector(Index) when is_integer(Index) -> index(Index);
selector({key, Key}) -> key(Key);
% The default is map access
selector(Key) -> key(Key).

badvalue(V, P) -> error({badvalue, P, V}).

list_replace(Index, Value, List) -> list_modify(replace, Index, 1, Value, List).

list_remove(Index, List) -> list_modify(remove, Index, 1, [], List).

list_modify(replace, Index, Index, Value, [_Head | Tail]) ->
    [Value | Tail];
list_modify(remove, Index, Index, _Value, [_Head | Tail]) ->
    Tail;
list_modify(Op, Index, Pos, Value, [Head | Tail]) ->
    [Head | list_modify(Op, Index, Pos + 1, Value, Tail)].
