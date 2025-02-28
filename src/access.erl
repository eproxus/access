-module(access).

% API
-export([get/2]).
-export([find/2]).
-export([update/3]).
-export([remove/2]).
% Selectors
-export([all/0]).

-compile([{inline, [get/4, get_and_update/4, selector/2, key/1, index/1]}]).

%--- API -----------------------------------------------------------------------

get(Path, Data) ->
    % traverse(get, Path, Data, fun(V) -> V end, fun badvalue/2).
    get(Path, Data, fun(V) -> V end, fun badvalue/2).

find(Path, Data) ->
    get(Path, Data, fun(V) -> {ok, V} end, fun(_V, _P) -> error end).

update(Path, Value, Data) ->
    {_, New} = get_and_update(
        Path, Data, fun(Current) -> {Current, Value} end, fun badvalue/2
    ),
    New.

remove(Path, Data) ->
    {_, New} = get_and_update(
        Path, Data, fun(_Current) -> pop end, fun badvalue/2
    ),
    New.

%--- Selectors -----------------------------------------------------------------

key(Key) ->
    fun
        (get, #{Key := Value}, Present) ->
            {ok, Present(Value)};
        (get, #{}, _Fun) ->
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
        (get, List, _Fun) when is_list(List) ->
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
            {List, lists:map(Next, List)};
        (get, Map, Next) when is_map(Map) ->
            {Map, lists:map(Next, maps:values(Map))};
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

get(Path, Data, Present, Missing) ->
    get(Path, Data, Present, Missing, []).

get([], Data, Present, _Missing, _Acc) ->
    Present(Data);
get([Key | Path], Data, Present, Missing, Acc) ->
    Fun = selector(Key, Data),
    case Fun(get, Data, fun(V) -> get(Path, V, Present, Missing, [Key | Acc]) end) of
        {_Old, Value} -> Value;
        error -> Missing(Data, lists:reverse([Key | Acc]))
    end;
get(Path, Data, _Present, _Missing, _Acc) ->
    error({badpath, Path, Data}).

get_and_update(Path, Data, Present, Missing) ->
    get_and_update(Path, Data, Present, Missing, []).

get_and_update([], Data, Present, _Missing, _Acc) ->
    Present(Data);
get_and_update([Key | Path], Data, Present, Missing, Acc) ->
    Fun = selector(Key, Data),
    case
        Fun(get_and_update, Data, fun(V) ->
            get_and_update(Path, V, Present, Missing, [Key | Acc])
        end)
    of
        {Old, New} -> {Old, New};
        error -> Missing(Data, lists:reverse([Key | Acc]))
    end;
get_and_update(Path, Data, _Present, _Missing, _Acc) ->
    error({badpath, Path, Data}).

selector(Key, Data) ->
    Fun =
        case {Key, Data} of
            {Key, Data} when is_function(Key, 3) -> Key;
            {Key, Data} when is_integer(Key), is_list(Data) -> index(Key);
            % FIXME: Return fun that just returns `error` instead?
            _Else -> key(Key)
        end,
    Fun.

badvalue(V, P) -> error({badvalue, P, V}).

list_replace(Index, Value, List) -> list_modify(replace, Index, 1, Value, List).

list_remove(Index, List) -> list_modify(remove, Index, 1, [], List).

list_modify(replace, Index, Index, Value, [_Head | Tail]) ->
    [Value | Tail];
list_modify(remove, Index, Index, _Value, [_Head | Tail]) ->
    Tail;
list_modify(Op, Index, Pos, Value, [Head | Tail]) ->
    [Head | list_modify(Op, Index, Pos + 1, Value, Tail)].
