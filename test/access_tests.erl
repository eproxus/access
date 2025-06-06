-module(access_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MAP, #{a => 1, b => 2, c => 3}).
-define(MAPP, #{a := 1, b := 2, c := 3}).
-define(MAP(Nested), maps:merge(?MAP, #{nested => Nested})).

-define(LIST, [a, b, c]).
-define(LIST(Nested), [a, b, Nested, c]).

%--- Tests ---------------------------------------------------------------------

get_nested_test_() ->
    [
        % Maps
        ?_assertEqual(1, access:get([nested, a], ?MAP(?MAP))),
        % Lists
        ?_assertEqual(a, access:get([1], ?LIST)),
        ?_assertEqual(a, access:get([3, 1], ?LIST(?LIST))),
        ?_assertEqual(a, access:get([nested, 1], ?MAP(?LIST))),
        ?_assertEqual(a, access:get([1], ?LIST)),
        % Mixed maps and lists
        ?_assertEqual(1, access:get([3, a], ?LIST(?MAP))),
        ?_assertEqual(1, access:get([3, a], ?LIST(?MAP))),
        ?_assertEqual(a, access:get([nested, 1], ?MAP(?LIST)))
    ].

path_explicit_key_test_() ->
    Map = #{1 => #{{key, x} => a}},
    Path = [{key, 1}, {key, {key, x}}],
    [
        ?_assertEqual(a, access:get(Path, Map)),
        ?_assertEqual({ok, a}, access:find(Path, Map)),
        ?_assertEqual(#{1 => #{{key, x} => x}}, access:update(Path, x, Map)),
        ?_assertEqual(#{1 => #{}}, access:remove(Path, Map)),
        ?_assertError(
            {badvalue, [{key, 1}], ?LIST},
            access:get([{key, 1}], ?LIST)
        )
    ].

path_explicit_index_test_() ->
    List = ?LIST(?LIST),
    [
        ?_assertEqual(a, access:get([{index, 3}, {index, 1}], List)),
        ?_assertEqual({ok, a}, access:find([{index, 3}, {index, 1}], List)),
        ?_assertEqual(
            ?LIST([x, b, c]),
            access:update([{index, 3}, {index, 1}], x, List)
        ),
        ?_assertEqual(
            ?LIST([b, c]),
            access:remove([{index, 3}, {index, 1}], List)
        ),
        ?_assertError(
            {badvalue, [{index, 1}], ?MAPP}, access:get([{index, 1}], ?MAP)
        )
    ].

get_errors_test_() ->
    Fun = access:all(),
    [
        % Paths
        ?_assertError({badpath, x, #{}}, access:get(x, #{})),
        ?_assertError({badpath, Fun, #{}}, access:get(Fun, #{})),
        % Flat
        ?_assertError({badvalue, [x], ?MAPP}, access:get([x], ?MAP)),
        ?_assertError({badvalue, [4], ?LIST}, access:get([4], ?LIST)),
        ?_assertError({badvalue, [4], ?MAPP}, access:get([4], ?MAP)),
        % Nested
        ?_assertError(
            {badvalue, [nested, x], ?MAPP},
            access:get([nested, x], ?MAP(?MAP))
        ),
        ?_assertError(
            {badvalue, [nested, 4], ?LIST},
            access:get([nested, 4], ?MAP(?LIST))
        ),
        ?_assertError(
            {badvalue, [3, x], ?MAPP},
            access:get([3, x], ?LIST(?MAP))
        ),
        ?_assertError(
            {badvalue, [3, 4], ?LIST},
            access:get([3, 4], ?LIST(?LIST))
        )
    ].

get_all_test_() ->
    [
        ?_assertEqual(
            lists:sort([1, 2, 3]),
            lists:sort(access:get([access:all()], ?MAP))
        ),
        ?_assertEqual(?LIST, access:get([access:all()], ?LIST)),
        ?_assertEqual(?LIST, access:get([nested, access:all()], ?MAP(?LIST))),
        ?_assertEqual(
            [?LIST, ?LIST],
            access:get([access:all(), access:all()], #{a => ?LIST, b => ?LIST})
        ),
        ?_assertEqual(?LIST(?LIST), access:get([access:all(), access:all()], ?LIST(?LIST))),
        ?_assertEqual(
            [1, 1, 2, 2, 3, 3],
            lists:sort(lists:flatten(access:get([access:all(), access:all()], ?MAP(?MAP))))
        ),
        ?_assertEqual([2, 2], access:get([access:all(), b], [?MAP, ?MAP]))
    ].

find_test_() ->
    % Tests with basic keys for access:find(...) that returns {ok, Value} or false:
    [
        % {ok, Value}
        ?_assertEqual({ok, 1}, access:find([a], ?MAP)),
        ?_assertEqual({ok, b}, access:find([2], ?LIST)),
        ?_assertEqual({ok, b}, access:find([nested, 2], ?MAP(?LIST))),
        ?_assertEqual({ok, 2}, access:find([3, b], ?LIST(?MAP))),
        % error
        ?_assertEqual(error, access:find([x], ?MAP)),
        ?_assertEqual(error, access:find([4], ?LIST)),
        ?_assertEqual(error, access:find([nested, 4], ?MAP(?LIST))),
        ?_assertEqual(error, access:find([3, x], ?LIST(?MAP)))
    ].

update_test_() ->
    [
        ?_assertEqual(
            maps:merge(?MAP, #{b => -1}), access:update([b], -1, ?MAP)
        ),
        ?_assertEqual(
            [a, x, c],
            access:update([2], x, ?LIST)
        ),
        ?_assertEqual(
            ?MAP([a, x, c]),
            access:update([nested, 2], x, ?MAP(?LIST))
        ),
        ?_assertEqual(
            ?LIST(maps:merge(?MAP, #{b => -1})),
            access:update([3, b], -1, ?LIST(?MAP))
        ),
        ?_assertEqual(
            ?LIST([a, x, c]),
            access:update([3, 2], x, ?LIST(?LIST))
        )
    ].

update_error_test_() ->
    Fun = access:all(),
    [
        % Invalid path
        ?_assertError({badpath, x, #{}}, access:update(x, unused, #{})),
        ?_assertError({badpath, Fun, #{}}, access:update(Fun, unused, #{})),
        % Nested
        ?_assertError(
            {badvalue, [3, x], ?MAPP}, access:update([3, x], -1, ?LIST(?MAP))
        ),
        % TODO: Replace badmap?
        ?_assertError(
            {badvalue, [2, x], b}, access:update([2, x], -1, ?LIST(?MAP))
        )
    ].

update_all_test_() ->
    [
        ?_assertEqual(
            [x, x, x],
            access:update([access:all()], x, ?LIST)
        ),
        ?_assertEqual(
            [#{a => -1}, #{a => -1}, #{a => -1}],
            access:update(
                [access:all(), a],
                -1,
                [#{a => 1}, #{a => 2}, #{a => 3}]
            )
        ),
        ?_assertEqual(
            #{a => -1, b => -1, c => -1},
            access:update([access:all()], -1, ?MAP)
        )
    ].

get_and_update_test_() ->
    [
        % Basic map tests
        ?_assertEqual(
            {1, maps:merge(?MAP, #{a => -1})},
            access:get_and_update([a], -1, ?MAP)
        ),
        ?_assertEqual(
            {2, maps:merge(?MAP, #{b => -1})},
            access:get_and_update([b], -1, ?MAP)
        ),

        % Basic list tests
        ?_assertEqual(
            {a, [x, b, c]},
            access:get_and_update([1], x, ?LIST)
        ),
        ?_assertEqual(
            {b, [a, x, c]},
            access:get_and_update([2], x, ?LIST)
        ),

        % Nested tests with maps and lists
        ?_assertEqual(
            {1, ?MAP(maps:merge(?MAP, #{a => -1}))},
            access:get_and_update([nested, a], -1, ?MAP(?MAP))
        ),
        ?_assertEqual(
            {a, ?MAP([x, b, c])},
            access:get_and_update([nested, 1], x, ?MAP(?LIST))
        ),
        ?_assertEqual(
            {1, ?LIST(maps:merge(?MAP, #{a => -1}))},
            access:get_and_update([3, a], -1, ?LIST(?MAP))
        ),
        ?_assertEqual(
            {a, ?LIST([x, b, c])},
            access:get_and_update([3, 1], x, ?LIST(?LIST))
        ),

        % Explicit key and index tests
        ?_assertEqual(
            {1, maps:merge(?MAP, #{a => -1})},
            access:get_and_update([{key, a}], -1, ?MAP)
        ),
        ?_assertEqual(
            {a, [x, b, c]},
            access:get_and_update([{index, 1}], x, ?LIST)
        ),

        % all accessor tests - note that map values may be returned in any order
        ?_test(begin
            {Values, Updated} = access:get_and_update([access:all()], -1, ?MAP),
            ?assertEqual(lists:sort([1, 2, 3]), lists:sort(Values)),
            ?assertEqual(#{a => -1, b => -1, c => -1}, Updated)
        end),
        ?_assertEqual(
            {?LIST, [-1, -1, -1]},
            access:get_and_update([access:all()], -1, ?LIST)
        ),

        % Error cases
        ?_assertError({badvalue, [x], ?MAPP}, access:get_and_update([x], -1, ?MAP)),
        ?_assertError({badvalue, [4], ?LIST}, access:get_and_update([4], -1, ?LIST)),
        ?_assertError({badpath, x, #{}}, access:get_and_update(x, -1, #{}))
    ].

remove_test_() ->
    [
        ?_assertEqual(maps:without([a], ?MAP), access:remove([a], ?MAP)),
        ?_assertEqual([a, c], access:remove([2], ?LIST)),
        ?_assertEqual(?MAP([a, c]), access:remove([nested, 2], ?MAP(?LIST))),
        ?_assertEqual(
            ?LIST(maps:without([a], ?MAP)), access:remove([3, a], ?LIST(?MAP))
        )
    ].

remove_all_test_() ->
    [
        ?_assertEqual(#{}, access:remove([access:all()], ?MAP)),
        ?_assertEqual([], access:remove([access:all()], ?LIST)),
        ?_assertEqual(
            ?MAP([]), access:remove([nested, access:all()], ?MAP(?LIST))
        ),
        ?_assertEqual(?LIST(#{}), access:remove([3, access:all()], ?LIST(?MAP)))
    ].

merge_test_() ->
    [
        ?_assertEqual(
            #{a => 1, b => -1, c => #{a => 1, b => -1, c => 3}, d => 4},
            access:merge([
                #{a => 1, b => 2, c => #{a => 1, b => 2}},
                #{b => -1, c => #{b => -1, c => 3}, d => 4}
            ])
        ),
        ?_assertEqual(
            [a, b, c, 1, 2, 3],
            access:merge([[a, b, c], [1, 2, 3]])
        ),
        ?_assertEqual(#{}, access:merge([[], #{}])),
        ?_assertEqual([], access:merge([#{}, []])),
        ?_assertEqual(
            [1, 2, 3],
            access:merge([[a, b, c], [1, 2, 3]], #{strategy => #{[] => replace}})
        ),
        ?_assertEqual(
            #{b => 2},
            access:merge([#{a => 1}, #{b => 2}], #{strategy => #{[] => replace}})
        ),
        ?_assertEqual(
            #{
                a => [1],
                b => #{
                    x => #{v => 2, w => 3}
                },
                c => 3
            },
            access:merge(
                [
                    #{
                        a => #{v => 1},
                        b => #{
                            x => #{v => 1, w => 3},
                            y => 2
                        },
                        c => 3
                    },
                    #{
                        a => [1],
                        b => #{
                            x => #{v => 2},
                            z => 3
                        },
                        c => 3
                    }
                ],
                #{strategy => #{[b] => intersect}}
            )
        )
    ].

merge_list_intersect_test_() ->
    [
        ?_assertEqual(
            [b],
            access:merge([[a, b, c], [1, b, 2]], #{strategy => #{[] => intersect}})
        )
        % ?_assertEqual(
        %     [#{a => 2}],
        %     access:merge([[#{a => 1}, #{b => 2}], [#{b => 2}, #{a => 2}]], #{strategy => #{[access:all()] => intersect}})
        % )
    ].

merge_custom_fun_test_() ->
    [
        ?_assertEqual(
            [],
            access:merge([[], []], #{strategy => #{[] => fun(A, B, _Fun) -> lists:append(A, B) end}})
        ),
        ?_assertEqual(
            [1, 2, 3, a, b, c],
            access:merge([[1, 2, 3], [a, b, c]], #{
                strategy => #{[] => fun(A, B, _Fun) -> lists:append(A, B) end}
            })
        )
    ].
