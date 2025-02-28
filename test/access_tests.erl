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
        ?_assertEqual(1, access:get([nested, a], ?MAP(?MAP))),
        ?_assertEqual(a, access:get([1], ?LIST)),
        ?_assertEqual(a, access:get([3, 1], ?LIST(?LIST))),
        ?_assertEqual(a, access:get([nested, 1], ?MAP(?LIST))),
        ?_assertEqual(a, access:get([1], ?LIST)),
        ?_assertEqual(1, access:get([3, a], ?LIST(?MAP))),
        ?_assertEqual(1, access:get([3, a], ?LIST(?MAP))),
        ?_assertEqual(a, access:get([nested, 1], ?MAP(?LIST)))
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
            {badvalue, [nested, x], ?MAPP}, access:get([nested, x], ?MAP(?MAP))
        ),
        ?_assertError(
            {badvalue, [nested, 4], ?LIST}, access:get([nested, 4], ?MAP(?LIST))
        ),
        ?_assertError(
            {badvalue, [3, x], ?MAPP}, access:get([3, x], ?LIST(?MAP))
        ),
        ?_assertError(
            {badvalue, [3, 4], ?LIST}, access:get([3, 4], ?LIST(?LIST))
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
        % FIXME: Should this return [a, b, [a, b, c], c] (i.e. inclusive) or [[a, b, c]] (i.e. filtering)?
        % ?_assertEqual([?LIST], access:get([access:all(), access:all()], ?LIST(?LIST)))

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
        ?_assertEqual(?MAP([]), access:remove([nested, access:all()], ?MAP(?LIST))),
        ?_assertEqual(?LIST(#{}), access:remove([3, access:all()], ?LIST(?MAP)))
    ].
