# access [![License][license-img]][license]

A library for generic access and update functionality on nested data structures
in Erlang.

## Overview

`access` provides a convenient way to work with deeply nested Erlang data
structures, including maps and lists. It allows you to:

- Get values from nested structures
- Find values with safe error handling
- Update values in nested structures
- Remove elements from nested structures
- Operate on all elements of a collection

A path is a list of keys or indices that specify the location of a value within
a nested structure. An integer is assumed to be an index into a list, and any
other term is assumed to be a key into a map. A map containing keys that are
matched to specific types can be explicitly accessed using the `{key, Type}`
syntax (e.g., `{key, 1}` in case `1` is a key or '{key, {key, X}}' in case
`{key, X}` is a key).

An integer is explicitly assumed to be an index into a list, so that the error
`{badvalue, Path, Value}` can be raised if `Value` is not a list.

## Usage Examples

```erlang
% Getting a nested value
Map = #{a => #{b => 1}},
access:get([a, b], Map).  % Returns 1

% Safe access with find
access:find([a, missing], Map).  % Returns error

% Updating a nested value
NewMap = access:update([a, b], 2, Map),  % Returns #{a => #{b => 2}}

% Removing a nested value
access:remove([a, b], Map).  % Returns #{a => #{}}

% Working with lists
List = [1, [2, 3], 4],
access:get([2, 1], List).  % Returns 2

% Using the all() selector
access:get([access:all()], [a, b, c]).  % Returns [a, b, c]
access:update([access:all()], x, [a, b, c]).  % Returns [x, x, x]
access:remove([access:all()], #{a => 1, b => 2}).  % Returns #{}
access:remove([a, access:all()], #{a => [1, 2, 3]}).  % Returns #{a => []}

% Explicit path types can be used
access:get([{key, 1}], #{1 => a}) % Returns a
access:get([{key, {key, x}}], #{{key, x} => a}) % Returns a
access:get([{index, 2}], [a, b, c]) % Returns b
```

## Status

This library is currently under development and the API may change.

## License

This project uses the [MIT License][license].

[license]: LICENSE.md
[license-img]: https://img.shields.io/badge/license-MIT-blue.svg
