# access [![License][license-img]][license]

A library for generic access and update functionality on nested data structures in Erlang.

## Overview

`access` provides a convenient way to work with deeply nested Erlang data structures, including maps and lists. It allows you to:

- Get values from nested structures
- Find values with safe error handling
- Update values in nested structures
- Remove elements from nested structures
- Operate on all elements of a collection

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
access:get([access:all()], [1, 2, 3]).  % Returns [1, 2, 3]
access:update([access:all()], 0, [1, 2, 3]).  % Returns [0, 0, 0]
```

## Status

This library is currently under development and the API may change.

## License

This project uses the [MIT License][license].

[license]: LICENSE.md
[license-img]: https://img.shields.io/badge/license-MIT-blue.svg
