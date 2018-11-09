# Changelog
All the changes for Alta's primary (and currently only) backend, the C transpiler, will be kept in this file.

This project follows [semantic versioning](https://semver.org).

## [Unreleased]
Nothing yet.

## [0.0.0] - 2018-11-08
### Added
  * Function and variable name mangling
    * Mangled function names include parameter types so that function overloading can be easily implemented in the future without much fuss on the backend
  * `return` support
  * Function definition support
    * Automatic function declaration in headers
  * Alta type to C type translation
    * Alta's `byte` is basically equivalent to C's `char`, and `int` is the same for both
    * `const` and `ptr` translation, as well
    * `ref`s are translted to pointers just like `ptr`
  * Variable definition and retrieval support
  * Theoretical accessor support
    * Theoretical because accessors aren't a thing in Alta yet, so we have no way of testing it
    * Theoretically supported, though, because it *should* work as-is when accessors are added to Alta
  * Integer literal translation