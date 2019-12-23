# Changelog
All the changes for Alta's primary (and currently only) backend, the C transpiler, will be kept in this file.

This project follows [semantic versioning](https://semver.org).

## [Unreleased]
 * Many, MANY changes that are TBD (to-be-documented)
 * Output class typedef definitions before the actual structure definitions (to allow them to store pointers to themselves)

## [0.8.0] - 2018-12-20
### Added
  * `void` type support added
  * New `CTranspiler.vararg` attribute for parameters
    * This attribute lets Talta know that the annotated parameter is a C-style vararg parameter and that it should format arguments for this parameter accordingly
    * This is used, for example, in the declaration for `printf`:
      * `declare function printf(string: ptr byte, `**`@CTranspiler.vararg`**` data: any...): int`
      * This tells Alta that instead of passing arguments to printf like this:
        * `printf("some %d string", (any[]){ 3 })`
        * Which, by the way, is invalid C code because of the `any` type
      * ...it should pass them like this:
        * `printf("some %d string", 3)`
### Changed
  * Attributes are now executed at detail time
    * **Note**: this change was made in order to comply with the corresponding change in AltaCore
    * `CTranspiler.include` has been updated accordingly, and the new attribute (`CTranspiler.vararg`) also implements this behavior
### Updated
  * [AltaCore v0.10.0](https://github.com/alta-lang/alta-core/blob/v0.10.0/CHANGELOG.md#0100---2018-12-20)
  * [Ceetah v0.5.0](https://github.com/alta-lang/ceetah/blob/v0.5.0/CHANGELOG.md#050---2018-12-20)
### Additional Release Notes
  * We now have a policy of changelog modularization, and this is in effect for the various subprojects
    * In other words, refer to the changelogs of updated dependencies for additional changes
  * CI integration has been added for automated builds (and automated testing once tests are added)

## [0.7.1] - 2018-12-03
### Changed
  * Build artificats are now put into their own folders in the build directory: `bin` for executables and `lib` for libraries
### Updated
  * AltaCore v0.9.0
  * Ceetah v0.4.1

## [0.7.0] - 2018-12-01
### Added
  * General attribute support
  * `CTranspiler` attribute domain
    * BTW, `[AttributeString]` = `const ptr const byte`, which is a string literal
    * Currently, it contains the following attributes:
      * `include(header: [AttributeString])` = Instructs the C transpiler to include the specified `header` in the current module's header output
### Updated
  * AltaCore v0.8.0

## [0.6.0] - 2018-11-28
### Added
  * String literal support
### Updated
  * AltaCore v0.7.0

## [0.5.0] - 2018-11-27
### Added
  * Namespace support
### Updated
  * AltaCore v0.6.0

## [0.4.0] - 2018-11-22
### Added
  * Package versions are now included as part of mangled module names
  * Hoisted types support
    * Currently only necessary for function-pointer types
  * Function calls can now be transpiled!
### Updated
  * AltaCore v0.5.1

## [0.3.0] - 2018-11-18
### Added
  * Function-pointer type support
### Updated
  * AltaCore v0.4.0
  * Ceetah v0.3.0

## [0.2.2] - 2018-11-18
### Fixed
  * Mangle function parameter names
    * Previously, they were left unmangled
### Updated
  * AltaCore v0.3.1

## [0.2.1] - 2018-11-18
### Updated
  * AltaCore v0.3.0

## [0.2.0] - 2018-11-13
### Added
  * Boolean transpilation
    * Both boolean literals and the boolean type
  * Binary operations (`+`, `-`, `*`, `/`)
  * Module importation!
    * Full support for cherry-pick imports, theoretical support for alias imports
### Updated
  * AltaCore v0.2.0
  * Ceetah v0.2.0

## [0.1.0] - 2018-11-11
### Added
  * Assignment expressions are here!
  * We now use `shared_ptr`s in some places because Ceetah and AltaCore have switched to using them instead of raw pointers
    * We didn't completely switch because we can use raw pointers in places where we don't allocate new objects or keep pointers to objects (although we *could* fully switch if we wanted to)
### Updated
  * Ceetah v0.1.0
  * AltaCore v0.1.0

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
