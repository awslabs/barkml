## 0.6.8

Changes:

* Moves the Lexer and Parser to keep track of line and column
* Updates StandardLoader to parse each module by itself for filename reporting

## 0.6.7

Fixes:

* Multiline strings were not properly lexing

## 0.6.6

Fixes:

* Comments and Labels weren't propagated to a few value types
* Null was not being parsed nor reserved

## 0.6.5

Version is bumping from a .0 -> .5 because there are no major changes in the language syntax but the lexer/parser has
been moved away from peg.

Changes:

* Implements a new token lexer using Logos
* New Recursive descent parser
* Improved multi-file handling as a single file in Multi mode
* Removes need for statements to be new-line delimited
* Commas are now optional in arrays and tables
* All assignment identifiers can be an identifier (no collision with a keyword) or a string
* Reserves keywords for future use

## 0.6.0

Features:

* Reworks value structure to track id, label and even value type in a better way.
* Adds typing system and type hint to the language
* Upgrades macro resolution to a full scope system

## 0.5.2

Fixes:

* Switch StandardLoader to use BTreeMap so macro resolution doesn't indeterministically fail on module order pre-merge
  or append

## 0.5.1

Fixes:

* read_dir causing inconsistent resolution of macros with multiple configuration files

## 0.5.0

Features:

* Added Module type

> Files in barkml are now loaded into a parent Value known as a Module. This allows better handling for multiple
> file loaders

* Moved definition of a configuration loader to a trait
* Added Semantic Version and Version Requirement types and support

> Users can now define standard semantic versions in their configuration files as well
> as version requirements

## 0.4.1

Fixes:

* Fix import issue in lib.rs
* Remove unimplemented feature

## 0.3.0 - 0.4.0

Features:

* Added config loader construct

> The Loader builder gives users the ability to load configuration files with more control. It also
> introduces an ability to load and merge multiple configuration files in a directory

* Added precision numeral values

> Users can now define a precision for integers using standard rust suffixes (u8, i8, u16, i16, u32, i32, u64, i64).
> Users can also define a precision for floating point numbers using suffixes (f32, f64)

Changes:

* Merged Value and Statement into one enum for easier use

## 0.2.0 - Binary encoding support

Added feature 'binary' that implements a conversion layer
to encode BarkML into a binary representation through the use
of `MessagePack`

## 0.1.0 - Initial Release

Initial release of barkml.
