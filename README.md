## BarkML

BarkML is a declarative configuration format that is inspired by other languages shuch as toml, hcl and more. It was
created initially to be used with operational tools and generative tooling. The language defaults to utf-8 parsing and
the language supports self-referential macro replacements.

## Features

default_features = [] no features are default

* **full** All features listed below
* **binary** MessagePack binary encoding support

# Language Specification

## Statements

Statements in BarkML must be defined ending with a new-line.

### Control Statements

Control statements are generally used for specifying metadata related to the file,
section or blocks. They are assignment statements to any variable starting with $

**Syntax:**

```
$<name> = (!<label>)? <value>
```

**Example:**

```
$schema = !MyProgram "1.0.0"
```

The above example could be a way to define what schema this barkml utilizes. Note schema
checking is currently not implemented directly in BarkML, it is on the writer of a tool using
BarkML to define and check for a schema.

### Sections

The top-most grouping of data in BarkML are sections. Sections look identical to
sections in TOML. Unlike TOML though BarkML does not allow nested sections. Sections are
meant to be global groupings in a configuration file.

**Syntax:**

```
[<name>|"<name>"]
<child-statements...>
```

**Example:**

```
[section-a]
foo = "bar"

["section-b"]
foo = "baz"
```

_NOTE: If the use case of the configuration file is to be sent over network or streamed to a service for some reason,
the use of sections should be avoided as if the data sent is cutoff it could still result in a valid BarkML file.
Instead
use blocks as defined below to group statements in this use case_

### Comments

BarkML supports the definition of comments by defining lines starting with # followed by one white space.
Any back to back lines with # will be concatenated into a multiline comment.

**Syntax:**

```
# <any text without a newline>
# <...>
```

**Example:**

```
# This is a comment in BarkML
```

### Blocks

BarkML supports grouping and labeling a set of statements as blocks. These blocks can have 0 or more labels associated
with them

**Syntax:**

```
<id> ["<label>" | '<label>'] {
  <child-statements...>
}
```

### Assignments

The standard assignment statement will define a single entry of data. Like
with control statements a value can be given an optional label as well, this can
be useful for declaring types for a value for the tool handling the config file.

**Syntax:**

```
[<id> | "<id>"] = (!<label>)? <value>
```

**Examples:**

```
foo = "bar"
"baz" = 3.14
```

## Values

### Integers

Integers are specified as numerical values and unless provided a suffix will be read as a signed 64-bit number.
Numbers can be specified with precision utilizing the following suffices

| Suffix | Precision        |
|--------|------------------|
| u8     | Unsigned 8-byte  |
| u16    | Unsigned 16-byte |
| u32    | Unsigned 32-byte |
| u64    | Unsigned 64-byte |
| i8     | Signed 8-byte    |
| i16    | Signed 16-byte   |
| i32    | Signed 32-byte   |
| i64    | Signed 64-byte   |

**Examples:**

```
5
-2
5u32
-2i64
```

### Floating Point Numbers

Floating point numbers are read by default as 64-byte floating point unless one of the below suffixes are provided.
Floating point numbers
can be specified with exponents as well.

| Suffix | Precision |
|--------|-----------|
| f32    | 32-byte   |
| f64    | 64-byte   |

**Examples:**

```
3.14
3.14f32
-5.2
5.2e10
5.2e+10
3.1e-10
30.0E+2
```

### Semantic Versions

BarkML supports inline semantic version declarations. However to prevent collision with floating
point numbers, any semantic version must specify at least <major>.<minor>.<patch>

**Examples:**

```
1.0.0
0.1.0-beta.1
0.1.0-prerelease-build.5
```

### Semantic Version Requirements

BarkML also supports the definition of version requirements. The only exception is that currently
the wildcard '*' support is not explicitly supported (it is still inferred when not specifying every part of version).
You also must always specify a requirement operator in barkml.

```
>1.1
^4
~5.3
```

### String Values

Strings can be defined either with a single quote or a double quote. Both are effectively parsed identically.

**Examples:**

```
'my-string'
"my string value"
```

### Byte Data

BarkML supports storing random byte data via base64 encoded byte strings. To avoid any confusion with what
standard of base64 is used between configuration files, BarkML standardizes on expecting URL Safe with No Padding
base64 data. This rust crate will automatically encode and decode any `Vec<u8>` data to and from this encoding standard.

**Examples:**

```
# binarystring in base64
b'YmluYXJ5c3RyaW5n'
```

### Labels

Label values are identifiers prefixed by !. These are primarily used before a value in an assignment statement
or control statement, but also can be used by themselves as a symbol value.

**Examples:**

```
!MyLabel
```

### Booleans

BarkML supports the use of multiple identifiers to define boolean values in configuration files

**Options for True**

- true
- True
- TRUE
- yes
- Yes
- YES
- on
- On
- ON

**Options for False**

- false
- False
- FALSE
- no
- No
- NO
- off
- Off
- OFF

## Null

BarkML also supports multiple identifiers to define a null value

**Options**

- null
- Null
- NULL
- nil
- Nil
- NIL
- none
- None
- NONE

## Arrays

BarkML supports dynamic arrays, meaning that the type of the sub entry of any array does not
have to match. Arrays are always wrapped in `[]` and can contain 0 or more values delimited by commas

**Example:**

```
[5, 3.14, 'foo']
```

## Tables

BarkML also supports the definition of tables

**Example:**

```
{
  foo = 5,
  "bar" = 3.14
}
```

## Macro Replacements

BarkML supports the use of self referential macros. These macros will at parse time lookup and replace
values with a previously defined value. These macro values must reference data via a root path from the
top of the configuration file. Macro replacements are full replacements

**Example:**

```
version = "1.0.0"
[section]
val = 5
[section-b]
parent-version = m!version
other-val = m!section.val
```

## Macro Strings

BarkML also allows the use of macro replacements inside of a string declaration. A macro string
can define one or more replacements by utilizing `{}` inside of the string

```
version = "1.0.0"
motd = m'Hello from {version}'
```

_NOTE: Macro strings must only use single quotes_

## Security

See [CONTRIBUTING](CONTRIBUTING.md#security-issue-notifications) for more information.

## License

This library is licensed under the MIT-0 License. See the LICENSE file.
