# Active Oberon Compiler project (2019 Standard)

This is the start of a project to build a native active Oberon 2019 compiler with inline assembler, written in Rust.

- cargo build
- cargo test
- cargo run

## Reserved keywords

| | | | | | | | |
|----|-----|-----|-----|-----|-----|------|------|
| ADDRESS | ALIAS | ARRAY | AWAIT | BEGIN | BY | CELL | CELLNET | 
| CASE | CODE | DEFINITION | DO | DIV | END | ENUM | ELSE | 
| ELSIF | EXIT | EXTERN | FALSE | FOR | FINALLY | IF | IMAG |
| IN | IS | IMPORT | LOOP | MODULE | MOD | NIL | OBJECT |
| OF | OR | OUT | OPERATOR | POINTER | PROCEDURE  | PORT | REPEAT |
| RECORD | RETURN | RESULT | SELF | THEN | TRUE | TO | TYPE | 
| UNTIL | VAR | WHILE | WITH |

**If you don't use strict mode, lower case keywords is allowed also** 

| | | | | | | | |
|----|-----|-----|-----|-----|-----|------|------|
| address | alias | array | await | begin | by | cell | cellnet | 
| case | code | definition | do | div | end | enum | else | 
| elsif | exit | Eextern | false | for | finally | if | imag |
| in | is | import | loop | module | mod | nil | object |
| of | or | out | operator | pointer | procedure  | port | repeat |
| record | return | result | self | then | true | to | type | 
| until | var | while | with |

## Operators or delimiters

||||||||||||
|-|-|-|-|-|-|-|-|-|-|-|
| < | <= | = | >= | > | # | := | : | ; | .. | . | , |
| + | +* | - | ^ | & | ? | ?? | ! | !! | << | >> | <<? |
| >>? | .< | .<= | .>= | .> | .+ | .* | .= | .# | ~ | 
| \ | ´ | ( | ) | [ | ] | { | } | ** | * | / | ./ | 

## Reserved procedures and data types

| | | | | | | | |
|----|-----|-----|-----|-----|-----|------|------|
| ABS | ADDRESS | ADDRESSOF | ALL | ANY | ASH | ASSERT |
| BOOLEAN | CAP | CAS | CHAR | CHR | COMPLEX | COMPLEX32 |
| COMPLEX64 | COPY | DEC | DECMUL | DIM | ENTIER | ENTIERH |
| EXCL | FIRST | FLOAT32 | FLOAT64 | FLOOR | HALT | IM |
| INC | INCL | INCMUL | INCR | INTEGER | INTEGERSET | LAST | 
| LEN | LONG |LONGINTEGER | LSH | MAX | MIN | OBJECT |
| ODD | RANGE | RE | REAL | RESHAPE | ROL | ROR |
| ROT | SET | SET8 | SET16 |SET32 | SET64 | SHL | 
| SHORT | SHR | SIGNED8 | SIGNED16 | SIGNED32 | SIGNED64 | SIZE | 
| SIZEOF | STEP | SUM | UNSIGNED8 | UNSIGNED16 | UNSIGNED32 | UNSIGNED32 |
| UNSIGNED64 |





