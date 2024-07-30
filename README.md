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
| \ | ´ | 




