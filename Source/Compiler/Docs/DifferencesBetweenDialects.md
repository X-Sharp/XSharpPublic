# Differences between the various X# dialects



Feature | Core    | VO     | Vulcan  | Harbour  | XBase++ | FoxPro
------- | ------- |------- | ------- | -------- |-------- | ------
4 letter abbreviations`*` | - | Yes | - | Yes | Yes | Yes
&& as comment char | - | Yes | - | Yes | Yes | Yes
&& as AND operator | Yes | - | Yes | - | - | - |
`*` at start of like = comment | Yes | Yes | Yes | Yes | Yes | Yes
AND, OR, NOT, XOR | - | - | - | - | - | Yes
DATETIME type | Yes | Yes | Yes | Yes | Yes | Yes
DATETIME literals | Yes | Yes | Yes | Yes | Yes | Yes
DEFINE statement with optiona `AS <type>` | Yes | Yes | Yes | Yes | Yes | Yes
ENDFOR as alternative for NEXT | - | - | - | - | - | Yes
FOR EACH as alternative for FOREACH | - | - | - | - | - | Yes
GLOBAL statement | Yes | Yes | Yes | Yes | Yes | Yes
IIF Expression ans /vo10 | Yes | Yes | Yes | Yes | Yes | Yes
NULL_STRING `***`| Yes | Yes | Yes | Yes | Yes | Yes
Typed Literal arrays `**`| Yes | Yes | Yes | Yes | Yes | Yes 

`*` only for VO Compatible keywords. Not for newer X# keywords.

`**` Prefix the array with `<TypeName>` as in `<STRING>{"aa", "bb", "cc"}` . To declare a type array of a certain size use for example `STRING[]{5}` for a string array of 5 elements.

`***` With /vo2 this is compiled to `String.Empty`. Otherwise into `NULL`

## Features that require runtime support 

Core has *NOTHING* that requires runtime support.


Feature | VO      | Vulcan  | Harbour  | XBase++ | FoxPro
------- | ------- | ------- | -------- |-------- | ------
ALIAS (`->`) operator | Yes | Yes | Yes | Yes | Yes
ARRAY Type  |  Yes | Yes | Yes | Yes | Yes
BINARY type |  Yes | Yes | Yes | Yes | Yes
BINARY literals |  Yes | Yes | Yes | Yes | Yes
BREAK statement | Yes | Yes | Yes | Yes | Yes
BEGIN SEQUENCE..  |  Yes | Yes | Yes | Yes | Yes
CLIPPER calling convention |  Yes | Yes | Yes | Yes | Yes
CODEBLOCK type |  Yes | Yes | Yes | Yes | Yes
CURRENCY type, literals |  Yes | Yes | Yes | Yes | Yes
DATE type, literals |  Yes | Yes | Yes | Yes | Yes
FIELD statement |  Yes | Yes | Yes | Yes | Yes
FLOAT type, literals |  Yes | Yes | Yes | Yes | Yes
Late Binding    |  Yes | Yes | Yes | Yes | Yes
LPARAMETERS statement | - | - | - | - | Yes
MACRO Compiler  |  Yes | Yes | Yes | Yes | Yes
MEMVAR, PUBLIC, PRIVATE, PARAMETERS |  Yes | - | Yes | Yes | Yes
PSZ Type | Yes | Yes | Yes | Yes | Yes
PSZ Indexer | 1 based | 0 based | 1 based | 1 based | 1 based
SYMBOL Type, Literals, NULL_SYMBOL | Yes | Yes | Yes | Yes | Yes
TOP level statements | - | - | - | - | Yes
USUAL type and NIL | Yes | Yes | Yes | Yes | Yes `*`
TEXT .. ENDTEXT | - | - | - | - | Yes
VOSTRUCT and AS/IS syntax| Yes | Yes | - | - | -
UNION and AS/IS syntax| Yes | Yes | - | - | -
Untyped literal arrays | Yes | Yes | Yes | Yes | Yes
Missing types allowed | Yes | Yes | Yes | Yes | Yes
Missing arguments in calls (`MyFunc(1,,2)`) | Yes | Yes | Yes | Yes | Yes

`*` In FoxPro NIL has the value FALSE

## Pseudo Functions

Function | Core    | VO     | Vulcan  | Harbour  | XBase++ | FoxPro
------- | ------- |------- | ------- | -------- |-------- | ------
_Chr() `*` | Yes | Yes | Yes | Yes | Yes | Yes
_GetInst()| Yes | Yes | Yes | Yes | Yes | Yes
PCount()  | - | Yes | Yes | Yes | Yes | Yes
GetMParam()| - | Yes | Yes | Yes | Yes | Yes
GetFParam()| - | Yes | Yes | Yes | Yes | Yes
Args()| - | Yes | Yes | Yes | Yes | Yes
SLen()| - | Yes | Yes | Yes | Yes | Yes
Chr() `*`| Yes | Yes | Yes | Yes | Yes | Yes
AltD()`**`| Yes | Yes | Yes | Yes | Yes | Yes
PCall() | - | Yes | Yes | Yes | Yes | Yes
PCallNative()| - | Yes | Yes | Yes | Yes | Yes

`*` Only for values < 127.

`**` This gets translated to:
```
IF System.Diagnostics.Debugger.Attached
   System.Diagnostics.Debugger.Break()
ENDIF
```
