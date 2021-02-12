# Some differences between the languages C# and X#

This document describes some of the differences between C# and X# that you will see reflected in the modifications in the Roslyn source code.

CSharp (C#) | XSharp (X#)
------------ | ------------
Case Sensitive | Case Insensitive, unless the commandline option /cs is used
Entry point=Main | Entry point = Start
x | allows literals of type IntPtr and Void
x | allows enums to be treated like integers
x | generates a `<Module>` class with some methods in it
x | uses own guids for Company and Language in PDB files
x | different resolution for static method calls: Functions are preferred over static methods in the same class.
x | different resolution for ambiguous code: code in user assemblies takes precedence over code in the X# runtime.
x | memvars (declared and undeclared) and fields in workareas are allowed. The compiler translates these to special identifiers and they are translated into function calls in the runtime.
x | allows string comparisons with normal `>` and `<` operator
x | allows to use `1.0.*.*` syntax for Assembly versions
x | allows to pass NULL to REF parameters. Of course you MUST check for this in the receiving code.
x | allows to update FOREACH variables
x | default parameter values are allowed anywhere in the parameter list. They are encoded with a special attribute
x | use @ as alternative to REF when passing variables. In the VO and Vulcan dialect this may also pass the address of something.
x | SUPER() call does not have to be the first statement in the body of a constructor(). 
Early bound or Dynamic | Allows late binding on OBJECT and USUAL as well.
omitting REF only for parameters to calls for COM objects | REF may be omitted everywhere.
x | allow binary operators '-' for strings
x | allow mixed variable types inside IIF() expressions
x | STATIC LOCALS (are implemented as REF locals. The value is stored in the Functions class)
x | Codeblocks (are like a lambda expression inside an anonymous type)
x | Has a preprocessor that can translate User Defined Commands (UDCs) into functional code.
x | types are optional. The compiler will treat missing types as USUAL 
x | Allows more than one indexed property in a type. The name is not automatically `This`. To create a `This` property use the SELF keyword.






