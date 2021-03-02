# Converting Parse Trees

The X# compiler uses a front end written with Antlr to produce Parse Trees.
The elements in these parse trees are all subclasses of the XSharpParserRuleContext class.

After the parse process we are using a Tree Walker to walk the Antlr parse trees and to create
similar Roslyn parse trees that represent the same code.

We have added a `XNode` property to the Roslyn CSharpSyntaxNode class. 
We store the original Antlr parse tree node in this property.
We have also added a private `xflags` field to the Roslyn CSharpSyntaxNode. 
This field stores some boolean flags (in a single 32 bit integer of a flags enum) to mark nodes with special properties.
We have added properties to the SyntaxNode to Get/Set these flags.

The XSharpParserRuleContext also has a field where we store the CSharpSyntaxNode that was generated for the
element in the Parse Tree.

These 2 way links are needed because the C# syntax can differ quite a lot from the X# syntax and we need 
to be able to determine what the original source code for a symbol was during compilation.
Also the order of tokens in the source may be different.
For example

```
string sValue;   // C#

LOCAL sValue AS STRING // X#
```

As you can see the type comes before the identifier in C# and C# also does not have 
`LOCAL` and `AS` keywords for anormal type declations.  C# on the other hand requires a semi colon
that is not needed in X#

This looks fairly similar:

```
var sValue = "Hello World";     // C#

var sValue := "Hello World"     // X#
```

The assignment operator is different and the semi colon. 

The C# parse tree also contains white space and comments. The X# parse tree does not have that.

This also introduces some challenges, especially when generating XML comment files for assemblies.


The compiler produces what is called "Compilation Units" for each source file.
We have augmented the C# compilation unit SyntaxTree node and added some extra
information at the file level to these nodes such as:
- the list of tokens 
- the list of #pragma directives from the file
- some flags (for example does the file have XML comments)
- are there any partial properties in the compilation unit (ACCESS without ASSIGN for a PARTIAL class) that may require extra processing after all files have been parsed.
- the list of literal symbols in the file
- the list of literal PSZs in the file
- etc.









