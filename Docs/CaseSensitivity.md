# CASE Sensitivity

This document describes the various changes that we had to make to the Roslyn source code that are related to Case Sensitivity.
These changes are all marked in the source files with `#if XSHARP` or `#if ! XSHARP` and were mainly caused by differences between the 2 development languages.

## Introduction

Since C# is case sensitive we had to review all the locations in the Roslyn code where string comparisons are made.
Some of these were easy to find, because the code was using a `String.Compare()` call, but others are more difficult 
to find, because the source code is simply comparing 2 string literals or because the source code is using a dictionary 
like collection and is not specifying the kind of comparer that needs to be used to compare the elements in the collection.
And the default behavior for .Net is then a case sensitive comparison.

## Some examples of this

Example 1:

```
#if XSHARP
   var map = new Dictionary<string, MethodData>(XSharpString.Comparer);
#else
   var map = new Dictionary<string, MethodData>();
#endif
```

As you can see the original code does not specify a comparison routine. 
We are now using a (singleton) Comparer instance. This instance is set 
at startup of the compiler depending on the current setting of the `/cs` compiler option.

Example 2:

```
#if XSHARP
   var names = new Dictionary<string, int>(n, XSharpString.Comparer);
#else
   var names = new Dictionary<string, int>(n, StringOrdinalComparer.Instance);
#endif
```

Here the original code used an OrdinalComparer and we use our Comparer.

Example 3:

```
#if XSHARP
                if (XSharpString.Equals(param.Name, argumentName))
#else
                if (param.Name == argumentName)
#endif
```

This one was more difficult to spot because the original code did not use a comparer but used  `==` comparison operator.
We are replacing this with a call to our Equals method that takes care of the case (in)sensitivity.

Example 4:

On some spots the Roslyn code is comparion to literal strings:

```
#if XSHARP
   else if (XSharpString.Equals(call.Method.Name, "GroupBy"))
#else
   else if (call.Method.Name == "GroupBy")
#endif
```

We are really surprised that these literals are literally "all over" the Roslyn source code. 
We would have expected that the Roslyn development team would have collected these as contant fields
in a static class so they would have been easier to find. This is something that we have done in our code
(in the XSharp\src\Compiler\XSharpCodeAnalysis\LiteralNames.cs files). That would have made things easier as well.

Hopefully we have found all the occasions of case sensitive comparisons and have taken care of them.
But it is very well possible that in newer source files from Roslyn we may have overlooked a comparis
on.
If you see strange behavior for symbols that are not found, then this is the first thing to look for.


## Case Sensitivity and the "Shared Compiler

For performance Roslyn uses a mechanism where the command line compiler can start a background process, the shared compiler.
For the C# compiler (csc.exe) this process is the VBCSCompiler.exe.
For X# we have a command line compiler called xsc.exe and the background process is called XSCompiler.exe.

The idea behind this background compiler is that this process can cache metadata for often used assemblies, 
which makes recompilation of the almost identical code much faster since all the metadata has been read already.

The foreground compiler communicates with the background process using **named pipes**. 
The commandline arguments are sent over this pipe and the background process returns the compilation result
and possible compilation error messages as a large string.

The challenge for us was that when you would be mixing case sensitive and case insensitive compilations then the 
background process could have a cache based on case insensitive comparisons and when the next compilation would 
be case _in_sensitive then this cache would be invalid.

As a solution we are therefore inspecting the commandline option and starting a separate background process 
for case sensitive and for case insensitive compilations.

The case insensitive compilation (which is our default) uses the 'normal' mechanism to create a named pipe.
The case sensitive compilation adds a `"__CS"` suffix to the pipe name.

The Roslyn code checks to see if there is a process listening to this pipe and when not then it starts a 
new process.

As a result you could have 2 background XSCompiler.exe processes running if you are mixing the case sensitivity 
(which is not something that we recommend by the way).









