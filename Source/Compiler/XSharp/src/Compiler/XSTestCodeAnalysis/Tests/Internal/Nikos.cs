/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and   
limitations under the License.
*/
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSTestCodeAnalysis
{
    public partial class TestClass
    {
        [Test(Author = "Frank Maraite", Id = "N1", Title = "Error: Inconsistent visibility (should be warning)")]
        public static void Error_inconsistent_accessibility()
        {
            var s = ParseSource(@"
INTERNAL CLASS Test
	PROTECTED TestX AS LOGIC
END CLASS

CLASS TestUsing
	PUBLIC PROPERTY Test AS Test
		GET
			RETURN _Test
		END GET
	END PROPERTY
	PROTECTED _Test AS Test
END CLASS
");
            CompileAndLoadWithoutErrors(s);
        }

        [Test(Author = "Nikos", Id = "N2", Title = "Array base 1 (no /az)")]
        public static void Array_base_1()
        {
            var s = ParseStartFunction(@"
LOCAL a AS INT[]
a := <INT>{1}
? a[1]
? a[1U]
a[1] := 5
? a[1L]
");
            CompileAndRunWithoutExceptions(s);
        }

        [Test(Author = "Nikos", Id = "N3", Title = "Array base 0 (/az)")]
        public static void Array_base_0()
        {
            var s = ParseStartFunction(@"
LOCAL a AS INT[]
a := <INT>{1}
? a[0]
? a[0U]
a[0] := 5
? a[0L]
");
            CompileAndRunWithoutExceptions("/az", s);
        }

        [Test(Author = "Nikos", Id = "N3", Title = "Do not init string vars to empty string (no /vo2)")]
        public static void InitStringVarsToNullString()
        {
            var s = ParseStartFunction(@"
LOCAL s AS STRING
IF s != NULL
  THROW Exception{'<s> is not NULL'}
ENDIF

LOCAL s2 AS System.String
IF s2 != NULL
  THROW Exception{'<s2> is not NULL'}
ENDIF
");
            CompileAndRunWithoutExceptions(s);
        }

        [Test(Author = "Nikos", Id = "N4", Title = "Init string vars to empty string (/vo2)")]
        public static void InitStringVarsToEmptyString()
        {
            var s = ParseStartFunction(@"
LOCAL s AS STRING
IF s == NULL
  THROW Exception{'<s> is NULL'}
ENDIF

LOCAL s2 AS STRING
IF s2 == NULL
  THROW Exception{'<s2> is NULL'}
ENDIF
");
            CompileAndRunWithoutExceptions("/vo2", s);
        }

        [Test(Author = "Nikos", Id = "N5", Title = "Do not init string fields to empty string (no /vo2)")]
        public static void InitStringFieldsToNullString()
        {
            var s = ParseSource(@"
CLASS Test
    PUBLIC S AS STRING
    PUBLIC S2 AS System.String
    STATIC PUBLIC SS AS STRING
    STATIC PUBLIC SS2 AS System.String
END CLASS

STRUCTURE TestS
    PUBLIC S AS STRING
    PUBLIC S2 AS System.String
    STATIC PUBLIC SS AS STRING
    STATIC PUBLIC SS2 AS System.String
END STRUCTURE

FUNCTION Start() AS VOID
    IF Test.ss != NULL
      THROW Exception{'<Test.ss> is not NULL'}
    ENDIF
    IF Test.ss2 != NULL
      THROW Exception{'<Test.ss2> is not NULL'}
    ENDIF

    IF TestS.ss != NULL
      THROW Exception{'<TestS.ss> is not NULL'}
    ENDIF
    IF TestS.ss2 != NULL
      THROW Exception{'<TestS.ss2> is not NULL'}
    ENDIF

    VAR t := Test{}
    IF t:s != NULL
      THROW Exception{'<t:s> is not NULL'}
    ENDIF
    IF t:s2 != NULL
      THROW Exception{'<t:s2> is not NULL'}
    ENDIF

    VAR ts := TestS{}
    IF ts:s != NULL
      THROW Exception{'<ts:s> is not NULL'}
    ENDIF
    IF ts:s2 != NULL
      THROW Exception{'<ts:s2> is not NULL'}
    ENDIF
");
            CompileAndRunWithoutExceptions(s);
        }

        [Test(Author = "Nikos", Id = "N6", Title = "Init string fields to empty string (/vo2)")]
        public static void InitStringFieldsToEmptyString()
        {
            var s = ParseSource(@"
CLASS Test
    PUBLIC S AS STRING
    PUBLIC S2 AS System.String
    STATIC PUBLIC SS AS STRING
    STATIC PUBLIC SS2 AS System.String
END CLASS

STRUCTURE TestS
    PUBLIC S AS STRING
    PUBLIC S2 AS System.String
    STATIC PUBLIC SS AS STRING
    STATIC PUBLIC SS2 AS System.String
END STRUCTURE

FUNCTION Start() AS VOID
    IF Test.ss == NULL
      THROW Exception{'<Test.ss> is NULL'}
    ENDIF
    IF Test.ss2 == NULL
      THROW Exception{'<Test.ss2> is NULL'}
    ENDIF

// unsupported
/*    IF TestS.ss == NULL
      THROW Exception{'<TestS.ss> is NULL'}
    ENDIF
    IF TestS.ss2 == NULL
      THROW Exception{'<TestS.ss2> is NULL'}
    ENDIF*/

    VAR t := Test{}
    IF t:s == NULL
      THROW Exception{'<t:s> is NULL'}
    ENDIF
    IF t:s2 == NULL
      THROW Exception{'<t:s2> is NULL'}
    ENDIF

// unsupported
/*    VAR ts := TestS{}
    IF ts:s == NULL
      THROW Exception{'<ts:s> is NULL'}
    ENDIF
    IF ts:s2 == NULL
      THROW Exception{'<ts:s2> is NULL'}
    ENDIF*/
");
            CompileAndRunWithoutExceptions("/vo2", s);
        }

        [Test(Author = "Nikos", Id = "N7", Title = "Non-vritual instance methods by default (no /vo3)")]
        public static void NonVirtualInstanceMethodsByDEfault()
        {
            var s = ParseSource(@"
CLASS Test
    PUBLIC METHOD TestValue() AS INT
        RETURN 0
END CLASS

CLASS Test2 INHERIT Test
    PUBLIC METHOD TestValue() AS INT
        RETURN 1
END CLASS

FUNCTION Start() AS VOID
    LOCAL t AS Test
    t := (Test)(Test2{})
    IF t:TestValue() == 1
      THROW Exception{'<t:TestValue> == 1'}
    ENDIF
");
            CompileAndRunWithoutExceptions(s);
        }

        [Test(Author = "Nikos", Id = "N8", Title = "Vritual instance methods by default (/vo3)")]
        public static void VirtualInstanceMethodsByDEfault()
        {
            var s = ParseSource("/vo3", @"
CLASS Test
    PUBLIC METHOD TestValue() AS INT
        RETURN 0
END CLASS

CLASS Test2 INHERIT Test
    PUBLIC METHOD TestValue() AS INT
        RETURN 1
END CLASS

FUNCTION Start() AS VOID
    LOCAL t AS Test
    t := (Test)(Test2{})
    IF t:TestValue() == 0
      THROW Exception{'<t:TestValue> == 0'}
    ENDIF
");
            CompileAndRunWithoutExceptions("/vo3", s);
        }

        [Test(Author = "Nikos", Id = "N9", Title = "FOR LOCAL variable declaration")]
        public static void ForLocalDeclaration()
        {
            var s = ParseStartFunction(@"
LOCAL total := 0 AS INT
FOR LOCAL i := 1 AS INT TO 10
    total += 1
NEXT
FOR VAR i := 1 TO 10
    total += 1
NEXT
FOR LOCAL IMPLIED i := 1 UPTO 10
    total += 1
NEXT
FOR IMPLIED i := 10 DOWNTO 1
    total += 1
NEXT
IF total != 40
    THROW Exception{'total != 40'}
ENDIF
");
            CompileAndRunWithoutExceptions(s);
        }

        [Test(Author = "Nikos", Id = "N10", Title = "Default namespace (/ns)")]
        public static void DefaultNamespace()
        {
            var s = ParseSource(@"
CLASS Test
END CLASS

FUNCTION Start() AS VOID
VAR t := CustomNs.Test{}
");
            CompileWithErrors(s);

            s = ParseSource("/ns:CustomNs", @"
CLASS Test
END CLASS

FUNCTION Start() AS VOID
VAR t := CustomNs.Test{}
");
            CompileAndRunWithoutExceptions("/ns:CustomNs", s);

            s = ParseSource("/ns:CustomNs", @"
BEGIN NAMESPACE CustomNs
CLASS Test
END CLASS
END NAMESPACE

FUNCTION Start() AS VOID
VAR t := CustomNs.Test{}
");
            CompileAndRunWithoutExceptions("/ns:CustomNs", s);

            s = ParseSource("/ns:CustomNs", @"
USING CustomNs

CLASS Test
END CLASS

FUNCTION Start() AS VOID
VAR t := CustomNs.Test{}
");
            CompileAndRunWithoutExceptions("/ns:CustomNs", s);

            s = ParseSource("/ns:CustomNs", @"
#USING global::CustomNs

CLASS Test
END CLASS

FUNCTION Start() AS VOID
VAR t := CustomNs.Test{}
");
            CompileAndRunWithoutExceptions("/ns:CustomNs", s);

            s = ParseSource("/ns:ns1.ns2", @"
CLASS Test
END CLASS

FUNCTION Start() AS VOID
VAR t := ns1.ns2.Test{}
");
            CompileAndRunWithoutExceptions("/ns:ns1.ns2", s);
        }

        [Test(Author = "Nikos", Id = "N11", Title = "Preprocessor defines, conditional sections")]
        public static void PreprocessorDefines()
        {
            string args = "/define:TESTING";
            var s = ParseSource(args, @"
#ifdef TESTING
FUNCTION Start() AS VOID
    VAR o := Test{}
#else
FUNCTION Start() AS VOID
    THROW Exception{}
#endif
#undef TESTING
#ifndef TESTING
CLASS Test
END CLASS
#endif
");
            CompileAndRunWithoutExceptions(args, s);
        }

        [Test(Author = "Nikos", Id = "N12", Title = "Lamda expressions")]
        public static void LamdaExpressions()
        {
            string args = "";
            var s = ParseSource(args, @"
FUNCTION Start() AS VOID
    LOCAL sq AS System.Func<Double,Double>
    sq := {|x|x^2}
    IF sq(2) != 4
        THROW Exception{}
    ENDIF
    sq := {|x|
        ? 'square of', x
        RETURN x^2
        }
    IF sq(2) != 4
        THROW Exception{}
    ENDIF
    sq := {|x| Console.WriteLine(x), x^2}
    IF sq(2) != 4
        THROW Exception{}
    ENDIF
    LOCAL empty AS System.Action
    empty := {||}
    empty()
");
            CompileAndRunWithoutExceptions(args, s);
        }

        [Test(Author = "Nikos", Id = "N13", Title = "Missing type in X# core should throw error")]
        public static void MissingTypeCore()
        {
            CompileWithErrors(ParseSource(@"
FUNCTION Test()
"));
            CompileWithErrors(ParseSource(@"
FUNCTION Test() AS VOID
    LOCAL o
"));
        }

        [Test(Author = "Nikos", Id = "N14", Title = "Using statement")]
        public static void UsingStatement()
        {
            CompileAndRunWithoutExceptions(ParseSource(@"
PROCEDURE Start()
    VAR ms := System.IO.MemoryStream{}
    BEGIN USING ms
    END USING
"));
            CompileAndRunWithoutExceptions(ParseSource(@"
FUNCTION Start() AS VOID
    BEGIN USING VAR ms := System.IO.MemoryStream{}
    END USING
"));
        }

        [Test(Author = "Nikos", Id = "N15", Title = "Constructor chaining (core dialect)")]
        public static void CtorChainCore()
        {
            CompileAndLoadWithoutErrors(ParseSource(@"
CLASS Parent
    CONSTRUCTOR(o AS OBJECT)
END CLASS

CLASS Child INHERIT Parent
    CONSTRUCTOR()
        SUPER(null)
END CLASS
"));
            CompileWithErrors(ParseSource(@"
CLASS Parent
    CONSTRUCTOR(o AS OBJECT)
END CLASS

CLASS Child INHERIT Parent
    CONSTRUCTOR()
        SUPER(SELF)
END CLASS
"));
        }

        [Test(Author = "Nikos", Id = "N16", Title = "Constructor chaining (vulcan dialect)")]
        public static void CtorChainVulcan()
        {
            CompileAndRunWithoutExceptions("/dialect:vulcan", ParseSource("/dialect:vulcan /r:VulcanRTFuncs.dll /r:VulcanRT.dll", @"
CLASS Parent
    PUBLIC Inits := 0 AS INT
    CONSTRUCTOR()
        Inits += 1
END CLASS

CLASS Child INHERIT Parent
    CONSTRUCTOR()
        SUPER()
END CLASS

FUNCTION Start() AS VOID
    LOCAL i AS INT
    i := Child{}:Inits
    IF i != 1
        THROW Exception{'Inits = '+ i}
    ENDIF
    RETURN
"), VulcanRuntime);
        }

        [Test(Author = "Nikos", Id = "N17", Title = "Anonymous types")]
        public static void AnonTypes()
        {
            CompileAndRunWithoutExceptions(ParseSource(@"
FUNCTION Start() AS VOID
    VAR o := CLASS { Name := ""test"", Value := ""something"" }
    IF o:Name != ""test"" || o:Value != ""something""
        THROW Exception{'Incorrect members'}
    ENDIF
    RETURN
"));
        }

        [Test(Author = "Nikos", Id = "N18", Title = "NameOf operator")]
        public static void NameOfOperator()
        {
            CompileAndRunWithoutExceptions(ParseSource(@"
class Test
    property Prop as int auto
    method Meth() as void
    method Meth(a as int) as void
end class

FUNCTION Start() AS VOID
    VAR o := Test{}
    LOCAL n AS STRING
    n := nameof(Test.prop)
    IF n != ""Prop""
        THROW Exception{'n == ""'+n+'""'}
    ENDIF
    n := nameof(o:prop)
    IF n != ""Prop""
        THROW Exception{'n == ""'+n+'""'}
    ENDIF
    n := nameof(Test.meth)
    IF n != ""Meth""
        THROW Exception{'n == ""'+n+'""'}
    ENDIF
    n := nameof(o:meth)
    IF n != ""Meth""
        THROW Exception{'n == ""'+n+'""'}
    ENDIF
    RETURN
"));
            CompileWithErrors(ParseSource(@"
class Test
    method Meth() as void
    method meth(a as int) as void
end class

FUNCTION Start() AS VOID
    LOCAL n AS STRING
    n := nameof(Test.meth)
    RETURN
"));
            CompileWithErrors(ParseSource(@"
class Test
    method Meth() as void
    method meth(a as int) as void
end class

FUNCTION Start() AS VOID
    VAR o := Test{}
    LOCAL n AS STRING
    n := nameof(o:meth)
    RETURN
"));
        }
    }
}