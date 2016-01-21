using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSTestCodeAnalysis
{
    public partial class TestClass
    {
        [Test(Author = "Chris", Id = "C1", Title = "Crash when initializing a local INT[]")]
        public static void Crash_InitLocalIntArray()
        {
            var s = ParseStartFunction(@"
LOCAL a AS INT[]
a := INT[]{10}
");
            CompileAndRunWithoutExceptions(s);
        }

        [Test(Author = "Chris", Id = "C2", Title = "Error XS0515: Access modifiers not allowed on static constructors")]
        public static void Error_XS0515_StaticConstructor()
        {
            var s = ParseSource(@"
CLASS Test
STATIC CONSTRUCTOR()
END CLASS
");
            CompileAndLoadWithoutErrors(s);
        }

        [Test(Author = "Chris", Id = "C3", Title = "Error XS0165: Use of unassigned local (should be warning)")]
        public static void Error_XS0165_UseOfUnassignedLocal()
        {
            var s = ParseStartFunction(@"
LOCAL cRet as INT
? cRet
");
            CompileAndRunWithoutExceptions(s);
        }



        // 4
        [Test(Author = "Chris", Id = "C4", Title = "Crash with STRUCTURE declaration")]
        public static void Crash_StructDeclaration()
        {
            var s = ParseSource(@"
STRUCTURE xFloat
CONSTRUCTOR(n AS INT)
END STRUCTURE
");
            CompileAndLoadWithoutErrors(s);
        }




        // 17
//      [Test(Author = "Chris", Id = "C17", Title = "error XS1003: Syntax error, 'ForStmt' expected")]
// Update at Nikos' house:
        [Test(Author = "Chris", Id = "C17", Title = "error XS9002: Parser error")]
        public static void error_XS1003_FOR_LOCAL()
        {
            var s = ParseSource(@"
FUNCTION Start() AS VOID
FOR LOCAL n := 1 AS INT UPTO 10
	? n
NEXT
");
            CompileAndRunWithoutExceptions(s);
        }




        // 31
        [Test(Author = "Chris", Id = "C31", Title = "error XS0175: Use of keyword 'SUPER' is not valid in this context")]
        public static void cannot_use_super_in_constructor_body()
        {
            var s = ParseSource(@"
CLASS TestClass
CONSTRUCTOR()
LOCAL n AS INT
SUPER() // or allow it here at least
n := 1
SUPER()
END CLASS
");
            CompileAndLoadWithoutErrors(s);
        }




        // 34
        [Test(Author = "Chris", Id = "C34", Title = "error XS0119: 'TestClass.MessageBox()' is a method, which is not valid in the given context")]
        public static void error_XS0119_MessageBox_is_a_method_which_is_not_valid_in_the_given_context()
        {
            var s = ParseSource(@"
#using System.Windows.Forms
CLASS TestClass
	METHOD MessageBox() AS VOID
		MessageBox.Show(""test"")    
        RETURN
END CLASS
");
            CompileAndLoadWithoutErrors(s);
        }



        // 40
        [Test(Author = "Chris", Id = "C40", Title = "error XS1057: 'StaticClass.sp': static classes cannot contain protected members")]
        public static void error_XS1057_PROTECTs_in_STATIC_Class()
        {
            var s = ParseSource(@"
// vulcan allows it
STATIC CLASS StaticClass
	STATIC PROTECT sp AS INT
END CLASS
");
            CompileAndLoadWithoutErrors(s);
        }

        // 41
        [Test(Author = "Chris", Id = "C41", Title = "error XS1112: Do not use ExtensionAttribute. Use the 'SELF' keyword instead.")]
        public static void error_XS1112_Extension_Attribute()
        {
            var s = ParseSource(@"
PUBLIC STATIC CLASS Extensions
[System.Runtime.CompilerServices.ExtensionAttribute] ;
PUBLIC STATIC METHOD MyExtension( n AS INT ) AS INT
RETURN 0
END CLASS
");
            CompileAndLoadWithoutErrors(s);
        }



        // 42
        [Test(Author = "Chris", Id = "C42", Title = "Several errors with defining extension method")]
        public static void Several_errors_with_defining_extension_method()
        {
            var s = ParseSource(@"
CLASS Extensions
METHOD MyExtension( SELF n AS INT ) AS INT
RETURN 0
END CLASS

FUNCTION Start() AS VOID
  ? 0:MyExtension()
");
            CompileAndLoadWithoutErrors(s);
        }




        // 43
        [Test(Author = "Chris", Id = "C43", Title = "error XS1620: Argument 2 must be passed with the 'out' keyword")]
        public static void error_XS1620_passing_args_by_reference()
        {
            var s = ParseSource(@"
FUNCTION Start() AS VOID
// vulcan compiles both
LOCAL r AS REAL8
System.Double.TryParse(""1.2"", r) 
System.Double.TryParse(""1.2"", REF r)
");
            CompileAndLoadWithoutErrors(s);
        }



        // 46
        [Test(Author = "Chris", Id = "C46", Title = "error XS0549: 'TestClass.TestProp.get' is a new virtual member in sealed class 'TestClass'")]
        public static void error_XS0549_new_virtual_member_in_sealed_class()
        {
            var s = ParseSource(@"
SEALED CLASS TestClass
VIRTUAL PROPERTY TestProp AS INT
	GET
		RETURN 0
	END GET
END PROPERTY
END CLASS
");
            CompileAndLoadWithoutErrors(s);
        }



        // 63
        [Test(Author = "Chris", Id = "C63", Title = "error XS0246: The type or namespace name 'TestClass' could not be found")]
        public static void error_XS0246_function_inside_namespace()
        {
            var s = ParseSource(@"
// I never liked this behavior, but in vulcan this works
BEGIN NAMESPACE ns
CLASS TestClass
END CLASS
FUNCTION Test() AS VOID
LOCAL o AS TestClass
END NAMESPACE
");
            CompileAndLoadWithoutErrors(s);
        }




        // 65
        [Test(Author = "Chris", Id = "C65", Title = "compiler crash because of using keywords as names")]
        public static void crash_with_keywords_as_identifiers()
        {
            var s = ParseSource(@"
// crash because of using keywords as names
CLASS TestClass
ACCESS OPTIONS AS INT
RETURN 0
END CLASS
");
            CompileAndLoadWithoutErrors(s);

            var s2 = ParseSource(@"
// crash because of using keywords as names
CLASS TestClass
PROPERTY INT AS INT AUTO
END CLASS
");
            CompileWithErrors(s2);
        }



        // 66
        [Test(Author = "Chris", Id = "C66", Title = "Empty arguments in method call")]
        public static void crash_with_keywords_with_empty_params()
        {
            var s = ParseSource(@"
FUNCTION Start() AS VOID
SomeMethod( , , 1)
");
            CompileAndLoadWithoutErrors(s);
        }





        // 69
        [Test(Author = "Chris", Id = "C69", Title = "error XS0201: Only assignment, call, increment, decrement, and new object expressions can be used as a statement")]
        public static void Error_XS0201_not_operator_with_logic()
        {
            var s = ParseStartFunction(@"
LOCAL lVar AS LOGIC
IF ! lVar
	? lVar
END IF
");
            CompileAndRunWithoutExceptions(s);
        }

        // 70
        [Test(Author = "Chris", Id = "C70", Title = "error XS0216: The operator 'Foo.operator ==(Foo, string)' requires a matching operator '!=' to also be defined")]
        public static void Error_XS0216_equals_and_notequals_operator()
        {
            var s = ParseSource(@"
// vulcan compiles it without errors
CLASS Foo
	OPERATOR ==(a AS Foo , b AS STRING) AS LOGIC
	RETURN TRUE
END CLASS

FUNCTION Start() AS VOID
    VAR a := Foo{}
    IF !(a == '')
        THROW Exception{'Operator == failed!'}
    END IF
");
            CompileAndRunWithoutExceptions(s);
        }



        // 71
        [Test(Author = "Chris", Id = "C71", Title = "compiler crash with * comment")]
        public static void Crash_with_Asterisk_comment()
        {
            var s = ParseSource(@"
CLASS Foo
// NOTE: Old comments not supported yet!
*** comments here ***
END CLASS
");
            CompileWithErrors(s);
        }



        // 72
        [Test(Author = "Chris", Id = "C72", Title = "compiler crash with PARAMETERS as identifier")]
        public static void Crash_with_PARAMETERS_as_indentifier()
        {
            var s = ParseSource(@"
CLASS Test
PROTECT Parameters AS System.Collections.ArrayList
CONSTRUCTOR()
? .not. Parameters:Contains(1) 
END CLASS
");
            CompileAndLoadWithoutErrors(s);
        }


        // 73
        [Test(Author = "Chris", Id = "C73", Title = "error XS0106: The modifier 'public','virtual','override' is not valid for this item")]
        public static void Error_XS0106_explicit_property_implementation()
        {
            var s = ParseSource(@"
CLASS TestClass IMPLEMENTS ITest
   VIRTUAL PROPERTY ITest.MyProp AS Boolean GET FALSE
END CLASS

INTERFACE ITest
	PROPERTY MyProp AS Boolean GET
END INTERFACE
");
            CompileAndLoadWithoutErrors(s);
        }


        // 74
        [Test(Author = "Chris", Id = "C74", Title = "Crash with constructor() as void")]
        public static void Crash_with_constructor_as_void()
        {
            var s = ParseSource(@"
CLASS BaseClass
CONSTRUCTOR(c AS STRING)
END CLASS

CLASS ChildClass INHERIT BaseClass
CONSTRUCTOR(n AS STRING) AS VOID
SUPER(n)
END CLASS
");
            CompileWithErrors(s);
        }


        // 75
        [Test(Author = "Chris", Id = "C75", Title = "Crash with LOCAL as keyword")]
        public static void Crash_with_LOCAL_as_keyword()
        {
            var s = ParseSource(@"
CLASS TestClass
METHOD TestMethod() AS VOID
LOCAL o AS ARRAY
LOCAL o AS SYMBOL
LOCAL o AS DATE
LOCAL o AS USUAL
END CLASS
");
            CompileWithErrors(s);
        }


        // 76
        [Test(Author = "Chris", Id = "C76", Title = "Crash with keyword")]
        public static void Crash_Keyword()
        {
            var s = ParseSource(@"
CLASS INT // or almost any other keyword
END CLASS
");
            CompileWithErrors(s);
        }


        // 77
        [Test(Author = "Chris", Id = "C77", Title = "warning XS0168: The variable 'n' is declared but never used")]
        public static void warning_XS0168_variable_declared_but_never_used()
        {
            var s = ParseStartFunction(@"
FUNCTION Test() AS VOID
	LOCAL n AS INT
	TRY
		n := 1
		? n
	END TRY
RETURN 
");
            CompileWithoutWarnings(s);
        }


        // 78
        [Test(Author = "Chris", Id = "C78", Title = "error XS0106: The modifier 'static' is not valid for this item")]
        public static void error_XS0106_INTERNAL_VOSTRUCT()
        {
            var s = ParseSource(@"
INTERNAL VOSTRUCT _winPOINT
	MEMBER x AS LONG
	MEMBER y AS LONG
");
            CompileAndLoadWithoutErrors(s);
        }



        // 79
        [Test(Author = "Chris", Id = "C79", Title = "error XS0119: 'Xs$Globals.Directory()' is a method, which is not valid in the given context")]
        public static void error_XS0119_Directory_CreateDirectory()
        {
            var s = ParseSource(@"
#using System.IO
FUNCTION Directory() AS INT
Directory.CreateDirectory('')
RETURN 0
");
            CompileAndLoadWithoutErrors(s);
        }



        // 80
        [Test(Author = "Chris", Id = "C80", Title = "error XS0666: 'MyStruct.c': new protected member declared in struct")]
        public static void error_XS0666_PROTECTs_in_STRUCTURE()
        {
            var s = ParseSource(@"
// this does compile in vulcan!!!
STRUCTURE MyStruct
	PROTECT c AS STRING
END STRUCTURE
");
            CompileAndLoadWithoutErrors(s);
        }


        // 81
        [Test(Author = "Chris", Id = "C81", Title = "error XS0266: Cannot implicitly convert type 'MyEnum' to 'int'. An explicit conversion exists (are you missing a cast?)")]
        public static void error_XS0266_Cannot_convert_Enum_to_int()
        {
            var s = ParseSource(@"
ENUM MyEnum AS INT
	MEMBER m1 := 1
END ENUM

FUNCTION Start() AS VOID
	LOCAL a AS INT[]
	a := INT[]{3}
	a[MyEnum.m1] := 1
RETURN
");
            CompileAndRunWithoutExceptions(s);
        }




        // 82
        [Test(Author = "Chris", Id = "C82", Title = "Error XS1061: 'string' does not contain a definition for 'Chars' and no extension ...")]
        public static void Error_XS1061_unavailable_String_Chars()
        {
            var s = ParseStartFunction(@"
LOCAL s AS STRING
LOCAL c AS Char
s := System.String{'a',3}
c := s:Chars[1]
");
            CompileAndRunWithoutExceptions(s);
        }


 
        // 83
        [Test(Author = "Chris", Id = "C83", Title = "Error XS0034: Operator '*' is ambiguous on operands of type 'int' and 'double'")]
        public static void Error_XS0034_Operator_is_ambiguous()
        {
            var s = ParseStartFunction(@"
LOCAL y AS INT
y:= 1 - INT(y * 1.2)
");
            CompileAndRunWithoutExceptions(s);
        }


        // 84
        [Test(Author = "Chris", Id = "C84", Title = "error XS7038: Failed to emit module 'SmallXtest'.")]
        public static void Error_XS7038_Failed_to_emit_module()
        {
            var s = ParseSource(@"
// compile with /unsafe
FUNCTION Start() AS VOID
LOCAL h AS PTR
test(h)

PROC test( h AS PTR)
");
            CompileAndLoadWithoutErrors("/unsafe",s);
        }



        // 85
        [Test(Author = "Chris", Id = "C85", Title = "error XS9002: Parser error: no viable alternative at input '(ac,'")]
        public static void Error_Calling_Generic_static_method()
        {
            var s = ParseSource(@"
FUNCTION Start() AS VOID
LOCAL ac AS STRING[]
ac := <STRING>{""1"",""2"",""3""}
? System.Array.IndexOf<STRING>(ac,""2"")
");
            CompileAndRunWithoutExceptions(s);
        }





        // 86
        [Test(Author = "Chris", Id = "C86", Title = "compiler crash with #pragma options")]
        public static void Crash_with_pragma_options()
        {
            var s = ParseSource(@"
#pragma options (""lb"",on)
FUNCTION Start() AS VOID
");
            CompileAndRunWithoutExceptions(s);
        }




        // 87
        [Test(Author = "Chris", Id = "C87", Title = "error XS0106: The modifier 'public' is not valid for this item")]
        public static void Error_XS0106_access_in_interface()
        {
            var s = ParseSource(@"
INTERFACE ITest
	ACCESS Name AS STRING
END INTERFACE
");
            CompileAndLoadWithoutErrors(s);
        }



        // 88
        [Test(Author = "Chris", Id = "C88", Title = "error XS0034: Operator '*' is ambiguous on operands of type 'int' and 'double'")]
        public static void error_XS0034_operator_ambiguous_on_int_and_double()
        {
            var s = ParseSource(@"
FUNCTION Start() AS VOID
LOCAL n := 2 AS INT
n := (INT) (n * 42.5)
? n
");
            CompileAndLoadWithoutErrors(s);
        }




        // 89
        [Test(Author = "Chris", Id = "C89", Title = "Parser error: no viable alternative at input 'PROPERTY SELF'mismatched input ']' expecting EOS")]
        public static void Parser_error_default_indexed_property()
        {
            var s = ParseSource(@"
CLASS TestClass
PROPERTY SELF[index AS DWORD] AS INT GET 0
END CLASS
");
            CompileAndLoadWithoutErrors(s);
        }




    }
}
