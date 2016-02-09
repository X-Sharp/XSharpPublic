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
System.Globalization.CultureInfo.DefaultThreadCurrentCulture  := System.Globalization.CultureInfo.CreateSpecificCulture(""en-US"")
System.Double.TryParse('1.2', r) 
if r != 1.2
    THROW Exception{String.Format('V = {0}',r)}
endif
System.Double.TryParse('2.2', REF r)
if r != 2.2
    THROW Exception{String.Format('V = {0}',r)}
endif
System.Double.TryParse('3.2', OUT r)
if r != 3.2
    THROW Exception{String.Format('V = {0}',r)}
endif
");
            CompileAndRunWithoutExceptions(s);
        }



        // 46
        // RvdH 20160201: I do not thing this is a bug. There is no need to make a new virtual property in a sealed class
        // The compiler is simply telling the user that there is a design error in his code...
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


        // 68
        [Test(Author = "Chris", Id = "C68", Title = "error XS0019: Operator '|' cannot be applied to operands of type 'int' and 'MyEnum'")]
        public static void or_operator_with_enum_and_int()
        {
            var s = ParseSource(@"
// not sure I like it, but vulcan does compile this
FUNCTION Start() AS VOID
LOCAL e AS MyEnum
e := MyEnum.m2
? ((INT)e | MyEnum.m1) != 0
? ((INT)e & MyEnum.m2) != 0

ENUM MyEnum AS Int32
	MEMBER m1 := 0
	MEMBER m2 := 1
END ENUM
");
            CompileAndRunWithoutExceptions(s);
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
   VIRTUAL METHOD ITest.MyMeth() AS Boolean
       RETURN False
END CLASS

INTERFACE ITest
	PROPERTY MyProp AS Boolean GET
	METHOD MyMeth() AS Boolean
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
    if a[1] != 1
        throw Exception{'Failed'}
    endif
RETURN
");
            CompileAndRunWithoutExceptions(s);
        }




        // 82
        // nvk: This should normally be part of the runtime, imo (not a compiler error)
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

        // 90
        [Test(Author = "Chris", Id = "C90", Title = "Crash with uint32")]
        public static void Crash_with_uint32()
        {
            var s = ParseSource(@"
FUNCTION Start() AS VOID
LOCAL d := 0X80880000 AS DWORD
? d 
LOCAL i := 0X70880000 AS INT
? i
");
            CompileAndLoadWithoutErrors(s);
        }

        // 91
        [Test(Author = "Chris", Id = "C91", Title = "VO incompatibility with virtual methods")]
        public static void Vo_incompatibility_with_virtual_methods()
        {
            var s = ParseSource(@"
// same code in vulcan compiled with no /vo options enabled
// vulcan calls child method, xsharp calls parent
CLASS ParentClass
    VIRTUAL METHOD OnTest() AS STRING
        ? 'parent'
    RETURN 'parent'
END CLASS

CLASS ChildClass INHERIT ParentClass
    METHOD OnTest() AS STRING
        ? 'child'
    RETURN 'child'
END CLASS

FUNCTION Start() AS VOID
    LOCAL o AS ParentClass
    o:= ChildClass{ }
    IF o:OnTest() == 'parent'
        THROW System.Exception{ 'parent called, vulcan calls child'}
    END IF
RETURN
");
            CompileAndRunWithoutExceptions(s);
        }

        // 92
        [Test(Author = "Chris", Id = "C92", Title = "Roslyn ASSERTION failed ")]
        public static void Roslyn_assert_failed()
        {
            var s = ParseSource(@"
//92. ASSERTION failed at LanguageService.CodeAnalysis.XSharp.Symbols.SourceMemberContainerTypeSymbol.CheckNonOverrideMember, SourceMemberContainerSymbol_ImplementationChecks.cs:line 817
// also no warnings, while a warning should be reported about hiding parent property
CLASS ParentClass
    PROPERTY Name AS INT GET 0
END CLASS

CLASS ChildClass INHERIT ParentClass
    VIRTUAL PROPERTY Name AS INT GET 0
END CLASS
");
            CompileWithWarnings(s);
        }

        // 93
        [Test(Author = "Chris", Id = "C93", Title = "Cannot override inherited method")]
        public static void Cannot_override_inherited_method()
        {
            var s = ParseSource(@"
//93. error XS0506: 'ChildClass.OnTest()': cannot override inherited member 'ParentClass.OnTest()' because it is not marked virtual, abstract, or override

// vulcan only warning about hiding base method
CLASS ParentClass
    METHOD OnTest() AS VOID
END CLASS

CLASS ChildClass INHERIT ParentClass
    VIRTUAL METHOD OnTest() AS VOID
END CLASS
");
            CompileWithWarnings(s);
        }

        // 94
        [Test(Author = "Chris", Id = "C94", Title = "String comparison relational ops '<','>' (trasnlated to String.Compare() call)")]
        public static void String_comparison_relational_operators()
        {
            var s = ParseStartFunction(@"
LOCAL c1,c2,c3 AS STRING
c1 := 'aa'
c2:= 'bb'
c3:= 'aa'
IF c1 > c2
    THROW Exception{'c1 > c2'}
ENDIF
IF !(c3 < c2)
    THROW Exception{'!(c3 > c2)'}
ENDIF
IF c1 >= c2
    THROW Exception{'c1 >= c2'}
ENDIF
IF !(c3 <= c2)
    THROW Exception{'!(c3 >= c2)'}
ENDIF
IF !(c1 >= c3 && c1 <= c3)
    THROW Exception{'!(c1 >= c3 && c1 <= c3)'}
ENDIF
");
            CompileAndLoadWithoutErrors(s);
        }

        // 95
        [Test(Author = "Chris", Id = "C95", Title = "Operator > is ambiguous on float & double")]
        public static void Operator_gt_ambiguous_float_double()
        {
            var s = ParseSource(@"
//95. error XS0034: Operator '>' is ambiguous on operands of type 'float' and 'double'
FUNCTION Start() AS VOID
LOCAL r4 AS REAL4
LOCAL r8 AS REAL8
? r4 > 0.5 // error XS0034
? r4 > r8  // no error here!
");
            CompileAndLoadWithoutErrors(s);
        }

        // 96
        [Test(Author = "Chris", Id = "C96", Title = "Cannot implicitly call operator or accessor")]
        public static void Cannot_implicitly_call_operator_or_accessor()
        {
            var s = ParseSource(@"
//96. error XS0571: 'ArrayList.this[int].get': cannot explicitly call operator or accessor
// that was part of code probably more than 10 years old, from the time that the vulcan (or cule?) compiler did not support accessing indexed properties
// not saying we should support this, just mentioning it because I had a lot of code using this!
FUNCTION Start() AS VOID
LOCAL a AS System.Collections.ArrayList
a := System.Collections.ArrayList{}
a:Add(17)
IF (int)a:get_Item(0) != 17
    THROW Exception{'a:get_Item(0) != 17'}
ENDIF
");
            CompileAndLoadWithoutErrors(s);
        }

        // 97
        [Test(Author = "Chris", Id = "C97", Title = "Cannot implicitly convert INT to INTPTR")]
        public static void Cannot_implicitly_convert_int_to_intptr()
        {
            var s = ParseSource(@"
//97. error XS0266: Cannot implicitly convert type 'int' to 'System.IntPtr'. An explicit conversion exists (are you missing a cast?)
FUNCTION Start() AS VOID
LOCAL p AS IntPtr
p := 1
p := 1u
");
            CompileAndLoadWithoutErrors(s);
        }

        // 98
        [Test(Author = "Chris", Id = "C98", Title = "Partial declarations different base classes")]
        public static void Partial_declarations_different_base_classes_v1()
        {
            var s1 = ParseSource(@"
//98. error XS0263: Partial declarations of 'test' must not specify different base classes
// file 1
PARTIAL CLASS test
END CLASS
");
            var s2 = ParseSource(@"
// file 2
PARTIAL CLASS test INHERIT System.Dynamic.DynamicObject // System.Windows.Forms.Form
END CLASS
");
            CompileAndLoadWithoutErrors(s1, s2);
        }

        // 99
        [Test(Author = "Chris", Id = "C99", Title = "Keyword 'this' not available in the current context")]
        public static void Keyword_this_not_available_in_current_context()
        {
            var s = ParseSource(@"
//Not sure if this one is the same as 31:
//99. error XS0027: Keyword 'this' is not available in the current context

CLASS Parent
CONSTRUCTOR(o AS OBJECT)
END CLASS

CLASS Child INHERIT Parent
CONSTRUCTOR()
SUPER(SELF)
END CLASS 
");
            CompileAndLoadWithoutErrors(s);
        }

        // 100
        [Test(Author = "Chris", Id = "C100", Title = "Extraneous input at dll import")]
        public static void Extraneous_input_dll_import()
        {
            var s = ParseSource(@"
//100. error: extraneous input 'ANSI','UNICODE','AUTO' expecting EOS
_DLL FUNCTION GetPrivateProfileString1(c1 AS STRING, c2 AS STRING, c3 AS STRING,o AS System.Text.StringBuilder, nSize AS DWORD, cFileName AS STRING ) AS DWORD PASCAL:KERNEL32.GetPrivateProfileStringA ANSI
_DLL FUNCTION GetPrivateProfileString2(c1 AS STRING, c2 AS STRING, c3 AS STRING,o AS System.Text.StringBuilder, nSize AS DWORD, cFileName AS STRING ) AS DWORD PASCAL:KERNEL32.GetPrivateProfileStringA UNICODE
_DLL FUNCTION GetPrivateProfileString3(c1 AS STRING, c2 AS STRING, c3 AS STRING,o AS System.Text.StringBuilder, nSize AS DWORD, cFileName AS STRING ) AS DWORD PASCAL:KERNEL32.GetPrivateProfileStringA AUTO
_DLL FUNCTION GetPrivateProfileString4(c1 AS STRING, c2 AS STRING, c3 AS STRING,o AS System.Text.StringBuilder, nSize AS DWORD, cFileName AS STRING ) AS DWORD PASCAL:KERNEL32#111
");
            CompileAndLoadWithoutErrors(s);
        }

        // 101
        [Test(Author = "Chris", Id = "C101", Title = "DLLimport must be specified on a method marked 'static extern'")]
        public static void Dllimport_static_extern()
        {
            var s = ParseSource(@"
//101. error XS0601: The DllImport attribute must be specified on a method marked 'static' and 'extern'
#using System.Runtime.InteropServices
// vulcan does not require EXTERN
CLASS Test
    [DllImport('gdi32.dll',  EntryPoint:='CreateSolidBrush')];
    STATIC METHOD CreateSolidBrush(hDC AS DWORD) AS IntPtr
END CLASS
");
            CompileAndLoadWithoutErrors(s);
            s = ParseSource(@"
// nvk: this should throw an error because the method has a body
#using System.Runtime.InteropServices
CLASS Test
    [DllImport('gdi32.dll',  EntryPoint:='CreateSolidBrush')];
    STATIC METHOD CreateSolidBrush(hDC AS DWORD) AS IntPtr
        RETURN (IntPtr)0
END CLASS
");
            CompileWithErrors(s);
        }

        // 102
        [Test(Author = "Chris", Id = "C102", Title = "Cannot be extern with body")]
        public static void Cannot_be_extern_with_body()
        {
            var s = ParseSource(@"
//102. error XS0179: 'Test.CreateSolidBrush(uint)' cannot be extern and declare a body
#using System.Runtime.InteropServices
CLASS Test
    [DllImport('gdi32.dll',  EntryPoint:='CreateSolidBrush')];
    STATIC EXTERN METHOD CreateSolidBrush(hDC AS DWORD) AS IntPtr
END CLASS
");
            CompileAndLoadWithoutErrors(s);
        }

        // 103
        [Test(Author = "Chris", Id = "C103", Title = "Extraneous input DO")]
        public static void Extraneous_input_do()
        {
            var s = ParseSource(@"
//103. error: extraneous input 'DO' expecting EOS
FUNCTION Start() AS VOID
WHILE FALSE
END DO

WHILE FALSE
ENDDO
");
            CompileAndLoadWithoutErrors(s);
        }

        // 104
        [Test(Author = "Chris", Id = "C104", Title = "Method cannot be sealed because it is not an override")]
        public static void Method_cannot_be_sealed_not_override()
        {
            var s = ParseSource(@"
// vulcan incompatibility, related to #91
CLASS Parent
    VIRTUAL METHOD Test() AS VOID
END CLASS

CLASS Child INHERIT Parent
    SEALED METHOD Test() AS VOID
END CLASS
");
            CompileAndLoadWithoutErrors(s);
        }

        // 105
        [Test(Author = "Chris", Id = "C105", Title = "FOR local declaration")]
        public static void For_local_declaration()
        {
            var s = ParseSource(@"
// used to be compile crash, bug 17
FUNCTION Start() AS VOID
FOR LOCAL n := 1 AS INT UPTO 10
NEXT
");
            CompileAndRunWithoutExceptions(s);
        }

        // 106
        [Test(Author = "Chris", Id = "C106", Title = "Crash with missing global type")]
        public static void Crash_with_missing_global_type()
        {
            var s = ParseSource(@"
GLOBAL ggg := 123 // AS INT missing
");
            CompileWithErrors(s);
        }

        // 107
        [Test(Author = "Chris", Id = "C107", Title = "No error on missing local type, gets treated as INT")]
        public static void No_error_on_missing_local_type()
        {
            var s = ParseSource(@"
FUNCTION Start() AS VOID
LOCAL c
c := '123'
");
            CompileWithErrors(s);
        }

        // 108
        [Test(Author = "Chris", Id = "C108", Title = "Name conflict with '.' static member access")]
        public static void Name_conflict_static_access()
        {
            var s = ParseSource(@"
INTERFACE ITest
    METHOD Foo() AS VOID
END INTERFACE

CLASS Test
    STATIC METHOD StaticMethod() AS VOID
END CLASS

FUNCTION Start() AS VOID
LOCAL Test AS ITest
Test.StaticMethod()
");
            CompileAndLoadWithoutErrors(s);
        }

        // 109
        [Test(Author = "Chris", Id = "C109", Title = "Static class cannot derive from parent")]
        public static void Static_class_cannot_derive_from_parent()
        {
            var s = ParseSource(@"
CLASS Parent
END CLASS
STATIC CLASS Child INHERIT Parent
    STATIC T := 1 AS INT
END CLASS
FUNCTION Start() AS VOID
    IF Child.T != 1
        THROW Exception{""STATIC check fail""}
    ENDIF
");
            CompileAndLoadWithoutErrors(s);
        }

        // 110
        // nvk 20160203: I don't think we want to 'fix' this ...
        [Test(Author = "Chris", Id = "C110", Title = "Partial declarations different base classes")]
        public static void Partial_declarations_different_base_classes()
        {
            var s = ParseSource(@"
// just mentioning it as it is an incompatibility with vulcan
// not sure if it's a good idea to 'fix' this particular one, though
// allthough it's very common scenario in vulcan code...
CLASS Parent
END CLASS

PARTIAL CLASS TestClass INHERIT Parent
END CLASS
PARTIAL CLASS TestClass
END CLASS
");
            CompileWithErrors(s);
        }

        // 111
        // RvdH 20160201: I do not thing this is a bug. There is no need to seal a method when it is not an override
        // of a method in a parent class. See my comments for Bug #62 on PlanIO.
        // nvk 20160203: If vulcan compiles it, I think we should turn it into a warning. It will be the responsibility
        // of the user to hide the warning or not.
        [Test(Author = "Chris", Id = "C111", Title = "Method cannot be sealed because it is not an override")]
        public static void Method_cannot_be_sealed_because_it_is_not_an_override()
        {
            var s = ParseSource(@"
PARTIAL CLASS TestClass
SEALED METHOD mmm() AS VOID
END CLASS
FUNCTION Start() AS VOID
  VAR o := TestClass{}
  o:mmm()
");
            CompileAndLoadWithoutErrors(s);
        }

        // 112
        [Test(Author = "Chris", Id = "C112", Title = "Interface members cannot have a definition")]
        public static void Interface_members_cannot_have_a_definition()
        {
            var s = ParseSource(@"
// related to #50, but different order of SET/GET keywords
INTERFACE ITest
    PROPERTY prop1 AS INT GET SET // ok
    PROPERTY prop2 AS INT SET GET // error
END INTERFACE
");
            CompileAndLoadWithoutErrors(s);
        }

        // 113
        [Test(Author = "Chris", Id = "C113", Title = "Inconsistent accessibility of inherited class")]
        public static void Inconsistent_accessibility_of_inherited_class()
        {
            var s = ParseSource(@"
INTERNAL CLASS Parent
END CLASS
CLASS Child INHERIT Parent
END CLASS
FUNCTION Start() AS VOID
    VAR o := Child{}
");
            CompileAndLoadWithoutErrors(s);
        }

        // 114
        [Test(Author = "Chris", Id = "C114", Title = "Virtual access properties with /vo3")]
        public static void Virtual_access_prop_vo3()
        {
            var s = ParseSource("/vo3", @"
CLASS Parent
    ACCESS acc AS STRING
    RETURN 'Parent'
    METHOD met() AS STRING
    RETURN 'Parent'
    PROPERTY prop AS STRING GET 'Parent'
END CLASS

CLASS Child INHERIT Parent
    ACCESS acc AS STRING
    RETURN 'Child'
    METHOD met() AS STRING
    RETURN 'Child'
    PROPERTY prop AS STRING GET 'Child'
END CLASS

FUNCTION Start() AS VOID
    LOCAL o AS Parent
    o:= Child{ }
    if o: met() != 'Child'
        Throw Exception{'Method called parent!'}
    end if
    if o : prop != 'Child'
        Throw Exception{'Property called parent!'}
    end if
    if o : acc != 'Child'
        Throw Exception{'Access property called parent!'}
    end if
");
            CompileAndRunWithoutExceptions("/vo3", s);
        }

        // 115
        [Test(Author = "Chris", Id = "C115", Title = "Nested default namespace not found")]
        public static void Nested_default_namespace_not_found()
        {
            var s = ParseSource("/ns:ns1.ns2", @"
CLASS TestClass
END CLASS
FUNCTION Start() AS VOID
LOCAL o AS ns1.ns2.TestClass
");
            CompileAndLoadWithoutErrors("/ns:ns1.ns2", s);
        }

        // 116
        [Test(Author = "Chris", Id = "C116", Title = "Nested default namespace: cannot access class")]
        public static void Nested_default_namespace_cannot_access_class()
        {
            var s = ParseSource("/ns:ns1.ns2",@"
BEGIN NAMESPACE ns1.ns2
CLASS TestClass
END CLASS
END NAMESPACE

CLASS AnotherClass
    PROTECT o AS TestClass
END CLASS
");
            CompileAndLoadWithoutErrors("/ns:ns1.ns2", s);
        }

        // 117
        [Test(Author = "Chris", Id = "C117", Title = "Property SET must declare a body")]
        public static void Prop_set_must_declare_body()
        {
            var s = ParseSource(@"
INTERFACE ITest
    PROPERTY prop AS INT GET SET
    PROPERTY propg AS INT GET
    //PROPERTY props AS INT SET
END INTERFACE

CLASS TestClass IMPLEMENTS ITest
    VIRTUAL PROPERTY prop AS INT GET 0 SET
    VIRTUAL PROPERTY propg AS INT GET
    //VIRTUAL PROPERTY props AS INT SET // nvk: I think it's reasonable for this to throw an error
END CLASS 
");
            CompileAndLoadWithoutErrors(s);
        }

        // 118
        [Test(Author = "Chris", Id = "C118", Title = "Compiler crash after: Assertion Failed: binding should be enclosed in a conditional access")]
        public static void Crash_118()
        {
            var s = ParseStartFunction(@"
LOCAL n AS INT
LOCAL s AS STRING
s := ( n ):ToString() 
");
            CompileAndLoadWithoutErrors(s);
        }

        // 119
        [Test(Author = "Chris", Id = "C119", Title = "Constructor chaining in body")]
        public static void Ctor_chain_in_body()
        {
            var s = ParseStartFunction(@"
CLASS Test
    CONSTRUCTOR(n AS INT)
    CONSTRUCTOR()
    SUPER()
    SELF(1)
    RETURN
END CLASS 
");
            CompileAndLoadWithoutErrors(s);
        }

        // 120
        [Test(Author = "Chris", Id = "C120", Title = "Name conflict with '.' for static member access")]
        public static void Name_conflict_static_member_access()
        {
            var s = ParseSource(@"
CLASS Alignment
    STATIC EXPORT Left AS INT
END CLASS

CLASS Foo
    PROPERTY Alignment AS INT
        SET
            IF Alignment.Left == 1
            ENDIF
        END SET
    END PROPERTY
    METHOD Bar() AS VOID
    ? Alignment.Left
END CLASS 
");
            CompileAndLoadWithoutErrors(s);
        }

        // 121
        [Test(Author = "Chris", Id = "C121", Title = "Crash with empty constructors")]
        public static void Crash_empty_ctor()
        {
            var s = ParseSource(@"
CLASS Test
    PUBLIC STATIC One AS INT
    PRIVATE CONSTRUCTOR
    STATIC CONSTRUCTOR
        One := 1
END CLASS 

FUNCTION Start() AS VOID
    IF Test.One != 1
        THROW Exception{'Static constructor not called!'}
    ENDIF
");
            CompileAndRunWithoutExceptions("/debug+ /debug:full",s);
        }

        // 122
        [Test(Author = "Chris", Id = "C122", Title = "Asterisk comments not allowed")]
        public static void Asterisk_comments()
        {
            var s = ParseSource(@"
FUNCTION Start() AS VOID
* comments here
RETURN
");
            CompileAndRunWithoutExceptions(s);
        }

    }
}
