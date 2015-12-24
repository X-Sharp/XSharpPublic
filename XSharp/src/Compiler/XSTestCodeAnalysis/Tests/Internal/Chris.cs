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
        public static void Crash_InitLocalIntArray() {
            var s = ParseStartFunction(@"
LOCAL a AS INT[]
a := INT[]{10}
");
            CompileAndRunWithoutExceptions(s);
        }

        [Test(Author = "Chris", Id = "C2", Title = "Error XS0515: Access modifiers not allowed on static constructors")]
        public static void Error_XS0515_StaticConstructor() {
            var s = ParseSource(@"
CLASS Test
STATIC CONSTRUCTOR()
END CLASS
");
            CompileAndLoadWithoutErrors(s);
        }

        [Test(Author = "Chris", Id = "C3", Title = "Error XS0165: Use of unassigned local (should be warning)")]
        public static void Error_XS0165_UseOfUnassignedLocal() {
            var s = ParseStartFunction(@"
LOCAL cRet as INT
? cRet
");
            CompileAndRunWithoutExceptions(s);
        }

        [Test(Author = "Chris", Id = "C4", Title = "Crash with STRUCTURE declaration")]
        public static void Crash_StructDeclaration() {
            var s = ParseSource(@"
STRUCTURE xFloat
CONSTRUCTOR(n AS INT)
END STRUCTURE
");
            CompileAndLoadWithoutErrors(s);
        }

        [Test(Author = "Chris", Id = "C65.1", Title = "Crash with invalid code (Use keywords as names #1)")]
        public static void Crash_InvalidCode_KeywordNames_1() {
            var s = ParseSource(@"
CLASS TestClass
ACCESS OPTIONS AS INT
RETURN 0
");
            CompileWithErrors(s);
        }

        [Test(Author = "Chris", Id = "C65.2", Title = "Crash with invalid code (Use keywords as names #2)")]
        public static void Crash_InvalidCode_KeywordNames_2() {
            var s = ParseSource(@"
CLASS TestClass
PROPERTY INT AS INT AUTO
END CLASS
");
            CompileWithErrors(s);
        }

        [Test(Author = "Chris", Id = "C66", Title = "Crash with missing arguments")]
        public static void Crash_MissingArguments() {
            var s = ParseSource(@"
FUNCTION Start() AS VOID
SomeMethod( , , 1) 
");
            CompileWithErrors(s);
        }

        [Test(Author = "Chris", Id = "C74", Title = "Crash with invalid code (constructor return type)")]
        public static void Crash_InvalidCode_NoValidCtor() {
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

        [Test(Author = "Chris", Id = "C75", Title = "Crash with unsupported native type")]
        public static void Crash_UnsupportedNativeType() {
            var s = ParseSource(@"
CLASS TestClass
METHOD TestMethod() AS VOID
LOCAL a AS ARRAY
LOCAL s AS SYMBOL
LOCAL d AS DATE
LOCAL u AS USUAL
END CLASS 
");
            CompileWithErrors(s);
        }
    }
}
