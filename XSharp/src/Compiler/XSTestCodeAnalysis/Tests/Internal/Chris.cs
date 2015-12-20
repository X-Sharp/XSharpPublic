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
    }
}
