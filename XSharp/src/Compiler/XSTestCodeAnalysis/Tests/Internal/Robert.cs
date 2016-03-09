using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSTestCodeAnalysis
{
    public partial class TestClass
    {
        [Test(Author = "Robert", Id = "R1", Title = "NOP Statement")]
        public static void Nop_Statement()
        {
            var s = ParseSource(@"
Function Start AS VOID
LOCAL i as LONG
IF i == 0
    ? i
ELSE
    NOP
ENDIF
RETURN

");
            CompileAndLoadWithoutErrors(s);
        }
        [Test(Author = "Robert", Id = "R2", Title = "On and Off keywords")]
        public static void On_And_Off_Keywords()
        {
            var s = ParseSource(@"
ENUM Test
MEMBER On
MEMBER Off
END ENUM
");
            CompileAndLoadWithoutErrors(s);
        }

        [Test(Author = "Robert", Id = "R3", Title = "MissingParameterTypes")]
        public static void MissingParameterTypes()
        {
            var s = ParseSource(@"
CLASS Test
CONSTRUCTOR(a)
METHOD Foo(bar) AS LONG
RETURN 1
END CLASS
");
            CompileWithErrors(s);
        }

        [Test(Author = "Robert", Id = "R4", Title = "ConstructorCallingConvention")]
        public static void ConstructorCallingConvention()
        {
            var s = ParseSource(@"
CLASS Test
CONSTRUCTOR() STRICT
END CLASS
");
            CompileAndLoadWithoutErrors(s);
        }

    }
}
