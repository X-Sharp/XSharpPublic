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
    }
}
