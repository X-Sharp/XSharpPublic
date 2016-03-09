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

        [Test(Author = "Robert", Id = "R5", Title = "_AND and _OR Operation incorrect")]
        public static void AndOperationIncorrect()
        {
            var s = ParseSource(@"
FUNCTION Start AS VOID
 LOCAL iResult as LONG
 iResult := _OR(1 ==2) 
 if iResult != 3 
        THROW Exception{String.Format('Result incorrect',iResult)}
 endif
 iResult := _AND(2 == 2)
 if iResult != 2 
        THROW Exception{String.Format('Result incorrect',iResult)}
 endif
RETURN
");
            CompileWithErrors(s);
        }

        [Test(Author = "Robert", Id = "R6", Title = "_AND and _OR Operation Correct")]
        public static void AndOperationCorrect()
        {
            var s = ParseSource(@"
FUNCTION Start AS VOID
 LOCAL iResult as LONG
 iResult := _OR(1 ,2) 
 if iResult != 3 
    THROW Exception{String.Format('Result incorrect: {0} ',iResult)}
 endif
 iResult := _AND(2 , 2)
 if iResult != 2 
    THROW Exception{String.Format('Result incorrect: {0}',iResult)}
 endif
 RETURN
");
            CompileAndRunWithoutExceptions(s);
        }


    }
}
