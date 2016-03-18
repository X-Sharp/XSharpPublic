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


        [Test(Author = "Robert", Id = "R7", Title = "MissingTypes")]
        public static void MissingTypes()
        {
            var s = ParseSource(@"
DEFINE Foo := 10
FUNCTION Start AS VOID
 
RETURN
VOSTRUCT MyTest
    MEMBER Mem1

UNION MyUnion
    MEMBER Member1
    MEMBER Member2 

DELEGATE Tester 

CLASS Test
    EXPORT Name
    PROPERTY FullName GET Name
    EVENT OnNameChanged 
    OPERATOR+(oLeft as Test, oRight as Test)
        RETURN oLeft
END CLASS
");
            CompileAndRunWithoutExceptions(s);

        }

        [Test(Author = "Robert", Id = "R8", Title = "** AS alias for Exponent")]
        public static void StarStarExponent()
        {
            var s = ParseStartFunction(@"
LOCAL r AS REAL8
r := (int) (2 ** 0)
IF r != 1
  THROW Exception{'Exp 0 failed'}
ENDIF
r := (int) (2 ** 1)
IF r != 2
  THROW Exception{'Exp 1 failed'}
ENDIF
r := (int) (2 ** 2)
IF r != 4
  THROW Exception{'Exp 2 failed'}
ENDIF
r := (int) (2 ** 3)
IF r != 8
  THROW Exception{'Exp 3 failed'}
ENDIF
");
            CompileAndRunWithoutExceptions(s);
        }
        [Test(Author = "Robert", Id = "R9", Title = "**= and ^= Expression")]
        public static void ExponentEqualsExponent()
        {
            var s = ParseStartFunction(@"
LOCAL r AS REAL8
r := 3
r ^= 2
IF r != 9
  THROW Exception{'Exp ^=  failed'}
ENDIF
r := 3
r **= 2
IF r != 9
  THROW Exception{'Exp **= failed'}
ENDIF

");
            CompileAndRunWithoutExceptions(s);
        }


        [Test(Author = "Robert", Id = "R10", Title = "> and >> ")]
        public static void GTandRShift()
        {
            var s = ParseStartFunction(@"
LOCAL x AS LONG
x := 8
x := x >> 1
IF x != 4
  THROW Exception{'Right shift failed:' +x:ToString()}
ENDIF
x >>= 1
IF x != 2
  THROW Exception{'Right shift failed' +x:ToString()}
ENDIF


");
            CompileAndRunWithoutExceptions(s);
        }
        [Test(Author = "Robert", Id = "R11", Title = "NestedGeneric List<Tuple<int,int>>")]
        public static void NestedGeneric()
        {
            var s = ParseSource(@"
using System.Collections.Generic
FUNCTION Start as int
var x := List<Tuple<int,int>>{}
return x:Count
");
            CompileAndRunWithoutExceptions(s);
        }


    }
}


