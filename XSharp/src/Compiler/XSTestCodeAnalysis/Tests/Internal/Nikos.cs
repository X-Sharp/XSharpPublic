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
            CompileAndRunWithoutExceptions("/az",s);
        }

        [Test(Author = "Nikos", Id = "N3", Title = "Do not init string vars to empty string (no /vo2)")]
        public static void InitStringVarsToNullString()
        {
            var s = ParseStartFunction(@"
LOCAL s AS STRING
IF s != NULL
  THROW Exception{'<s> is not NULL'}
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
");
            CompileAndRunWithoutExceptions("/vo2", s);
        }
    }
}
