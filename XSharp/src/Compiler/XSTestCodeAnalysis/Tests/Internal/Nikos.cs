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
    }
}
