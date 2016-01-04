using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;

namespace XSTestCodeAnalysis
{
    class TestExecutor
    {
        static string TestTitle(MethodInfo m)
        {
            string title = '"' + (m.GetTestAttribute().GetTitle() ?? m.Name) + '"';
            if (m.GetTestAttribute().GetAuthor() != null)
                title += " (Reported by: "+m.GetTestAttribute().GetAuthor()+")";
            if (m.GetTestAttribute().GetId() != null)
                title += " (Id: "+m.GetTestAttribute().GetId()+")";
            return title;
        }

        public static int RunTests()
        {
            int errors = 0;

            foreach (var m in from t in Assembly.GetExecutingAssembly().GetTypes() where t.IsClass 
                              from m in t.GetMethods() where m.GetParameters().Length == 0 &&
                                                             m.ReturnType == typeof(void) &&
                                                             !m.ContainsGenericParameters &&
                                                             m.IsStatic &&
                                                             m.CustomAttributes.Any(a => a.AttributeType == typeof(TestAttribute))
                              select m) {
                try {
                    m.Invoke(null, new object[] {});
                    Console.ForegroundColor = ConsoleColor.Green;
                    Console.WriteLine("Success: {0}",TestTitle(m));
                    Console.ResetColor();
                }
                catch (Exception e) {
                    Console.ForegroundColor = ConsoleColor.Yellow;
                    Console.WriteLine("Fail: {0}",TestTitle(m));
                    Console.ResetColor();
                    Console.WriteLine("{0}",e);
                }
            }

            return errors;
        }
    }
}
