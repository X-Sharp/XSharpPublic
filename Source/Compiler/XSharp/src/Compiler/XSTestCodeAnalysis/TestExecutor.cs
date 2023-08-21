/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and   
limitations under the License.
*/
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

        public static TestResults RunTests(TestOptions opts)
        {
            int total = 0;
            int errors = 0;

            foreach (var m in from t in Assembly.GetExecutingAssembly().GetTypes() where t.IsClass 
                              from m in t.GetMethods() where m.GetParameters().Length == 0 &&
                                                             m.ReturnType == typeof(void) &&
                                                             !m.ContainsGenericParameters &&
                                                             m.IsStatic &&
                                                             m.CustomAttributes.Any(a => a.AttributeType == typeof(TestAttribute))
                              select m) {
                string testId = m.GetTestAttribute().GetId();
                if (opts.IsTestEnabled(testId))
                {
                    total += 1;
                    try
                    {
                        m.Invoke(null, new object[] { });
#if false // nvk: do not show successes!
                        Console.ForegroundColor = ConsoleColor.Green;
                        Console.WriteLine("Success: {0}", TestTitle(m));
                        Console.ResetColor();
#endif
                    }
                    catch (Exception e)
                    {
                        errors += 1;
                        Console.ForegroundColor = ConsoleColor.Yellow;
                        Console.WriteLine("Fail: {0}", TestTitle(m));
                        Console.ResetColor();
                        Console.WriteLine("{0}", e.InnerException.Message);
                    }
                }
            }

            return new TestResults(total, errors);
        }
    }
}
