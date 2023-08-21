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
using System.IO;
using System.Runtime.InteropServices;

namespace XSTestCodeAnalysis
{
    public class Program
    {
        public static int Main(string[] args)
        {
#if (DEBUG)
            System.Diagnostics.Debug.Listeners.Add(new System.Diagnostics.ConsoleTraceListener());
#endif
            var res = TestExecutor.RunTests(new TestOptions(args));

            if (res.Total > 0)
            {
                Console.ForegroundColor = ConsoleColor.White;
                Console.WriteLine("Total {0} tests, failed {1} tests", res.Total, res.Errors);
                Console.ResetColor();
            }

            return res.Errors;
        }
    }
}
