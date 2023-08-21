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
    class TestOptions
    {
        HashSet<string> TestIds = null;

        public TestOptions(string[] args)
        {
            foreach (var arg in args)
            {
                if (arg.StartsWith("/"))
                {
                    var opt = arg.Substring(1).Split(':');
                    if (opt.Length == 0 || opt.Length > 2)
                        throw new ArgumentException("Invalid Option: " + arg);
                    string argName = opt[0].ToLower();
                    string argVal = opt.Length > 1 ? opt[1] : "";
                    switch (argName)
                    {
                        case "run":
                            EnableTest(argVal);
                            break;
                        default:
                            throw new ArgumentException("Invalid Option: " + arg);
                    }
                }
                else
                    throw new ArgumentException("Invalid Option: " + arg);
            }
        }

        void EnableTest(string id)
        {
            if (TestIds == null)
            {
                TestIds = new HashSet<string>(Microsoft.CodeAnalysis.CaseInsensitiveComparison.Comparer);
            }
            TestIds.Add(id);
        }

        public bool IsTestEnabled(string id)
        {
            if (TestIds != null)
            {
                return TestIds.Contains(id);
            }
            return true;
        }
    }
}