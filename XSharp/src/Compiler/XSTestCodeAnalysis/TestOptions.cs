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