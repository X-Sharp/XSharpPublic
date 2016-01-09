using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;

namespace XSTestCodeAnalysis
{
    struct TestResults
    {
        public int Total;
        public int Errors;

        public TestResults(int total, int errors)
        {
            Total = total;
            Errors = errors;
        }
    }
}