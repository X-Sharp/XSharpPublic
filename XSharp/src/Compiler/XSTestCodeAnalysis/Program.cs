// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.IO;
using System.Runtime.InteropServices;

namespace XSTestCodeAnalysis
{
    public class Program
    {
        public static int Main(string[] args)
        {
/*#if (DEBUG)
            System.Diagnostics.Debug.Listeners.Add(new System.Diagnostics.ConsoleTraceListener());
#endif*/
            return TestExecutor.RunTests();
        }
    }
}
