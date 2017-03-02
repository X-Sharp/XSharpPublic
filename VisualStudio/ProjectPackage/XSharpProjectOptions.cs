//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System;
using Microsoft.VisualStudio.Project;
using System.Diagnostics;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;

using System.IO;
using MSBuild = Microsoft.Build.Evaluation;
using System.Runtime.InteropServices;

namespace XSharp.Project
{
    /// <summary>
    /// This class extends the ConfigProvider
    /// </summary>
    /// 
    public class XSharpProjectOptions: ProjectOptions
    {
        public string Dialect { get; set; }
        public string DisabledWarnings { get; set; }
        public string PPDefines { get; set; }
        public string IncludePaths { get; set; }
        public bool NoStdDefs { get; set; }
        public ConfigCanonicalName Configuration { get; set; }
        public bool MustInitialize { get; set; }
        public string[] VOFlags { get; set; }
        public XSharpProjectOptions() : base()
        {
            MustInitialize = true;
        }
    }
}
