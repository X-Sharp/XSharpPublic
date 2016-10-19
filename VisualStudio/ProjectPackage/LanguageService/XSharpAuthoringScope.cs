//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using MVTI=Microsoft.VisualStudio.TextManager.Interop;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.LanguageService
{
    internal class XSharpAuthoringScope : AuthoringScope
    {
        public override string GetDataTipText(int line, int col, out MVTI.TextSpan span)
        {
            span = new MVTI.TextSpan();
            return null;
        }

        public override Declarations GetDeclarations(MVTI.IVsTextView view,
                                                     int line,
                                                     int col,
                                                     TokenInfo info,
                                                     ParseReason reason)
        {
            return null;
        }

        public override Methods GetMethods(int line, int col, string name)
        {
            return null;
        }

        public override string Goto(VSConstants.VSStd97CmdID cmd, MVTI.IVsTextView textView, int line, int col, out MVTI.TextSpan span)
        {
            span = new MVTI.TextSpan();
            return null;
        }

    }
}
