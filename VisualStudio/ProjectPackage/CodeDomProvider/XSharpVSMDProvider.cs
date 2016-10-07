//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using Microsoft.VisualStudio.Designer.Interfaces;
using Microsoft.VisualStudio.Project;
using System.Diagnostics;

namespace XSharp.Project
{
    internal class XSharpVSMDProvider : IVSMDCodeDomProvider
    {
        private readonly FileNode _XSharpFileNode;

        public XSharpVSMDProvider(FileNode XSharpFileNode)
        {
            Trace.Assert(XSharpFileNode is XSharpFileNode || XSharpFileNode is XSharpDependentFileNode);
            _XSharpFileNode = XSharpFileNode;
        }

        #region IVSMDCodeDomProvider Members

        object IVSMDCodeDomProvider.CodeDomProvider
        {
            get
            {
                //
                var XSharpDependentFileNode = _XSharpFileNode as XSharpDependentFileNode;

                if (XSharpDependentFileNode != null)
                    return XSharpDependentFileNode.CodeDomProvider;

                var XSharpFileNode = _XSharpFileNode as XSharpFileNode;

                if (XSharpFileNode != null)
                    return XSharpFileNode.CodeDomProvider;

                // Mmm, not sure I can do something for you, anyway..
                return new CodeDom.XSharpCodeDomProvider(); 
            }
        }

        #endregion
    }
}
