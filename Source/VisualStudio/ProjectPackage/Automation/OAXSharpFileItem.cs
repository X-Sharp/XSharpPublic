//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Project;

using EnvDTE;
using XSharp.Project.WPF;
using XSharp.Project.FileCodeModel;
using Microsoft.VisualStudio.Debugger.Interop;

namespace XSharp.Project
{
 
    [ComVisible(true)]
    [Guid("5DD5CB32-E9C3-4321-891C-1363401CA106")]
    public class OAXSharpFileItem : OAFileItem
    {
        #region Constructors
        /// <summary>
        /// Public constructor.
        /// </summary>
        /// <param name="project">Automation project.</param>
        /// <param name="node">Custom file node.</param>
        public OAXSharpFileItem(OAProject project, FileNode node)
            : base(project, node)
        {
        }
        #endregion

        public override Window Open(string viewKind)
        {
            if (string.Compare(viewKind, EnvDTE.Constants.vsViewKindPrimary) == 0)
            {
                // Get the subtype and decide the viewkind based on the result.
                //
                if (Node.HasDesigner)
                    return base.Open(EnvDTE.Constants.vsViewKindDesigner);
            }

            return base.Open(viewKind);
        }

        /// <summary>
        /// Gets the FileCodeModel object for the project item.
        /// </summary>
        public override EnvDTE.FileCodeModel FileCodeModel
        {
            get
            {
                return new XSharpFileCodeModel(this, Node);
            }
        }

    }
}
