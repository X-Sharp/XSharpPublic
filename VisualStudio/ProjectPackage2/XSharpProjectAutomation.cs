//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Runtime.InteropServices;
using Microsoft.VisualStudioTools.Project.Automation;
using Microsoft.VisualStudioTools.Project;

using EnvDTE;


namespace XSharp.Project
{
    [ComVisible(true)]
    internal class OAXSharpProject : OAProject
    {
        #region Constructors
        /// <summary>
        /// Public constructor.
        /// </summary>
        /// <param name="project">Custom project.</param>
        internal  OAXSharpProject(XSharpProjectNode project)
            : base(project)
        {
        }
        #endregion
    }

    [ComVisible(true)]
    [Guid("5DD5CB32-E9C3-4321-891C-1363401CA106")]
    internal class OAXSharpFileItem : OAFileItem
    {
        #region Constructors
        /// <summary>
        /// Public constructor.
        /// </summary>
        /// <param name="project">Automation project.</param>
        /// <param name="node">Custom file node.</param>
        internal OAXSharpFileItem(OAProject project, FileNode node)
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
    }
}
