//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Project;

using EnvDTE;


namespace XSharp.Project
{
    [ComVisible(true)]
    public class OAXSharpProject : OAProject
    {
        #region Constructors
        /// <summary>
        /// Public constructor.
        /// </summary>
        /// <param name="project">Custom project.</param>
        public OAXSharpProject(XSharpProjectNode project)
            : base(project)
        {
        }
        #endregion
    }

}
