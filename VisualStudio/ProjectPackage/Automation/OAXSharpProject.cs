//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Project.Automation;
using System;
using System.Collections;
using System.Collections.Generic;
using EnvDTE;
using Microsoft.VisualStudio.Project;
using VSLangProj;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
#if PACKAGEREFERENCE
using VSLangProj150;
using VSLangProj140;
using VSLangProj80;
#endif
namespace XSharp.Project
{
    /// <summary>
    /// Represents automation object corresponding to a project.
    /// </summary>
    [CLSCompliant(false), ComVisible(true)]
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Interoperability", "CA1409:ComVisibleTypesShouldBeCreatable")]
    public class OAXSharpProject : OAProject
    {
        // =========================================================================================
        // Member variables
        // =========================================================================================

        /// <summary>
        /// Properties associated with the project.
        /// </summary>
        private OAProperties properties;
        #region Constructors
        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="OAXSharpProject"/> class.
        /// </summary>
        /// <param name="project">The node to which this project belongs.</param>
        public OAXSharpProject(XProjectNode xproject)
            : base(xproject)
        {
            if (xproject != null)
            {
                this.properties = new OAProperties(xproject.NodeProperties);
            }
        }
        #endregion
        /// <summary>
        /// Properties of the project
        /// </summary>
        /// <value>Collection of all project properties</value>
        public override EnvDTE.Properties Properties
        {
            get
            {
                return this.properties;
            }
        }
		
    }
 }
