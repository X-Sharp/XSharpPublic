//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Project.Automation;
using System;
using Microsoft.VisualStudio.Project;
using Community.VisualStudio.Toolkit;
using System.IO;

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
        // do not name this "NuGet" because then the stack will contain NuGet
        private bool MustReturnCSharp()
        {
            // HACK: Check to see if we are called from NuGet
            var stack = new System.Diagnostics.StackTrace();

            var max = stack.FrameCount;
            if (max > 5)
                max = 5;
            for (int iFrame = 0; iFrame < max; iFrame++)
            {
                var m = stack.GetFrame(iFrame).GetMethod();
                var t = m.DeclaringType;
                var a = t.Assembly;
                if (a.FullName.Contains("NuGet"))
                    return true;

            }
            return false;
        }
        const string CSharpProjectType = "{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}";
        /// <summary>
        /// Gets a GUID string indicating the kind or type of the object.
        /// </summary>

        public override string Kind
        {
            get
            {
                // When we are loading or building return the string fast
                if (XSharpProjectNode.InContextMenu &&
                    XSharpFileNode.CurrentItem != null &&
                    XSharpFileNode.CurrentItem.FileType == XSharpModel.XFileType.Config &&
                    XSharpFileNode.CurrentItem.FileName.ToLower().EndsWith("packages.config") &&
                    MustReturnCSharp() )
                {
                    // The Nuget Utility to convert packages.confif has a hard coded check for C#, VB and F#
                    return CSharpProjectType;
                }
                return Project.ProjectGuidString;
            }
        }

    }
 }
