//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Project.Automation;
using System;
using Microsoft.VisualStudio.Project;

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
            // HACK
            var stack = new System.Diagnostics.StackTrace();
            foreach (var frame in stack.GetFrames())
            {
                if (frame.GetMethod().DeclaringType.Name.Contains("NuGet"))
                {
                    return true;
                }
            }
            return false;

        }
        const string CSharpProjectType = "{fae04ec0-301f-11d3-bf4b-00c04f79efbc}";
        /// <summary>
        /// Gets a GUID string indicating the kind or type of the object.
        /// </summary>
        
        public override string Kind
        {
            get
            {
                if (MustReturnCSharp())
                {
                    // The Nuget Utility has a hard coded check for C#, VB and F#
                    return CSharpProjectType;
                }
                return Project.ProjectGuid.ToString("B");
            }
        }

    }
 }
