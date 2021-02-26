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
    /// <summary>
    /// Represents an automation friendly version of a language-specific project.
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Interoperability", "CA1409:ComVisibleTypesShouldBeCreatable")]
    [ComVisible(true), CLSCompliant(false)]
    public class OAXSharpVSProject : OAVSProject
    {
        private OAVSProjectImports imports;
        private VSProjectEvents events;
        internal OAXSharpVSProject(ProjectNode project) : base(project)
        {
            this.imports = new OAVSProjectImports(this.Project);
            this.events = new OAVSProjectEvents(this);
        }
        public override Imports Imports
        {
            get
            {
                return imports;
            }
        }
        
        public override VSProjectEvents Events
        {
            get
            {
                return events;
            }
        }

    }
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Interoperability", "CA1409:ComVisibleTypesShouldBeCreatable")]
    [ComVisible(true), CLSCompliant(false)]
    public class OAVSProjectImports : VSLangProj.Imports
    {
        EnvDTE.Project project;
        List<string> imports;

        internal OAVSProjectImports(EnvDTE.Project prj)
        {
            project = prj;
            imports = new List<string>();
        }

        public EnvDTE.Project ContainingProject
        {
            get
            {
                return project;
            }
        }

        public int Count
        {
            get
            {
                return imports.Count;
            }
        }

        public DTE DTE
        {
            get
            {
                return ThreadHelper.JoinableTaskFactory.Run(async delegate
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                        return project.DTE;
                    });
            }
        }

        public object Parent
        {
            get
            {
                return project;
            }
        }

        public void Add(string bstrImport)
        {
            imports.Add(bstrImport);
        }

        public IEnumerator GetEnumerator()
        {
            return imports.GetEnumerator();
        }

        public string Item(int lIndex)
        {
            if (lIndex >= 0 && lIndex < imports.Count)
                return imports[lIndex];
            return null;
        }

        public void Remove(object index)
        {
            if (index is Int32)
            {
                int iIndex = (Int32)index;
                if (iIndex > 0 && iIndex <= imports.Count)
                    imports.Remove(imports[iIndex - 1]);
            }
            else if (index is String)
            {
                string sIndex = index as String;
                if (imports.Contains(sIndex))
                {
                    imports.Remove(sIndex);
                }
            }
        }
    }
}
