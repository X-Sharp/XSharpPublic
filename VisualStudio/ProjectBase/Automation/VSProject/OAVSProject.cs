/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation.
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A
 * copy of the license can be found in the License.txt file at the root of this distribution. 
 * 
 * You must not remove this notice, or any other, from this software.
 *
 * ***************************************************************************/

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using EnvDTE;
using VSLangProj;

namespace Microsoft.VisualStudio.Project.Automation
{
    /// <summary>
    /// Represents an automation friendly version of a language-specific project.
    /// </summary>
    [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "OAVS")]
    [ComVisible(true), CLSCompliant(false)]
    public class OAVSProject : VSProject
    {
        #region fields
        private ProjectNode project;
        private OAVSProjectEvents events;
        private OAVSProjectImports imports;
        #endregion

        #region ctors
        internal OAVSProject(ProjectNode project)
        {
            this.project = project;
            this.imports = new OAVSProjectImports(this.Project);
        }
        #endregion

        #region VSProject Members

        public virtual ProjectItem AddWebReference(string bstrUrl)
        {
			Debug.Fail("VSProject.AddWebReference not implemented");
            throw new NotImplementedException();
        }

        public virtual BuildManager BuildManager
        {
            get
            {
                return new OABuildManager(this.project);
            }
        }

        public virtual void CopyProject(string bstrDestFolder, string bstrDestUNCPath, prjCopyProjectOption copyProjectOption, string bstrUsername, string bstrPassword)
        {
			Debug.Fail("VSProject.References not implemented");
            throw new NotImplementedException();
        }

        public virtual ProjectItem CreateWebReferencesFolder()
        {
			Debug.Fail("VSProject.CreateWebReferencesFolder not implemented");
            throw new NotImplementedException();
        }

        public virtual DTE DTE
        {
            get
            {
                return (EnvDTE.DTE)this.project.Site.GetService(typeof(EnvDTE.DTE));
            }
        }

        public virtual VSProjectEvents Events
        {
            get
            {
                if(events == null)
                    events = new OAVSProjectEvents(this);
                return events;
            }
        }

        public virtual void Exec(prjExecCommand command, int bSuppressUI, object varIn, out object pVarOut)
        {
			Debug.Fail("VSProject.Exec not implemented");
            throw new NotImplementedException(); ;
        }

        public virtual void GenerateKeyPairFiles(string strPublicPrivateFile, string strPublicOnlyFile)
        {
			Debug.Fail("VSProject.GenerateKeyPairFiles not implemented");
            throw new NotImplementedException(); ;
        }

        public virtual string GetUniqueFilename(object pDispatch, string bstrRoot, string bstrDesiredExt)
        {
			Debug.Fail("VSProject.GetUniqueFilename not implemented");
            throw new NotImplementedException();
        }

        public virtual Imports Imports
        {
            get
            {
                return imports;
            }
        }

        public virtual EnvDTE.Project Project
        {
            get
            {
                return this.project.GetAutomationObject() as EnvDTE.Project;
            }
        }

        public virtual References References
        {
            get
            {
                ReferenceContainerNode references = project.GetReferenceContainer() as ReferenceContainerNode;
                if(null == references)
                {
                    return new OAReferences(null, project);
                }
                return references.Object as References;
            }
        }

        public virtual void Refresh()
        {
			Debug.Fail("VSProject.Refresh not implemented");
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1065:DoNotRaiseExceptionsInUnexpectedLocations")]
        public virtual string TemplatePath
        {
            get
            {
				Debug.Fail("VSProject.TemplatePath not implemented");
                throw new NotImplementedException();
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1065:DoNotRaiseExceptionsInUnexpectedLocations")]
        public virtual ProjectItem WebReferencesFolder
        {
            get
            {
				Debug.Fail("VSProject.WebReferencesFolder not implemented");
                throw new NotImplementedException();
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1065:DoNotRaiseExceptionsInUnexpectedLocations")]
        public virtual bool WorkOffline
        {
            get
            {
				Debug.Fail("VSProject.WorkOffLine not implemented");
                throw new NotImplementedException();
            }
            set
            {
				Debug.Fail("VSProject.Set_WorkOffLine not implemented");
                throw new NotImplementedException();
            }
        }

        #endregion
    }

    /// <summary>
    /// Provides access to language-specific project events
    /// </summary>
    [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "OAVS")]
    [ComVisible(true), CLSCompliant(false)]
    public class OAVSProjectEvents : VSProjectEvents
    {
        #region fields
        private OAVSProject vsProject;
        private VSLangProj.ImportsEvents importsEvents;
        #endregion

        #region ctors
        public OAVSProjectEvents(OAVSProject vsProject)
        {
            this.vsProject = vsProject;
            this.importsEvents = new VSLangProj.ImportsEventsClass();
        }
        #endregion

        #region VSProjectEvents Members

        public virtual BuildManagerEvents BuildManagerEvents
        {
            get
            {
                return vsProject.BuildManager as BuildManagerEvents;
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1065:DoNotRaiseExceptionsInUnexpectedLocations")]
        public virtual ImportsEvents ImportsEvents
        {
            get
            {
                return importsEvents;
            }
        }

        public virtual ReferencesEvents ReferencesEvents
        {
            get
            {
                return vsProject.References as ReferencesEvents;
            }
        }

        #endregion
    }

    public class OAVSProjectImports : VSLangProj.Imports
    {
        EnvDTE.Project project;
        List<string> imports;

        internal OAVSProjectImports( EnvDTE.Project prj)
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
                return project.DTE;
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

