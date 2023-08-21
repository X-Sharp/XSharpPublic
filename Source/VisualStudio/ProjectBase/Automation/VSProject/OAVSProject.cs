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
using Microsoft.VisualStudio.Shell;
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
        #endregion

        #region ctors
        public OAVSProject(ProjectNode project)
        {
            this.project = project;
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
                ThreadHelper.ThrowIfNotOnUIThread();
                return (EnvDTE.DTE)this.project.Site.GetService(typeof(EnvDTE.DTE));
            }
        }

        public virtual VSProjectEvents Events
        {
            get
            {
                if (events == null)
                    events = new OAVSProjectEvents(this);
                return events;
            }
        }

        public virtual void Exec(prjExecCommand command, int bSuppressUI, object varIn, out object pVarOut)
        {
            Debug.Fail("VSProject.Exec not implemented");
            throw new NotImplementedException(); 
        }

        public virtual void GenerateKeyPairFiles(string strPublicPrivateFile, string strPublicOnlyFile)
        {
            Debug.Fail("VSProject.GenerateKeyPairFiles not implemented");
            throw new NotImplementedException(); 
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
                Debug.Fail("VSProject.Imports not implemented");
                throw new NotImplementedException();
            }
        }

        public virtual EnvDTE.Project Project
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                return this.project.GetAutomationObject() as EnvDTE.Project;
            }
        }

        EnvDTE.Properties _properties = null;
        public virtual EnvDTE.Properties Properties
        {
            get
            {
                if (_properties == null)
                {
                    _properties = new OAProperties(this.project.NodeProperties);
                }
                return _properties;
            }
        }
        public virtual References References
        {
            get
            {
                ReferenceContainerNode references = project.GetReferenceContainer() as ReferenceContainerNode;
                if (null == references)
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
        #endregion

        #region ctors
        public OAVSProjectEvents(OAVSProject vsProject)
        {
            this.vsProject = vsProject;
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
                //Debug.Fail("VSProjectEvents.ImportsEvents not implemented");
                //throw new NotImplementedException();
                return null;
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

}

