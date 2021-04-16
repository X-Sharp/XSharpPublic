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
using Microsoft.VisualStudio.Shell.Interop;
using VSLangProj;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;

namespace Microsoft.VisualStudio.Project.Automation
{
    [CLSCompliant(false)]
    public class OABuildManager : ConnectionPointContainer,
                                    IEventSource<_dispBuildManagerEvents>,
                                    BuildManager,
                                    BuildManagerEvents
    {
        private ProjectNode projectManager;

        internal OABuildManager(ProjectNode project)
        {
            projectManager = project;
            AddEventSource<_dispBuildManagerEvents>(this as IEventSource<_dispBuildManagerEvents>);
        }


        #region BuildManager Members

        public virtual string BuildDesignTimeOutput(string bstrOutputMoniker)
        {
            throw new NotImplementedException();
        }

        public virtual EnvDTE.Project ContainingProject
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                return projectManager.GetAutomationObject() as EnvDTE.Project;
            }
        }

        public virtual EnvDTE.DTE DTE
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                return projectManager.Site.GetService(typeof(EnvDTE.DTE)) as EnvDTE.DTE;
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1065:DoNotRaiseExceptionsInUnexpectedLocations")]
        public virtual object DesignTimeOutputMonikers
        {
            get
            {
                // do not return the file because then we also need to implement BuildDesignTimeOutput 
                // and really build the DLL in the background for the designer
                //var project = this.projectManager as XSharp.Project.XProjectNode;
                //string file = this.projectManager.GetOutputAssembly(project.CurrentConfig.ConfigCanonicalName);
                //return new string[] { file };
                return new string[] {  };
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1065:DoNotRaiseExceptionsInUnexpectedLocations")]
        public virtual object Parent
        {
            get { throw new NotImplementedException(); }
        }

        #endregion

        #region _dispBuildManagerEvents_Event Members

        public event _dispBuildManagerEvents_DesignTimeOutputDeletedEventHandler DesignTimeOutputDeleted;

        public event _dispBuildManagerEvents_DesignTimeOutputDirtyEventHandler DesignTimeOutputDirty;

        #endregion

		private void OnDesignTimeOutputDeleted(object sender, EventArgs args)
		{
			if (DesignTimeOutputDeleted == null)
				return;
            ThreadHelper.ThrowIfNotOnUIThread();

            string moniker = OABuildManager.GetOutputMoniker(sender);
			if (!String.IsNullOrEmpty(moniker))
				DesignTimeOutputDeleted(moniker);
		}
        private static string GetOutputMoniker(object sender)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            IVsOutput2 output = sender as IVsOutput2;
            if (output == null)
                return null;
            string moniker;
            output.get_CanonicalName(out moniker);
            return moniker;
        }

        #region IEventSource<_dispBuildManagerEvents> Members

        void IEventSource<_dispBuildManagerEvents>.OnSinkAdded(_dispBuildManagerEvents sink)
        {
            DesignTimeOutputDeleted += new _dispBuildManagerEvents_DesignTimeOutputDeletedEventHandler(sink.DesignTimeOutputDeleted);
            DesignTimeOutputDirty += new _dispBuildManagerEvents_DesignTimeOutputDirtyEventHandler(sink.DesignTimeOutputDirty);
        }

        void IEventSource<_dispBuildManagerEvents>.OnSinkRemoved(_dispBuildManagerEvents sink)
        {
            DesignTimeOutputDeleted -= new _dispBuildManagerEvents_DesignTimeOutputDeletedEventHandler(sink.DesignTimeOutputDeleted);
            DesignTimeOutputDirty -= new _dispBuildManagerEvents_DesignTimeOutputDirtyEventHandler(sink.DesignTimeOutputDirty);
        }

        #endregion

        protected virtual void OnDesignTimeOutputDeleted(string outputMoniker)
        {
            var handlers = this.DesignTimeOutputDeleted;
            if (handlers != null)
            {
                handlers(outputMoniker);
            }
        }

        protected virtual void OnDesignTimeOutputDirty(string outputMoniker)
        {
            var handlers = this.DesignTimeOutputDirty;
            if (handlers != null)
            {
                handlers(outputMoniker);
            }
        }
    }
}
