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
using EnvDTE;
using System;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;

namespace Microsoft.VisualStudio.Project.Automation
{
    [SuppressMessage("Microsoft.Interoperability", "CA1405:ComVisibleTypeBaseTypesShouldBeComVisible")]
    [ComVisible(true), CLSCompliant(false)]
    public class OANestedProjectItem : OAProjectItem<NestedProjectNode>
    {
        #region fields
        EnvDTE.Project nestedProject;
        #endregion

        #region ctors
        internal OANestedProjectItem(OAProject project, NestedProjectNode node)
            : base(project, node)
        {
            Utilities.ArgumentNotNull("node", node);

            object nestedproject;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            if(ErrorHandler.Succeeded(node.NestedHierarchy.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_ExtObject, out nestedproject)))
            {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                this.nestedProject = nestedproject as EnvDTE.Project;
            }
            });
        }

        #endregion

        #region overridden methods
        /// <summary>
        /// Returns the collection of project items defined in the nested project
        /// </summary>
        public override ProjectItems ProjectItems
        {
            get
            {
                return ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                if(this.nestedProject != null)
                {
                    return this.nestedProject.ProjectItems;
                }
                return null;
                });
            }
        }

        /// <summary>
        /// Returns the nested project.
        /// </summary>
        public override EnvDTE.Project SubProject
        {
            get
            {
                return this.nestedProject;
            }
        }
        #endregion
    }
}
