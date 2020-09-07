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
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using System.Threading.Tasks;
using EnvDTE;
using Microsoft.VisualStudio.Shell;

namespace Microsoft.VisualStudio.Project.Automation
{
    /// <summary>
    /// Represents an automation object for a folder in a project
    /// </summary>
    [SuppressMessage("Microsoft.Interoperability", "CA1405:ComVisibleTypeBaseTypesShouldBeComVisible")]
    [ComVisible(true), CLSCompliant(false)]
    public class OAFolderItem : OAProjectItem<FolderNode>
    {
        #region ctors
        internal OAFolderItem(OAProject project, FolderNode node)
            : base(project, node)
        {
        }

        #endregion

        #region overridden methods
        public override ProjectItems Collection
        {
            get
            {
                return ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    ProjectItems items = new OAProjectItems(this.Project, this.Node);
                    return items;
                });
            }
        }

        public override ProjectItems ProjectItems
        {
            get
            {
                return this.Collection;
            }
        }
        #endregion
    }
}
