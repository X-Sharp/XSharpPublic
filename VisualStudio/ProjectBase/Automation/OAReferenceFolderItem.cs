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
using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace Microsoft.VisualStudio.Project.Automation
{
    /// <summary>
    /// Contains OAReferenceItem objects
    /// </summary>
    [ComVisible(true), CLSCompliant(false)]
    public class OAReferenceFolderItem : OAProjectItem<ReferenceContainerNode>
    {
        #region ctors
        internal OAReferenceFolderItem(OAProject project, ReferenceContainerNode node)
            : base(project, node)
        {
        }

        #endregion

        #region overridden methods
        /// <summary>
        /// Returns the project items collection of all the references defined for this project.
        /// </summary>
        public override ProjectItems ProjectItems
        {
            get
            {
                return new OANavigableProjectItems(this.Project, this.GetListOfProjectItems(), this.Node);
            }
        }


        #endregion

        #region Helper methods
        private List<ProjectItem> GetListOfProjectItems()
        {
            List<ProjectItem> list = new List<ProjectItem>();
            for(HierarchyNode child = this.Node.FirstChild; child != null; child = child.NextSibling)
            {
                ReferenceNode node = child as ReferenceNode;

                if(node != null)
                {
                    ThreadHelper.JoinableTaskFactory.Run(async delegate
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        list.Add(node.Object as ProjectItem);
                    });
                }
            }

            return list;
        }
        #endregion
    }
}
