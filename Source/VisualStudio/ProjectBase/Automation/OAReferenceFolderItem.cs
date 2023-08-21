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

    }
}
