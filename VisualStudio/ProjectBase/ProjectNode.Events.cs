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

namespace Microsoft.VisualStudio.Project
{
    public partial class ProjectNode
    {
        internal event EventHandler<ProjectPropertyChangedArgs> OnProjectPropertyChanged;

        protected virtual void RaiseProjectPropertyChanged(string propertyName, string oldValue, string newValue)
		{
            var onPropChanged = OnProjectPropertyChanged;
            if (onPropChanged != null)
			{
                try
                {
                    onPropChanged(this, new ProjectPropertyChangedArgs(propertyName, oldValue, newValue));
                }
                catch (Exception)
                {
                }
            }
        }
    }

}
