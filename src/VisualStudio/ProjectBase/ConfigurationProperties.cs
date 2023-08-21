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

using Microsoft.VisualStudio.Shell;
using System;
using System.Runtime.InteropServices;

namespace Microsoft.VisualStudio.Project
{
    /// <summary>
    /// Defines the config dependent properties exposed through automation
    /// </summary>
    [ComVisible(true)]
    [Guid("80FBE513-9DC8-4028-8488-F7E9A37D30FD")]
    public interface IProjectConfigProperties
    {
        string OutputPath { get; set; }
    }

    /// <summary>
    /// Implements the configuration dependent properties interface
    /// </summary>
    [CLSCompliant(false), ComVisible(true)]
    [ClassInterface(ClassInterfaceType.None)]
    public class ProjectConfigProperties : IProjectConfigProperties
    {
        #region fields
        private ProjectConfig projectConfig;
        #endregion

        #region ctors
        public ProjectConfigProperties(ProjectConfig projectConfig)
        {
            this.projectConfig = projectConfig;
        }
        #endregion

        #region IProjectConfigProperties Members

        public virtual string OutputPath
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                return this.projectConfig.GetConfigurationProperty(ProjectFileConstants.OutputPath.ToString(), true);
            }
            set
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                this.projectConfig.SetConfigurationProperty(ProjectFileConstants.OutputPath.ToString(), value);
            }
        }

        #endregion
    }
}
