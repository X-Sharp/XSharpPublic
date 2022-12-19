//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Project;
using System;
using System.Runtime.InteropServices;
using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;
using Microsoft.VisualStudio.Shell;

namespace XSharp.Project
{
    /// <summary>
    /// Represent the methods for creating projects within the solution.
    /// </summary>
    [Guid(XSharpConstants.guidXSharpProjectFactoryString)]
    public class XSharpProjectFactory : ProjectFactory
    {
        #region Fields
        private XSharpProjectPackage package;
        #endregion

        #region Constructors
        /// <summary>
        /// Explicit default constructor.
        /// </summary>
        /// <param name="package">Value of the project package for initialize internal package field.</param>
        public XSharpProjectFactory(XSharpProjectPackage package)
            : base(package)
        {
            this.package = package;
        }
        #endregion

        #region Overriden implementation
        /// <summary>
        /// Creates a new project by cloning an existing template project.
        /// </summary>
        /// <returns></returns>
        protected override ProjectNode CreateProject()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            XSharpProjectNode project = new XSharpProjectNode(this.package);
            IOleServiceProvider provider = null;
            var serviceProvider = this.package as IServiceProvider;
            // ProjectPackage already switches to UI thread inside GetService
            provider = (IOleServiceProvider) serviceProvider.GetService(typeof(IOleServiceProvider));
            project.SetSite(provider);
            return project;
        }
        #endregion
    }
}
