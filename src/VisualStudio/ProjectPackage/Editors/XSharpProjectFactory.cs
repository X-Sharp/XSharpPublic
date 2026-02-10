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
using System.Linq;
using System.Collections.Generic;
using Community.VisualStudio.Toolkit;

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
#if !DEV17
        internal static List<string> InvalidProjectFiles = new List<string>();
#endif
        #region Overriden implementation
        protected override void CreateProject(string fileName, string location, string name, uint flags, ref Guid projectGuid, out IntPtr project, out int canceled)
        {
            project = IntPtr.Zero;
#if !DEV17
            if (InvalidProjectFiles.Contains(fileName.ToLower()))
            {
                InvalidProjectFiles.Remove(fileName.ToLower());
                VS.MessageBox.ShowError("The project file " + fileName + " is an SDK style project and cannot be loaded inside this version of Visual Studio");
                canceled = 1;
                return;
            }
#endif
            base.CreateProject(fileName, location, name, flags, ref projectGuid, out project, out canceled);
        }
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
        protected override ProjectNode CreateSdkProject()
        {
#if DEV17
            ThreadHelper.ThrowIfNotOnUIThread();
            var project = new XSharpSdkProjectNode(this.package);
            IOleServiceProvider provider = null;
            var serviceProvider = this.package as IServiceProvider;
            // ProjectPackage already switches to UI thread inside GetService
            provider = (IOleServiceProvider)serviceProvider.GetService(typeof(IOleServiceProvider));
            project.SetSite(provider);
            return project;
#else
            return null;
#endif
        }
#endregion
    }
}
