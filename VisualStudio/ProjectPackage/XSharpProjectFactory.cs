
using Microsoft.VisualStudio.Project;
using System;
using System.Runtime.InteropServices;
using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;

namespace XSharp.Project
{
    /// <summary>
    /// Represent the methods for creating projects within the solution.
    /// </summary>
    [Guid(GuidStrings.guidXSharpProjectFactoryString)]
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
            XSharpProjectNode project = new XSharpProjectNode(this.package);
            project.SetSite((IOleServiceProvider)((IServiceProvider)this.package).GetService(typeof(IOleServiceProvider)));
            return project;
        }
        #endregion
    }
}
