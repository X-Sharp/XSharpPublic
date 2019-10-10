//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Runtime.Versioning;
using System.Windows.Forms;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Project;
using System.ComponentModel;
using System.Linq;
using System.Reflection;
namespace XSharp.Project
{
    /// <summary>
    /// This class implements general property page for the project type.
    /// </summary>
    [ComVisible(true)]
    [Guid("53651BEA-799A-45EB-B58C-C884F5417219")]
    [ClassInterface(ClassInterfaceType.AutoDual)]
    public class XSharpGeneralPropertyPage : XSharpSettingsPage
    {
        #region Fields
        private string assemblyName;
        private OutputType outputType;
        private string rootNamespace;
        private string startupObject;
        private string applicationIcon;
        private FrameworkName targetFrameworkMoniker;
        private Dialect dialect;
        private bool vulcanCompatibleResources;
        private bool noWin32Manifest;
        #endregion Fields

        #region Constants
        internal const string captVulcanCompatibleResouces = "Vulcan Compatible Managed Resources";
        internal const string descVulcanCompatibleResouces = "Use Vulcan Compatible Managed Resources (when 'True' then resources files are included in the assembly without namespace prefix. When 'False' then the resource files are prefixed with the namespace of the app, just like in other .Net languages, such as C#))";
        internal const string captDialect = "Dialect";
        internal const string descDialect = "Select the compiler dialect to use when compiling this project. Changing the dialect may also change the 'Allow NamedArguments' setting on the Language page.";
        internal const string captWin32Manifest = "Suppress default Win32 manifest";
        internal const string descWin32Manifest = "Suppress default Win32 manifest. You will have to supply your own Win32 manifest if you suppress the default one. (/nowin32manifest)";
        internal const string catResources = "Resources";
        #endregion

        #region Constructors
        /// <summary>
        /// Explicitly defined default constructor.
        /// </summary>
        public XSharpGeneralPropertyPage()
        {
            this.Name = Resources.GetString(Resources.GeneralCaption);
        }
        #endregion

        #region Properties
        [ResourcesCategory(Resources.AssemblyName)]
        [LocDisplayName(Resources.AssemblyName)]
        [ResourcesDescription(Resources.AssemblyNameDescription)]
        /// <summary>
        /// Gets or sets Assembly Name.
        /// </summary>
        /// <remarks>IsDirty flag was switched to true.</remarks>
        public string AssemblyName
        {
            get { return this.assemblyName; }
            set { this.assemblyName = value; this.IsDirty = true; }
        }

        [ResourcesCategory(Resources.Application)]
        [LocDisplayName(Resources.OutputType)]
        [ResourcesDescription(Resources.OutputTypeDescription)]
        /// <summary>
        /// Gets or sets OutputType.
        /// </summary>
        /// <remarks>IsDirty flag was switched to true.</remarks>
        public OutputType OutputType
        {
            get { return this.outputType; }
            set { this.outputType = value; this.IsDirty = true; }
        }

        [ResourcesCategory(Resources.Application)]
        [DisplayName(captDialect)]
        [Description(descDialect)]
        [RefreshProperties(System.ComponentModel.RefreshProperties.All)]
        public Dialect Dialect
        {
            get { return this.dialect; }
            set {
                this.dialect = value;
                this.IsDirty = true;
                this.ProjectMgr.SetProjectProperty(nameof(Dialect), this.dialect.ToString());
                this.ProjectMgr.SetProjectProperty("NamedArgs", "");
                switch (dialect)
                {
                    case Dialect.Core:
                    case Dialect.Vulcan:
                        this.ProjectMgr.SetProjectProperty("Memvar", "");
                        this.ProjectMgr.SetProjectProperty("Undeclared", "");
                        break;
                }
            ;
            }
        }


        [ResourcesCategory(catResources)]
        [DisplayName(captVulcanCompatibleResouces)]
        [Description(descVulcanCompatibleResouces)]
        public bool VulcanCompatibleResources
        {
            get { return this.vulcanCompatibleResources; }
            set { this.vulcanCompatibleResources = value; this.IsDirty = true; }
        }

        [ResourcesCategory(catResources)]
        [DisplayName(captWin32Manifest)]
        [Description(descWin32Manifest)]
        public bool NoWin32Manifest
        {
            get { return this.noWin32Manifest; }
            set { this.noWin32Manifest = value; this.IsDirty = true; }
        }


        [ResourcesCategory(Resources.Application)]
        [LocDisplayName(Resources.DefaultNamespace)]
        [ResourcesDescription(Resources.DefaultNamespaceDescription)]
        /// <summary>
        /// Gets or sets Default Namespace.
        /// </summary>
        /// <remarks>IsDirty flag was switched to true.</remarks>
        public string RootNamespace
        {
            get { return this.rootNamespace; }
            set { this.rootNamespace = value; this.IsDirty = true; }
        }

        [ResourcesCategory(Resources.Application)]
        [LocDisplayName(Resources.StartupObject)]
        [ResourcesDescription(Resources.StartupObjectDescription)]
        /// <summary>
        /// Gets or sets Startup Object.
        /// </summary>
        /// <remarks>IsDirty flag was switched to true.</remarks>
        public string StartupObject
        {
            get { return this.startupObject; }
            set { this.startupObject = value; this.IsDirty = true; }
        }

        [ResourcesCategory(catResources)]
        [LocDisplayName(Resources.ApplicationIcon)]
        [ResourcesDescription(Resources.ApplicationIconDescription)]
        /// <summary>
        /// Gets or sets Application Icon.
        /// </summary>
        /// <remarks>IsDirty flag was switched to true.</remarks>
        public string ApplicationIcon
        {
            get { return this.applicationIcon; }
            set { this.applicationIcon = value; this.IsDirty = true; }
        }

        [ResourcesCategory(Resources.Project)]
        [LocDisplayName(Resources.ProjectFile)]
        [ResourcesDescription(Resources.ProjectFileDescription)]
        /// <summary>
        /// Gets the path to the project file.
        /// </summary>
        /// <remarks>IsDirty flag was switched to true.</remarks>
        public string ProjectFile
        {
            get { return Path.GetFileName(this.ProjectMgr.ProjectFile); }
        }

        [ResourcesCategory(Resources.Project)]
        [LocDisplayName(Resources.ProjectFolder)]
        [ResourcesDescription(Resources.ProjectFolderDescription)]
        /// <summary>
        /// Gets the path to the project folder.
        /// </summary>
        /// <remarks>IsDirty flag was switched to true.</remarks>
        public string ProjectFolder
        {
            get { return Path.GetDirectoryName(this.ProjectMgr.ProjectFolder); }
        }

        [ResourcesCategory(Resources.Project)]
        [LocDisplayName(Resources.OutputFile)]
        [ResourcesDescription(Resources.OutputFileDescription)]
        /// <summary>
        /// Gets the output file name depending on current OutputType.
        /// </summary>
        /// <remarks>IsDirty flag was switched to true.</remarks>
        public string OutputFile
        {
            get
            {
                switch(this.outputType)
                {
                    case OutputType.Exe:
                    case OutputType.WinExe:
                        {
                            return this.assemblyName + ".exe";
                        }

                    default:
                        {
                            return this.assemblyName + ".dll";
                        }
                }
            }
        }

        [ResourcesCategory(Resources.Application)]
        [LocDisplayName(Resources.TargetFrameworkMoniker)]
        [ResourcesDescription(Resources.TargetFrameworkMonikerDescription)]
        [PropertyPageTypeConverter(typeof(FrameworkNameConverter))]
        /// <summary>
        /// Gets or sets Target Platform PlatformType.
        /// </summary>
        /// <remarks>IsDirty flag was switched to true.</remarks>
        public FrameworkName TargetFrameworkMoniker
        {
            get { return this.targetFrameworkMoniker; }
            set { this.targetFrameworkMoniker = value; IsDirty = true; }
        }

        #endregion

        #region Overriden Implementation
        /// <summary>
        /// Returns class FullName property value.
        /// </summary>
        public override string GetClassName()
        {
            return this.GetType().FullName;
        }

        /// <summary>
        /// Bind properties.
        /// </summary>
        protected override void BindProperties()
        {
            if(this.ProjectMgr == null)
            {
                return;
            }

            this.assemblyName = this.ProjectMgr.GetProjectProperty(nameof(AssemblyName), true);
            string outputType = this.ProjectMgr.GetProjectProperty(nameof(OutputType), false);
            this.rootNamespace = this.ProjectMgr.GetProjectProperty(nameof(RootNamespace), false);
            this.startupObject = this.ProjectMgr.GetProjectProperty(nameof(StartupObject), false);
            this.applicationIcon = this.ProjectMgr.GetProjectProperty(nameof(ApplicationIcon), false);
            this.vulcanCompatibleResources = getCfgLogic(nameof(VulcanCompatibleResources), false);
            this.noWin32Manifest = getCfgLogic(nameof(NoWin32Manifest), false);
            if (outputType != null && outputType.Length > 0)
            {
                try
                {
                    this.outputType = (OutputType)Enum.Parse(typeof(OutputType), outputType);
                }
                catch(ArgumentException)
                {
                    this.outputType = OutputType.Library;
                }
            }



            try
            {
                this.targetFrameworkMoniker = this.ProjectMgr.TargetFrameworkMoniker;
            }
            catch (ArgumentException)
            {
                foreach (var name in new FrameworkNameConverter().GetStandardValues())
                {
                    this.targetFrameworkMoniker = (FrameworkName)name;
                    break;
                }
            }

            string strdialect = this.ProjectMgr.GetProjectProperty(nameof(Dialect), false);
            try
            {
                this.dialect = (Dialect)Enum.Parse(typeof(Dialect), strdialect);
            }
            catch (ArgumentException)
            {
                this.dialect = Dialect.Core;
            }

        }

        /// <summary>
        /// Apply Changes on project node.
        /// </summary>
        /// <returns>E_INVALIDARG if internal ProjectMgr is null, otherwise applies changes and return S_OK.</returns>
        protected override int ApplyChanges()
        {
            if(this.ProjectMgr == null)
            {
                return VSConstants.E_INVALIDARG;
            }

            IVsPropertyPageFrame propertyPageFrame = (IVsPropertyPageFrame)this.ProjectMgr.Site.GetService((typeof(SVsPropertyPageFrame)));
            bool reloadRequired = this.ProjectMgr.TargetFrameworkMoniker != this.targetFrameworkMoniker;

            this.ProjectMgr.SetProjectProperty(nameof(AssemblyName), this.assemblyName);
            this.ProjectMgr.SetProjectProperty(nameof(OutputType), this.outputType.ToString());
            this.ProjectMgr.SetProjectProperty(nameof(RootNamespace), this.rootNamespace);
            this.ProjectMgr.SetProjectProperty(nameof(StartupObject), this.startupObject);
            this.ProjectMgr.SetProjectProperty(nameof(ApplicationIcon), this.applicationIcon);
            this.ProjectMgr.SetProjectProperty(nameof(Dialect), this.dialect.ToString());
            this.ProjectMgr.SetProjectProperty(nameof(VulcanCompatibleResources), this.vulcanCompatibleResources.ToString());
            this.ProjectMgr.SetProjectProperty(nameof(NoWin32Manifest), this.noWin32Manifest.ToString());
            // reset properties for projectnode
            ((XSharpProjectNode)this.ProjectMgr).OutputFile = null;
            ((XSharpProjectNode)this.ProjectMgr).RootNameSpace = this.rootNamespace;
            if (reloadRequired)
            {
                if (MessageBox.Show(SR.GetString(SR.ReloadPromptOnTargetFxChanged), SR.GetString(SR.ReloadPromptOnTargetFxChangedCaption), MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
                {
                    this.ProjectMgr.TargetFrameworkMoniker = this.targetFrameworkMoniker;
                }
            }

            this.IsDirty = false;

            if (reloadRequired)
            {
                // This prevents the property page from displaying bad data from the zombied (unloaded) project
                propertyPageFrame.HideFrame();
                propertyPageFrame.ShowFrame(this.GetType().GUID);
            }

            return VSConstants.S_OK;
        }

        #endregion


    }
}
