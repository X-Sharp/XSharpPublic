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
using Microsoft.VisualStudioTools.Project;
using System.Linq;
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
        private Platform platformtarget;

        #endregion Fields

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
        [LocDisplayName("Dialect")]
        [ResourcesDescription("Select the compiler dialect to use when compiling this project")]
        public Dialect Dialect
        {
            get { return this.dialect; }
            set { this.dialect = value; this.IsDirty = true; }
        }

        [ResourcesCategory(Resources.Project)]
        [LocDisplayName("Platform Target")]
        [ResourcesDescription("Select the compiler dialect to use when compiling this project")]
        public Platform PlatformTarget
        {
            get { return this.platformtarget; }
            set { this.platformtarget = value; this.IsDirty = true; }
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

        [ResourcesCategory(Resources.Application)]
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

        [ResourcesCategory(Resources.Project)]
        [LocDisplayName(Resources.TargetFrameworkMoniker)]
        [ResourcesDescription(Resources.TargetFrameworkMonikerDescription)]
        [PropertyPageTypeConverter(typeof(EnumConverter))]
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
        public virtual string GetClassName()
        {
            return this.GetType().FullName;
        }

        /// <summary>
        /// Bind properties.
        /// </summary>
        protected virtual void BindProperties() {
            if(this.ProjectMgr == null)
            {
                return;
            }

            this.assemblyName = this.ProjectMgr.GetProjectProperty(nameof(AssemblyName), true);
            string outputType = this.ProjectMgr.GetProjectProperty(nameof(OutputType), false);
            this.rootNamespace = this.ProjectMgr.GetProjectProperty(nameof(RootNamespace), false);
            this.startupObject = this.ProjectMgr.GetProjectProperty(nameof(StartupObject), false);
            this.applicationIcon = this.ProjectMgr.GetProjectProperty(nameof(ApplicationIcon), false);

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


            string platform = this.ProjectMgr.GetProjectProperty(nameof(PlatformTarget), false);
            try
            {
                this.platformtarget = (Platform)Enum.Parse(typeof(Platform), platform);
            }
            catch (ArgumentException)
            {
                this.platformtarget = Platform.AnyCPU;
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
        protected virtual int ApplyChanges()
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
            this.ProjectMgr.SetProjectProperty(nameof(PlatformTarget), this.platformtarget.ToString());

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
