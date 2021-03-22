//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

namespace XSharp.Project
{
    using Microsoft.VisualStudio.PlatformUI;
    using System;
    using System.Diagnostics;
    using System.Drawing;
    using System.Text;
    using System.Collections.Generic;
    using System.Windows.Forms;
    using System.ComponentModel;
    using Microsoft.VisualStudio.Project;
    using Microsoft.VisualStudio.Language.Intellisense;
    using Microsoft.VisualStudio.Shell;
    using XSharpModel;

    /// <summary>
    /// Property page contents for the Candle Settings page.
    /// </summary>
    internal partial class XGeneralPropertyPagePanel : XPropertyPagePanel
    {

        #region Constants
        internal const string DefaultValue = "<default>";
        internal const string captVulcanCompatibleResouces = "Vulcan Compatible Managed Resources";
        internal const string descVulcanCompatibleResouces = "Use Vulcan Compatible Managed Resources (when 'True' then resources files are included in the assembly without namespace prefix. When 'False' then the resource files are prefixed with the namespace of the app, just like in other .Net languages, such as C#)";
        internal const string captDialect = "Dialect";
        internal const string descDialect = "Select the compiler dialect to use when compiling this project. Changing the dialect may also change the 'Allow NamedArguments' setting on the Language page.";
        internal const string captWin32Manifest = "Suppress default Win32 manifest";
        internal const string descWin32Manifest = "Suppress default Win32 manifest. You will have to supply your own Win32 manifest if you suppress the default one. (/nowin32manifest)";
        internal const string catResources = "Resources";
        internal const string captUseNativeVersion = "Prefer native version resource over managed version resource";
        internal const string descUseNativeVersion = "When your application includes a native version resource, use this native version resource and do not generate a resource based on the global assembly properties such as AssemblyTitle, AssemblyVersion etc (/usenativeversion)";
        internal const string captPreferNative = "Prefer native version resource info over managed version info";
        internal const string descPreferNative = "Prefer native version resource over managed version info, The default behavior is to create a version resource based in the various [Assembly..] attributes.";
        internal const string captBindingRedirects = "Auto-generate binding redirects";
        internal const string descBindingRedirects = "The binding redirects are added to the output configuration (app.config) file when the app is compiled.";
        internal const string captStartup = "Startup object:";
        internal const string captOutputType = "Output Type:";
        internal const string captTargetFramework = "Target Framework:";
        internal const string captAppName = "Application Name";
        internal const string captNamespace = "Default Namespace:";
        internal const string descNamespace = "Specifies the base namespace for files added to the project.";
        internal const string descAssembly = "Specifies the name of the output file that will hold the assembly manifest.";
        internal const string descFramework = "Specifies the version of .NET that the application targets. This option can have different values depending on which versions of .NET are installed on your computer.";
        internal const string descOutputType = "Specifies the type of application to build.";
        internal const string descStartup = "Defines the entry point to be called when the application loads. Generally this is set either to the main form in your application or to the 'Start' function that should run when the application starts. Class libraries do not define an entry point.";
        internal const string descIcon = "Sets the .ico file that you want to use as your program icon. Note you must specify the icon and manifest -or- a resource file.";
        internal const string captIcon = "Application Icon:";
        #endregion

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPagePanel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The parent property page to which this is bound.</param>
        public XGeneralPropertyPagePanel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            this.InitializeComponent();

            this.tbAssemblyName.Tag = XSharpProjectFileConstants.AssemblyName;
            this.tbDefaultNamespace.Tag = XSharpProjectFileConstants.RootNamespace;
            this.tbAppIcon.Tag = XSharpProjectFileConstants.ApplicationIcon;
            this.comboStartupObject.Tag = XSharpProjectFileConstants.StartupObject;
            this.chkPreferNativeVersion.Tag = XSharpProjectFileConstants.UseNativeVersion;
            this.chkSuppressDefaultManifest.Tag = XSharpProjectFileConstants.NoWin32Manifest;
            this.chkVulcanCompatibleResources.Tag = XSharpProjectFileConstants.VulcanCompatibleResources;
            this.chkAutoGenerateBindingRedirects.Tag = XSharpProjectFileConstants.AutoGenerateBindingRedirects;
            this.comboDialect.Tag = XSharpProjectFileConstants.Dialect;
            this.comboOutputType.Tag = XSharpProjectFileConstants.OutputType;
            this.comboTargetFramework.Tag = XSharpProjectFileConstants.TargetFrameworkVersion;

            lblDefaultNamespace.Text = captNamespace;
            toolTip1.SetToolTip(lblDefaultNamespace, descNamespace);
            toolTip1.SetToolTip(tbDefaultNamespace, descNamespace);

            lblApplicationName.Text = captAppName;
            toolTip1.SetToolTip(lblApplicationName, descAssembly);
            toolTip1.SetToolTip(tbAssemblyName, descAssembly);
            labelIcon.Text = captIcon;
            toolTip1.SetToolTip(labelIcon, descIcon);
            toolTip1.SetToolTip(tbAppIcon, descIcon);
            toolTip1.SetToolTip(btnIcon, descIcon);
            labelStartupObject.Text = captStartup;
            toolTip1.SetToolTip(labelStartupObject, descStartup);
            toolTip1.SetToolTip(comboStartupObject, descStartup);

            chkSuppressDefaultManifest.Text = captWin32Manifest;
            toolTip1.SetToolTip(chkSuppressDefaultManifest, descWin32Manifest);

            chkPreferNativeVersion.Text = captPreferNative;
            toolTip1.SetToolTip(chkPreferNativeVersion, descPreferNative);
            chkVulcanCompatibleResources.Text = captVulcanCompatibleResouces;
            toolTip1.SetToolTip(chkVulcanCompatibleResources, descVulcanCompatibleResouces);
            chkAutoGenerateBindingRedirects.Text = captBindingRedirects;
            toolTip1.SetToolTip(chkAutoGenerateBindingRedirects, descBindingRedirects);


            FillCombo(new DialectConverter() , comboDialect);
            toolTip1.SetToolTip(lblDialect, descDialect);
            toolTip1.SetToolTip(comboDialect, descDialect);
            FillCombo(new OutputTypeConverter(), comboOutputType);
            toolTip1.SetToolTip(lblOutputType, descOutputType);
            toolTip1.SetToolTip(comboOutputType, descOutputType);
            FillCombo(new FrameworkNameConverter(), comboTargetFramework);
            toolTip1.SetToolTip(lblTargetFramework, descFramework);
            toolTip1.SetToolTip(comboTargetFramework, descFramework);


            // hook up the form to both editors
            Color defaultBackground = SystemColors.ButtonFace;
            Color defaultForeground = SystemColors.WindowText;
            UpdateWindowColors(this, defaultBackground, defaultForeground);
        }

        void GetStartupClasses()
        {
            this.comboStartupObject.Items.Clear();
            this.comboStartupObject.Items.Add(DefaultValue);
            var prjfile = ParentPropertyPage.ProjectMgr.ProjectFile;
            var list = XDatabase.GetStartupClasses(System.IO.Path.GetFileName(prjfile));
            foreach (var item in list)
            {
                this.comboStartupObject.Items.Add(item);
            }
        }

        protected internal override void BindProperties()
        {
            this.GetStartupClasses();
            base.BindProperties();
        }

        /// <summary>
        /// Adjust the  color values. Adjusts the text color and text
        /// area background color
        /// </summary>
        /// <param name="clrBackground">The desired color for the background of the text area</param>
        /// <param name="clrForeground">The desired text color</param>
        static void UpdateWindowColors(Control control, Color clrBackground, Color clrForeground)
        {
            // Update the window background
            if (control is TextBox)
                control.BackColor = Color.White;
            else
                control.BackColor = clrBackground;
            control.ForeColor = clrForeground;

            // Also update the label
            foreach (Control child in control.Controls)
            {
                UpdateWindowColors(child, clrBackground, clrForeground);
            }
        }

        private void btnIcon_Click(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            ShowOpenFileDialog(tbAppIcon, descIcon, "Icon Files (*.ico)|*.ico|All files (*.*)|*.*");
        }
    }
}
