//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using System.Runtime.Versioning;
using Microsoft.VisualStudio.Shell.Interop;
using Community.VisualStudio.Toolkit;
using XSharp.Settings;
namespace XSharp.Project
{
    /// <summary>
    /// Property page for the build events.
    /// </summary>
    [ComVisible(true)]
    [Guid(XSharpConstants.GeneralPropertiesPage)]
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProvideObject(typeof(XSharpGeneralPropertyPage))]
    public class XSharpGeneralPropertyPage : XPropertyPage
    {
        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpBuildEventsPropertyPage"/> class.
        /// </summary>
        OutputTypeConverter converterOutPut;
        DialectConverter converterDialect;
        FrameworkNameConverter converterFramework;
        public XSharpGeneralPropertyPage()
        {
            this.PageName = "Application";
            this.PerConfig = false;
            converterDialect = new DialectConverter();
            converterFramework = new FrameworkNameConverter();
            converterOutPut = new OutputTypeConverter();
        }

        // =========================================================================================
        // Methods
        // =========================================================================================

        /// <summary>
        /// Gets a project property.
        /// </summary>
        /// <param name="propertyName">The name of the property to get.</param>
        /// <returns>
        /// Value of the property, or null if the property is unset or inconsistent across configurations.
        /// </returns>
        public override string GetProperty(string propertyName)
        {
            string value = base.GetProperty(propertyName);

            if (propertyName == XSharpProjectFileConstants.OutputType)
            {
                var outputType = (OutputType) converterOutPut.ConvertFrom(value);
                value = (string)converterOutPut.ConvertTo(outputType, typeof(string));
            }
            else if (propertyName == XSharpProjectFileConstants.Dialect)
            {
                var dialect = (Dialect)converterDialect.ConvertFrom(value);
                value = (string) converterDialect.ConvertTo(dialect, typeof(string));
            }
            else if (propertyName == XSharpProjectFileConstants.TargetFrameworkVersion)
            {
                if (!value.StartsWith(".NETFramework"))
                    value = ".NETFramework,Version =" + value;
                value = converterFramework.ConvertFrom(value).ToString();
            }
            if (propertyName == XSharpProjectFileConstants.StartupObject)
            {
                if (string.IsNullOrEmpty(value))
                    value = GeneralPropertyPagePanel.DefaultValue;
            }

            return value;
        }

        /// <summary>
        /// Sets a project property.
        /// </summary>
        /// <param name="propertyName">Name of the property to set.</param>
        /// <param name="value">Value of the property.</param>
        public override void SetProperty(string propertyName, string value)
        {
            string newValue = value;
            string oldValue = "";
            if (propertyName == XSharpProjectFileConstants.OutputType)
            {
                var output = (OutputType)converterOutPut.ConvertFrom(value);
                value = output.ToString();
            }
            else if (propertyName == XSharpProjectFileConstants.Dialect)
            {
                var dialect = (Dialect)converterDialect.ConvertFrom(value);
                value = dialect.ToString();
                var strAllowdot = base.GetProperty(XSharpProjectFileConstants.Allowdot);
                if (!string.IsNullOrEmpty(strAllowdot))
                {
                    bool bAllowDot;
                    bAllowDot = strAllowdot.ToLower() == "true";
                    value = dialect.ToString();
                    switch (dialect)
                    {
                        case Dialect.Core:
                        case Dialect.FoxPro:
                            if (!bAllowDot)
                            {
                                base.SetProperty(XSharpProjectFileConstants.Allowdot, "true");
                            }
                            break;
                        default:
                            if (bAllowDot)
                                base.SetProperty(XSharpProjectFileConstants.Allowdot, "false");
                            break;
                    }
                }
                var strAllowOldStyle = base.GetProperty(XSharpProjectFileConstants.AllowOldStyleAssignments);
                if (!string.IsNullOrEmpty(strAllowOldStyle))
                {
                    bool bAllowOldStyle;
                    bAllowOldStyle = strAllowOldStyle.ToLower() == "true";
                    if (dialect == Dialect.FoxPro && !bAllowOldStyle)
                    {
                        base.SetProperty(XSharpProjectFileConstants.AllowOldStyleAssignments, "true");
                    }
                }
            }
            else if (propertyName == XSharpProjectFileConstants.TargetFrameworkVersion)
            {
                oldValue = base.GetProperty(XSharpProjectFileConstants.TargetFrameworkVersion);
                value = value.ToLower();
                var pos = value.IndexOf("version=");
                if (pos > 0)
                {
                    oldValue = value.Substring(0, pos) + "version=" + oldValue;
                    value = value.Substring(pos + "version=".Length);
                }
            }
            else if (propertyName == XSharpProjectFileConstants.StartupObject)
            {
                if (value == GeneralPropertyPagePanel.DefaultValue)
                {
                    value = "";
                }
            }

            ThreadHelper.ThrowIfNotOnUIThread();
            var oldvalueFromFile = base.GetProperty(propertyName);
            bool changed = value != oldvalueFromFile;

            if (changed)
            {
                if (propertyName == XSharpProjectFileConstants.TargetFrameworkVersion)
                {
                    string message = "Changing the target framework requires that the current project be closed and then reopened.\n"
                     + "Any unsaved changes within the project will be automatically saved.\n\n"
                     + "Changing Target Framework may require manual modification of project files in order to build\n\n"
                     + "Are you sure you want to change the Target Framework for this project?";
                    if (!VS.MessageBox.ShowConfirm(message))
                    {
                        var genPanel = PropertyPagePanel as XGeneralPropertyPagePanel;
                        genPanel.resetFramework(oldValue);
                        return;
                    }
                        
                    base.SetProperty(propertyName, value);
                    ProjectReloader.Reload(newValue, this.ProjectMgr);
                }
                else
                {
                    base.SetProperty(propertyName, value);
                }
            }
        }

        /// <summary>
        /// Creates the controls that constitute the property page. This should be safe to re-entrancy.
        /// </summary>
        /// <returns>The newly created main control that hosts the property page.</returns>
        protected override XPropertyPagePanel CreatePropertyPagePanel()
        {
            return new XGeneralPropertyPagePanel(this);
        }
    }
    internal static class ProjectReloader
    {
        internal static void Reload(string newValue, XProjectNode project)
        {

            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                await VS.Commands.ExecuteAsync(KnownCommands.File_SaveAll);
                var newName = new FrameworkName(newValue);
                var retargetingService = await VS.GetRequiredServiceAsync<SVsTrackProjectRetargeting, IVsTrackProjectRetargeting2>();
                if (retargetingService != null)
                {
                    // We surround our batch retargeting request with begin/end because in individual project load
                    // scenarios the solution load context hasn't done it for us.

                    var res = retargetingService.CheckForProjectRetarget(0, project);
                    Marshal.ThrowExceptionForHR(retargetingService.BeginRetargetingBatch());
                    Marshal.ThrowExceptionForHR(retargetingService.BatchRetargetProject(project, newName.FullName, true));
                    Marshal.ThrowExceptionForHR(retargetingService.EndRetargetingBatch());
                    var buildCfg = new BuildableProjectConfig(project.CurrentConfig);
                    buildCfg.RefreshReferences();
                }
            });

            

        }
    }
}
