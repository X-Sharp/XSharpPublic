//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

namespace XSharp.Project
{
    using System;
    using System.Linq;
    using System.Globalization;
    using System.Runtime.InteropServices;
    using System.Windows.Forms;
    using Microsoft.VisualStudio;
    using Microsoft.VisualStudio.Package;
    using Microsoft.VisualStudio.Project;
    using Microsoft.VisualStudio.Shell;
    using System.Runtime.Versioning;
    using Microsoft.VisualStudio.Shell.Interop;

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
                    value = XGeneralPropertyPagePanel.DefaultValue;
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
                if (value == XGeneralPropertyPagePanel.DefaultValue)
                {
                    value = "";
                }
            }

            ThreadHelper.ThrowIfNotOnUIThread();
            var oldvalue = base.GetProperty(propertyName);
            bool changed = value != oldvalue;

            if (changed)
            {
                base.SetProperty(propertyName, value);
                if (propertyName == XSharpProjectFileConstants.TargetFrameworkVersion)
                {
                    var oldName = new FrameworkName(oldValue);
                    var newName = new FrameworkName(newValue);
                    var options = this.ProjectMgr.GetProjectOptions(this.ProjectMgr.CurrentConfig.ConfigCanonicalName);
                    var retargetingService = this.ProjectMgr.GetService(typeof(SVsTrackProjectRetargeting)) as IVsTrackProjectRetargeting;
                    if (retargetingService != null)
                    {
                        // We surround our batch retargeting request with begin/end because in individual project load
                        // scenarios the solution load context hasn't done it for us.
                        Marshal.ThrowExceptionForHR(retargetingService.BeginRetargetingBatch());
                        Marshal.ThrowExceptionForHR(retargetingService.BatchRetargetProject(this.ProjectMgr, newName.FullName, true));
                        Marshal.ThrowExceptionForHR(retargetingService.EndRetargetingBatch());
                    }
                    var buildCfg = new BuildableProjectConfig(ProjectMgr.CurrentConfig);
                    buildCfg.RefreshReferences();

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
}
