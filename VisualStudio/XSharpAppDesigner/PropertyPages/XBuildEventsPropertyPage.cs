//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

namespace XSharp.Project
{
    using System;
    using System.Globalization;
    using System.Runtime.InteropServices;
    using System.Windows.Forms;
    using Microsoft.VisualStudio;
    using Microsoft.VisualStudio.Package;
    using Microsoft.VisualStudio.Project;
    using Microsoft.VisualStudio.Shell;

    /// <summary>
    /// Property page for the build events.
    /// </summary>
    [ComVisible(true)]
    [Guid(XSharpConstants.BuildEventsPropertiesPage)]
    [ProvideObject(typeof(XSharpBuildEventsPropertyPage))]
    public class XSharpBuildEventsPropertyPage : XPropertyPage
    {
        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpBuildEventsPropertyPage"/> class.
        /// </summary>
        public XSharpBuildEventsPropertyPage()
        {
            this.PageName = "Build Events";
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
            string value = getCfgString(propertyName);

            if (propertyName == XProjectFileConstants.RunPostBuildEvent)
            {
                RunPostBuildEvent outputType;

                if (!Enum.TryParse(value, true, out outputType))
                {
                    outputType = RunPostBuildEvent.OnBuildSuccess;
                }
                value = ((int)outputType).ToString(CultureInfo.InvariantCulture);
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
            if (propertyName == XProjectFileConstants.RunPostBuildEvent)
            {
                RunPostBuildEvent runType = (RunPostBuildEvent)Int32.Parse(value, CultureInfo.InvariantCulture);
                if (Enum.IsDefined(typeof(RunPostBuildEvent), runType))
                {
                    value = runType.ToString();
                }
                else
                {
                    value = null;
                }
            }

            SetConfigProperty(propertyName, value);
        }

        internal string getCfgString(String Name, string defaultValue = "")
        {
            string property;
            string value = this.GetUnevaluatedConfigProperty(Name);
            if (!String.IsNullOrEmpty(value))
                property = value;
            else
                property = defaultValue;
            return property;
        }

        public void SetConfigProperty(string name, string value)
        {
            CCITracing.TraceCall();
            if (value == null)
            {
                value = String.Empty;
            }

            if (this.ProjectMgr != null)
            {
                for (int i = 0, n = this.ProjectConfigs.Count; i < n; i++)
                {
                    ProjectConfig config = ProjectConfigs[i];

                    config.SetConfigurationProperty(name, value);
                }

                this.ProjectMgr.SetProjectFileDirty(true);
            }
        }
        public string GetUnevaluatedConfigProperty(string propertyName)
        {
            if (this.ProjectMgr != null)
            {
                string unifiedResult = null;

                for (int i = 0; i < this.ProjectConfigs.Count; i++)
                {
                    ProjectConfig config = ProjectConfigs[i];
                    string property = config.GetUnevaluatedConfigurationProperty(propertyName);

                    if (property != null)
                    {
                        string text = property.Trim();

                        if (i == 0)
                            unifiedResult = text;
                        else if (unifiedResult != text)
                            return ""; // tristate value is blank then
                    }
                }

                return unifiedResult;
            }

            return String.Empty;
        }
        /// <summary>
        /// Creates the controls that constitute the property page. This should be safe to re-entrancy.
        /// </summary>
        /// <returns>The newly created main control that hosts the property page.</returns>
        protected override XPropertyPagePanel CreatePropertyPagePanel()
        {
            return new XBuildEventsPropertyPagePanel(this);
        }
    }
}
