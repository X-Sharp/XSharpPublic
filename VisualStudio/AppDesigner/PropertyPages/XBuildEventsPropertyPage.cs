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
        string[] names = { "Always",
            "When the build updates the project output",
             "On successful build"};
        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpBuildEventsPropertyPage"/> class.
        /// </summary>
        public XSharpBuildEventsPropertyPage()
        {
            this.PageName = "Build Events";
            this.PerConfig = true;
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

            if (propertyName == XSharpProjectFileConstants.RunPostBuildEvent)
            {
                RunPostBuildEvent postbuildevent;

                if (!Enum.TryParse(value, true, out postbuildevent))
                {
                    postbuildevent = RunPostBuildEvent.OnBuildSuccess;
                }
                value = names[(int)postbuildevent];
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
            if (propertyName == XSharpProjectFileConstants.RunPostBuildEvent)
            {
                var postbuildevent = RunPostBuildEvent.Always;
                value = value.ToLower();
                if (value.IndexOf("always") >= 0)
                {
                    postbuildevent = RunPostBuildEvent.Always;
                }
                else if (value.IndexOf("successful") >= 0)
                {
                    postbuildevent = RunPostBuildEvent.OnBuildSuccess;
                }
                else if (value.IndexOf("updated") >= 0)
                {
                    postbuildevent = RunPostBuildEvent.OnOutputUpdated;
                }
                value = postbuildevent.ToString();
            }

            SetConfigProperty(propertyName, value);
        }

        /// <summary>
        /// Creates the controls that constitute the property page. This should be safe to re-entrancy.
        /// </summary>
        /// <returns>The newly created main control that hosts the property page.</returns>
        protected override XPropertyPagePanel CreatePropertyPagePanel()
        {
            return new XBuildEventsPropertyPagePanel(this, names);
        }
    }
}
