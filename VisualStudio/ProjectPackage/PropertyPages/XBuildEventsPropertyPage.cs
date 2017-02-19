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
    using Microsoft.Build.BuildEngine;
    using Microsoft.VisualStudio;
    using Microsoft.VisualStudio.Package;

    /// <summary>
    /// Property page for the build events.
    /// </summary>
    [ComVisible(true)]
    [Guid("49306259-9119-466E-8780-486CFBE2597D")]
    internal class XSharpBuildEventsPropertyPage : XPropertyPage
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
            string value = base.GetProperty(propertyName);

            if (propertyName == XProjectFileConstants.RunPostBuildEvent)
            {
                try
                {
                    RunPostBuildEvent outputType = (RunPostBuildEvent)Enum.Parse(typeof(RunPostBuildEvent), value, true);
                    value = ((int)outputType).ToString(CultureInfo.InvariantCulture);
                }
                catch (ArgumentException)
                {
                    value = null;
                }

                if (value == null)
                {
                    value = ((int)RunPostBuildEvent.OnBuildSuccess).ToString(CultureInfo.InvariantCulture);
                }
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

            base.SetProperty(propertyName, value);
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
