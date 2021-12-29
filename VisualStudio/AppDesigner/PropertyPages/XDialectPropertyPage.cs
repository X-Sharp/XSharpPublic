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

    /// <summary>
    /// Property page for the build events.
    /// </summary>
    [Guid(XSharpConstants.DialectPropertiesPage)]
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProvideObject(typeof(XSharpDialectPropertyPage))]
    public class XSharpDialectPropertyPage : XPropertyPage
    {
        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpBuildEventsPropertyPage"/> class.
        /// </summary>
        public XSharpDialectPropertyPage()
        {
            this.PageName = "Dialect";
            this.PerConfig = false;
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
            return value;
        }

        /// <summary>
        /// Sets a project property.
        /// </summary>
        /// <param name="propertyName">Name of the property to set.</param>
        /// <param name="value">Value of the property.</param>
        public override void SetProperty(string propertyName, string value)
        {
            //todo enable / disable controls based on contents
            ThreadHelper.ThrowIfNotOnUIThread();
            base.SetProperty(propertyName, value);
        }

        /// <summary>
        /// Creates the controls that constitute the property page. This should be safe to re-entrancy.
        /// </summary>
        /// <returns>The newly created main control that hosts the property page.</returns>
        protected override XPropertyPagePanel CreatePropertyPagePanel()
        {
            if (panel == null)
            {
                panel = new XDialectPropertyPagePanel(this);
            }
            return panel;
        }
        XDialectPropertyPagePanel panel = null;
        protected override void Project_OnProjectPropertyChanged(object sender, ProjectPropertyChangedArgs e)
        {
            if (panel != null)
            {
                panel.Project_OnProjectPropertyChanged(sender, e);
            }
        }
        

    }
}
