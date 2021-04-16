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
    [ComVisible(true)]
    [Guid(XSharpConstants.BuildPropertiesPage)]
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProvideObject(typeof(XSharpBuildPropertyPage))]
    public class XSharpBuildPropertyPage : XPropertyPage
    {

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpBuildEventsPropertyPage"/> class.
        /// </summary>
        public XSharpBuildPropertyPage()
        {
            this.PageName = "Build";
            this.PerConfig = true;
        }
  
        /// <summary>
        /// Sets a project property.
        /// </summary>
        /// <param name="propertyName">Name of the property to set.</param>
        /// <param name="value">Value of the property.</param>
        public override void SetProperty(string propertyName, string value)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            base.SetProperty(propertyName, value);
        }

        XBuildPropertyPagePanel panel;
        /// <summary>
        /// Creates the controls that constitute the property page. This should be safe to re-entrancy.
        /// </summary>
        /// <returns>The newly created main control that hosts the property page.</returns>
        protected override XPropertyPagePanel CreatePropertyPagePanel()
        {
            panel = new XBuildPropertyPagePanel(this);
            return panel;
        }
        protected override void Project_OnProjectPropertyChanged(object sender, ProjectPropertyChangedArgs e)
        {
            if (panel != null)
            {
                panel.Project_OnProjectPropertyChanged(sender, e);
            }
        }
    }
}
