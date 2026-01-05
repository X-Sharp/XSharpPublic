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
    [Guid(XSharpConstants.GlobalUsingsPropertiesPage)]
    [ProvideObject(typeof(XSharpGlobalUsingsPropertiesPage))]
    public class XSharpGlobalUsingsPropertiesPage : XPropertyPage
    {
        // =========================================================================================
        // Constructors
        // =========================================================================================
        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpBuildEventsPropertyPage"/> class.
        /// </summary>
        public XSharpGlobalUsingsPropertiesPage()
        {
            this.PageName = "Global Usings";
            this.PerConfig = false;
        }

        // =========================================================================================
        // Methods
        // =========================================================================================


        /// <summary>
        /// Creates the controls that constitute the property page. This should be safe to re-entrancy.
        /// </summary>
        /// <returns>The newly created main control that hosts the property page.</returns>
        protected override XPropertyPagePanel CreatePropertyPagePanel()
        {
            return new XGlobalUsingsPropertyPagePanel(this);
        }
    }
}
