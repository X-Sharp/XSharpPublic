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
    using System.Windows.Forms;
    using Microsoft.VisualStudio.Project;
    /// <summary>
    /// Property page contents for the Candle Settings page.
    /// </summary>
    internal partial class XBuildEventsPropertyPagePanel : XPropertyPagePanel
    {

        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPagePanel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The parent property page to which this is bound.</param>
        public XBuildEventsPropertyPagePanel(XPropertyPage parentPropertyPage, string[] names)
            : base(parentPropertyPage)
        {
        }
    }
}
