//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;

using System;
using System.Drawing;
using System.Windows.Forms;

using XSharp.Settings;

using XSharpModel;

namespace XSharp.Project
{
    /// <summary>
    /// Property page contents for the Candle Settings page.
    /// </summary>
    internal partial class XGeneralPropertyPagePanel : XPropertyPagePanel
    {
        public XGeneralPropertyPagePanel() : base()
        {
        }
        private string TargetFrameworkMoniker =>
            ParentPropertyPage.ProjectMgr?.TargetFrameworkMoniker.FullName;
        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPagePanel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The parent property page to which this is bound.</param>
        public XGeneralPropertyPagePanel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
        }
    }
}
