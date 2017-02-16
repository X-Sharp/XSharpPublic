//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

namespace XSharp.Project
{
    using System;
    using System.Diagnostics;
    using System.Text;
    using System.Windows.Forms;

    /// <summary>
    /// Property page contents for the Candle Settings page.
    /// </summary>
    internal partial class XBuildEventsPropertyPagePanel : XPropertyPagePanel
    {
        // =========================================================================================
        // Member Variables
        // =========================================================================================

        private XBuildEventEditorForm editorForm = new XBuildEventEditorForm(null);

        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPagePanel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The parent property page to which this is bound.</param>
        public XBuildEventsPropertyPagePanel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            this.InitializeComponent();

            // hook up the form to both editors
            this.preBuildEditor.Initialize((XProjectNode)  parentPropertyPage.ProjectMgr, this.editorForm);
            this.postBuildEditor.Initialize((XProjectNode) parentPropertyPage.ProjectMgr, this.editorForm);

            this.preBuildEditor.TextBox.Tag = XProjectFileConstants.PreBuildEvent;
            this.postBuildEditor.TextBox.Tag = XProjectFileConstants.PostBuildEvent;
            this.runPostBuildComboBox.Tag = XProjectFileConstants.RunPostBuildEvent;
        }
    }
}
