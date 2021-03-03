//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

namespace Microsoft.VisualStudio.Project
{
    using System;
    using System.ComponentModel;
    using System.Drawing;
    using System.Windows.Forms;
    using System.Windows.Forms.Design;

    /// <summary>
    /// Customized group box used for property page groups.
    /// </summary>
    [DefaultProperty("Text")]
    public partial class XGroupBox : Panel
    {
        // =========================================================================================
        // Member Variables
        // =========================================================================================

        private XGroupLabel groupLabel;

        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XGroupBox"/> class.
        /// </summary>
        public XGroupBox()
        {
            this.InitializeComponent();
        }

        // =========================================================================================
        // Properties
        // =========================================================================================

        /// <summary>
        /// Gets or sets the group label text.
        /// </summary>
        [Browsable(true)]
        public override string Text
        {
            get { return this.groupLabel.Text; }
            set { this.groupLabel.Text = value; }
        }

        /// <summary>
        /// Gets the space, in pixels, that is specified by default between controls.
        /// </summary>
        protected override Padding DefaultMargin
        {
            get { return new Padding(3, 12, 3, 3); }
        }

        /// <summary>
        /// Gets the internal spacing, in pixels, of the contents of a control.
        /// </summary>
        protected override Padding DefaultPadding
        {
            get { return new Padding(24, 24, 0, 0); }
        }

        // =========================================================================================
        // Methods
        // =========================================================================================

        /// <summary>
        /// Occurs when the control has resized.
        /// </summary>
        /// <param name="e">The <see cref="EventArgs"/> object that contains the event data.</param>
        protected override void OnResize(EventArgs e)
        {
            base.OnResize(e);

            if (this.groupLabel != null)
            {
                this.groupLabel.Width = this.ClientSize.Width + 3;
            }
        }

        /// <summary>
        /// Initializes the component's controls and other properties.
        /// </summary>
        private void InitializeComponent()
        {
            this.groupLabel = new Microsoft.VisualStudio.Project.XGroupLabel();
            this.SuspendLayout();
            // 
            // groupLabel
            // 
            this.groupLabel.Location = new System.Drawing.Point(0, 0);
            this.groupLabel.Name = "groupLabel";
            this.groupLabel.Size = new System.Drawing.Size(100, 23);
            this.groupLabel.TabIndex = 0;
            // 
            // XGroupBox
            // 
            this.Controls.Add(this.groupLabel);
            this.ResumeLayout(false);

        }
    }
}
