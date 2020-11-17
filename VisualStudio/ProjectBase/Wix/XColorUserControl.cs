//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
namespace Microsoft.VisualStudio.Project
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel;
    using System.Drawing;
    using System.Data;
    using System.Text;
    using System.Windows.Forms;

    /// <summary>
    /// A control of which all children are being colored
    /// according to the VS colors service
    /// </summary>
    public partial class XColorUserControl : UserControl
    {
        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Constructor for the control
        /// </summary>
        internal XColorUserControl()
        {
            this.InitializeComponent();
        }

        // =========================================================================================
        // Methods
        // =========================================================================================

        /// <summary>
        /// Override to return our specialized controls collection which allows us 
        /// to control the colors of the controls as they're being added.
        /// </summary>
        /// <returns>A collection that will contain all controls on the control</returns>
        protected override ControlCollection CreateControlsInstance()
        {
            return new XColorUserControlCollection(this);
        }

        /// <summary>
        /// Resets the control colors when the system colors change
        /// </summary>
        /// <param name="e">The object representing the event   </param>
        protected override void OnSystemColorsChanged(EventArgs e)
        {
            base.OnSystemColorsChanged(e);

            // This sets the background control for all this control and all of its children
            //this.BackColor = XHelperMethods.GetVsColor(XHelperMethods.Vs2010Color.VSCOLOR_BUTTONFACE);

            // The forecolor has to be set explicitly for each control
            XHelperMethods.SetControlTreeColors(this);
        }

        /// <summary>
        /// A collection of child controls on the WixColorUserControl
        /// which sets the proper colors on which added control
        /// </summary>
        internal class XColorUserControlCollection : Control.ControlCollection
        {
            // =========================================================================================
            // Constructors
            // =========================================================================================

            /// <summary>
            /// Constructor for the collection of child controls
            /// </summary>
            /// <param name="owner">The parent control of the collection</param>
            internal XColorUserControlCollection(Control owner)
                : base(owner)
            {
            }

            // =========================================================================================
            // Methods
            // =========================================================================================

            /// <summary>
            /// Adds a control to the collection of child controls
            /// </summary>
            /// <param name="value">Control to be added</param>
            public override void Add(Control value)
            {
                base.Add(value);
                XHelperMethods.SetControlTreeColors(value);
            }
        }
    }   
}
