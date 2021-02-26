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
    using System.Collections.Generic;
    using System.Windows.Forms;
    using System.ComponentModel;
    using Microsoft.VisualStudio.Project;
    /// <summary>
    /// Property page contents for the Candle Settings page.
    /// </summary>
    internal partial class XGeneralPropertyPagePanel : XPropertyPagePanel
    {
        private void FillCombo(TypeConverter converter, System.Windows.Forms.ComboBox combo)
        {
            foreach (var enumvalue in converter.GetStandardValues(null))
            {
                var name = converter.ConvertTo(enumvalue, typeof(System.String));
                combo.Items.Add(name); // new comboItem ((int) enumvalue, name));
            }
        }


        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPagePanel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The parent property page to which this is bound.</param>
        public XGeneralPropertyPagePanel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            this.InitializeComponent();

            this.tbAssemblyName.Tag = ProjectFileConstants.AssemblyName;
            this.tbDefaultNamespace.Tag = ProjectFileConstants.RootNamespace;
            this.tbAppIcon.Tag = ProjectFileConstants.ApplicationIcon;
            this.tbStartupObject.Tag = ProjectFileConstants.StartupObject;
            this.chkPreferNativeVersion.Tag = "UseNativeVersion";
            this.chkSuppressDefaultManifest.Tag = "NoWin32Manifest";
            this.chkVulcanCompatibleResources.Tag = "VulcanCompatibleResources";
            this.chkAutoGenerateBindingRedirects.Tag = "AutoGenerateBindingRedirects";
            this.comboDialect.Tag = "Dialect";
            this.comboOutputType.Tag = ProjectFileConstants.OutputType;
            this.comboTargetFramework.Tag = "TargetFrameworkVersion";
            FillCombo(new DialectConverter() , comboDialect);
            FillCombo(new OutputTypeConverter(), comboOutputType);
            FillCombo(new FrameworkNameConverter(), comboTargetFramework);


            // hook up the form to both editors
            Color defaultBackground = SystemColors.ButtonFace;
            Color defaultForeground = SystemColors.WindowText;
            UpdateWindowColors(this, defaultBackground, defaultForeground);
        }
        /// <summary>
        /// Adjust the  color values. Adjusts the text color and text
        /// area background color
        /// </summary>
        /// <param name="clrBackground">The desired color for the background of the text area</param>
        /// <param name="clrForeground">The desired text color</param>
        static void UpdateWindowColors(Control control, Color clrBackground, Color clrForeground)
        {
            // Update the window background
            if (control is TextBox)
                control.BackColor = Color.White;
            else
                control.BackColor = clrBackground;
            control.ForeColor = clrForeground;

            // Also update the label
            foreach (Control child in control.Controls)
            {
                UpdateWindowColors(child, clrBackground, clrForeground);
            }
        }
      

    }
}
