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
    using Microsoft.VisualStudio.Shell;

    /// <summary>
    /// Property page contents for the Candle Settings page.
    /// </summary>

    internal partial class XDialectPropertyPagePanel : XPropertyPagePanel
    {
        #region Constants
        internal const string DialectCaption = "Dialect";
        internal const string VO1Caption = "Allow Init() and Axit() as aliases for Constructor/Destructor";
        internal const string VO2Caption = "Initialize strings";
        internal const string VO3Caption = "All instance methods virtual";
        internal const string VO4Caption = "Implicit signed/unsigned conversions";
        internal const string VO5Caption = "Implicit Clipper calling convention";
        internal const string VO6Caption = "Implicit pointer conversions";
        internal const string VO7Caption = "Implicit casts and conversions";
        internal const string VO8Caption = "Compatible preprocessor";
        internal const string VO9Caption = "Handle problems with incorrect or missing return statements";
        internal const string VO10Caption = "Compatible IIF Behavior";
        internal const string VO11Caption = "Compatible numeric conversions";
        internal const string VO12Caption = "Clipper Compatible integer divisions";
        internal const string VO13Caption = "Compatible string comparisons";
        internal const string VO14Caption = "Use FLOAT literals";
        internal const string VO15Caption = "Treat missing types as USUAL";
        internal const string VO16Caption = "Generate Clipper constructors";
        internal const string XPP1Caption = "Inherit from Abstract class";
        internal const string FOX1Caption = "Inherit from Custom class";
        internal const string FOX2Caption = "Compatible Array Handling";
        internal const string VO1Description = "Allow Init() and Axit() as aliases for Constructor/Destructor (/vo1)";
        internal const string VO2Description = "Initialize strings to empty string (String.Empty) ( /vo2). Please note that in .NET a NULL_STRING is not the same as a string with length 0. When enabled this will initialize local string variables regardless of the setting of 'initialize locals' setting from the Language page.";
        internal const string VO3Description = "Add the virtual modifier to all methods by default (which is the normal Visual Objects behavior) (/vo3)";
        internal const string VO4Description = "Implicit signed/unsigned integer conversions (/vo4)";
        internal const string VO5Description = "Methods without parameters and calling convention are compiled as Clipper calling convention (/vo5). \nPlease note that without this switch all methods without parameters will be seen as STRICT. Methods with untyped parameters are always seen as CLIPPER calling convention.";
        internal const string VO6Description = "Implicit conversions between typed function PTR and PTR (/vo6)";
        internal const string VO7Description = "Compatible implicit casts and Conversions (/vo7)";
        internal const string VO8Description = "Makes the preprocessor case insensitive and also controls how #ifdef inspects #defines (/vo8)";
        internal const string VO9Description = "Allow missing return statements or allow return statements with incorrect return values (/vo9)";
        internal const string VO10Description = "Compatible IIF Behavior, allow different types of return values in TRUE and FALSE expression (/vo10)";
        internal const string VO11Description = "Compatible arithmetic conversions  (/vo11)";
        internal const string VO12Description = "Compatible integer divisions, integer divisions may return a float  (/vo12)";
        internal const string VO13Description = "Compatible string comparisons, respects SetExact and collation table (/vo13)";
        internal const string VO14Description = "Store floating point literals as FLOAT and not as System.Double (REAL8)  (/vo14)";
        internal const string VO15Description = "Missing type clauses for locals, instance variables and parameters are treated as USUAL (VO and Vulcan dialect). The default = TRUE for the VO dialect and FALSE for the other dialects. We strongly recommend to set this to FALSE because this will help you to find problems in your code and non optimal code. If you have to use the USUAL type we recommend to explicitly declare variables and parameters as USUAL (/vo15)";
        internal const string VO16Description = "Automatically create clipper calling convention constructors for classes without constructor where the parent class has a Clipper Calling convention constructor.(/vo16)";
        internal const string XPP1Description = "All classes without parent class inherit from the XPP Abstract class.(/xpp1)";
        internal const string FOX1Description = "All classes are assumed to inherit from the Custom class. This also affects the way in which properties are processed by the compiler.(/fox1)";
        internal const string FOX2Description = "FoxPro compatible array handling (Allows parenthesized arrays and assigning a single value to an array to fill all elements). WARNING Allowing parenthesized arrays may slow down the execution of your program !(/fox2)";
        internal const string CatCompatibility = "All dialects";
        internal const string CatNotCore = "Not in Core dialect";
        internal const string XPPCompatibility = "Xbase++ Compatibility";
        internal const string FOXCompatibility = "Visual FoxPro Compatibility";
        #endregion

        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPagePanel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The parent property page to which this is bound.</param>
        public XDialectPropertyPagePanel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            this.InitializeComponent();
            this.chkVO1.Text = VO1Caption;
            this.chkVO2.Text = VO2Caption;
            this.chkVO3.Text = VO3Caption;
            this.chkVO4.Text = VO4Caption;
            this.chkVO5.Text = VO5Caption;
            this.chkVO6.Text = VO6Caption;
            this.chkVO7.Text = VO7Caption;
            this.chkVO8.Text = VO8Caption;
            this.chkVO9.Text = VO9Caption;
            this.chkVO10.Text = VO10Caption;
            this.chkVO11.Text = VO11Caption;
            this.chkVO12.Text = VO12Caption;
            this.chkVO13.Text = VO13Caption;
            this.chkVO14.Text = VO14Caption;
            this.chkVO15.Text = VO15Caption;
            this.chkVO16.Text = VO16Caption;
            this.chkFox1.Text = FOX1Caption;
            this.chkFox2.Text = FOX2Caption;
            this.chkXPP1.Text = XPP1Caption;
            this.chkVO1.Tag = XSharpProjectFileConstants.Vo1;
            this.chkVO2.Tag = XSharpProjectFileConstants.Vo2;
            this.chkVO3.Tag = XSharpProjectFileConstants.Vo3;
            this.chkVO4.Tag = XSharpProjectFileConstants.Vo4;
            this.chkVO5.Tag = XSharpProjectFileConstants.Vo5;
            this.chkVO6.Tag = XSharpProjectFileConstants.Vo6;
            this.chkVO7.Tag = XSharpProjectFileConstants.Vo7;
            this.chkVO8.Tag = XSharpProjectFileConstants.Vo8;
            this.chkVO9.Tag = XSharpProjectFileConstants.Vo9;
            this.chkVO10.Tag = XSharpProjectFileConstants.Vo10;
            this.chkVO11.Tag = XSharpProjectFileConstants.Vo11;
            this.chkVO12.Tag = XSharpProjectFileConstants.Vo12;
            this.chkVO13.Tag = XSharpProjectFileConstants.Vo13;
            this.chkVO14.Tag = XSharpProjectFileConstants.Vo14;
            this.chkVO15.Tag = XSharpProjectFileConstants.Vo15;
            this.chkVO16.Tag = XSharpProjectFileConstants.Vo16;
            this.chkFox1.Tag = XSharpProjectFileConstants.Fox1;
            this.chkFox2.Tag = XSharpProjectFileConstants.Fox2;
            this.chkXPP1.Tag = XSharpProjectFileConstants.Xpp1;
            toolTip1.SetToolTip(chkVO1, VO1Description);
            toolTip1.SetToolTip(chkVO2, VO2Description);
            toolTip1.SetToolTip(chkVO3, VO3Description);
            toolTip1.SetToolTip(chkVO4, VO4Description);
            toolTip1.SetToolTip(chkVO5, VO5Description);
            toolTip1.SetToolTip(chkVO6, VO6Description);
            toolTip1.SetToolTip(chkVO7, VO7Description);
            toolTip1.SetToolTip(chkVO8, VO8Description);
            toolTip1.SetToolTip(chkVO9, VO9Description);
            toolTip1.SetToolTip(chkVO10, VO10Description);
            toolTip1.SetToolTip(chkVO11, VO11Description);
            toolTip1.SetToolTip(chkVO12, VO12Description);
            toolTip1.SetToolTip(chkVO13, VO13Description);
            toolTip1.SetToolTip(chkVO14, VO14Description);
            toolTip1.SetToolTip(chkVO15, VO15Description);
            toolTip1.SetToolTip(chkVO16, VO16Description);

            toolTip1.SetToolTip(chkFox1, FOX1Description);
            toolTip1.SetToolTip(chkFox2, FOX2Description);
            toolTip1.SetToolTip(chkXPP1, XPP1Description);
            this.lblAllDialects.Text = CatCompatibility;
            this.lblNotInCore.Text = CatNotCore;
            this.lblVFP.Text = FOXCompatibility;
            this.lblXPP.Text = XPPCompatibility;

            // hook up the form to both editors
            Color defaultBackground = SystemColors.ButtonFace;
            Color defaultForeground = SystemColors.WindowText;
            UpdateWindowColors(this, defaultBackground, defaultForeground);

        }

        internal void Project_OnProjectPropertyChanged(object sender, ProjectPropertyChangedArgs e)
        {
            try
            {
                if (e.OldValue != e.NewValue)
                {
                    if (e.PropertyName.ToLower() == "dialect")
                    {
                        if (e.NewValue.ToLower() == "foxpro")
                        {
                            chkFox1.Checked = true;
                            chkFox2.Checked = true;
                        }
                        else
                        {
                            chkFox1.Checked = false;
                            chkFox2.Checked = false;
                        }
                        EnableDialectOptions(e.NewValue);
                    }
                }
            }
            catch (Exception)
            {
            }
        }


        private void EnableDialectOptions(string dialect)
        {
            dialect = dialect.ToLower();
            chkFox1.Enabled = dialect == "foxpro";
            chkFox2.Enabled = dialect == "foxpro";
            chkXPP1.Enabled = dialect == "xpp";
            bool core = dialect == "core";
            chkVO5.Enabled = !core;
            chkVO6.Enabled = !core;
            chkVO7.Enabled = !core;
            chkVO11.Enabled = !core;
            chkVO12.Enabled = !core;
            chkVO13.Enabled = !core;
            chkVO14.Enabled = !core;
            chkVO15.Enabled = !core;
            chkVO16.Enabled = !core;
            if (core)
            {
                chkVO5.Checked = false;
                chkVO6.Checked = false;
                chkVO7.Checked = false;
                chkVO11.Checked = false;
                chkVO12.Checked = false;
                chkVO13.Checked = false;
                chkVO14.Checked = false;
                chkVO15.Checked = false;
                chkVO16.Checked = false;
                chkFox1.Checked = false;
                chkXPP1.Checked = false;
            }
            if (!chkFox1.Enabled )
                chkFox1.Checked = false;
            if (!chkFox2.Enabled)
                chkFox2.Checked = false;
            if (!chkXPP1.Enabled )
                chkXPP1.Checked = false;
        }
        internal void SetDialectOptions(string dialect)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            EnableDialectOptions(dialect);
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

        protected internal override void BindProperties()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            base.BindProperties();
            SetDialectOptions(ParentPropertyPage.GetProperty(XSharpProjectFileConstants.Dialect) ?? "Core");
        }
    }
}
