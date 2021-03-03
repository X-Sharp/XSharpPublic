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
    internal partial class XLanguagePropertyPagePanel : XPropertyPagePanel
    {
      
        //todo Conditionally enable/disable certain options
        #region Constants
        internal const string LanguageCaption = "Language";
        internal const string CMDCaption = "Extra Command Line Options";
        internal const string AZCaption = "Use Zero Based Arrays";
        internal const string CSCaption = "Case Sensitive";
        internal const string INSCaption = "Enable Implicit Namespace lookup";
        internal const string LBCaption = "Allow Late Binding";
        internal const string NamedArgCaption = "Allow Named Arguments";
        internal const string NSCaption = "Prefix classes with default Namespace";
        internal const string OVFCaption = "Overflow Exceptions";
        internal const string UnsafeCaption = "Allow Unsafe Code";
        internal const string MemVarCaption = "Enable Memvar support";
        internal const string UndeclaredCaption = "Enable Undeclared variables support";
        internal const string InitLocalsCaption = "Initialize Local variables";

        internal const string CSDescription = "Enable/Disable case sensitivity (/cs)";
        internal const string AZDescription = "Use Zero Based Arrays (/az)";
        internal const string INSDescription = "Enable the implicit lookup of classes defined in assemblies with an Implicit Namespace attribute (/ins)";
        internal const string LBDescription = "Allow property access and method calls on expressions of type OBJECT and USUAL (/lb)";
        internal const string NamedArgDescription = "Allow named arguments (Default = FALSE for the Core dialect and TRUE for the other dialects). Changing the dialect may also automatically change this setting. (/namedargs)";
        internal const string NSDescription = "Prefix all classes that do not have a namespace prefix and are not in a begin namespace ... end namespace block with the namespace of the assembly (/ns:<Namespace>)";
        internal const string OVFDescription = "Check for Overflow and Underflow for numeric expressions, like the CHECKED keyword. (/ovf)";
        internal const string UnsafeDescription = "Allow Unsafe code inside this assembly (/unsafe)";
        internal const string InitLocalsDescription = "Automatically initialize local variables without initialization expression. Please note that for locals of type string the initial value will depend on the 'Initialize strings' setting from the Dialect page.(/initlocals)";

        internal const string NoStdDefCaption = "Suppress standard header file";
        internal const string NoStdDefDescription = "Suppress inclusion of the standard header file (XSharpDefs.xh) in every file (/nostddef)";
        internal const string INCCaption = "Additional Include paths";
        internal const string INCDescription = "Additional include paths for the preprocessor (it also looks through the folders set with the include environment variable) (/i)";
        internal const string StdDefCaption = "Alternate standard header file";
        internal const string StdDefDescription = "Name of an alternative standard header file (alternative for XSharpDefs.xh)  (/stddefs)";
        internal const string MemVarDescription = "Enable support for memory variables (MEMVAR, PUBLIC, PRIVATE & PARAMETERS). (/memvar)\rPlease note that this is NOT supported for the Core and Vulcan dialects";
        internal const string UndeclaredDescription = "Enable support for undeclared variables (these are resolved to MEMVARs). (/undeclared)\rPlease note that this requires /memvar to be enabled as well.";

        internal const string CatGeneral = "General";
        internal const string CatNamespaces = "Namespaces";
        internal const string CatPreprocessor = "Preprocessor";
        internal const string CatMemVars = "Memory Variables";

        #endregion

        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPagePanel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The parent property page to which this is bound.</param>
        public XLanguagePropertyPagePanel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            this.InitializeComponent();
            chkAZ.Text = AZCaption;
            chkAZ.Tag = "AZ";
            toolTip1.SetToolTip(chkAZ, AZDescription);
            chkCS.Text = CSCaption;
            chkCS.Tag = "CS";
            toolTip1.SetToolTip(chkCS, CSDescription);
            chkIns.Text = INSCaption;
            chkIns.Tag = "INS";
            toolTip1.SetToolTip(chkIns, INSDescription);
            chkInitLocals.Text = InitLocalsCaption;
            chkInitLocals.Tag = "InitLocals";
            toolTip1.SetToolTip(chkInitLocals, InitLocalsDescription);
            chkLB.Text = LBCaption;
            chkLB.Tag = "LB";
            toolTip1.SetToolTip(chkLB, LBDescription);
            chkMemVar.Text = MemVarCaption;
            chkMemVar.Tag = "MEMVAR";
            toolTip1.SetToolTip(chkMemVar, MemVarDescription);
            chkMemVar.CheckStateChanged += ChkMemVar_CheckStateChanged;
            chkNamedArgs.Text = NamedArgCaption;
            chkNamedArgs.Tag = "NamedArgs";
            toolTip1.SetToolTip(chkNamedArgs, NamedArgDescription);
            chkNoStandardDefs.Text = NoStdDefCaption;
            chkNoStandardDefs.Tag = "NoStandardDefs";
            toolTip1.SetToolTip(chkNoStandardDefs, NoStdDefDescription);
            chkNS.Text = NSCaption;
            chkNS.Tag = "NS";
            toolTip1.SetToolTip(chkNS, NSDescription);
            chkOvf.Text = OVFCaption;
            chkOvf.Tag = "OVF";
            toolTip1.SetToolTip(chkOvf, OVFDescription);
            chkUndefined.Text = UndeclaredCaption;
            chkUndefined.Tag = "Undeclared";
            toolTip1.SetToolTip(chkUndefined, UndeclaredDescription);
            chkUnsafe.Text = UnsafeCaption;
            chkUnsafe.Tag = "Unsafe";
            toolTip1.SetToolTip(chkUnsafe, UnsafeDescription);

            lblIncludePaths.Text = INCCaption;
            tbIncludePath.Tag = "IncludePaths";
            toolTip1.SetToolTip(lblIncludePaths, INCDescription);
            toolTip1.SetToolTip(tbIncludePath, INCDescription);
            lblStandardDefs.Text = StdDefCaption;
            tbStandardDefs.Tag = "StandardDefs";
            toolTip1.SetToolTip(lblStandardDefs, StdDefDescription);
            toolTip1.SetToolTip(tbStandardDefs, StdDefDescription);
            // hook up the form to both editors
            Color defaultBackground = SystemColors.ButtonFace;
            Color defaultForeground = SystemColors.WindowText;
            UpdateWindowColors(this, defaultBackground, defaultForeground);

        }
        private void ChkMemVar_CheckStateChanged(object sender, EventArgs e)
        {
            EnableMemVars();
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
        internal void SetDialectOptions(string dialect)
        {
            dialect = dialect.ToLower();
            bool Core = dialect == "core";

            chkMemVar.Enabled = !Core;
            chkLB.Enabled = !Core;

            if (!Core)
            {
                EnableMemVars(); // this sets Undefined 
            }
            else
            {
                chkUndefined.Enabled = false;
                chkMemVar.Checked = false;
                chkUndefined.Checked = false;
                chkLB.Checked = false;
            }
        }
        internal void Project_OnProjectPropertyChanged(object sender, ProjectPropertyChangedArgs e)
        {
            try
            {
                if (e.PropertyName.ToLower() == "dialect")
                {
                    SetDialectOptions(e.NewValue);
                }
            }
            catch (Exception)
            {
            }
        }
        private void EnableMemVars()
        {
            chkUndefined.Enabled = chkMemVar.Checked;
        }

        private void EnableDisableStandardDefs()
        {
            if (chkNoStandardDefs.Checked)
            {
                tbStandardDefs.Text = "";
            }
        }
        protected internal override void BindProperties()
        {
            base.BindProperties();
            SetDialectOptions(ParentPropertyPage.GetProperty("dialect"));
        }

    }
}
