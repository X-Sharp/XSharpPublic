//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
namespace Microsoft.VisualStudio.Project
{
    using Microsoft.VisualStudio.Shell;
    using System;
    using System.ComponentModel;
    using System.Diagnostics;
    using System.Drawing;
    using System.Globalization;
    using System.Windows.Forms;
    using XSharp.Project;
    /// <summary>
    /// Property page contents for the Project Build Settings page.
    /// </summary>
    public partial class XPropertyPagePanel : XColorUserControl
    {
        // =========================================================================================
        // Member Variables
        // =========================================================================================

        private XPropertyPage parentPropertyPage;

        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XPropertyPagePanel"/> class.
        /// </summary>
        public XPropertyPagePanel()
            : this(null)
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="XPropertyPagePanel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The parent property page to which this is bound.</param>
        public XPropertyPagePanel(XPropertyPage parentPropertyPage)
        {
            this.parentPropertyPage = parentPropertyPage;
            this.InitializeComponent();

            this.Font = XHelperMethods.GetDialogFont();
            this.ForeColor = XHelperMethods.GetVsColor(XHelperMethods.Vs2010Color.VSCOLOR_BUTTONTEXT);
        }

        // =========================================================================================
        // Properties
        // =========================================================================================

        /// <summary>
        /// Gets or sets the parent property page to which this is bound.
        /// </summary>
        internal XPropertyPage ParentPropertyPage
        {
            get { return this.parentPropertyPage; }
            set { this.parentPropertyPage = value; }
        }

        // =========================================================================================
        // Methods
        // =========================================================================================

        /// <summary>
        /// Called before saving to make sure there aren't any unapplied changes.
        /// </summary>
        internal void Apply()
        {
            TextBox textBox = this.ActiveControl as TextBox;
            if (textBox == null)
            {
                FolderBrowserTextBox folderBrowser = this.ActiveControl as FolderBrowserTextBox;
                if (folderBrowser != null)
                {
                    textBox = folderBrowser.TextBox;
                }
            }

            if (textBox == null)
            {
                XBuildEventEditor buildEventEditor = this.ActiveControl as XBuildEventEditor;
                if (buildEventEditor != null)
                {
                    textBox = buildEventEditor.TextBox;
                }
            }
            ThreadHelper.ThrowIfNotOnUIThread();

            if (textBox != null && textBox.Modified)
            {
                this.HandleTextBoxLostFocus(textBox, null);
            }
        }

        /// <summary>
        /// Called once after the page is created to hook up default property events.
        /// </summary>
        internal void HookupEvents()
        {
            this.HookupEvents(this);
        }

        /// <summary>
        /// Binds the properties from the MSBuild project file to the controls on the property page.
        /// </summary>
        protected internal virtual void BindProperties()
        {
            this.BindProperties(this);
        }

        /// <summary>
        /// Called when a control has been successfully validated.
        /// </summary>
        /// <param name="sender">The control that was validated.</param>
        /// <param name="e">Parameters for the event.</param>
        protected virtual void HandleControlValidated(object sender, EventArgs e)
        {
            Control control = (Control)sender;
            string propertyName = (string)control.Tag;
            string value = null;

            TextBox textBox = control as TextBox;
            CheckBox checkBox = control as CheckBox;
            ComboBox comboBox = control as ComboBox;

            if (textBox != null && !textBox.Modified)
            {
                return;
            }

            string currentValue = this.ParentPropertyPage.GetProperty(propertyName);
            if (checkBox != null)
            {
                if (checkBox.CheckState == CheckState.Indeterminate)
                {
                    value = null;
                }
                else
                {
                    value = checkBox.Checked.ToString(CultureInfo.InvariantCulture);
                }

                CheckState currentCheckState = this.ParentPropertyPage.GetPropertyCheckState(propertyName);
                if (currentCheckState == CheckState.Indeterminate)
                {
                    currentValue = null;
                }
                else
                {
                    currentValue = (currentCheckState == CheckState.Checked).ToString();
                }
            }
            else if (comboBox != null)
            {
                value = comboBox.SelectedItem.ToString();
            }
            else if (textBox != null)
            {
                value = this.ParentPropertyPage.Normalize(propertyName, control.Text);
            }
            else
            {
                value = control.Text;
            }
            ThreadHelper.ThrowIfNotOnUIThread();

            if (value != null && !String.Equals(value, currentValue, StringComparison.Ordinal))
            {
                // Note query-edit for TextBoxes was already done on the ModifiedChanged event.
                if (textBox != null || this.ParentPropertyPage.ProjectMgr.QueryEditProjectFile(false))
                {
                    this.ParentPropertyPage.SetProperty(propertyName, value);
                }
                else
                {
                    if (checkBox != null)
                    {
                        checkBox.CheckState = this.ParentPropertyPage.GetPropertyCheckState(propertyName);
                    }
                    else if (comboBox != null)
                    {
                        comboBox.SelectedItem = currentValue;
                    }
                    else
                    {
                        control.Text = currentValue;
                    }
                }
            }

            if (textBox != null)
            {
                // Set to normalized text.
                textBox.Text = value;
                textBox.Modified = false;
            }
        }

        private void BindProperties(Control control)
        {
            string propertyBinding = control.Tag as string;
            if (!String.IsNullOrEmpty(propertyBinding))
            {
                CheckBox checkBox = control as CheckBox;
                if (checkBox != null)
                {
                    checkBox.CheckState = this.ParentPropertyPage.GetPropertyCheckState(propertyBinding);
                }
                else
                {
                    string value = this.ParentPropertyPage.GetProperty(propertyBinding);

                    ComboBox comboBox = control as ComboBox;
                    if (comboBox != null)
                    {
                        int index = 0;
                        comboBox.SelectedIndex = -1;
                        foreach (string item in comboBox.Items)
                        {
                            if (item == value)
                            {
                                comboBox.SelectedIndex = index;
                                break;
                            }
                            index++;
                        }
                    }
                    else
                    {
                        control.Text = (value != null ? value : String.Empty);
                    }
                }
            }

            foreach (Control child in control.Controls)
            {
                this.BindProperties(child);
            }
        }

        private void HookupEvents(Control control)
        {
            string propertyBinding = control.Tag as string;
            if (propertyBinding != null)
            {
                TextBox textBox = control as TextBox;
                if (textBox != null)
                {
                    textBox.LostFocus += this.HandleTextBoxLostFocus;
                    textBox.ModifiedChanged += this.HandleTextBoxModifiedChanged;
                }
                else
                {
                    CheckBox checkBox = control as CheckBox;
                    if (checkBox != null)
                    {
                        checkBox.CheckStateChanged += this.HandleControlValidated;
                    }
                    else
                    {
                        ComboBox comboBox = control as ComboBox;
                        if (comboBox != null)
                        {
                            comboBox.SelectedValueChanged += this.HandleControlValidated;
                        }
                        else
                        {
                            FoldersSelector selector = control as FoldersSelector;
                            if (selector != null)
                            {
                                selector.FolderValidating += this.HandleControlValidating;
                                selector.FoldersChanged += this.HandleControlValidated;
                            }
                        }
                    }
                }
            }

            foreach (Control child in control.Controls)
            {
                this.HookupEvents(child);
            }
        }

        /// <summary>
        /// Called when text in a text box changed. Ensures the project file is editable and sets the page dirty flag.
        /// </summary>
        /// <param name="sender">The control whose text changed.</param>
        /// <param name="e">Parameters for the event.</param>
        private void HandleTextBoxModifiedChanged(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            TextBox textBox = (TextBox)sender;
            if (textBox.Modified && !this.ParentPropertyPage.IsDirty)
            {
                // We may be about to show a query-edit dialog. Prevent the focus change from triggering validation.
                textBox.Modified = false;

                if (this.ParentPropertyPage.ProjectMgr.QueryEditProjectFile(false))
                {
                    this.ParentPropertyPage.IsDirty = true;
                    textBox.Modified = true;
                }
                else
                {
                    // Revert to the saved property value.
                    string propertyName = (string)textBox.Tag;
                    textBox.Text = this.ParentPropertyPage.GetProperty(propertyName);
                }
            }
        }

        /// <summary>
        /// Called when a control is being validated. Set e.Cancel to true to cause the validation to fail.
        /// </summary>
        /// <param name="sender">The control being validated.</param>
        /// <param name="e">Parameters for the event.</param>
        private void HandleControlValidating(object sender, System.ComponentModel.CancelEventArgs e)
        {
            Control control = (Control)sender;
            string propertyName = (string)control.Tag;
            string value = control.Text;

            TextBox textBox = control as TextBox;
            if (textBox != null)
            {
                if (!textBox.Modified)
                {
                    return;
                }

                value = this.ParentPropertyPage.Normalize(propertyName, value);
            }
            else
            {
                FoldersSelector foldesSelector = control as FoldersSelector;
                if (foldesSelector != null)
                {
                    value = foldesSelector.TextBox.Text;
                }
            }

            string currentValue = this.ParentPropertyPage.GetProperty(propertyName);
            ThreadHelper.ThrowIfNotOnUIThread();

            if (!String.Equals(value, currentValue, StringComparison.Ordinal))
            {
                try
                {
                    PropertyValidator.ValidateProperty(propertyName, value);
                }
                catch (ProjectPropertyArgumentException ex)
                {
                    if (textBox != null)
                    {
                        // Don't retrigger Validation when losing focus due to the message box.
                        textBox.Modified = false;
                    }

                    XHelperMethods.ShowErrorMessageBox(this.ParentPropertyPage.Site, ex.Message);
                    e.Cancel = true;

                    if (textBox != null)
                    {
                        textBox.Text = currentValue;
                        textBox.Modified = false;

                        // Clear the page dirty flag if it was only this textbox that was modified.
                        int isDirty;
                        if (this.ParentPropertyPage.ProjectMgr.IsDirty(out isDirty) == 0 && isDirty == 0)
                        {
                            this.ParentPropertyPage.IsDirty = false;
                        }
                    }
                }
            }
            else if (textBox != null && textBox.Modified)
            {
                // The text wasn't significantly changed, but might need to be adjusted to the trimmed value.
                textBox.Text = value;
                textBox.Modified = false;

                // Clear the page dirty flag if it was only this textbox that was modified.
                int isDirty;
                if (this.ParentPropertyPage.ProjectMgr.IsDirty(out isDirty) == 0 && isDirty == 0)
                {
                    this.ParentPropertyPage.IsDirty = false;
                }
            }
        }

        /// <summary>
        /// The "Validating" events don't fire in all cases when textboxes lose focus. So we use the LostFocus event instead.
        /// </summary>
        /// <param name="sender">The control that lost focus.</param>
        /// <param name="e">Parameters for the event.</param>
        private void HandleTextBoxLostFocus(object sender, EventArgs e)
        {
            TextBox textBox = sender as TextBox;
            ThreadHelper.ThrowIfNotOnUIThread();
            if (textBox != null && textBox.Modified)
            {
                CancelEventArgs ce = new CancelEventArgs();
                this.HandleControlValidating(sender, ce);
                if (!ce.Cancel)
                {
                    this.HandleControlValidated(sender, e);
                }
            }
        }
        protected void FillCombo(TypeConverter converter, System.Windows.Forms.ComboBox combo)
        {
            foreach (var enumvalue in converter.GetStandardValues(null))
            {
                var name = converter.ConvertTo(enumvalue, typeof(System.String));
                combo.Items.Add(name); // new comboItem ((int) enumvalue, name));
            }
        }

        protected void ShowOpenFileDialog(TextBox tb, string description, string filters)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var openFileDialog = new OpenFileDialog();
            openFileDialog.FileName = tb.Text;
            openFileDialog.Filter = filters;
            openFileDialog.FilterIndex = 0;
            openFileDialog.Title = description;
            var result = openFileDialog.ShowDialog();
            if (result == DialogResult.OK)
            {
                ParentPropertyPage.SetProperty((string)tb.Tag, openFileDialog.FileName);
                tb.Text = openFileDialog.FileName;
            }
        }
        protected void showMacroDialog(TextBox tb, string caption)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var form = new XSharpSLEPropertyForm();
            XBuildMacroCollection mc = new XBuildMacroCollection((ProjectNode)this.ParentPropertyPage.ProjectMgr);
            form.SetMacros(mc);
            form.PropertyText.Text = tb.Text;
            form.Text = caption;
            var result = form.ShowDialog();
            if (result == DialogResult.OK)
            {
                tb.Text = form.PropertyText.Text;
                this.ParentPropertyPage.SetProperty((string)tb.Tag, tb.Text);
            }
        }

    }
}
