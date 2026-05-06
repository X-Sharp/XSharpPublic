//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using System;
using System.Drawing;
using System.Windows;
using System.Windows.Forms;
using System.Windows.Forms.Integration;

namespace XSharp.Project
{
    /// <summary>
    /// Abstract base class that bridges a WPF <see cref="FrameworkElement"/> (typically a
    /// WPF <c>UserControl</c>) into a WinForms <see cref="UserControl"/> so that it can be
    /// hosted inside the Visual Studio COM <c>IPropertyPage2</c> property sheet frame.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Visual Studio's property sheet is WinForms-based.  XAML content must be embedded
    /// via <see cref="ElementHost"/> (<c>System.Windows.Forms.Integration</c>), which acts
    /// as a WinForms control whose child is a WPF <see cref="UIElement"/>.
    /// </para>
    /// <para>
    /// This class:
    /// <list type="bullet">
    ///   <item>Extends <see cref="UserControl"/> to satisfy the WinForms hosting contract
    ///   (provides <c>Handle</c>, <c>Bounds</c>, <c>Show</c>/<c>Hide</c>, etc.).</item>
    ///   <item>Implements <see cref="IPropertyPagePanel"/> so that
    ///   <see cref="XPropertyPage"/> can treat it exactly like a WinForms panel.</item>
    ///   <item>Creates an <see cref="ElementHost"/> docked to fill the control.</item>
    ///   <item>Merges the <c>VsThemeDictionary.xaml</c> resource dictionary into the
    ///   hosted WPF content's resources so that all standard WPF controls automatically
    ///   follow the active VS theme (Blue / Dark / Light / High Contrast).</item>
    /// </list>
    /// </para>
    /// <para>
    /// Concrete XAML hosts (e.g., <c>XGeneralPropertyPageXamlHost</c>) must:
    /// <list type="number">
    ///   <item>Override <see cref="CreateXamlContent"/> to return the concrete WPF
    ///   <c>UserControl</c>.</item>
    ///   <item>Call <see cref="SetViewModel"/> with the page's
    ///   <see cref="XPropertyPageViewModel"/> to establish the WPF
    ///   <c>DataContext</c>.</item>
    ///   <item>Implement <see cref="BindProperties"/>, <see cref="ApplyChanges"/>, and
    ///   <see cref="HookupEvents"/> to delegate to the ViewModel.</item>
    /// </list>
    /// </para>
    /// </remarks>
    public abstract class XPropertyPageXamlHost : UserControl, IPropertyPagePanel
    {
        // =========================================================================================
        // Member Variables
        // =========================================================================================

        /// <summary>The WinForms/WPF bridge control.</summary>
        private ElementHost elementHost;

        /// <summary>The WPF content currently hosted inside the element host.</summary>
        private FrameworkElement xamlContent;

        /// <summary>The ViewModel bound to the WPF content's <c>DataContext</c>.</summary>
        private XPropertyPageViewModel viewModel;

        /// <summary>The parent COM property page.</summary>
        private XPropertyPage parentPropertyPage;

        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XPropertyPageXamlHost"/> class,
        /// creates the <see cref="ElementHost"/>, and loads the WPF content produced by
        /// <see cref="CreateXamlContent"/>.
        /// </summary>
        /// <param name="parentPropertyPage">
        /// The <see cref="XPropertyPage"/> that owns this panel.
        /// </param>
        protected XPropertyPageXamlHost(XPropertyPage parentPropertyPage)
        {
            this.parentPropertyPage = parentPropertyPage;

            // Fill the UserControl so VS can resize it freely.
            this.Dock = DockStyle.Fill;

            // Make the UserControl background transparent so the VS
            // property-page host background shows through (no grey border).
            this.BackColor = Color.Transparent;

            // Create the ElementHost bridge, also filling its parent.
            elementHost = new ElementHost
            {
                Dock = DockStyle.Fill,
                // Transparent background so no white/grey rectangle appears
                // around the WPF content when VS uses a dark or custom theme.
                BackColorTransparent = true,
            };
            this.Controls.Add(elementHost);

            // Create the WPF content via the abstract factory method.
            FrameworkElement content = CreateXamlContent();

            // Inject VS theme styles so all standard controls follow the active VS theme.
            InjectVsTheme(content);

            // Set as the ElementHost child.
            SetXamlContent(content);
        }

        // =========================================================================================
        // IPropertyPagePanel — Control surface
        // =========================================================================================

        /// <summary>
        /// Gets the underlying WinForms <see cref="Control"/> that Visual Studio hosts.
        /// For XAML hosts this is the <see cref="XPropertyPageXamlHost"/> itself (a
        /// <see cref="UserControl"/>), which wraps the <see cref="ElementHost"/>.
        /// </summary>
        public Control Control => this;

        // =========================================================================================
        // IPropertyPagePanel — Dirty flag
        // =========================================================================================

        /// <summary>
        /// Gets or sets a value indicating whether this panel has unsaved changes.
        /// Setting this to <see langword="true"/> propagates the dirty state to the
        /// owning <see cref="XPropertyPage"/> so the VS property sheet enables
        /// the <em>Apply</em> button.
        /// </summary>
        public bool IsDirty
        {
            get => viewModel?.isDirty ?? false;
            set
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                if (viewModel != null)
                    viewModel.isDirty = value;
                if (parentPropertyPage != null)
                    parentPropertyPage.IsDirty = value;
            }
        }

        // =========================================================================================
        // IPropertyPagePanel — Lifecycle  (abstract, implemented by concrete hosts)
        // =========================================================================================

        /// <summary>
        /// Wires up change-notification handlers so that edits made in the XAML controls
        /// propagate the dirty flag to the owning <see cref="XPropertyPage"/>.
        /// Typically delegates to <see cref="XPropertyPageViewModel.HookupEvents"/>.
        /// </summary>
        public abstract void HookupEvents();

        /// <summary>
        /// Loads all project property values from MSBuild and updates the ViewModel's
        /// observable properties so that the bound XAML controls display them.
        /// Typically delegates to <see cref="XPropertyPageViewModel.BindProperties"/>.
        /// </summary>
        public abstract void BindProperties();

        /// <summary>
        /// Reads all current ViewModel property values and writes them back to the
        /// MSBuild project.  Called when the user clicks <em>Apply</em> or <em>OK</em>.
        /// </summary>
        public void ApplyChanges()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            ApplyChangesCore();
        }

        /// <summary>
        /// Concrete implementation of <see cref="ApplyChanges"/>; override in each host.
        /// </summary>
        protected abstract void ApplyChangesCore();

        // =========================================================================================
        // Abstract Methods
        // =========================================================================================

        /// <summary>
        /// Creates the WPF <see cref="FrameworkElement"/> (typically a
        /// <c>UserControl</c>) that will be displayed inside the
        /// <see cref="ElementHost"/>.
        /// </summary>
        /// <returns>
        /// A new instance of the WPF control for this property page.
        /// </returns>
        protected abstract FrameworkElement CreateXamlContent();

        // =========================================================================================
        // Protected Methods
        // =========================================================================================

        /// <summary>
        /// Sets (or replaces) the WPF content hosted inside the <see cref="ElementHost"/>
        /// and applies VS theming to it.
        /// </summary>
        /// <param name="content">The WPF <see cref="FrameworkElement"/> to host.</param>
        protected void SetXamlContent(FrameworkElement content)
        {
            xamlContent = content;
            elementHost.Child = content;

            // Ensure the ViewModel DataContext is kept in sync if it was already set.
            if (viewModel != null && content != null)
                content.DataContext = viewModel;
        }

        /// <summary>
        /// Sets the ViewModel and assigns it as the <c>DataContext</c> of the hosted
        /// WPF content so that WPF data bindings resolve correctly.
        /// </summary>
        /// <param name="vm">
        /// The <see cref="XPropertyPageViewModel"/> to bind.
        /// Must not be <see langword="null"/>.
        /// </param>
        protected void SetViewModel(XPropertyPageViewModel vm)
        {
            viewModel = vm;
            if (xamlContent != null)
                xamlContent.DataContext = vm;
        }

        // =========================================================================================
        // Private Methods
        // =========================================================================================

        /// <summary>
        /// Merges <c>VsThemeDictionary.xaml</c> into the WPF content's merged resource
        /// dictionaries so that all standard WPF controls (Button, CheckBox, ComboBox,
        /// TextBox, Label, …) automatically adopt the active VS IDE theme.
        /// </summary>
        /// <param name="content">The WPF element into whose resources the dictionary is merged.</param>
        private static void InjectVsTheme(FrameworkElement content)
        {
            if (content == null)
                return;

            try
            {
                var dict = new ResourceDictionary
                {
                    Source = new Uri(
                        "pack://application:,,,/XSharp.AppDesigner2022;component/VsThemeDictionary.xaml",
                        UriKind.Absolute)
                };
                content.Resources.MergedDictionaries.Add(dict);
            }
            catch
            {
                // Non-fatal: theming will fall back to default WPF styles.
                // This can happen in design-time or unit-test hosts where the
                // pack:// URI scheme is not registered.
            }
        }
    }
}
