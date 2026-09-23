//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Project;
using System.Windows;
using System.Windows.Controls;

namespace XSharp.Project
{
    /// <summary>
    /// WPF <c>UserControl</c> that provides the XAML UI for the Build Events property page.
    /// Used for SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The <c>DataContext</c> is an <see cref="XBuildEventsPropertyPageViewModel"/> instance,
    /// set by the hosting <see cref="XBuildEventsPropertyPageXamlHost"/> via
    /// <see cref="XPropertyPageXamlHost.SetViewModel"/>.
    /// </para>
    /// <para>
    /// All property binding is declared in <c>XBuildEventsPropertyPage.xaml</c>.  This
    /// code-behind only handles the two <em>Edit…</em> button clicks, which open
    /// <see cref="XSharpBuildEventEditorDialog"/> — matching the WinForms
    /// <c>XBuildEventEditor.btnEdit_Click</c> behaviour.
    /// </para>
    /// <para>
    /// The class name is <c>XBuildEventsPropertyPageView</c> (not <c>XBuildEventsPropertyPage</c>)
    /// to avoid a naming collision with the COM page class
    /// <see cref="XSharpBuildEventsPropertyPage"/>.
    /// </para>
    /// </remarks>
    public partial class XBuildEventsPropertyPageView : UserControl
    {
        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPageView"/> class.
        /// </summary>
        public XBuildEventsPropertyPageView()
        {
            InitializeComponent();
        }

        // =========================================================================================
        // Event Handlers — Edit buttons
        // =========================================================================================

        /// <summary>
        /// Handles the <em>Edit Pre-build…</em> button click.
        /// Opens <see cref="XBuildEventEditorForm"/> pre-populated with the current
        /// <see cref="XBuildEventsPropertyPageViewModel.PreBuildEvent"/> value.
        /// If the user confirms, the ViewModel property is updated.
        /// </summary>
        private void OnEditPreBuildClick(object sender, RoutedEventArgs e)
        {
            var vm = DataContext as XBuildEventsPropertyPageViewModel;
            if (vm == null) return;

            var project = vm.ParentPage.ProjectMgr as ProjectNode;

            var dlg = new XSharpBuildEventEditorDialog { Title = "Pre-build Event Command Line" };
            if (project != null)
                dlg.InitializeMacroList(new XBuildMacroCollection(project));

            dlg.EditorText = vm.PreBuildEvent ?? string.Empty;

            if (dlg.ShowDialog() == true)
            {
                vm.PreBuildEvent = dlg.EditorText;
            }
        }

        /// <summary>
        /// Handles the <em>Edit Post-build…</em> button click.
        /// Opens <see cref="XBuildEventEditorForm"/> pre-populated with the current
        /// <see cref="XBuildEventsPropertyPageViewModel.PostBuildEvent"/> value.
        /// If the user confirms, the ViewModel property is updated.
        /// </summary>
        private void OnEditPostBuildClick(object sender, RoutedEventArgs e)
        {
            var vm = DataContext as XBuildEventsPropertyPageViewModel;
            if (vm == null) return;

            var project = vm.ParentPage.ProjectMgr as ProjectNode;

            var dlg = new XSharpBuildEventEditorDialog { Title = "Post-build Event Command Line" };
            if (project != null)
                dlg.InitializeMacroList(new XBuildMacroCollection(project));

            dlg.EditorText = vm.PostBuildEvent ?? string.Empty;

            if (dlg.ShowDialog() == true)
            {
                vm.PostBuildEvent = dlg.EditorText;
            }
        }
    }
}
