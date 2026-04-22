//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using System.Windows;
using System.Windows.Controls;
using XSharp.Settings;

namespace XSharp.Project
{
    /// <summary>
    /// WPF <c>UserControl</c> that provides the XAML UI for the Debug property page.
    /// Used for SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The <c>DataContext</c> is an <see cref="XDebugPropertyPageViewModel"/> instance,
    /// set by the hosting <see cref="XDebugPropertyPageXamlHost"/> via
    /// <see cref="XPropertyPageXamlHost.SetViewModel"/>.
    /// </para>
    /// <para>
    /// All property binding is declared in <c>XDebugPropertyPage.xaml</c>.  This
    /// code-behind only handles the two <em>Browse…</em> button clicks:
    /// <list type="bullet">
    ///   <item><description>
    ///     <see cref="OnBrowseCommandClick"/> — opens <see cref="XSharpSLEPropertyForm"/>
    ///     for the debugger command field, matching the WinForms <c>btnCommand_Click</c>.
    ///   </description></item>
    ///   <item><description>
    ///     <see cref="OnBrowseWorkingDirectoryClick"/> — opens <see cref="XSharpSLEPropertyForm"/>
    ///     for the working directory field, matching <c>btnDebuggerWorkingDirectory_Click</c>.
    ///   </description></item>
    /// </list>
    /// </para>
    /// <para>
    /// The class name is <c>XDebugPropertyPageView</c> (not <c>XDebugPropertyPage</c>)
    /// to avoid a naming collision with the COM page class
    /// <see cref="XSharpDebugPropertyPage"/>.
    /// </para>
    /// </remarks>
    public partial class XDebugPropertyPageView : UserControl
    {
        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XDebugPropertyPageView"/> class.
        /// </summary>
        public XDebugPropertyPageView()
        {
            InitializeComponent();
        }

        // =========================================================================================
        // Event Handlers — Browse buttons
        // =========================================================================================

        /// <summary>
        /// Handles the <em>Browse</em> (…) button click for the Debugger Command field.
        /// Opens <see cref="XSharpSLEPropertyForm"/> (macro/string editor) pre-populated
        /// with the build macros and the current value.
        /// If the user confirms, the ViewModel's <see cref="XDebugPropertyPageViewModel.DebuggerCommand"/>
        /// property is updated and the change is saved to the project.
        /// </summary>
        /// <remarks>
        /// Replicates WinForms <c>btnCommand_Click</c> → <c>showMacroDialog</c>.
        /// </remarks>
        private void OnBrowseCommandClick(object sender, RoutedEventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            var vm = DataContext as XDebugPropertyPageViewModel;
            if (vm == null) return;

            var project = vm.ParentPage.ProjectMgr as ProjectNode;
            if (project == null) return;

            using (var form = new XSharpSLEPropertyForm())
            {
                var mc = new XBuildMacroCollection(project);
                form.SetMacros(mc);
                form.PropertyText.Text = vm.DebuggerCommand ?? string.Empty;
                form.Text = DebugPropertyPagePanel.descDebuggerCommand;

                if (form.ShowDialog() == System.Windows.Forms.DialogResult.OK)
                {
                    vm.DebuggerCommand = form.PropertyText.Text;
                    vm.ParentPage.SetProperty(XSharpProjectFileConstants.DebuggerCommand, vm.DebuggerCommand);
                }
            }
        }

        /// <summary>
        /// Handles the <em>Browse</em> (…) button click for the Working Directory field.
        /// Opens <see cref="XSharpSLEPropertyForm"/> (macro/string editor) pre-populated
        /// with the build macros and the current value.
        /// If the user confirms, the ViewModel's
        /// <see cref="XDebugPropertyPageViewModel.DebuggerWorkingDirectory"/> property is updated.
        /// </summary>
        /// <remarks>
        /// Replicates WinForms <c>btnDebuggerWorkingDirectory_Click</c> → <c>showMacroDialog</c>.
        /// </remarks>
        private void OnBrowseWorkingDirectoryClick(object sender, RoutedEventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            var vm = DataContext as XDebugPropertyPageViewModel;
            if (vm == null) return;

            var project = vm.ParentPage.ProjectMgr as ProjectNode;
            if (project == null) return;

            using (var form = new XSharpSLEPropertyForm())
            {
                var mc = new XBuildMacroCollection(project);
                form.SetMacros(mc);
                form.PropertyText.Text = vm.DebuggerWorkingDirectory ?? string.Empty;
                form.Text = DebugPropertyPagePanel.descDebuggerWorkingDirectory;

                if (form.ShowDialog() == System.Windows.Forms.DialogResult.OK)
                {
                    vm.DebuggerWorkingDirectory = form.PropertyText.Text;
                    vm.ParentPage.SetProperty(XSharpProjectFileConstants.DebuggerWorkingDirectory, vm.DebuggerWorkingDirectory);
                }
            }
        }
    }
}
