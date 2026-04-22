//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Shell;
using System.Windows;
using System.Windows.Controls;

namespace XSharp.Project
{
    /// <summary>
    /// WPF <c>UserControl</c> that provides the XAML UI for the Global Usings property page.
    /// Used for SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The <c>DataContext</c> is an <see cref="XGlobalUsingsPropertyPageViewModel"/> instance,
    /// set by the hosting <see cref="XGlobalUsingsPropertyPageXamlHost"/> via
    /// <see cref="XPropertyPageXamlHost.SetViewModel"/>.
    /// </para>
    /// <para>
    /// All property binding is declared in <c>XGlobalUsingsPropertyPage.xaml</c>.  This
    /// code-behind handles the Add / Delete button clicks for the
    /// <c>&lt;Using&gt;</c> MSBuild item grid.
    /// </para>
    /// <para>
    /// The class name is <c>XGlobalUsingsPropertyPageView</c> (not <c>XGlobalUsingsPropertyPage</c>)
    /// to avoid a naming collision with the COM page class
    /// <see cref="XSharpGlobalUsingsPropertiesPage"/>.
    /// </para>
    /// </remarks>
    public partial class XGlobalUsingsPropertyPageView : UserControl
    {
        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XGlobalUsingsPropertyPageView"/> class.
        /// </summary>
        public XGlobalUsingsPropertyPageView()
        {
            InitializeComponent();
        }

        // =========================================================================================
        // Event Handlers
        // =========================================================================================

        /// <summary>
        /// Handles the <em>Add Using</em> button click.
        /// Delegates to <see cref="XGlobalUsingsPropertyPageViewModel.AddUsing"/>.
        /// </summary>
        private void OnAddUsingClick(object sender, RoutedEventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            var vm = DataContext as XGlobalUsingsPropertyPageViewModel;
            vm?.AddUsing();
        }

        /// <summary>
        /// Handles the <em>Delete</em> button click inside a DataGrid row.
        /// The button's <c>Tag</c> is the <see cref="UsingInfo"/> bound to the row.
        /// Delegates to <see cref="XGlobalUsingsPropertyPageViewModel.DeleteUsing"/>.
        /// </summary>
        private void OnDeleteUsingClick(object sender, RoutedEventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            var vm = DataContext as XGlobalUsingsPropertyPageViewModel;
            if (vm == null) return;

            var btn  = sender as Button;
            var item = btn?.Tag as UsingInfo;
            vm.DeleteUsing(item);
        }
    }
}
