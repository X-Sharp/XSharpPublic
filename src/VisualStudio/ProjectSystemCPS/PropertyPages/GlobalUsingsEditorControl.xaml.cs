//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Runtime.InteropServices;
using System.Windows;
using System.Windows.Controls;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

namespace XSharp.ProjectSystem.Properties
{
    /// <summary>
    /// Hosts the button that opens the Global Usings dialog from the property page.
    /// </summary>
    public partial class GlobalUsingsEditorControl : UserControl
    {
        public GlobalUsingsEditorControl()
        {
            InitializeComponent();
        }

        private void OnManageClick(object sender, RoutedEventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            var projectPath = GetSelectedProjectPath();
            if (string.IsNullOrEmpty(projectPath))
                return;

            var service = new GlobalUsingsService(projectPath);
            var viewModel = new GlobalUsingsDialogViewModel(service.GetUsings());
            var dialog = new GlobalUsingsDialog { DataContext = viewModel };

            if (dialog.ShowModal() == true)
                service.SetUsings(viewModel.Usings);
        }

        /// <summary>
        /// Returns the full path of the project selected in the shell, which is the
        /// project whose property page hosts this control.
        /// </summary>
        private static string GetSelectedProjectPath()
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            if (!(Package.GetGlobalService(typeof(SVsShellMonitorSelection)) is IVsMonitorSelection monitorSelection))
                return null;

            IntPtr hierarchyPtr = IntPtr.Zero;
            IntPtr containerPtr = IntPtr.Zero;
            try
            {
                int hr = monitorSelection.GetCurrentSelection(
                    out hierarchyPtr, out uint _, out IVsMultiItemSelect _, out containerPtr);

                if (ErrorHandler.Failed(hr) || hierarchyPtr == IntPtr.Zero)
                    return null;

                if (!(Marshal.GetObjectForIUnknown(hierarchyPtr) is IVsHierarchy hierarchy))
                    return null;

                if (hierarchy is IVsProject project &&
                    ErrorHandler.Succeeded(project.GetMkDocument(VSConstants.VSITEMID_ROOT, out string path)))
                {
                    return path;
                }

                return null;
            }
            finally
            {
                if (hierarchyPtr != IntPtr.Zero)
                    Marshal.Release(hierarchyPtr);
                if (containerPtr != IntPtr.Zero)
                    Marshal.Release(containerPtr);
            }
        }
    }
}
