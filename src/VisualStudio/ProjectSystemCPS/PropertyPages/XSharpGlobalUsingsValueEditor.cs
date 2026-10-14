//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.Properties;
using Microsoft.VisualStudio.Shell;

using System;
using System.ComponentModel.Composition;
using System.Threading.Tasks;

namespace XSharp.ProjectSystem.Properties
{
    /// <summary>
    /// Launches the dialog that manages the <c>&lt;Using&gt;</c> items of an X# project.
    /// </summary>
    /// <remarks>
    /// Always returns <see langword="null"/> so that the project system does not persist
    /// a value for the owning property: the dialog writes the <c>&lt;Using&gt;</c> items
    /// itself.
    /// </remarks>
    [Export(typeof(Microsoft.VisualStudio.ProjectSystem.VS.PropertyPages.Designer.IPropertyEditor))]
    [ExportMetadata("Name", EditorName)]
    [AppliesTo(XSharpCapabilities.XSharpCps)]
    internal sealed class XSharpGlobalUsingsValueEditor : IPropertyPageUIValueEditor
    {
        internal const string EditorName = "XSharpGlobalUsingsEditor";

        private readonly UnconfiguredProject _unconfiguredProject;

        [ImportingConstructor]
        public XSharpGlobalUsingsValueEditor(UnconfiguredProject unconfiguredProject)
        {
            _unconfiguredProject = unconfiguredProject;
        }

        public async Task<string> EditValueAsync(
            IServiceProvider serviceProvider, IProperty ruleProperty, string currentValue)
        {
            var service = new GlobalUsingsService(_unconfiguredProject.FullPath);
            var usings = service.GetUsings();

            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

            var viewModel = new GlobalUsingsDialogViewModel(usings);
            var dialog = new GlobalUsingsDialog { DataContext = viewModel };

            if (dialog.ShowModal() == true)
            {
                service.SetUsings(viewModel.Usings);
            }

            // null => value unchanged, nothing written to the project file.
            return null;
        }
        public async Task<object> EditValueAsync(IServiceProvider serviceProvider, Microsoft.VisualStudio.ProjectSystem.Properties.IProperty ruleProperty, object currentValue)
        {
            var service = new GlobalUsingsService(_unconfiguredProject.FullPath);
            var usings = service.GetUsings();

            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

            var viewModel = new GlobalUsingsDialogViewModel(usings);
            var dialog = new GlobalUsingsDialog { DataContext = viewModel };

            if (dialog.ShowModal() == true)
            {
                service.SetUsings(viewModel.Usings);
            }

            // Returning the incoming value leaves the property untouched.
            return currentValue;
        }


    }
}
