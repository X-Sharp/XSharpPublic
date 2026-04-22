//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Generic;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using XSharp.Settings;

namespace XSharp.Project
{
    /// <summary>
    /// MVVM ViewModel that backs the XAML/WPF Debug property page for
    /// SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Each MSBuild property shown on the Debug page is exposed as an observable
    /// property backed by <see cref="XPropertyPageViewModel.SetProperty{T}"/> so that
    /// WPF data binding automatically triggers
    /// <see cref="System.ComponentModel.INotifyPropertyChanged"/> on every change.
    /// </para>
    /// <para>
    /// <see cref="DebuggerCommand"/> and <see cref="DebuggerWorkingDirectory"/> are
    /// editable TextBoxes whose <em>Browse (…)</em> buttons open
    /// <see cref="XSharpSLEPropertyForm"/> (macro/string editor) — matching the WinForms
    /// <c>btnCommand_Click</c> and <c>btnDebuggerWorkingDirectory_Click</c> handlers.
    /// </para>
    /// </remarks>
    internal sealed class XDebugPropertyPageViewModel : XPropertyPageViewModel
    {
        // =========================================================================================
        // Backing fields — string properties
        // =========================================================================================

        private string _debuggerCommand            = string.Empty;
        private string _debuggerCommandArguments   = string.Empty;
        private string _debugType                  = string.Empty;
        private string _debuggerWorkingDirectory   = string.Empty;

        // =========================================================================================
        // Backing fields — bool checkboxes
        // =========================================================================================

        private bool _enableUnmanagedDebugging;

        // =========================================================================================
        // Backing fields — combo items (debug type)
        // =========================================================================================

        private List<string> _debugTypeValues = new List<string>();

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XDebugPropertyPageViewModel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The <see cref="XPropertyPage"/> that owns this panel.</param>
        public XDebugPropertyPageViewModel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            // Populate debug type combo values from the same converter the WinForms panel uses.
            var conv = new DebugTypeConverter();
            foreach (var v in conv.GetStandardValues())
                _debugTypeValues.Add(v?.ToString() ?? string.Empty);
        }

        // =========================================================================================
        // Internal access — for code-behind browse handlers
        // =========================================================================================

        /// <summary>
        /// Exposes the owning property page so that the code-behind browse button handlers
        /// can access <c>ProjectMgr</c> and call <c>SetProperty</c>.
        /// </summary>
        internal XPropertyPage ParentPage => parentPropertyPage;

        // =========================================================================================
        // Observable Properties — strings
        // =========================================================================================

        /// <summary>Gets or sets the debugger executable path.</summary>
        public string DebuggerCommand
        {
            get => _debuggerCommand;
            set => SetProperty(ref _debuggerCommand, value);
        }

        /// <summary>Gets or sets the command-line arguments passed to the debugger.</summary>
        public string DebuggerCommandArguments
        {
            get => _debuggerCommandArguments;
            set => SetProperty(ref _debuggerCommandArguments, value);
        }

        /// <summary>Gets or sets the debug information type (e.g. <c>"full"</c>, <c>"pdbonly"</c>, <c>"none"</c>).</summary>
        public string DebugType
        {
            get => _debugType;
            set => SetProperty(ref _debugType, value);
        }

        /// <summary>Gets or sets the debugger working directory.</summary>
        public string DebuggerWorkingDirectory
        {
            get => _debuggerWorkingDirectory;
            set => SetProperty(ref _debuggerWorkingDirectory, value);
        }

        // =========================================================================================
        // Observable Properties — checkboxes
        // =========================================================================================

        /// <summary>Gets or sets whether native/unmanaged code debugging is enabled.</summary>
        public bool EnableUnmanagedDebugging
        {
            get => _enableUnmanagedDebugging;
            set => SetProperty(ref _enableUnmanagedDebugging, value);
        }

        // =========================================================================================
        // Observable Properties — combo items
        // =========================================================================================

        /// <summary>Gets the list of debug type values for the combo box.</summary>
        public List<string> DebugTypeValues => _debugTypeValues;

        // =========================================================================================
        // String properties — labels and tooltips from DebugPropertyPagePanel
        // =========================================================================================

        /// <summary>Gets the localized label for the Debugger Command field.</summary>
        public string CaptDebuggerCommand            => DebugPropertyPagePanel.captDebuggerCommand;
        /// <summary>Gets the localized label for the Debugger Arguments field.</summary>
        public string CaptDebuggerCommandArguments   => DebugPropertyPagePanel.captDebuggerCommandArguments;
        /// <summary>Gets the localized label for the Debug Type combo.</summary>
        public string CaptDebugType                  => DebugPropertyPagePanel.captDebugType;
        /// <summary>Gets the localized label for the Working Directory field.</summary>
        public string CaptDebuggerWorkingDirectory   => DebugPropertyPagePanel.captDebuggerWorkingDirectory;
        /// <summary>Gets the localized label for the Unmanaged Debugging checkbox.</summary>
        public string CaptEnableUnmanagedDebugging   => DebugPropertyPagePanel.captEnableUnmanagedDebugging;

        /// <summary>Gets the localized tooltip for the Debugger Command field.</summary>
        public string DescDebuggerCommand            => DebugPropertyPagePanel.descDebuggerCommand;
        /// <summary>Gets the localized tooltip for the Debugger Arguments field.</summary>
        public string DescDebuggerCommandArguments   => DebugPropertyPagePanel.descDebuggerCommandArguments;
        /// <summary>Gets the localized tooltip for the Debug Type combo.</summary>
        public string DescDebugType                  => DebugPropertyPagePanel.descDebugType;
        /// <summary>Gets the localized tooltip for the Working Directory field.</summary>
        public string DescDebuggerWorkingDirectory   => DebugPropertyPagePanel.descDebuggerWorkingDirectory;
        /// <summary>Gets the localized tooltip for the Unmanaged Debugging checkbox.</summary>
        public string DescEnableUnmanagedDebugging   => DebugPropertyPagePanel.descEnableUnmanagedDebugging;

        // =========================================================================================
        // IPropertyPagePanel — ViewModel lifecycle
        // =========================================================================================

        /// <inheritdoc/>
        public override void HookupEvents()
        {
            PropertyChanged += (sender, e) => NotifyDirty();
        }

        /// <inheritdoc/>
        public override void BindProperties()
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            DebuggerCommand          = parentPropertyPage.GetProperty(XSharpProjectFileConstants.DebuggerCommand)          ?? string.Empty;
            DebuggerCommandArguments = parentPropertyPage.GetProperty(XSharpProjectFileConstants.DebuggerCommandArguments) ?? string.Empty;
            DebugType                = parentPropertyPage.GetProperty(XSharpProjectFileConstants.DebugType)                ?? string.Empty;
            DebuggerWorkingDirectory = parentPropertyPage.GetProperty(XSharpProjectFileConstants.DebuggerWorkingDirectory) ?? string.Empty;
            EnableUnmanagedDebugging = GetBoolPropertyValue(XSharpProjectFileConstants.EnableUnmanagedDebugging);

            isDirty = false;
        }

        /// <inheritdoc/>
        public override void ApplyChanges()
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            parentPropertyPage.SetProperty(XSharpProjectFileConstants.DebuggerCommand,          DebuggerCommand          ?? string.Empty);
            parentPropertyPage.SetProperty(XSharpProjectFileConstants.DebuggerCommandArguments, DebuggerCommandArguments ?? string.Empty);
            parentPropertyPage.SetProperty(XSharpProjectFileConstants.DebugType,                DebugType                ?? string.Empty);
            parentPropertyPage.SetProperty(XSharpProjectFileConstants.DebuggerWorkingDirectory, DebuggerWorkingDirectory ?? string.Empty);
            SetBoolPropertyValue(XSharpProjectFileConstants.EnableUnmanagedDebugging,           EnableUnmanagedDebugging);

            isDirty = false;
        }
    }
}
