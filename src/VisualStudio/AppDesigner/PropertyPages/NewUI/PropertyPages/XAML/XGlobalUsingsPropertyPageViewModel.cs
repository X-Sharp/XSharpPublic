//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.ObjectModel;
using System.Linq;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using XSharp.Settings;

namespace XSharp.Project
{
    /// <summary>
    /// MVVM ViewModel that backs the XAML/WPF Global Usings property page for
    /// SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Two sections are exposed:
    /// <list type="bullet">
    ///   <item><description>
    ///     <see cref="ImplicitUsings"/> — checkbox bound to the <c>ImplicitUsings</c> MSBuild
    ///     property (enabled / disabled).
    ///   </description></item>
    ///   <item><description>
    ///     <see cref="Usings"/> — an <see cref="ObservableCollection{T}"/> of
    ///     <see cref="UsingInfo"/> items populated from the <c>&lt;Using&gt;</c> MSBuild
    ///     project items.  Add/delete are performed immediately against the live project
    ///     (matching the WinForms behaviour).
    ///   </description></item>
    /// </list>
    /// </para>
    /// </remarks>
    internal sealed class XGlobalUsingsPropertyPageViewModel : XPropertyPageViewModel
    {
        // =========================================================================================
        // Backing fields
        // =========================================================================================

        private bool   _implicitUsings;
        private bool   _showImported = true;
        private string _newNamespace = string.Empty;
        private string _newAlias     = string.Empty;
        private bool   _newStatic;

        private bool _isBinding;
        private bool _isNotifying;

        private readonly ObservableCollection<UsingInfo> _usings = new ObservableCollection<UsingInfo>();

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XGlobalUsingsPropertyPageViewModel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The <see cref="XPropertyPage"/> that owns this panel.</param>
        public XGlobalUsingsPropertyPageViewModel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
        }

        // =========================================================================================
        // Internal access
        // =========================================================================================

        /// <summary>Exposes the owning property page for code-behind access.</summary>
        internal XPropertyPage ParentPage => parentPropertyPage;

        // =========================================================================================
        // Observable Properties — ImplicitUsings checkbox
        // =========================================================================================

        /// <summary>Gets or sets whether implicit global usings are enabled.</summary>
        public bool ImplicitUsings
        {
            get => _implicitUsings;
            set => SetProperty(ref _implicitUsings, value);
        }

        // =========================================================================================
        // Observable Properties — Using items grid
        // =========================================================================================

        /// <summary>Gets the collection of <c>&lt;Using&gt;</c> MSBuild items shown in the grid.</summary>
        public ObservableCollection<UsingInfo> Usings => _usings;

        /// <summary>Gets or sets whether imported (SDK-provided) usings are shown.</summary>
        public bool ShowImported
        {
            get => _showImported;
            set
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                SetProperty(ref _showImported, value);
                RefreshUsings();
            }
        }

        // =========================================================================================
        // Observable Properties — Add-using input fields
        // =========================================================================================

        /// <summary>Gets or sets the namespace text for the add-using input row.</summary>
        public string NewNamespace
        {
            get => _newNamespace;
            set => SetProperty(ref _newNamespace, value);
        }

        /// <summary>Gets or sets the alias text for the add-using input row.</summary>
        public string NewAlias
        {
            get => _newAlias;
            set => SetProperty(ref _newAlias, value);
        }

        /// <summary>Gets or sets whether the new using is static.</summary>
        public bool NewStatic
        {
            get => _newStatic;
            set => SetProperty(ref _newStatic, value);
        }

        // =========================================================================================
        // Label / tooltip strings
        // =========================================================================================

        /// <summary>Gets the label for the Implicit Usings group.</summary>
        public string CaptImplicitUsings       => GlobalUsingsPropertyPagePanel.captImplicitUsings;
        /// <summary>Gets the label for the ImplicitUsings checkbox.</summary>
        public string CaptEnableImplicitUsings => GlobalUsingsPropertyPagePanel.captEnableImplicitUsings;
        /// <summary>Gets the label for the Manage section.</summary>
        public string CaptManageUsings         => "Manage Implicit Globals";
        /// <summary>Gets the label for the Show Imported checkbox.</summary>
        public string CaptShowImported         => "Show imported";

        // =========================================================================================
        // Commands / Actions
        // =========================================================================================

        /// <summary>
        /// Adds a new <c>&lt;Using&gt;</c> MSBuild item to the project using
        /// <see cref="NewNamespace"/>, <see cref="NewAlias"/>, and <see cref="NewStatic"/>.
        /// Clears the input fields and refreshes the grid.
        /// </summary>
        public void AddUsing()
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            var ns = NewNamespace?.Trim();
            if (string.IsNullOrEmpty(ns))
                return;

            var project = parentPropertyPage.ProjectMgr as XProjectNode;
            if (project == null)
                return;

            var item = project.BuildProject.AddItem("Using", ns).FirstOrDefault();
            if (item != null)
            {
                if (!string.IsNullOrEmpty(NewAlias))
                    item.SetMetadataValue("Alias", NewAlias);
                if (NewStatic)
                    item.SetMetadataValue("Static", "true");
            }

            NewNamespace = string.Empty;
            NewAlias     = string.Empty;
            NewStatic    = false;

            // Re-evaluate so GetItems reflects the newly added item.
            parentPropertyPage.ProjectMgr?.BuildProject?.ReevaluateIfNecessary();
            RefreshUsings();
            NotifyDirty();
        }

        /// <summary>
        /// Removes the specified <see cref="UsingInfo"/> from the MSBuild project and
        /// refreshes the grid.
        /// </summary>
        /// <param name="item">The using item to delete.</param>
        public void DeleteUsing(UsingInfo item)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            if (item == null || item.Imported)
                return;

            var project = parentPropertyPage.ProjectMgr as XProjectNode;
            if (project == null)
                return;

            var buildItem = project.BuildProject.GetItems("Using")
                .FirstOrDefault(i => i.EvaluatedInclude == item.Namespace
                                     && !i.IsImported);
            if (buildItem != null)
            {
                project.BuildProject.RemoveItem(buildItem);
                project.BuildProject.MarkDirty();
                project.BuildProject.ReevaluateIfNecessary();
            }

            RefreshUsings();
            NotifyDirty();
        }

        // =========================================================================================
        // Private helpers
        // =========================================================================================

        private void RefreshUsings()
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            _usings.Clear();

            var project = parentPropertyPage.ProjectMgr as XProjectNode;
            if (project == null)
                return;

            foreach (var buildItem in project.BuildProject.GetItems("Using"))
            {
                if (buildItem.IsImported && !_showImported)
                    continue;

                _usings.Add(new UsingInfo
                {
                    Namespace = buildItem.EvaluatedInclude,
                    Alias     = buildItem.GetMetadataValue("Alias"),
                    Static    = string.Equals(buildItem.GetMetadataValue("Static"), "true", StringComparison.OrdinalIgnoreCase),
                    Imported  = buildItem.IsImported,
                });
            }
        }

        // =========================================================================================
        // IPropertyPagePanel — ViewModel lifecycle
        // =========================================================================================

        /// <inheritdoc/>
        public override void HookupEvents()
        {
            // Only ImplicitUsings triggers MSBuild write-through; Using items are live.
            PropertyChanged += (sender, e) =>
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                if (_isBinding || _isNotifying)
                    return;
                if (e.PropertyName == nameof(ImplicitUsings))
                {
                    NotifyDirty();
                    // Write the new value to the project file immediately so that
                    // ReevaluateIfNecessary picks up the SDK-provided <Using> items.
                    parentPropertyPage.SetProperty(
                        XSharpProjectFileConstants.ImplicitUsings,
                        _implicitUsings ? "enable" : "disable");
                    var proj = parentPropertyPage.ProjectMgr?.BuildProject;
                    proj?.MarkDirty();
                    proj?.ReevaluateIfNecessary();
                    RefreshUsings();
                    _isNotifying = true;
                    try   { OnPropertyChanged("Item[]"); }
                    finally { _isNotifying = false; }
                }
            };
        }

        /// <inheritdoc/>
        public override void BindProperties()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            _isBinding = true;
            try
            {
                var raw = parentPropertyPage.GetProperty(XSharpProjectFileConstants.ImplicitUsings) ?? string.Empty;
                ImplicitUsings = string.Equals(raw, "enable", StringComparison.OrdinalIgnoreCase)
                                 || string.Equals(raw, "true",   StringComparison.OrdinalIgnoreCase);

                RefreshUsings();
                isDirty = false;
            }
            finally
            {
                OnPropertyChanged(null);
                try   { OnPropertyChanged("Item[]"); }
                finally { _isBinding = false; }
            }
        }

        /// <inheritdoc/>
        protected override void ApplyChangesCore()
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            parentPropertyPage.SetProperty(
                XSharpProjectFileConstants.ImplicitUsings,
                ImplicitUsings ? "enable" : "disable");

            // Using items are already written live via AddUsing / DeleteUsing.
            isDirty = false;
        }
    }
}
