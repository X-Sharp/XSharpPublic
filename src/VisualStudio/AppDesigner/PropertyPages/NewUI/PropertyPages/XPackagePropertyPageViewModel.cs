//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Project;

namespace XSharp.Project
{
    /// <summary>
    /// MVVM ViewModel that backs the XAML/WPF Package property page for
    /// SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// Two groups of properties are exposed:
    /// <list type="bullet">
    ///   <item><description>
    ///     Assembly information: <see cref="AssemblyTitle"/>, <see cref="Description"/>,
    ///     <see cref="Company"/>, <see cref="Copyright"/>, <see cref="NeutralLanguage"/>.
    ///   </description></item>
    ///   <item><description>
    ///     NuGet package metadata: <see cref="PackageId"/>, <see cref="PackageVersion"/>,
    ///     <see cref="Authors"/>, <see cref="PackageTags"/>,
    ///     <see cref="PackageLicenseExpression"/>, <see cref="PackageProjectUrl"/>,
    ///     <see cref="RepositoryUrl"/>, <see cref="RepositoryType"/>,
    ///     <see cref="GeneratePackageOnBuild"/>, <see cref="IsPackable"/>.
    ///   </description></item>
    /// </list>
    /// </remarks>
    internal sealed class XPackagePropertyPageViewModel : XPropertyPageViewModel
    {
        // =========================================================================================
        // Backing fields — Assembly info
        // =========================================================================================

        private string _assemblyTitle         = string.Empty;
        private string _description           = string.Empty;
        private string _company               = string.Empty;
        private string _copyright             = string.Empty;
        private string _neutralLanguage       = string.Empty;

        // =========================================================================================
        // Backing fields — NuGet package metadata
        // =========================================================================================

        private string _packageId             = string.Empty;
        private string _packageVersion        = string.Empty;
        private string _authors               = string.Empty;
        private string _packageTags           = string.Empty;
        private string _packageLicenseExpression = string.Empty;
        private string _packageProjectUrl     = string.Empty;
        private string _repositoryUrl         = string.Empty;
        private string _repositoryType        = string.Empty;
        private bool   _generatePackageOnBuild;
        private bool   _isPackable = true;

        // =========================================================================================
        // Backing fields — write-through guard
        // =========================================================================================

        /// <summary>Prevents re-entrant ApplyChanges calls during BindProperties or post-apply refresh.</summary>
        private bool _isBinding;

        /// <summary>Suppresses ApplyChanges when firing PropertyChanged solely to refresh the UI converters.</summary>
        private bool _isNotifying;

        public XPackagePropertyPageViewModel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
        }

        // =========================================================================================
        // Observable Properties — Assembly info
        // =========================================================================================

        /// <summary>Gets or sets the assembly title (<c>AssemblyTitle</c> MSBuild property).</summary>
        public string AssemblyTitle
        {
            get => _assemblyTitle;
            set => SetProperty(ref _assemblyTitle, value);
        }

        /// <summary>Gets or sets the assembly/package description (<c>Description</c>).</summary>
        public string Description
        {
            get => _description;
            set => SetProperty(ref _description, value);
        }

        /// <summary>Gets or sets the company name (<c>Company</c>).</summary>
        public string Company
        {
            get => _company;
            set => SetProperty(ref _company, value);
        }

        /// <summary>Gets or sets the copyright string (<c>Copyright</c>).</summary>
        public string Copyright
        {
            get => _copyright;
            set => SetProperty(ref _copyright, value);
        }

        /// <summary>Gets or sets the assembly neutral language (<c>NeutralLanguage</c>).</summary>
        public string NeutralLanguage
        {
            get => _neutralLanguage;
            set => SetProperty(ref _neutralLanguage, value);
        }

        // =========================================================================================
        // Observable Properties — NuGet package metadata
        // =========================================================================================

        /// <summary>Gets or sets the NuGet package identifier (<c>PackageId</c>).</summary>
        public string PackageId
        {
            get => _packageId;
            set => SetProperty(ref _packageId, value);
        }

        /// <summary>Gets or sets the NuGet package version (<c>PackageVersion</c>).</summary>
        public string PackageVersion
        {
            get => _packageVersion;
            set => SetProperty(ref _packageVersion, value);
        }

        /// <summary>Gets or sets the semicolon-separated list of package authors (<c>Authors</c>).</summary>
        public string Authors
        {
            get => _authors;
            set => SetProperty(ref _authors, value);
        }

        /// <summary>Gets or sets the semicolon-delimited package tags (<c>PackageTags</c>).</summary>
        public string PackageTags
        {
            get => _packageTags;
            set => SetProperty(ref _packageTags, value);
        }

        /// <summary>Gets or sets the SPDX license expression (<c>PackageLicenseExpression</c>).</summary>
        public string PackageLicenseExpression
        {
            get => _packageLicenseExpression;
            set => SetProperty(ref _packageLicenseExpression, value);
        }

        /// <summary>Gets or sets the project URL (<c>PackageProjectUrl</c>).</summary>
        public string PackageProjectUrl
        {
            get => _packageProjectUrl;
            set => SetProperty(ref _packageProjectUrl, value);
        }

        /// <summary>Gets or sets the source repository URL (<c>RepositoryUrl</c>).</summary>
        public string RepositoryUrl
        {
            get => _repositoryUrl;
            set => SetProperty(ref _repositoryUrl, value);
        }

        /// <summary>Gets or sets the repository type, e.g. "git" (<c>RepositoryType</c>).</summary>
        public string RepositoryType
        {
            get => _repositoryType;
            set => SetProperty(ref _repositoryType, value);
        }

        /// <summary>Gets or sets whether the package is generated on every build (<c>GeneratePackageOnBuild</c>).</summary>
        public bool GeneratePackageOnBuild
        {
            get => _generatePackageOnBuild;
            set => SetProperty(ref _generatePackageOnBuild, value);
        }

        /// <summary>Gets or sets whether this project is packable (<c>IsPackable</c>).</summary>
        public bool IsPackable
        {
            get => _isPackable;
            set => SetProperty(ref _isPackable, value);
        }

        // =========================================================================================
        // IPropertyPagePanel — ViewModel lifecycle
        // =========================================================================================

        /// <inheritdoc/>
        public override void HookupEvents()
        {
            PropertyChanged += (sender, e) =>
            {
                if (_isBinding || _isNotifying)
                    return;
                ThreadHelper.ThrowIfNotOnUIThread();
                NotifyDirty();
                // Fire OnPropertyChanged("Item[]") to force WPF to re-evaluate all indexer
                // bindings ({Binding [PropertyName]}) which control Reset button enabled state.
                // The _isNotifying guard prevents this notification from re-entering ApplyChanges.
                _isNotifying = true;
                try   { OnPropertyChanged("Item[]"); }
                finally { _isNotifying = false; }
            };
        }

        /// <inheritdoc/>
        public override void BindProperties()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            _isBinding = true;
            try
            {
                // Assembly info
                AssemblyTitle   = parentPropertyPage.GetProperty(XSharpProjectFileConstants.AssemblyTitle)   ?? string.Empty;
                Description     = parentPropertyPage.GetProperty(XSharpProjectFileConstants.Description)     ?? string.Empty;
                Company         = parentPropertyPage.GetProperty(XSharpProjectFileConstants.Company)         ?? string.Empty;
                Copyright       = parentPropertyPage.GetProperty(XSharpProjectFileConstants.Copyright)       ?? string.Empty;
                NeutralLanguage = parentPropertyPage.GetProperty(XSharpProjectFileConstants.NeutralLanguage) ?? string.Empty;

                // NuGet package metadata
                PackageId               = parentPropertyPage.GetProperty(XSharpProjectFileConstants.PackageId)               ?? string.Empty;
                PackageVersion          = parentPropertyPage.GetProperty(XSharpProjectFileConstants.PackageVersion)          ?? string.Empty;
                Authors                 = parentPropertyPage.GetProperty(XSharpProjectFileConstants.Authors)                 ?? string.Empty;
                PackageTags             = parentPropertyPage.GetProperty(XSharpProjectFileConstants.PackageTags)             ?? string.Empty;
                PackageLicenseExpression= parentPropertyPage.GetProperty(XSharpProjectFileConstants.PackageLicenseExpression)?? string.Empty;
                PackageProjectUrl       = parentPropertyPage.GetProperty(XSharpProjectFileConstants.PackageProjectUrl)       ?? string.Empty;
                RepositoryUrl           = parentPropertyPage.GetProperty(XSharpProjectFileConstants.RepositoryUrl)           ?? string.Empty;
                RepositoryType          = parentPropertyPage.GetProperty(XSharpProjectFileConstants.RepositoryType)          ?? string.Empty;
                GeneratePackageOnBuild  = GetBoolPropertyValue(XSharpProjectFileConstants.GeneratePackageOnBuild);

                // IsPackable defaults to true when absent
                var isPackableRaw = parentPropertyPage.GetProperty(XSharpProjectFileConstants.IsPackable);
                IsPackable = string.IsNullOrEmpty(isPackableRaw)
                             || string.Equals(isPackableRaw, "true", System.StringComparison.OrdinalIgnoreCase);

                isDirty = false;
            }
            finally
            {
                // Fire OnPropertyChanged(null) while _isBinding=true to force WPF to re-read all
                // bound values even when a backing field did not change (e.g. after Reset).
                OnPropertyChanged(null);
                // Fire OnPropertyChanged("Item[]") while still inside _isBinding=true
                // so that indexer bindings ({Binding [PropertyName]}) are re-evaluated
                // (re-enabling/disabling Reset buttons) but the HookupEvents handler ignores it.
                try   { OnPropertyChanged("Item[]"); }
                finally { _isBinding = false; }
            }
        }

        /// <inheritdoc/>
        protected override void ApplyChangesCore()
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            // Assembly info
            SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.AssemblyTitle,   AssemblyTitle);
            SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.Description,     Description);
            SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.Company,         Company);
            SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.Copyright,       Copyright);
            SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.NeutralLanguage, NeutralLanguage);

            // NuGet package metadata
            SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.PackageId,                PackageId);
            SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.PackageVersion,           PackageVersion);
            SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.Authors,                  Authors);
            SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.PackageTags,              PackageTags);
            SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.PackageLicenseExpression, PackageLicenseExpression);
            SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.PackageProjectUrl,        PackageProjectUrl);
            SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.RepositoryUrl,            RepositoryUrl);
            SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.RepositoryType,           RepositoryType);
            SetBoolPropertyValue(XSharpProjectFileConstants.GeneratePackageOnBuild, GeneratePackageOnBuild);
            SetBoolPropertyValue(XSharpProjectFileConstants.IsPackable,             IsPackable);

            isDirty = false;
        }
    }
}
