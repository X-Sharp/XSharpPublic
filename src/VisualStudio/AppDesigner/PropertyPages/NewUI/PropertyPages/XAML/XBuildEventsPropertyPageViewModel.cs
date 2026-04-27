//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Generic;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;

namespace XSharp.Project
{
    /// <summary>
    /// MVVM ViewModel that backs the XAML/WPF Build Events property page for
    /// SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Three MSBuild properties are exposed:
    /// <list type="bullet">
    ///   <item><description>
    ///     <see cref="PreBuildEvent"/> — free-text command line; optional "Edit…" button
    ///     opens <see cref="XBuildEventEditorForm"/>.
    ///   </description></item>
    ///   <item><description>
    ///     <see cref="PostBuildEvent"/> — same as above for the post-build event.
    ///   </description></item>
    ///   <item><description>
    ///     <see cref="RunPostBuildEvent"/> — ComboBox with three human-readable values.
    ///     The raw↔display conversion is performed by
    ///     <see cref="XSharpBuildEventsPropertyPage.GetProperty"/> /
    ///     <see cref="XSharpBuildEventsPropertyPage.SetProperty"/>; this ViewModel simply
    ///     passes the display string through.
    ///   </description></item>
    /// </list>
    /// </para>
    /// </remarks>
    internal sealed class XBuildEventsPropertyPageViewModel : XPropertyPageViewModel
    {
        // =========================================================================================
        // Backing fields
        // =========================================================================================

        private string _preBuildEvent  = string.Empty;
        private string _postBuildEvent = string.Empty;
        private string _runPostBuildEvent = string.Empty;

        private readonly List<string> _runPostBuildEventValues;

        private bool _isBinding   = false;
        private bool _isNotifying = false;

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPageViewModel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The <see cref="XPropertyPage"/> that owns this panel.</param>
        /// <param name="runPostBuildNames">
        /// The three human-readable display strings for <c>RunPostBuildEvent</c>,
        /// taken from <see cref="XSharpBuildEventsPropertyPage"/>.
        /// </param>
        public XBuildEventsPropertyPageViewModel(XPropertyPage parentPropertyPage, string[] runPostBuildNames)
            : base(parentPropertyPage)
        {
            _runPostBuildEventValues = new List<string>(runPostBuildNames);
        }

        // =========================================================================================
        // Internal access — for code-behind "Edit…" button handlers
        // =========================================================================================

        /// <summary>
        /// Exposes the owning property page so that code-behind can access
        /// <c>ProjectMgr</c> and call <c>SetProperty</c>.
        /// </summary>
        internal XPropertyPage ParentPage => parentPropertyPage;

        // =========================================================================================
        // Observable Properties
        // =========================================================================================

        /// <summary>Gets or sets the pre-build event command line.</summary>
        public string PreBuildEvent
        {
            get => _preBuildEvent;
            set => SetProperty(ref _preBuildEvent, value);
        }

        /// <summary>Gets or sets the post-build event command line.</summary>
        public string PostBuildEvent
        {
            get => _postBuildEvent;
            set => SetProperty(ref _postBuildEvent, value);
        }

        /// <summary>
        /// Gets or sets the selected display string for when the post-build event runs.
        /// </summary>
        public string RunPostBuildEvent
        {
            get => _runPostBuildEvent;
            set => SetProperty(ref _runPostBuildEvent, value);
        }

        /// <summary>Gets the list of display values for the RunPostBuildEvent combo box.</summary>
        public List<string> RunPostBuildEventValues => _runPostBuildEventValues;

        // =========================================================================================
        // Label / tooltip strings (inline — Strings.prg constants are static readonly, not const)
        // =========================================================================================

        /// <summary>Gets the label for the Pre-build Event group.</summary>
        public string CaptPreBuildEvent        => "Pre-build Event Command Line";
        /// <summary>Gets the label for the Post-build Event group.</summary>
        public string CaptPostBuildEvent       => "Post-build Event Command Line";
        /// <summary>Gets the label for the Run post-build combo.</summary>
        public string CaptRunPostBuildEvent    => "Run the post-build event:";

        /// <summary>Gets the button text for the Pre-build Edit button.</summary>
        public string CaptEditPreBuild         => "Edit Pre-build...";
        /// <summary>Gets the button text for the Post-build Edit button.</summary>
        public string CaptEditPostBuild        => "Edit Post-build...";

        // =========================================================================================
        // IPropertyPagePanel — ViewModel lifecycle
        // =========================================================================================

        /// <inheritdoc/>
        public override void HookupEvents()
        {
            PropertyChanged += (sender, e) =>
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                if (_isBinding || _isNotifying)
                    return;

                ApplyChanges();
                NotifyDirty();

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
                PreBuildEvent     = parentPropertyPage.GetProperty(XSharpProjectFileConstants.PreBuildEvent)     ?? string.Empty;
                PostBuildEvent    = parentPropertyPage.GetProperty(XSharpProjectFileConstants.PostBuildEvent)    ?? string.Empty;
                RunPostBuildEvent = parentPropertyPage.GetProperty(XSharpProjectFileConstants.RunPostBuildEvent) ?? string.Empty;

                isDirty = false;
            }
            finally
            {
                _isBinding = false;
                OnPropertyChanged("Item[]");
            }
        }

        /// <inheritdoc/>
        public override void ApplyChanges()
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            parentPropertyPage.SetProperty(XSharpProjectFileConstants.PreBuildEvent,     PreBuildEvent  ?? string.Empty);
            parentPropertyPage.SetProperty(XSharpProjectFileConstants.PostBuildEvent,    PostBuildEvent ?? string.Empty);
            parentPropertyPage.SetProperty(XSharpProjectFileConstants.RunPostBuildEvent, RunPostBuildEvent ?? string.Empty);

            isDirty = false;
        }
    }
}
