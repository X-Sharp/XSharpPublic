//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using Microsoft.VisualStudio.Project;

namespace XSharp.Project
{
    // =========================================================================================
    // MacroItem — a single row in the macro list
    // =========================================================================================

    /// <summary>
    /// Represents one MSBuild macro displayed in the macro list of a WPF editor dialog.
    /// </summary>
    internal sealed class MacroItem
    {
        /// <summary>Gets the display name, e.g. <c>$(Configuration)</c>.</summary>
        public string DisplayName { get; }

        /// <summary>Gets the expanded macro value, e.g. <c>Debug</c>.</summary>
        public string Value { get; }

        /// <summary>Gets the raw macro name without delimiters, e.g. <c>Configuration</c>.</summary>
        public string MacroName { get; }

        /// <summary>
        /// Initializes a new instance of <see cref="MacroItem"/>.
        /// </summary>
        public MacroItem(string macroName, string value)
        {
            MacroName   = macroName;
            DisplayName = $"$({macroName})";
            Value       = value;
        }
    }

    // =========================================================================================
    // XSharpMacroDialogViewModel — shared base for all three WPF macro dialogs
    // =========================================================================================

    /// <summary>
    /// Shared MVVM ViewModel for the WPF macro editor dialogs
    /// (<see cref="XSharpSLEDialog"/>, <see cref="XSharpMLEDialog"/>,
    /// <see cref="XSharpBuildEventEditorDialog"/>).
    /// </summary>
    /// <remarks>
    /// <para>
    /// Manages the macro list (<see cref="Macros"/>), the currently selected macro
    /// (<see cref="SelectedMacro"/>), and the derived <see cref="CanInsertMacro"/>
    /// flag that controls the Insert button's <c>IsEnabled</c> state.
    /// </para>
    /// <para>
    /// Subclasses (or the dialog code-behind) add the text-area content as a plain
    /// property since it does not need change-notification — the dialog reads it
    /// directly on OK.
    /// </para>
    /// </remarks>
    internal class XSharpMacroDialogViewModel : INotifyPropertyChanged
    {
        // =========================================================================================
        // Backing fields
        // =========================================================================================

        private MacroItem _selectedMacro;

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpMacroDialogViewModel"/> class.
        /// </summary>
        public XSharpMacroDialogViewModel()
        {
            Macros = new ObservableCollection<MacroItem>();
        }

        // =========================================================================================
        // Observable Properties
        // =========================================================================================

        /// <summary>Gets the collection of macros displayed in the list.</summary>
        public ObservableCollection<MacroItem> Macros { get; }

        /// <summary>Gets or sets the currently selected macro row.</summary>
        public MacroItem SelectedMacro
        {
            get => _selectedMacro;
            set
            {
                _selectedMacro = value;
                OnPropertyChanged();
                OnPropertyChanged(nameof(CanInsertMacro));
            }
        }

        /// <summary>
        /// Gets whether a macro is currently selected (controls Insert button
        /// <c>IsEnabled</c>).
        /// </summary>
        public bool CanInsertMacro => _selectedMacro != null;

        // =========================================================================================
        // Methods
        // =========================================================================================

        /// <summary>
        /// Populates <see cref="Macros"/> from an <see cref="XBuildMacroCollection"/>.
        /// Mirrors <c>XSharpSLEPropertyForm.SetMacros</c> and
        /// <c>XBuildEventEditorForm.InitializeMacroList</c>.
        /// </summary>
        /// <param name="buildMacros">The project macro collection to display.</param>
        public void LoadMacros(XBuildMacroCollection buildMacros)
        {
            Macros.Clear();
            SelectedMacro = null;

            foreach (var p in buildMacros)
                Macros.Add(new MacroItem(p.MacroName, p.Value));
        }

        /// <summary>
        /// Returns the insertion text for the currently selected macro,
        /// e.g. <c>$(Configuration)</c>. Returns <see cref="string.Empty"/> when
        /// no macro is selected.
        /// </summary>
        public string GetInsertText()
            => SelectedMacro != null ? $"$({SelectedMacro.MacroName})" : string.Empty;

        // =========================================================================================
        // INotifyPropertyChanged
        // =========================================================================================

        /// <inheritdoc/>
        public event PropertyChangedEventHandler PropertyChanged;

        /// <summary>Raises <see cref="PropertyChanged"/> for the given member name.</summary>
        protected void OnPropertyChanged([CallerMemberName] string name = null)
            => PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(name));
    }
}
