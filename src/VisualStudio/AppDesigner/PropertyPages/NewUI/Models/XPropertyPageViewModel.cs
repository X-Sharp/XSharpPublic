//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Windows.Input;

namespace XSharp.Project
{
    /// <summary>
    /// Abstract base class for MVVM ViewModels that back XAML/WPF property page panels.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This class extends <see cref="XPropertyPagePanelBase"/> with
    /// <see cref="INotifyPropertyChanged"/> support so that WPF data-binding
    /// (<c>{Binding PropertyName, Mode=TwoWay}</c>) works correctly between the ViewModel
    /// and the XAML <c>UserControl</c>.
    /// </para>
    /// <para>
    /// Concrete ViewModels (e.g., <c>XGeneralPropertyPageViewModel</c>) inherit from this
    /// class, declare one observable property per MSBuild project property, and implement
    /// <see cref="BindProperties"/>, <see cref="ApplyChanges"/>, and
    /// <see cref="HookupEvents"/> from <see cref="IPropertyPagePanel"/>.
    /// </para>
    /// <para>
    /// The ViewModel is <em>not</em> a <see cref="System.Windows.Forms.Control"/>.
    /// It is held by an <see cref="XPropertyPageXamlHost"/> instance which acts as the
    /// actual WinForms control and sets the ViewModel as the WPF
    /// <c>FrameworkElement.DataContext</c>.
    /// </para>
    /// </remarks>
    public abstract class XPropertyPageViewModel : XPropertyPagePanelBase, INotifyPropertyChanged
    {
        // =========================================================================================
        // Events
        // =========================================================================================

        /// <summary>
        /// Raised when an observable property value has changed.
        /// WPF binding infrastructure subscribes to this event automatically.
        /// </summary>
        public event PropertyChangedEventHandler PropertyChanged;

        // =========================================================================================
        // Reset command
        // =========================================================================================

        private ICommand _resetCommand;

        /// <summary>
        /// A single <see cref="ICommand"/> shared across all reset (↺) buttons on the page.
        /// The <c>CommandParameter</c> must be the MSBuild property name string.
        /// The command removes the named property from the project file and then calls
        /// <see cref="BindProperties"/> to refresh all displayed values.
        /// It is enabled only when the property is explicitly overridden in the project file.
        /// </summary>
        public ICommand ResetCommand
        {
            get
            {
                if (_resetCommand == null)
                    _resetCommand = new ResetPropertyCommand(this);

                return _resetCommand;
            }
        }

        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XPropertyPageViewModel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">
        /// The <see cref="XPropertyPage"/> that owns the panel for which this ViewModel
        /// provides data.
        /// </param>
        protected XPropertyPageViewModel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
        }

        // =========================================================================================
        // IPropertyPagePanel — abstract (implemented by concrete ViewModels)
        // =========================================================================================

        /// <summary>
        /// Loads all project property values from MSBuild and updates the ViewModel's
        /// observable properties so that the bound XAML controls reflect them.
        /// </summary>
        public abstract void BindProperties();

        /// <summary>
        /// Reads all current ViewModel property values and saves them back to the
        /// MSBuild project.  Called when the user clicks <em>Apply</em> or <em>OK</em>.
        /// </summary>
        public void ApplyChanges()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            ApplyChangesCore();
        }

        /// <summary>Concrete implementation; override in each ViewModel.</summary>
        protected abstract void ApplyChangesCore();

        /// <summary>
        /// Subscribes to change events (typically the ViewModel's own
        /// <see cref="PropertyChanged"/> event) so that edits made in the UI
        /// propagate the dirty flag to the owning
        /// <see cref="XPropertyPage"/>.
        /// </summary>
        public abstract void HookupEvents();

        // =========================================================================================
        // Protected Methods — INotifyPropertyChanged helpers
        // =========================================================================================

        /// <summary>
        /// Sets a backing field to a new value and, if the value changed, raises
        /// <see cref="PropertyChanged"/> so that WPF updates the bound controls.
        /// </summary>
        /// <typeparam name="T">The type of the property.</typeparam>
        /// <param name="backingField">
        /// A reference to the backing field that stores the property value.
        /// </param>
        /// <param name="value">The new value to assign.</param>
        /// <param name="propertyName">
        /// The name of the property; automatically filled in by the compiler via
        /// <see cref="CallerMemberNameAttribute"/>.
        /// </param>
        /// <returns>
        /// <see langword="true"/> if the value changed and
        /// <see cref="PropertyChanged"/> was raised; <see langword="false"/> if
        /// the value was the same and no event was raised.
        /// </returns>
        protected bool SetProperty<T>(ref T backingField, T value,
            [CallerMemberName] string propertyName = null)
        {
            if (object.Equals(backingField, value))
                return false;

            backingField = value;
            OnPropertyChanged(propertyName);
            return true;
        }

        /// <summary>
        /// Raises the <see cref="PropertyChanged"/> event for the specified property.
        /// </summary>
        /// <param name="propertyName">
        /// The name of the property that changed.  Automatically filled in by
        /// <see cref="CallerMemberNameAttribute"/> when called from a property setter.
        /// </param>
        protected void OnPropertyChanged([CallerMemberName] string propertyName = null)
            => PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));

        // =========================================================================================
        // Protected Methods — Typed property access helpers
        // =========================================================================================

        /// <summary>
        /// Gets a boolean property value from the MSBuild project.
        /// </summary>
        /// <param name="propertyName">The MSBuild property name.</param>
        /// <returns>
        /// <see langword="true"/> if the stored value is the string <c>"true"</c>
        /// (case-insensitive); <see langword="false"/> otherwise.
        /// </returns>
        protected bool GetBoolPropertyValue(string propertyName)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            // Read only from the project file XML — not from evaluated/imported values.
            // This ensures that after a Reset (property removed from XML), we return
            // false rather than a stale SDK-imported value.
            var project = parentPropertyPage?.ProjectMgr?.BuildProject;
            if (project != null)
            {
                foreach (var propGroup in project.Xml.PropertyGroups)
                {
                    if (!string.IsNullOrEmpty(propGroup.Condition))
                        continue;
                    foreach (var prop in propGroup.Properties)
                    {
                        if (string.Equals(prop.Name, propertyName, System.StringComparison.OrdinalIgnoreCase)
                            && string.IsNullOrEmpty(prop.Condition))
                            return bool.TryParse(prop.Value, out var xmlResult) && xmlResult;
                    }
                }
                return false;
            }
            var value = GetProjectProperty(propertyName);
            return bool.TryParse(value, out var result) && result;
        }

        /// <summary>
        /// Writes a boolean value to the MSBuild project as a lowercase string
        /// (<c>"true"</c> or <c>"false"</c>).
        /// </summary>
        /// <param name="propertyName">The MSBuild property name.</param>
        /// <param name="value">The boolean value to store.</param>
        protected void SetBoolPropertyValue(string propertyName, bool value)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            SetProjectProperty(propertyName, value.ToString().ToLowerInvariant());
        }

        /// <summary>
        /// Writes a string property to the MSBuild project only when the property is already
        /// explicitly overridden in the project file, or when <paramref name="value"/> is a
        /// genuine user-supplied literal (non-empty and not an SDK expression such as
        /// <c>$(AssemblyName)</c>).
        /// This prevents SDK default expressions from being written as explicit overrides.
        /// </summary>
        protected void SetPropertyIfOverriddenOrNonEmpty(string propertyName, string value)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            bool overridden = IsPropertyOverridden(propertyName);
            bool isUserValue = !string.IsNullOrEmpty(value) && !value.Contains("$(");
            if (overridden || isUserValue)
                parentPropertyPage.SetProperty(propertyName, value ?? string.Empty);
        }

        // =========================================================================================
        // Indexer — used by XAML bindings to check override state per property
        // =========================================================================================

        /// <summary>
        /// Returns <see langword="true"/> when the named MSBuild property is explicitly set
        /// in the project file (overriding the SDK default).
        /// </summary>
        /// <remarks>
        /// Expose this as an indexer so XAML can bind with <c>{Binding [Authors]}</c>.
        /// Raise <c>PropertyChanged("Item[]")</c> to force WPF to re-evaluate all such bindings.
        /// </remarks>
        public bool this[string propertyName] => IsPropertyOverridden(propertyName);

        // Internal wrappers so that ResetPropertyCommand (a sibling class) can call the
        // protected base methods without needing reflection or inheritance.
        internal bool IsPropertyOverriddenInternal(string name) => IsPropertyOverridden(name);
        internal void ResetPropertyInternal(string name)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            ResetProperty(name);
        }
    }

    // =========================================================================================
    // ResetPropertyCommand — ICommand implementation used by all reset (↺) buttons
    // =========================================================================================

    /// <summary>
    /// An <see cref="ICommand"/> that removes a named MSBuild property from the project
    /// file and calls <see cref="XPropertyPageViewModel.BindProperties"/> to refresh the UI.
    /// The <c>CommandParameter</c> must be the MSBuild property name string.
    /// The command is always enabled.
    /// </summary>
    internal sealed class ResetPropertyCommand : ICommand
    {
        private readonly XPropertyPageViewModel _vm;

        public ResetPropertyCommand(XPropertyPageViewModel vm)
        {
            _vm = vm;
        }

        // Delegate to CommandManager so WPF re-evaluates CanExecute automatically
        // whenever it normally would (focus changes, input gestures, etc.).
        public event System.EventHandler CanExecuteChanged
        {
            add    { System.Windows.Input.CommandManager.RequerySuggested += value; }
            remove { System.Windows.Input.CommandManager.RequerySuggested -= value; }
        }

        public bool CanExecute(object parameter) => true;

        public void Execute(object parameter)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (parameter is string propertyName && !string.IsNullOrEmpty(propertyName))
            {
                // Flush any pending dirty changes first so other properties are
                // written to the project file before we reset this one.
                // BindProperties will then read them back correctly.
                if (_vm.isDirty)
                    _vm.ApplyChanges();
                _vm.ResetPropertyInternal(propertyName);
                _vm.BindProperties();
                _vm.isDirty = false;
            }
        }
    }
}
