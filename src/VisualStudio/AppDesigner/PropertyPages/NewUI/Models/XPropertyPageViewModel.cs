//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Project;
using System.ComponentModel;
using System.Runtime.CompilerServices;

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
        /// MSBuild project.
        /// Called when the user clicks <em>Apply</em> or <em>OK</em>.
        /// </summary>
        public abstract void ApplyChanges();

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
            => SetProjectProperty(propertyName, value.ToString().ToLowerInvariant());
    }
}
