//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Globalization;
using System.Windows;
using System.Windows.Data;

namespace XSharp.Project
{
    /// <summary>
    /// WPF value converter that negates a <see cref="bool"/> value.
    /// Used to bind <c>IsEnabled</c> to the inverse of a boolean property
    /// (e.g. disable a Delete button when <c>Imported</c> is <see langword="true"/>).
    /// </summary>
    [ValueConversion(typeof(bool), typeof(bool))]
    public sealed class BoolNegationConverter : IValueConverter
    {
        /// <inheritdoc/>
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
            => value is bool b ? (object)!b : true;

        /// <inheritdoc/>
        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
            => value is bool b ? (object)!b : false;
    }


    /// <summary>
    /// WPF value converter that maps a <see cref="bool"/> to a
    /// <see cref="Visibility"/> value, with optional inversion.
    /// Used at least in XGeneralPropertyPage.xaml, UserControl
    /// </summary>
    /// <remarks>
    /// <para>
    /// When <see cref="Invert"/> is <see langword="false"/> (the default):
    /// <list type="bullet">
    ///   <item><see langword="true"/>  → <see cref="Visibility.Visible"/></item>
    ///   <item><see langword="false"/> → <see cref="Visibility.Collapsed"/></item>
    /// </list>
    /// </para>
    /// <para>
    /// When <see cref="Invert"/> is <see langword="true"/>:
    /// <list type="bullet">
    ///   <item><see langword="true"/>  → <see cref="Visibility.Collapsed"/></item>
    ///   <item><see langword="false"/> → <see cref="Visibility.Visible"/></item>
    /// </list>
    /// </para>
    /// <para>
    /// Register two instances in XAML resources to cover both directions:
    /// <code><![CDATA[
    ///   <local:BoolToVisibilityConverter x:Key="BoolToVisibility"        Invert="False"/>
    ///   <local:BoolToVisibilityConverter x:Key="BoolToInverseVisibility" Invert="True"/>
    /// ]]></code>
    /// </para>
    /// </remarks>
    [ValueConversion(typeof(bool), typeof(Visibility))]
    public sealed class BoolToVisibilityConverter : IValueConverter
    {
        // =========================================================================================
        // Properties
        // =========================================================================================

        /// <summary>
        /// Gets or sets a value indicating whether the conversion result is inverted.
        /// When <see langword="true"/>, <see langword="true"/> maps to
        /// <see cref="Visibility.Collapsed"/> and vice-versa.
        /// </summary>
        public bool Invert { get; set; }

        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="BoolToVisibilityConverter"/> class
        /// with inversion disabled.
        /// </summary>
        public BoolToVisibilityConverter() { }

        /// <summary>
        /// Initializes a new instance of the <see cref="BoolToVisibilityConverter"/> class.
        /// </summary>
        /// <param name="invert">
        /// <see langword="true"/> to invert the mapping;
        /// <see langword="false"/> for the normal (non-inverted) mapping.
        /// </param>
        public BoolToVisibilityConverter(bool invert)
        {
            Invert = invert;
        }

        // =========================================================================================
        // IValueConverter
        // =========================================================================================

        /// <summary>
        /// Converts a <see cref="bool"/> to a <see cref="Visibility"/>.
        /// </summary>
        /// <param name="value">The <see cref="bool"/> value to convert.</param>
        /// <param name="targetType">Unused.</param>
        /// <param name="parameter">Unused.</param>
        /// <param name="culture">Unused.</param>
        /// <returns>
        /// <see cref="Visibility.Visible"/> when <paramref name="value"/> is
        /// <see langword="true"/> (or <see langword="false"/> when inverted);
        /// otherwise <see cref="Visibility.Collapsed"/>.
        /// </returns>
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            bool boolValue = value is bool b && b;
            if (Invert)
                boolValue = !boolValue;
            return boolValue ? Visibility.Visible : Visibility.Collapsed;
        }

        /// <summary>
        /// Converts a <see cref="Visibility"/> back to a <see cref="bool"/>.
        /// </summary>
        /// <param name="value">The <see cref="Visibility"/> value to convert back.</param>
        /// <param name="targetType">Unused.</param>
        /// <param name="parameter">Unused.</param>
        /// <param name="culture">Unused.</param>
        /// <returns>
        /// <see langword="true"/> when <paramref name="value"/> is
        /// <see cref="Visibility.Visible"/> (accounting for inversion);
        /// otherwise <see langword="false"/>.
        /// </returns>
        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            bool visible = value is Visibility v && v == Visibility.Visible;
            return Invert ? !visible : visible;
        }
    }

    /// <summary>
    /// WPF value converter that returns <see langword="true"/> when an MSBuild property is
    /// explicitly set in the project file (i.e., overrides the SDK default).
    /// </summary>
    /// <remarks>
    /// <para>
    /// Bind <c>IsEnabled</c> on a reset (↺) button like this:
    /// <code><![CDATA[
    ///   <Button IsEnabled="{Binding ., Converter={StaticResource PropertyOverridden},
    ///                                ConverterParameter=AssemblyName}" .../>
    /// ]]></code>
    /// </para>
    /// <para>
    /// The <c>value</c> must be an <see cref="XPropertyPageViewModel"/> (the DataContext).
    /// The <c>parameter</c> must be the MSBuild property name string.
    /// </para>
    /// </remarks>
    [ValueConversion(typeof(XPropertyPageViewModel), typeof(bool))]
    public sealed class PropertyOverriddenConverter : IValueConverter
    {
        /// <inheritdoc/>
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            if (value is XPropertyPageViewModel vm && parameter is string propertyName
                && !string.IsNullOrEmpty(propertyName))
            {
                return vm.IsPropertyOverriddenInternal(propertyName);
            }
            return false;
        }

        /// <inheritdoc/>
        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
            => throw new NotSupportedException();
    }
}
