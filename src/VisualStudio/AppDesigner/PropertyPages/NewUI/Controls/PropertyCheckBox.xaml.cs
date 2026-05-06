//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Windows;
using System.Windows.Controls;

namespace XSharp.Project
{
    /// <summary>
    /// Composite control that combines a <see cref="CheckBox"/> and a themed reset
    /// button into a single reusable unit for property pages.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The control does <em>not</em> set its own <c>DataContext</c> — it deliberately
    /// inherits the page ViewModel from its parent so that the reset button can bind
    /// directly to <c>ResetCommand</c> and to the ViewModel indexer
    /// <c>[PropertyName]</c> without any extra plumbing.
    /// </para>
    /// <para>
    /// Usage on a property page:
    /// <code>
    /// &lt;local:PropertyCheckBox
    ///     Label="{Binding CaptOptimize}"
    ///     Value="{Binding Optimize, Mode=TwoWay}"
    ///     PropertyName="Optimize"
    ///     ToolTip="{Binding DescOptimize}"
    ///     Margin="0,6,8,0"/&gt;
    /// </code>
    /// </para>
    /// </remarks>
    public partial class PropertyCheckBox : UserControl
    {
        // =========================================================================================
        // Dependency Properties
        // =========================================================================================

        /// <summary>
        /// The content (label) displayed next to the check box.
        /// Typically bound to a caption string on the ViewModel.
        /// </summary>
        public static readonly DependencyProperty LabelProperty =
            DependencyProperty.Register(
                nameof(Label),
                typeof(object),
                typeof(PropertyCheckBox),
                new PropertyMetadata(null));

        /// <summary>
        /// The boolean value bound to <see cref="CheckBox.IsChecked"/>.
        /// Should be bound TwoWay to the ViewModel property.
        /// </summary>
        public static readonly DependencyProperty ValueProperty =
            DependencyProperty.Register(
                nameof(Value),
                typeof(bool),
                typeof(PropertyCheckBox),
                new FrameworkPropertyMetadata(
                    false,
                    FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        /// <summary>
        /// The MSBuild property name.  Used as the reset button's
        /// <c>CommandParameter</c> and to build the dynamic
        /// <c>IsEnabled</c> indexer binding on the reset button.
        /// </summary>
        public static readonly DependencyProperty PropertyNameProperty =
            DependencyProperty.Register(
                nameof(PropertyName),
                typeof(string),
                typeof(PropertyCheckBox),
                new PropertyMetadata(string.Empty, OnPropertyNameChanged));

        // =========================================================================================
        // CLR Property Wrappers
        // =========================================================================================

        /// <inheritdoc cref="LabelProperty"/>
        public object Label
        {
            get => GetValue(LabelProperty);
            set => SetValue(LabelProperty, value);
        }

        /// <inheritdoc cref="ValueProperty"/>
        public bool Value
        {
            get => (bool)GetValue(ValueProperty);
            set => SetValue(ValueProperty, value);
        }

        /// <inheritdoc cref="PropertyNameProperty"/>
        public string PropertyName
        {
            get => (string)GetValue(PropertyNameProperty);
            set => SetValue(PropertyNameProperty, value);
        }

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of <see cref="PropertyCheckBox"/>.
        /// </summary>
        public PropertyCheckBox()
        {
            InitializeComponent();
        }

        // =========================================================================================
        // Private Helpers
        // =========================================================================================

        /// <summary>
        /// Called when <see cref="PropertyName"/> changes.
        /// Rewires the reset button's <c>CommandParameter</c> to match the new property name.
        /// </summary>
        private static void OnPropertyNameChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var control = (PropertyCheckBox)d;
            var name    = e.NewValue as string ?? string.Empty;

            // CommandParameter — plain string, no binding needed.
            control.resetButton.CommandParameter = name;
        }
    }
}
