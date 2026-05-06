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
    /// Composite control that combines a label, a <see cref="TextBox"/>, and a themed
    /// reset button into a single reusable unit for property pages.
    /// </summary>
    /// <remarks>
    /// The control does <em>not</em> set its own <c>DataContext</c> — it deliberately
    /// inherits the page ViewModel from its parent so that the reset button can bind
    /// directly to <c>ResetCommand</c> and to the ViewModel indexer
    /// <c>[PropertyName]</c> without any extra plumbing.
    /// <para>
    /// A Browse ("...") button is not included because its click handler and column
    /// layout differ per-usage; callers that need one add it themselves after this
    /// control in a surrounding <c>StackPanel</c>.
    /// </para>
    /// </remarks>
    public partial class PropertyTextBox : UserControl
    {
        // =========================================================================================
        // Dependency Properties
        // =========================================================================================

        /// <summary>Label text displayed above the text box.</summary>
        public static readonly DependencyProperty LabelProperty =
            DependencyProperty.Register(nameof(Label), typeof(object), typeof(PropertyTextBox),
                new PropertyMetadata(null));

        /// <summary>String value bound to <see cref="TextBox.Text"/>. Bind TwoWay.</summary>
        public static readonly DependencyProperty ValueProperty =
            DependencyProperty.Register(nameof(Value), typeof(string), typeof(PropertyTextBox),
                new FrameworkPropertyMetadata(string.Empty, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        /// <summary>
        /// MSBuild property name. Used as <c>CommandParameter</c> and to build the
        /// dynamic <c>IsEnabled</c> indexer binding on the reset button.
        /// </summary>
        public static readonly DependencyProperty PropertyNameProperty =
            DependencyProperty.Register(nameof(PropertyName), typeof(string), typeof(PropertyTextBox),
                new PropertyMetadata(string.Empty, OnPropertyNameChanged));

        /// <summary>Forwarded to <see cref="TextBox.IsReadOnly"/>. Default is <c>false</c>.</summary>
        public static readonly DependencyProperty IsReadOnlyProperty =
            DependencyProperty.Register(nameof(IsReadOnly), typeof(bool), typeof(PropertyTextBox),
                new PropertyMetadata(false));

        /// <summary>Margin applied to the label TextBlock. Default is <c>0,6,0,2</c>.</summary>
        public static readonly DependencyProperty LabelMarginProperty =
            DependencyProperty.Register(nameof(LabelMargin), typeof(Thickness), typeof(PropertyTextBox),
                new PropertyMetadata(new Thickness(0, 6, 0, 2)));

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
        public string Value
        {
            get => (string)GetValue(ValueProperty);
            set => SetValue(ValueProperty, value);
        }

        /// <inheritdoc cref="PropertyNameProperty"/>
        public string PropertyName
        {
            get => (string)GetValue(PropertyNameProperty);
            set => SetValue(PropertyNameProperty, value);
        }

        /// <inheritdoc cref="IsReadOnlyProperty"/>
        public bool IsReadOnly
        {
            get => (bool)GetValue(IsReadOnlyProperty);
            set => SetValue(IsReadOnlyProperty, value);
        }

        /// <inheritdoc cref="LabelMarginProperty"/>
        public Thickness LabelMargin
        {
            get => (Thickness)GetValue(LabelMarginProperty);
            set => SetValue(LabelMarginProperty, value);
        }

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>Initializes a new instance of <see cref="PropertyTextBox"/>.</summary>
        public PropertyTextBox()
        {
            InitializeComponent();
        }

        // =========================================================================================
        // Private Helpers
        // =========================================================================================

        private static void OnPropertyNameChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var control = (PropertyTextBox)d;
            var name    = e.NewValue as string ?? string.Empty;

            control.resetButton.CommandParameter = name;
        }
    }
}
