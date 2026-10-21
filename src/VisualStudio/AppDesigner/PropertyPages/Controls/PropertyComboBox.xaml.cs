//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections;
using System.Windows;
using System.Windows.Controls;

namespace XSharp.Project
{
    /// <summary>
    /// Composite control that combines a label, a <see cref="ComboBox"/>, and a themed
    /// reset button into a single reusable unit for property pages.
    /// </summary>
    /// <remarks>
    /// The control does <em>not</em> set its own <c>DataContext</c> — it deliberately
    /// inherits the page ViewModel from its parent so that the reset button can bind
    /// directly to <c>ResetCommand</c> and to the ViewModel indexer
    /// <c>[PropertyName]</c> without any extra plumbing.
    /// </remarks>
    public partial class PropertyComboBox : UserControl
    {
        // =========================================================================================
        // Dependency Properties
        // =========================================================================================

        /// <summary>Label text displayed above the combo box.</summary>
        public static readonly DependencyProperty LabelProperty =
            DependencyProperty.Register(nameof(Label), typeof(object), typeof(PropertyComboBox),
                new PropertyMetadata(null));

        /// <summary>Items collection for the combo box.</summary>
        public static readonly DependencyProperty ItemsProperty =
            DependencyProperty.Register(nameof(Items), typeof(IEnumerable), typeof(PropertyComboBox),
                new PropertyMetadata(null));

        /// <summary>Selected item; bind TwoWay to the ViewModel property.</summary>
        public static readonly DependencyProperty ValueProperty =
            DependencyProperty.Register(nameof(Value), typeof(object), typeof(PropertyComboBox),
                new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        /// <summary>
        /// MSBuild property name. Used as <c>CommandParameter</c> and to build the
        /// dynamic <c>IsEnabled</c> indexer binding on the reset button.
        /// </summary>
        public static readonly DependencyProperty PropertyNameProperty =
            DependencyProperty.Register(nameof(PropertyName), typeof(string), typeof(PropertyComboBox),
                new PropertyMetadata(string.Empty, OnPropertyNameChanged));

        /// <summary>Margin applied to the label TextBlock. Default is <c>0,6,0,2</c>.</summary>
        public static readonly DependencyProperty LabelMarginProperty =
            DependencyProperty.Register(nameof(LabelMargin), typeof(Thickness), typeof(PropertyComboBox),
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

        /// <inheritdoc cref="ItemsProperty"/>
        public IEnumerable Items
        {
            get => (IEnumerable)GetValue(ItemsProperty);
            set => SetValue(ItemsProperty, value);
        }

        /// <inheritdoc cref="ValueProperty"/>
        public object Value
        {
            get => GetValue(ValueProperty);
            set => SetValue(ValueProperty, value);
        }

        /// <inheritdoc cref="PropertyNameProperty"/>
        public string PropertyName
        {
            get => (string)GetValue(PropertyNameProperty);
            set => SetValue(PropertyNameProperty, value);
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

        /// <summary>Initializes a new instance of <see cref="PropertyComboBox"/>.</summary>
        public PropertyComboBox()
        {
            InitializeComponent();
        }

        // =========================================================================================
        // Private Helpers
        // =========================================================================================

        private static void OnPropertyNameChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var control = (PropertyComboBox)d;
            var name    = e.NewValue as string ?? string.Empty;

            control.resetButton.CommandParameter = name;
        }
    }
}
