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
    /// Composite control that renders a "Configuration:" label and a drop-down
    /// populated from an <see cref="XConfigSelectorViewModel"/>.
    /// </summary>
    /// <remarks>
    /// The control does <em>not</em> set its own <c>DataContext</c> — it inherits
    /// the page ViewModel.  The single <see cref="ConfigSelector"/> DP is the only
    /// wiring required; the inner ComboBox binds to
    /// <c>ConfigSelector.ConfigNames</c> and <c>ConfigSelector.SelectedConfig</c>
    /// via <c>ElementName</c> so that DataContext inheritance is not disrupted.
    /// </remarks>
    public partial class ConfigSelectorBar : UserControl
    {
        // =========================================================================================
        // Dependency Properties
        // =========================================================================================

        /// <summary>
        /// The <see cref="XConfigSelectorViewModel"/> that supplies the list of
        /// configuration names and tracks the selected one.
        /// </summary>
        public static readonly DependencyProperty ConfigSelectorProperty =
            DependencyProperty.Register(
                nameof(ConfigSelector),
                typeof(XConfigSelectorViewModel),
                typeof(ConfigSelectorBar),
                new PropertyMetadata(null));

        // =========================================================================================
        // CLR Property Wrapper
        // =========================================================================================

        /// <inheritdoc cref="ConfigSelectorProperty"/>
        public XConfigSelectorViewModel ConfigSelector
        {
            get => (XConfigSelectorViewModel)GetValue(ConfigSelectorProperty);
            set => SetValue(ConfigSelectorProperty, value);
        }

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>Initializes a new instance of <see cref="ConfigSelectorBar"/>.</summary>
        public ConfigSelectorBar()
        {
            InitializeComponent();
        }
    }
}
