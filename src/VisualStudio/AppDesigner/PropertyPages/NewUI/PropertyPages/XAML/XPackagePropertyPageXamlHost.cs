//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Windows;
using Microsoft.VisualStudio.Project;

namespace XSharp.Project
{
    /// <summary>
    /// ElementHost wrapper that bridges the WPF <see cref="XPackagePropertyPageView"/>
    /// into the WinForms-based VS property page infrastructure.
    /// </summary>
    internal sealed class XPackagePropertyPageXamlHost : XPropertyPageXamlHost
    {
        private readonly XPackagePropertyPageViewModel _viewModel;

        public XPackagePropertyPageXamlHost(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            _viewModel = new XPackagePropertyPageViewModel(parentPropertyPage);
            SetViewModel(_viewModel);
        }

        protected override FrameworkElement CreateXamlContent()
            => new XPackagePropertyPageView();

        public override void HookupEvents()    => _viewModel.HookupEvents();
        public override void BindProperties() => _viewModel.BindProperties();
        public override void ApplyChanges()   => _viewModel.ApplyChanges();
    }
}
