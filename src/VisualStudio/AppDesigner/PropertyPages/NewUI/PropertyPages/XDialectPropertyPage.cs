//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// This file is intentionally minimal.
//
// The SDK vs. legacy routing decision for the Dialect (Application) property page is made
// inside XSharpDialectPropertyPage.CreatePropertyPagePanel() in
// PropertyPages\XDialectPropertyPage.cs:
//
//   • SDK-style projects  → returns new XDialectPropertyPageXamlHost(this)
//   • Legacy (.NET FW)    → returns new XDialectPropertyPagePanelWinForms(this)
//
// XDialectPropertyPageXamlHost,
// XDialectPropertyPageViewModel, and
// XDialectPropertyPageView (XDialectPropertyPage.xaml) are all in the folder
// NewUI\PropertyPages\XAML\
//
// XDialectPropertyPagePanelWinForms is in
// NewUI\PropertyPages\.


// Routing stub — kept for .csproj <Compile> compatibility.
// SDK vs. legacy routing is in XSharpDialectPropertyPage.CreatePropertyPagePanel().
