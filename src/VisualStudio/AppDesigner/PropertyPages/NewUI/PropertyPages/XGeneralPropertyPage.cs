//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// This file is intentionally minimal.
//
// The SDK vs. legacy routing decision for the General (Application) property page is made
// inside XSharpGeneralPropertyPage.CreatePropertyPagePanel() in
// PropertyPages\XGeneralPropertyPage.cs:
//
//   • SDK-style projects  → returns new XGeneralPropertyPageXamlHost(this)
//   • Legacy (.NET FW)    → returns new XGeneralPropertyPagePanelWinForms(this)
//
// XGeneralPropertyPageXamlHost,
// XGeneralPropertyPageViewModel, and
// XGeneralPropertyPageView (XGeneralPropertyPage.xaml) are all in the folder
// NewUI\PropertyPages\XAML\
//
// XGeneralPropertyPagePanelWinForms is in
// NewUI\PropertyPages\.
