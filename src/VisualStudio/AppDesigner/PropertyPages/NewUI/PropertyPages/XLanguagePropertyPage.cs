//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// This file is intentionally minimal.
//
// The SDK vs. legacy routing decision for the Language property page is made
// inside XSharpLanguagePropertyPage.CreatePropertyPagePanel() in
// PropertyPages\XLanguagePropertyPage.cs:
//
//   • SDK-style projects  → returns new XLanguagePropertyPageXamlHost(this)
//   • Legacy (.NET FW)    → returns new XLanguagePropertyPagePanelWinForms(this)
//
// XLanguagePropertyPageXamlHost,
// XLanguagePropertyPageViewModel, and
// XLanguagePropertyPageView (XLanguagePropertyPage.xaml) are all in the folder
// NewUI\PropertyPages\XAML\
//
// XLanguagePropertyPagePanelWinForms is in
// NewUI\PropertyPages\.
