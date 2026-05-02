// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
using System;
using System.Windows;

namespace XSharp.LanguageService.OptionsPages
{
    public partial class CompletionOptionsControl : XSUserControl
    {
        public CompletionOptionsControl()
        {
            InitializeComponent();
            rtfDescription.Text = string.Join(Environment.NewLine,
                new[]
                {
                    "Code Completion is triggered by special characters such as '.' and ':'",
                    "The editor can also perform code completion at other locations such as:",
                    "\u2022  after certain keywords, such as AS, INHERIT, IMPLEMENTS, USING",
                    "\u2022  at the start of an expression, such as at the start of a line or after an",
                    "   opening '(', '{' or '[' or after :=",
                    "",
                    "The filling of these second \"generic\" completion lists will NOT be automatically",
                    "started, but you have to press the \"Complete Word\" key, which is usually assigned",
                    "to the Ctrl-Space key.",
                    "",
                    "On this page you can control where the editor will look for completion."
                });
        }

        private void OnAll(object sender, RoutedEventArgs e)
        {
            checkbuttons(true);
        }

        private void OnNone(object sender, RoutedEventArgs e)
        {
            checkbuttons(false);
        }
    }
}
