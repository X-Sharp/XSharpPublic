// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
using Microsoft.VisualStudio.Shell;
using System.Linq;
using System.Windows;
using System.Windows.Controls;

namespace XSharp.LanguageService.OptionsPages
{
    public class XSUserControl : UserControl
    {
        internal DialogPage optionPage;

        private void ReadControl(DependencyObject element, object options)
        {
            if (element is FrameworkElement fe && fe.Tag is string strTag)
            {
                var prop = options.GetType().GetProperty(strTag);
                if (prop != null)
                {
                    var val = prop.GetValue(options);
                    if (fe is CheckBox cb && val is bool bValue)
                        cb.IsChecked = bValue;
                    else if (fe is TextBox tb)
                        tb.Text = val?.ToString() ?? string.Empty;
                }
            }
            foreach (var child in LogicalTreeHelper.GetChildren(element).OfType<DependencyObject>())
                ReadControl(child, options);
        }

        internal virtual void ReadValues(object options)
        {
            ReadControl(this, options);
        }

        internal virtual void SaveValues(object options)
        {
            SaveControl(this, options);
        }

        private void SaveControl(DependencyObject element, object options)
        {
            if (element is FrameworkElement fe && fe.Tag is string strTag)
            {
                var prop = options.GetType().GetProperty(strTag);
                if (prop != null && prop.SetMethod != null)
                {
                    if (fe is CheckBox cb)
                        prop.SetValue(options, cb.IsChecked == true);
                    else if (fe is TextBox tb)
                    {
                        var old = prop.GetValue(options);
                        switch (old)
                        {
                            case int _:
                                if (int.TryParse(tb.Text, out int iNew))
                                    prop.SetValue(options, iNew);
                                break;
                            case string _:
                                prop.SetValue(options, tb.Text);
                                break;
                        }
                    }
                }
            }
            foreach (var child in LogicalTreeHelper.GetChildren(element).OfType<DependencyObject>())
                SaveControl(child, options);
        }

        protected void checkbuttons(bool check)
        {
            CheckAllCheckBoxes(this, check);
        }

        private void CheckAllCheckBoxes(DependencyObject element, bool check)
        {
            if (element is CheckBox cb)
                cb.IsChecked = check;
            foreach (var child in LogicalTreeHelper.GetChildren(element).OfType<DependencyObject>())
                CheckAllCheckBoxes(child, check);
        }
    }
}
