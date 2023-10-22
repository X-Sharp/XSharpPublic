using System;
using System.Collections.Generic;
using System.Windows.Controls;
using XSharp.Settings;

namespace XSharp.Debugger.UI
{
    internal static class Styling
    {
        internal static void SetTheme(this UserControl page, ListView lv, TextBox tb)
        {
            Community.VisualStudio.Toolkit.Themes.SetUseVsTheme(page, true);
            foreach (var dict in page.Resources.MergedDictionaries)
            {
                var types = new List<Type>();
                foreach (var key in dict.Keys)
                {
                    if (key is System.Type t)
                    {
                        if (t == typeof(ListView))
                        {
                            dict.Remove(t);
                            break;
                        }
                    }
                }
            }
            lv.Background = tb.Background;
            lv.Foreground = tb.Foreground;
        }
    }
}
