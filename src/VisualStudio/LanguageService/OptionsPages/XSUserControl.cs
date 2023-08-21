using Microsoft.VisualStudio.Shell;
using System;
using System.Windows.Forms;

namespace XSharp.LanguageService.OptionsPages
{
    public class XSUserControl : UserControl  
    {
        internal DialogPage optionPage;

        private void ReadControl(Control c, object options)
        {
            var tag = c.Tag;
            if (tag is string strTag)
            {

                var prop = options.GetType().GetProperty(strTag);
                if (prop != null)
                {
                    var val = prop.GetValue(options);
                    if (c is CheckBox cb && val is bool bValue)
                    {
                        cb.Checked = bValue;
                    }
                    if (c is NumericUpDown number && val is int iValue)
                    {
                        number.Value = iValue >= number.Minimum && iValue < number.Maximum ? iValue : number.Minimum ;
                    }
                    if (c is TextBox tb)
                    {
                        tb.Text = val.ToString();
                    }

                }
            }
            foreach (Control control in c.Controls)
            {
                ReadControl(control, options);
            }

        }
        internal virtual void ReadValues(object options)
        {
            foreach (Control control in this.Controls)
            {
                ReadControl(control, options);
            }
        }

        internal virtual void SaveValues(object options)
        {
            foreach (Control control in this.Controls)
            {
                SaveControl(control, options);
            }
        }
        private void SaveControl(Control c, Object options)
        {
            var tag = c.Tag;
            if (tag is string strTag)
            {
                var prop = options.GetType().GetProperty(strTag);
                if (prop != null && prop.SetMethod != null)
                {
                    if (c is CheckBox cb)
                    {
                        prop.SetValue(options, cb.Checked);
                    }
                    if (c is NumericUpDown number)
                    {
                        prop.SetValue(options, (int)number.Value);
                    }
                    if (c is TextBox tb)
                    {
                        var old = prop.GetValue(options);
                        switch (old)
                        {
                            case int _:
                                if (int.TryParse(tb.Text, out var iNew))
                                    prop.SetValue(options, iNew);

                                break;
                            case string _:
                                prop.SetValue(options, tb.Text);
                                break;
                        }
                    }

                }
            }
            foreach (Control control in c.Controls)
            {
                SaveControl(control, options);
            }
        }
        

        protected void checkbuttons(bool check)
        {
            foreach (var control in this.Controls)
            {
                if (control is CheckBox cb)
                {
                    cb.Checked = check;
                }
            }

        }


    }
}
