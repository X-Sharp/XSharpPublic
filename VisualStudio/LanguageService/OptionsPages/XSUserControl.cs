using Microsoft.VisualStudio.Shell;
using System;
using System.Windows.Forms;

namespace XSharp.LanguageService.OptionsPages
{
    public class XSUserControl : UserControl  
    {
        public DialogPage optionPage;

        private void ReadControl(Control c)
        {
            var tag = c.Tag;
            if (tag is string strTag)
            {

                var prop = optionPage.GetType().GetProperty(strTag);
                if (prop != null)
                {
                    var val = prop.GetValue(optionPage);
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
                ReadControl(control);
            }

        }
        internal virtual void ReadValues()
        {
            foreach (Control control in this.Controls)
            {
                ReadControl(control);
            }
        }

        internal virtual void SaveValues()
        {
            foreach (Control control in this.Controls)
            {
                SaveControl(control);
            }
        }
        private void SaveControl(Control c)
        {
            var tag = c.Tag;
            if (tag is string strTag)
            {

                var prop = optionPage.GetType().GetProperty(strTag);
                if (prop != null && prop.SetMethod != null)
                {
                    if (c is CheckBox cb)
                    {
                        prop.SetValue(optionPage, cb.Checked);
                    }
                    if (c is NumericUpDown number)
                    {
                        prop.SetValue(optionPage, (int)number.Value);
                    }
                    if (c is TextBox tb)
                    {
                        var old = prop.GetValue(optionPage);
                        switch (old)
                        {
                            case int iValue:
                                if (Int32.TryParse(tb.Text, out var iNew))
                                    prop.SetValue(optionPage, iNew);

                                break;
                            case string strValue:
                                prop.SetValue(optionPage, tb.Text);
                                break;
                        }
                    }

                }
            }
            foreach (Control control in c.Controls)
            {
                SaveControl(control);
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
