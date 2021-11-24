using System;
using System.Windows.Forms;
using XSharpModel;
namespace XSharp.LanguageService.OptionsPages
{
    public partial class IndentingOptionsControl : XSUserControl
    {

        public IndentingOptionsControl()
        {
            InitializeComponent();
            //
            ListViewItem lvi = new ListViewItem("Indent Entity contents");
            lvi.Tag = new String[] { "IndentEntityContent",
                @"{\rtf1\deftab400{\colortbl ;\red0\green77\blue187;}\cf1 CLASS \cf0 foo\par\cf1\tab PUBLIC \cf0 x \cf1 AS INT\cf0\par\par\cf1\tab METHOD \cf0 m1() \cf1 AS VOID\cf0\par}",
                @"{\rtf1\deftab400{\colortbl ;\red0\green77\blue187;}\cf1 CLASS \cf0 foo\par\cf1 PUBLIC \cf0 x \cf1 AS INT\cf0\par\par\cf1 METHOD \cf0 m1() \cf1 AS VOID\cf0\par}" };
            this.listIndentStyle.Items.Add(lvi);
            //
            lvi = new ListViewItem("Indent block contents");
            lvi.Tag = new String[] { "IndentBlockContent",
                @"{\rtf1\deftab400{\colortbl ;\red0\green77\blue187;}\cf1 FUNCTION \cf0 foo\par\cf1\tab LOCAL \cf0 x \cf1 AS INT\cf0\par\cf1\tab LOCAL \cf0 y \cf1 AS INT\cf0\par}",
                @"{\rtf1\deftab400{\colortbl ;\red0\green77\blue187;}\cf1 FUNCTION \cf0 foo\par\cf1 LOCAL \cf0 x \cf1 AS INT\cf0\par\cf1 LOCAL \cf0 y \cf1 AS INT\cf0\par}" };
            this.listIndentStyle.Items.Add(lvi);
            //
            lvi = new ListViewItem("Indent case contents");
            lvi.Tag = new String[] { "IndentCaseContent",
                @"{\rtf1\deftab400{\colortbl ;\red0\green77\blue187;}\cf1 DO CASE \par\cf1\tab CASE \cf0 x == 1\par\cf1\tab\tab nop\par\cf1\tab\tab nop\cf0\par}",
                @"{\rtf1\deftab400{\colortbl ;\red0\green77\blue187;}\cf1 DO CASE \par\cf1\tab CASE \cf0 x == 1\par\cf1\tab nop\par\cf1\tab nop\cf0\par}" };
            this.listIndentStyle.Items.Add(lvi);
            //
            lvi = new ListViewItem("Indent case label");
            lvi.Tag = new String[] { "IndentCaseLabel",
                @"{\rtf1\deftab400{\colortbl ;\red0\green77\blue187;}\cf1 DO CASE \par\cf1\tab CASE \cf0 x == 1\par\par\cf1\tab CASE \cf0 x == 2\par}",
                @"{\rtf1\deftab400{\colortbl ;\red0\green77\blue187;}\cf1 DO CASE \par\cf1 CASE \cf0 x == 1\par\par\cf1 CASE \cf0 x == 2\par}" };
            this.listIndentStyle.Items.Add(lvi);
        }


        private IndentingOptionsPage OurOptionPage => (IndentingOptionsPage)optionPage;
        internal override void ReadValues()
        {
            foreach (ListViewItem lvi in this.listIndentStyle.Items)
            {
                ReadListViewItem(lvi);
            }
        }

        private void ReadListViewItem(ListViewItem lvi)
        {
            var tag = lvi.Tag;
            if (tag is string[] tags)
            {
                // The Tag is a String[] with 3 elements :
                // 0 : Property Name
                // 1 : Text if Checked
                // 2 : Text if unChecked
                if (tags.Length > 0)
                {
                    var strTag = tags[0];
                    var prop = optionPage.GetType().GetProperty(strTag);
                    if (prop != null)
                    {
                        var val = prop.GetValue(optionPage);
                        if (val is bool bValue)
                        {
                            lvi.Checked = bValue;
                        }
                    }
                }
            }
        }

        internal override void SaveValues()
        {
            foreach (ListViewItem lvi in this.listIndentStyle.Items)
            {
                SaveListViewItem(lvi);
            }
        }

        private void SaveListViewItem(ListViewItem lvi)
        {
            var tag = lvi.Tag;
            if (tag is string[] tags)
            {
                // The Tag is a String[] with 3 elements :
                // 0 : Property Name
                // 1 : Text if Checked
                // 2 : Text if unChecked
                if (tags.Length > 0)
                {
                    var strTag = tags[0];
                    var prop = optionPage.GetType().GetProperty(strTag);
                    if (prop != null && prop.SetMethod != null)
                    {
                        prop.SetValue(optionPage, lvi.Checked);
                    }
                }
            }
        }

        private void listIndentStyle_ItemChecked(object sender, ItemCheckedEventArgs e)
        {
            var tag = e.Item.Tag;
            if (tag is string[] tags)
            {
                // The Tag is a String[] with 3 elements :
                // 0 : Property Name
                // 1 : Text if Checked
                // 2 : Text if unChecked
                if (tags.Length > 2)
                {
                    var strTag = tags[e.Item.Checked ? 1 : 2];
                    //
                    if (strTag.StartsWith("{"))
                        codeSample.Rtf = strTag;
                    else
                        codeSample.Text = strTag;
                }
            }
        }

        private void listIndentStyle_SelectedIndexChanged(object sender, EventArgs e)
        {
            var items = this.listIndentStyle.SelectedItems;
            if (items.Count > 0)
            {
                var tag = items[0].Tag;
                if (tag is string[] tags)
                {
                    // The Tag is a String[] with 3 elements :
                    // 0 : Property Name
                    // 1 : Text if Checked
                    // 2 : Text if unChecked
                    if (tags.Length > 2)
                    {
                        var strTag = tags[items[0].Checked ? 1 : 2];
                        //
                        if (strTag.StartsWith("{"))
                            codeSample.Rtf = strTag;
                        else
                            codeSample.Text = strTag;
                    }
                }
            }
        }
    }
}
