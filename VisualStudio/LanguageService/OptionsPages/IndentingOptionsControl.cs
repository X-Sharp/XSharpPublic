using System;
using System.Windows.Forms;
using XSharpModel;
namespace XSharp.LanguageService.OptionsPages
{
    public partial class IndentingOptionsControl : XSUserControl
    {
        private IndentingOptions Options => (IndentingOptions) optionPage.AutomationObject;
        public IndentingOptionsControl()
        {
            InitializeComponent();
            //
            treeIndentStyle.CheckBoxes = true;
            var tvi = new TreeNode("Indent entities inside namespace");
            tvi.Tag = new string[] { "IndentNamespace",
                @"\cf1 BEGIN NAMESPACE \cf0 MyNs\par\cf1{tab} CLASS \cf0 MyClass \par \par {tab} \cf1 END CLASS \cf0\par\cf1 END NAMESPACE\par}" };
            this.treeIndentStyle.Nodes.Add(tvi);
            //
            tvi = new TreeNode("Indent multiline members inside types");
            tvi.Tag = new string[] { "IndentEntityContent",
                @"\cf1 CLASS \cf0 foo\par\par\cf1{tab} METHOD \cf0 m1() \cf1 AS VOID\cf0\par\par\cf1{tab} CONSTRUCTOR \cf0 m1()\cf0\par}"};
            this.treeIndentStyle.Nodes.Add(tvi);

            tvi = new TreeNode("Indent single line members inside types");
            tvi.Tag = new string[] { "IndentFieldContent",
                @"\cf1 CLASS \cf0 foo\par\cf1{tab} PUBLIC \cf0 x \cf1 AS INT\cf0\par\par\cf1{tab} PROPERTY \cf0 y \cf1 AS STRING AUTO \cf0\par}"};
            this.treeIndentStyle.Nodes.Add(tvi);
            //
            tvi = new TreeNode("Indent statements inside entities");
            tvi.Tag = new string[] { "IndentBlockContent",
                @"\cf1 FUNCTION \cf0 foo\par\cf1{tab} LOCAL \cf0 x \cf1 AS INT\cf0\par\cf1{tab} LOCAL \cf0 y \cf1 AS INT\cf0\par}" };
            this.treeIndentStyle.Nodes.Add(tvi);
            //
            tvi = new TreeNode("Indent case label");
            tvi.Tag = new string[] { "IndentCaseLabel",
                @"\cf1 DO CASE \par\cf1{tab} CASE \cf0 x == 1\par\par\cf1{tab} CASE \cf0 x == 2\par}"};
            this.treeIndentStyle.Nodes.Add(tvi);
            //
            tvi = new TreeNode("Indent statements inside case block");
            tvi.Tag = new string[] { "IndentCaseContent",
                @"\cf1 DO CASE \par\cf1\tab CASE \cf0 x == 1\par\cf1\tab{tab} nop\par\cf1\tab{tab} nop\cf0\par}"};
            this.treeIndentStyle.Nodes.Add(tvi);
            //
            tvi = new TreeNode("Indent continuing Lines");
            tvi.Tag = new string[] { "IndentMultiLines",
                @"\cf1 FUNCTION \cf0 foo( \cf0 x \cf1 AS INT ; \par{tab}\cf0 y \cf1 AS INT \cf0 ;\par{tab} \cf0 z \cf1 AS INT \cf0) \par}" };
            this.treeIndentStyle.Nodes.Add(tvi);
            //

            tvi = new TreeNode("Indent preprocessor lines");
            tvi.Tag = new string[] { "IndentPreprocessorLines",
                @"\cf1 CLASS \cf0 foo\par{tab}\cf0#region FIELDS \par\cf1\tab PUBLIC \cf0 x \cf1 AS INT\cf0\par{tab} \cf0#endregion\par\cf1\tab METHOD \cf0 m1() \cf1 AS VOID\cf0\par}"};
            this.treeIndentStyle.Nodes.Add(tvi);
            this.treeIndentStyle.AfterCheck += TreeShowNode;
            this.treeIndentStyle.AfterSelect += TreeShowNode;
            this.treeIndentStyle.SelectedNode = treeIndentStyle.Nodes[0];
        }

        private void TreeShowNode(object sender, TreeViewEventArgs e)
        {
            ShowCodeSample(e.Node);
        }

        internal override void ReadValues(object options)
        {
            base.ReadValues(options);
            foreach (TreeNode tvi in this.treeIndentStyle.Nodes)
            {
                ReadItem(tvi);
            }
        }

        private void ReadItem(TreeNode tvi)
        {
            var tag = tvi.Tag;
            if (tag is string[] tags)
            {
                // The Tag is a String[] with 3 elements :
                // 0 : Property Name
                // 1 : Text if Checked
                // 2 : Text if unChecked
                if (tags.Length > 0)
                {
                    var strTag = tags[0];
                    var prop = typeof(IndentingOptions).GetProperty(strTag);
                    if (prop != null)
                    {
                        var val = prop.GetValue(Options);
                        if (val is bool bValue)
                        {
                            tvi.Checked = bValue;
                        }
                    }
                }
            }
        }

        internal override void SaveValues(object options)
        {
            base.SaveValues(options);
            foreach (TreeNode tvi in this.treeIndentStyle.Nodes)
            {
                SaveItem(tvi);
            }
        }

        private void SaveItem(TreeNode tvi)
        {
            var tag = tvi.Tag;
            if (tag is string[] tags)
            {
                // The Tag is a String[] with 3 elements :
                // 0 : Property Name
                // 1 : Text if Checked
                // 2 : Text if unChecked
                if (tags.Length > 0)
                {
                    var strTag = tags[0];
                    var prop = typeof(IndentingOptions).GetProperty(strTag);
                    if (prop != null && prop.SetMethod != null)
                    {
                        prop.SetValue(Options, tvi.Checked);
                    }
                }
            }
        }

        private void ShowCodeSample( TreeNode node)
        {
            var tag = node.Tag;
            var isChecked = node.Checked;
            if (tag is string[] tags)
            {
                // The Tag is a String[] with 3 elements :
                // 0 : Property Name
                // 1 : Text "{tab}" for possible tabs
                if (tags.Length > 1)
                {
                    var strTag = tags[1];
                    if (isChecked)
                        strTag = strTag.Replace("{tab}", @"\tab");
                    else
                        strTag = strTag.Replace("{tab}", "");
                    //
                    if (strTag.EndsWith("}"))
                        codeSample.Rtf = RTFPrefix + strTag;
                    else
                        codeSample.Text = strTag;
                }
            }
        }

        private string RTFPrefix
        {
            // Put it as a Getter, so we may later try to retrieve the X# Color definition for Keywords
            // instead of a constant
            get
            {
                return @"{\rtf1\deftab400{\colortbl ;\red0\green77\blue187;}";
            }
        }
    }
}
