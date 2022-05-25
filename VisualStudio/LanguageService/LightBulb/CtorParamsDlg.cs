using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using XSharpModel;
using MVP = Microsoft.VisualStudio.Package;

namespace XSharp.LanguageService.Editors.LightBulb
{
    public partial class CtorParamsDlg : Form
    {
        ImageList _imageList = new ImageList();
        List<XSharpModel.IXMemberSymbol> selectedFieldsNProps = new List<IXMemberSymbol>();

        public List<XSharpModel.IXMemberSymbol> FieldsNProps { get { return selectedFieldsNProps; } }

        public CtorParamsDlg()
        {
            InitializeComponent();
            this.InitImageList();
            //
            this.listMembers.ListViewItemSorter = new MemberTagValueComparer();
        }

        private void InitImageList()
        {
            Stream stream = typeof(MVP.LanguageService).Assembly.GetManifestResourceStream("Resources.completionset.bmp");
            _imageList.ImageSize = new Size(16, 16);
            _imageList.TransparentColor = Color.FromArgb(255, 0, 255);
            _imageList.Images.AddStrip(new Bitmap(stream));
            //
            this.listMembers.SmallImageList = _imageList;
        }

        public void FillMembers(List<XSharpModel.IXMemberSymbol> fieldsNProps)
        {
            int i = 0;
            foreach (var mbr in fieldsNProps)
            {
                ListViewItem lvi = new ListViewItem(mbr.Name);
                lvi.Checked = true;
                lvi.Tag = ( i++, mbr );
                if (mbr is XSymbol xmbr)
                {
                    lvi.ImageIndex = xmbr.Glyph;
                }
                this.listMembers.Items.Add(lvi);
            }
        }

        private void btnSelect_Click(object sender, EventArgs e)
        {
            foreach (ListViewItem lvi in listMembers.Items)
            {
                lvi.Checked = true;
            }
        }

        private void btnDeselect_Click(object sender, EventArgs e)
        {
            foreach (ListViewItem lvi in listMembers.Items)
            {
                lvi.Checked = false;
            }
        }

        private void btnUp_Click(object sender, EventArgs e)
        {
            if (listMembers.SelectedItems.Count > 0)
            {
                ListViewItem lvi = listMembers.SelectedItems[0];
                (int, IXMemberSymbol) t1 = ((int, IXMemberSymbol))lvi.Tag;
                int tagValue = t1.Item1;
                if ( tagValue > 0)
                {
                    ListViewItem otherLvi = listMembers.Items[tagValue-1];
                    (int, IXMemberSymbol) t2 = ((int, IXMemberSymbol))otherLvi.Tag;
                    t1.Item1 = t2.Item1;
                    t2.Item1 = tagValue;
                    //
                    lvi.Tag = t1;
                    otherLvi.Tag = t2;
                    //
                    listMembers.Sort();
                }
            }
        }

        private void btnDown_Click(object sender, EventArgs e)
        {
            if (listMembers.SelectedItems.Count > 0)
            {
                ListViewItem lvi = listMembers.SelectedItems[0];
                (int, IXMemberSymbol) t1 = ((int, IXMemberSymbol))lvi.Tag;
                int tagValue = t1.Item1;
                if (tagValue < listMembers.Items.Count-1)
                {
                    ListViewItem otherLvi = listMembers.Items[tagValue + 1];
                    (int, IXMemberSymbol) t2 = ((int, IXMemberSymbol))otherLvi.Tag;
                    t1.Item1 = t2.Item1;
                    t2.Item1 = tagValue;
                    //
                    lvi.Tag = t1;
                    otherLvi.Tag = t2;
                    //
                    listMembers.Sort();
                }
            }
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            this.DialogResult = DialogResult.Cancel;
        }

        private void btnOk_Click(object sender, EventArgs e)
        {
            foreach( ListViewItem lvi in listMembers.Items )
            {
                if ( lvi.Checked )
                {
                    (int, IXMemberSymbol) t1 = ((int, IXMemberSymbol))lvi.Tag;
                    this.selectedFieldsNProps.Add(t1.Item2);
                }
            }
            if ( this.selectedFieldsNProps.Count > 0 )
            {
                this.DialogResult = DialogResult.OK;
            }
            else
            {
                this.DialogResult = DialogResult.Cancel;
            }
        }
    }

    class MemberTagValueComparer : System.Collections.IComparer
    {
        public int Compare(object o1, object o2)
        {
            ListViewItem i1 = (ListViewItem)o1;
            ListViewItem i2 = (ListViewItem)o2;
            //
            (int, IXMemberSymbol) t1 = ((int, IXMemberSymbol))i1.Tag;
            (int, IXMemberSymbol) t2 = ((int, IXMemberSymbol))i2.Tag;
            //
            return t1.Item1.CompareTo(t2.Item1);
        }
    }
}
