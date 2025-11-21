using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

using XSharpModel;

namespace XSharp.Project
{
    public partial class CreateWindowsForm : Form
    {
        internal XProject project = null;
        internal string Path = "";
        public CreateWindowsForm()
        {
            InitializeComponent();
        }

        private void btnOk_Click(object sender, EventArgs e)
        {
            var name = tbNewForm.Text.Trim();

            var type = project.FindType(name, null);
            if (type != null)
            {
                MessageBox.Show("The class name '" + name + "' already exists in the project.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return;
            }
            // check to see if the file already exists
            var fileName = System.IO.Path.Combine(Path, name + ".prg");
            if (System.IO.File.Exists(fileName))
            {
                var result = MessageBox.Show("The file '" + fileName + "' already exists", "File Exists", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return;
            }
            // check to see if the file.designer already exists
            var fileNameDesigner = System.IO.Path.Combine(Path, name + ".designer.prg");
            if (System.IO.File.Exists(fileNameDesigner))
            {
                var result = MessageBox.Show("The file '" + fileNameDesigner + "' already exists", "File Exists", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return;
            }
            else
            {


                this.DialogResult = DialogResult.OK;
                this.Close();
            }
        }

        private void tbNewForm_TextChanged(object sender, EventArgs e)
        {
            var name = tbNewForm.Text.Trim();
            this.lblFileName.Text = $"{name}.prg, {name}.designer.prg";
        }

        private void CreateWindowsForm_VisibleChanged(object sender, EventArgs e)
        {
            this.lblFolderName.Text = this.Path;
        }

    }
}
