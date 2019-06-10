//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Project;
using System.Runtime.InteropServices;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Globalization;
using System.Windows.Forms;
using System.Drawing;
using Microsoft.VisualStudio;
using System.Drawing.Design;
using System.Security.Permissions;

namespace XSharp.Project
{
    internal class XSharpFileNameEditorAttribute : Attribute
    {
        public XSharpFileNameEditorAttribute(string title, string filter, int filterIndex)
        {
            _title = title;
            _filter = filter;
            _filterIndex = filterIndex;
        }

        private string _title;
        private string _filter;
        private int _filterIndex;

        public string title
        {
            get
            {
                return _title;
            }
        }

        public string filter
        {
            get
            {
                return _filter;
            }
        }

        public int filterIndex
        {
            get
            {
                return _filterIndex;
            }
        }

    };

    [SecurityPermission(SecurityAction.Demand, Flags = SecurityPermissionFlag.UnmanagedCode)]
    public class XSharpFileNameEditor : System.Windows.Forms.Design.FileNameEditor
    {
        private string _origFilename;
        private string _title;
        private string _filter;
        private int _filterIndex;

        public XSharpFileNameEditor()
            : base()
        {

        }

        public override Object EditValue(ITypeDescriptorContext context, IServiceProvider provider, Object value)
        {
            if (context.PropertyDescriptor.IsReadOnly)
                return value;
            _origFilename = (string)value;

            PropertyDescriptor property = context.PropertyDescriptor;
            XSharpFileNameEditorAttribute attr = (XSharpFileNameEditorAttribute)property.Attributes[typeof(XSharpFileNameEditorAttribute)];

            _title = attr.title;
            _filter = attr.filter;
            _filterIndex = attr.filterIndex;

            return base.EditValue(context, provider, value);
        }

        protected override void InitializeDialog(OpenFileDialog openFileDialog)
        {
            base.InitializeDialog(openFileDialog);

            openFileDialog.FileName = _origFilename;
            openFileDialog.Filter = _filter; // "Applications (*.exe)|*.exe|Class libraries (*.dll)|*.dll|All files (*.*)|*.*";

            string ext = Path.GetExtension(_origFilename);

            if (_filterIndex == -1)
            {
                if (String.Compare(ext, ".exe", false) == 0)
                {
                    openFileDialog.FilterIndex = 0;
                }
                else if (String.Compare(ext, ".dll", false) == 0)
                {
                    openFileDialog.FilterIndex = 1;
                }
                else
                {
                    openFileDialog.FilterIndex = 2;
                }
            }
            else
            {
                openFileDialog.FilterIndex = _filterIndex;
            }

            openFileDialog.Title = _title;
        }
    }
}
