/*****************************************************************************
 *
 * Copyright(c) Microsoft Corporation.
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A
* copy of the license can be found in the License.html file at the root of this distribution.If
* you cannot locate the Apache License, Version 2.0, please send an email to
* ironpy@microsoft.com.By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Apache License, Version 2.0.
 *
 * You must not remove this notice, or any other, from this software.
*
****************************************************************************/
/*****************************************************************************
* XSharp.BV
* Based on IronStudio/IronPythonTools/IronPythonTools/Navigation
*
****************************************************************************/

using System;
using System.Globalization;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Shell;
using ErrorHandler = Microsoft.VisualStudio.ErrorHandler;
using VSConstants = Microsoft.VisualStudio.VSConstants;
using XSharpModel;
using System.Collections.Generic;
using XSharp.Settings;

namespace XSharp.LanguageService
{
	internal class XSharpLibraryProject : XSharpLibraryNode
	{
        String _defaultNameSpace;

        public String DefaultNameSpace
        {
            get
            {
                return this._defaultNameSpace;
            }
        }
        internal override LibraryNode Clone()
        {
            return new XSharpLibraryProject(this);
        }

        internal XSharpLibraryProject(XSharpLibraryProject node) :
            base(node)
        {
            this._defaultNameSpace = node._defaultNameSpace;
        }

        internal XSharpLibraryProject(XProject prj, IVsHierarchy hierarchy)
            : base(prj.Name, LibraryNodeType.Package, prj.FileName)
        {
            this.ownerHierarchy = hierarchy;
            //
            //this.displayData.hImageList = XSharpProjectNode.ImageList.Handle;
            //this.displayData.Image = (ushort)XSharpProjectNode.XSharpProjectImageName.Project;
            //this.displayData.SelectedImage = (ushort)XSharpProjectNode.XSharpProjectImageName.Project;
            //
            this.NodeType = LibraryNodeType.Package;
            //
            //prj.ProjectNode
            this._defaultNameSpace = prj.ProjectNode.RootNameSpace;
            if (String.IsNullOrEmpty(this._defaultNameSpace))
                this._defaultNameSpace = "Default Namespace";
            //
            XSharpLibraryNode defaultNS = new XSharpLibraryNode( _defaultNameSpace, LibraryNodeType.Namespaces,"" );
            defaultNS.displayData.Image = (ushort)IconImageIndex._Namespace;
            defaultNS.displayData.SelectedImage = (ushort)IconImageIndex._Namespace;
            this.AddNode(defaultNS);
            Logger.Information("Added LibraryProject " + prj.Name);
            //
        }

        public LibraryNode SearchNameSpace(string nsName)
        {
            return children.Find(node => findbyName(node, nsName));
        }
        bool findbyName(LibraryNode node, string name)
        {
            return String.Compare(node.Name, name, true) == 0 && (node.NodeType & LibraryNodeType.Namespaces) != LibraryNodeType.None;
        }
    }
}
