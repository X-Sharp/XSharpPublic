//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/*****************************************************************************
* Based on IronStudio/IronPythonTools/IronPythonTools/Navigation
****************************************************************************/


using System;

using Microsoft.VisualStudio.Shell.Interop;
using XSharpModel;

namespace XSharp.LanguageService
{
    internal class XSharpLibraryProject : XSharpLibraryNode
	{
        string _defaultNameSpace;
        XProject _project;
        internal XProject XProject => _project;

        public string DefaultNameSpace
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
            _project = prj;
            if (prj.Id == -1)
            {
                XDatabase.Read(prj);
            }                
            this.NodeType = LibraryNodeType.Package;
            this._defaultNameSpace = prj.ProjectNode.RootNameSpace;
            if (string.IsNullOrEmpty(this._defaultNameSpace))
                this._defaultNameSpace = prj.Name;
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
            return children.Find(node => FindbyName(node, nsName));
        }
        bool FindbyName(LibraryNode node, string name)
        {
            return string.Compare(node.Name, name, true) == 0 && (node.NodeType & LibraryNodeType.Namespaces) != LibraryNodeType.None;
        }
    }
}
