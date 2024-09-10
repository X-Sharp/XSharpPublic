//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/*****************************************************************************
* Based on IronStudio/IronPythonTools/IronPythonTools/Navigation
****************************************************************************/



using System;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell.Interop;
using VSConstants = Microsoft.VisualStudio.VSConstants;
using System.Collections.Generic;
using Microsoft.VisualStudio.Shell;

namespace XSharp.LanguageService
{

    public class Library : IVsSimpleLibrary2
    {
        private readonly Guid guid;
        private readonly LibraryNode root;
        private _LIB_FLAGS2 capabilities;

        public Library(Guid libraryGuid)
        {
            this.guid = libraryGuid;
            root = new LibraryNode("", LibraryNode.LibraryNodeType.Package);
        }

        public void Refresh()
        {
            this.root.updateCount += 1;
        }

        public _LIB_FLAGS2 LibraryCapabilities
        {
            get { return capabilities; }
            set { capabilities = value; }
        }

        internal void AddNode(LibraryNode node)
        {
            lock (this)
            {
                root.AddNode(node);
            }
        }

        internal void RemoveNode(LibraryNode node)
        {
            lock (this)
            {
                //root = new LibraryNode(root);
                root.RemoveNode(node);
            }
        }

        internal LibraryNode SearchHierarchy(IVsHierarchy hierarchy)
        {
            LibraryNode found = null;
            lock (this)
            {
                
                foreach (LibraryNode nd in root.children)
                {
                    if (nd is XSharpLibraryNode)
                    {
                        XSharpLibraryNode vln = (XSharpLibraryNode)nd;
                        if (hierarchy.Equals(vln.ownerHierarchy))
                        {
                            found = nd;
                            break;
                        }
                    }
                }
            }
            return found;
        }

        #region IVsSimpleLibrary2 Members

        public int AddBrowseContainer(VSCOMPONENTSELECTORDATA[] pcdComponent, ref uint pgrfOptions, out string pbstrComponentAdded)
        {
            pbstrComponentAdded = null;
            return VSConstants.E_NOTIMPL;
        }

        public int CreateNavInfo(SYMBOL_DESCRIPTION_NODE[] rgSymbolNodes, uint ulcNodes, out IVsNavInfo ppNavInfo)
        {
            ppNavInfo = null;
            return VSConstants.E_NOTIMPL;
        }

        public int GetBrowseContainersForHierarchy(IVsHierarchy pHierarchy, uint celt, VSBROWSECONTAINER[] rgBrowseContainers, uint[] pcActual)
        {
            return VSConstants.E_NOTIMPL;
        }

        public int GetGuid(out Guid pguidLib)
        {
            pguidLib = guid;
            return VSConstants.S_OK;
        }

        public int GetLibFlags2(out uint pgrfFlags)
        {
            pgrfFlags = (uint)LibraryCapabilities;
            return VSConstants.S_OK;
        }

        public int GetList2(uint ListType, uint flags, VSOBSEARCHCRITERIA2[] pobSrch, out IVsSimpleObjectList2 ppIVsSimpleObjectList2)
        {
            ppIVsSimpleObjectList2 = null;
            int ret = VSConstants.S_OK;
            string strSearchCriteria = null;
            if ( pobSrch != null)
                strSearchCriteria = pobSrch[0].szName;
            //
            IVsSimpleObjectList2 result = null;
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                switch (ListType)
                {
                    case (uint)_LIB_LISTTYPE.LLT_PHYSICALCONTAINERS:
                        result = root as IVsSimpleObjectList2;
                        break;
                    case (uint)_LIB_LISTTYPE.LLT_HIERARCHY:
                        // Search in Projects
                        result = SearchNodes(ListType, strSearchCriteria);
                        //ppIVsSimpleObjectList2 = root as IVsSimpleObjectList2;
                        break;
                    case (uint)_LIB_LISTTYPE.LLT_NAMESPACES:
                        // Search NameSpaces with the searchCriteria
                        //ppIVsSimpleObjectList2 = root as IVsSimpleObjectList2;
                        result = SearchNodes(ListType, strSearchCriteria);
                        break;
                    case (uint)_LIB_LISTTYPE.LLT_MEMBERS:
                        // Search in Members : Classes / Enums / ...
                        //ppIVsSimpleObjectList2 = root as IVsSimpleObjectList2;
                        result = SearchNodes(ListType, strSearchCriteria);
                        break;
                }
            });
            ppIVsSimpleObjectList2 = result;
            return ret;
        }

        private LibraryNode SearchNodes(uint elementType, string strSearchCriteria)
        {
            List<LibraryNode.LibraryNodeType> nType = new List<LibraryNode.LibraryNodeType>();
            if (elementType == (uint)_LIB_LISTTYPE.LLT_HIERARCHY)
            {
                nType.Add(LibraryNode.LibraryNodeType.Hierarchy);
            }
            else if (elementType == (uint)_LIB_LISTTYPE.LLT_NAMESPACES)
            {
                nType.Add(LibraryNode.LibraryNodeType.Namespaces);
            }
            else if (elementType == (uint)_LIB_LISTTYPE.LLT_MEMBERS)
            {
                nType.Add(LibraryNode.LibraryNodeType.Classes);
                nType.Add(LibraryNode.LibraryNodeType.Members);
            }
            LibraryNode tmpRoot = new LibraryNode( "", LibraryNode.LibraryNodeType.Package );
            //
            FillRoot(tmpRoot, nType, strSearchCriteria, root);
            //
            return tmpRoot;
        }

        /// <summary>
        /// Search the Childrens of the currentRoot, to see if some are containing strSearchCriteria.
        /// If the Children has some childrens..., then call FillRoot recursively.
        /// </summary>
        /// <param name="tmpRoot">The current upper level, where corresponding Childrens must be added</param>
        /// <param name="elementTypes">What kind of Children are we looking for</param>
        /// <param name="strSearchCriteria">What are we looking for</param>
        /// <param name="currentRoot">The node to look after</param>
        private void FillRoot(LibraryNode tmpRoot, List<LibraryNode.LibraryNodeType> elementTypes, string strSearchCriteria, LibraryNode currentRoot )
        {
            foreach (LibraryNode node in currentRoot.children)
            {
                if ( elementTypes.Contains( node.NodeType ) && node.Name.ToLower().Contains( strSearchCriteria.ToLower() ) )
                { 
                    tmpRoot.children.Add( node.Clone() );
                }
                if (node.children.Count > 0)
                {
                    FillRoot(tmpRoot, elementTypes, strSearchCriteria, node);
                }
            }
        }

        public int GetSeparatorStringWithOwnership(out string pbstrSeparator)
        {
            pbstrSeparator = ".";
            return VSConstants.S_OK;
        }

        /// <summary>
        /// Return the kind of Hierarchy that the librayr is supporting
        /// </summary>
        public int GetSupportedCategoryFields2(int Category, out uint pgrfCatField)
        {
            pgrfCatField = (uint)_LIB_CATEGORY2.LC_HIERARCHYTYPE; // | (uint)_LIB_CATEGORY2.LC_PHYSICALCONTAINERTYPE;
            return VSConstants.S_OK;
        }

        public int LoadState(IStream pIStream, LIB_PERSISTTYPE lptType)
        {
            return VSConstants.S_OK;
        }

        public int RemoveBrowseContainer(uint dwReserved, string pszLibName)
        {
            return VSConstants.E_NOTIMPL;
        }

        public int SaveState(IStream pIStream, LIB_PERSISTTYPE lptType)
        {
            return VSConstants.S_OK;
        }

        public int UpdateCounter(out uint pCurUpdate)
        {
            var result = 0;
            uint counter = 0;
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                result = ((IVsSimpleObjectList2)root).UpdateCounter(out counter);
            });
            pCurUpdate = counter;
            return result;
        }

        #endregion
    }
}
