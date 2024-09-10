//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/*****************************************************************************
* Based on IronStudio/IronPythonTools/IronPythonTools/Navigation
****************************************************************************/

using Microsoft.VisualStudio.Shell.Interop;
using XSharpModel;

namespace XSharp.LanguageService
{
    /// <summary>
    /// Identify a module. The module is identify using the hierarchy that
    /// contains it and its item id inside the hierarchy.
    /// </summary>
    internal sealed class XSharpModuleId
    {
        private readonly IVsHierarchy ownerHierarchy;
        private readonly uint itemId;
        #region Properties
        public IVsHierarchy Hierarchy
        {
            get { return ownerHierarchy; }
        }
        public uint ItemID
        {
            get { return itemId; }
        }
        public uint ContentHashCode { get; set; }
        public XFile File { get; set; }
        public string FileKey => File.Project.Id.ToString()+"|" + File.FullPath;


        #endregion

        public XSharpModuleId(IVsHierarchy owner, uint id, XFile file)
        {
            this.ownerHierarchy = owner;
            this.itemId = id;
            this.File = file;
        }


        public override int GetHashCode()
        {
            int hash = 0;
            if (null != ownerHierarchy)
            {
                hash = ownerHierarchy.GetHashCode();
            }
            hash = hash ^ (int)itemId;
            return hash;
        }
        public override bool Equals(object obj)
        {
            XSharpModuleId other = obj as XSharpModuleId;
            if (null == obj)
            {
                return false;
            }
            if (!ownerHierarchy.Equals(other.ownerHierarchy))
            {
                return false;
            }
            return (itemId == other.itemId);
        }
    }
}
