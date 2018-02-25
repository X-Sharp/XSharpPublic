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
using Microsoft.VisualStudio.Shell.Interop;

namespace XSharp.Project
{
    /// <summary>
    /// Identify a module. The module is identify using the hierarchy that
    /// contains it and its item id inside the hierarchy.
    /// </summary>
    internal sealed class XSharpModuleId
    {
        private IVsHierarchy ownerHierarchy;
        private uint itemId;

        public XSharpModuleId(IVsHierarchy owner, uint id)
        {
            this.ownerHierarchy = owner;
            this.itemId = id;
        }

        public IVsHierarchy Hierarchy
        {
            get { return ownerHierarchy; }
        }
        public uint ItemID
        {
            get { return itemId; }
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