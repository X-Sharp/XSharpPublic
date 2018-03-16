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
using System.Collections.Generic;
using System.Diagnostics;

using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;

using VSConstants = Microsoft.VisualStudio.VSConstants;

namespace XSharp.Project
{

    internal class HierarchyEventArgs : EventArgs
    {
        private uint itemId;
        private string fileName;
        private IVsTextLines buffer;

        public HierarchyEventArgs(uint itemId, string canonicalName)
        {
            this.itemId = itemId;
            this.fileName = canonicalName;
        }

        /// <summary>
        /// Fullpath of the FileName associated with the ItemId
        /// </summary>
        public string CanonicalName
        {
            get { return fileName; }
        }

        /// <summary>
        /// Unique Id of the Item (File) in the Hierarchy it belongs to
        /// </summary>
        public uint ItemID
        {
            get { return itemId; }
        }
        public IVsTextLines TextBuffer
        {
            get { return buffer; }
            set { buffer = value; }
        }
    }


    internal class HierarchyListener : IVsHierarchyEvents, IDisposable
    {

        private IVsHierarchy hierarchy;
        private uint cookie;

        public HierarchyListener(IVsHierarchy hierarchy)
        {
            if (null == hierarchy)
            {
                throw new ArgumentNullException("The hierarchy to listen cannot be null");
            }
            this.hierarchy = hierarchy;
        }

        #region Public Methods

        public bool IsListening
        {
            get { return (0 != cookie); }
        }

        public void StartListening()
        {
            // The listener has already been registered in the Hierachy eventHandler list
            if (0 != cookie)
            {
                return;
            }
            // Register to receive any event that append to the hierarchy
            Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(
                hierarchy.AdviseHierarchyEvents(this, out cookie));
            //
            //if (doInitialScan)
            //{
            //    InternalScanHierarchy(VSConstants.VSITEMID_ROOT);
            //}
        }

        public void StopListening()
        {
            InternalStopListening(true);
        }
        #endregion

        #region IDisposable Members

        public void Dispose()
        {
            InternalStopListening(false);
            cookie = 0;
            hierarchy = null;
        }

        #endregion

        #region Public Events
        private EventHandler<HierarchyEventArgs> onItemAdded;
        public event EventHandler<HierarchyEventArgs> OnAddItem
        {
            add { onItemAdded += value; }
            remove { onItemAdded -= value; }
        }

        private EventHandler<HierarchyEventArgs> onItemDeleted;
        public event EventHandler<HierarchyEventArgs> OnDeleteItem
        {
            add { onItemDeleted += value; }
            remove { onItemDeleted -= value; }
        }

        #endregion

        #region IVsHierarchyEvents Members

        public int OnInvalidateIcon(IntPtr hicon)
        {
            // Do Nothing.
            return VSConstants.S_OK;
        }

        public int OnInvalidateItems(uint itemidParent)
        {
            // TODO: Find out if this event is needed.
            Debug.WriteLine("--> OnInvalidateItems");
            return VSConstants.S_OK;
        }

        public int OnItemAdded(uint itemidParent, uint itemidSiblingPrev, uint itemidAdded)
        {
            // Check if the item is a PRG file.
            Debug.WriteLine("--> OnItemAdded");
            string name;
            if (!IsPrgFile(itemidAdded, out name))
            {
                return VSConstants.S_OK;
            }

            // This item is a PRG file, so we can notify that it is added to the hierarchy.
            if (null != onItemAdded)
            {
                HierarchyEventArgs args = new HierarchyEventArgs(itemidAdded, name);
                onItemAdded(hierarchy, args);
            }
            return VSConstants.S_OK;
        }

        public int OnItemDeleted(uint itemid)
        {
            Debug.WriteLine("--> OnItemDeleted");
            // Notify that the item is deleted only if it is a PRG file.
            string name;
            if (!IsPrgFile(itemid, out name))
            {
                return VSConstants.S_OK;
            }
            if (null != onItemDeleted)
            {
                HierarchyEventArgs args = new HierarchyEventArgs(itemid, name);
                onItemDeleted(hierarchy, args);
            }
            return VSConstants.S_OK;
        }

        public int OnItemsAppended(uint itemidParent)
        {
            // TODO: Find out what this event is about.
            Debug.WriteLine("--> OnItemsAppended");
            return VSConstants.S_OK;
        }

        public int OnPropertyChanged(uint itemid, int propid, uint flags)
        {
            // Do Nothing.
            return VSConstants.S_OK;
        }

        #endregion

        private bool InternalStopListening(bool throwOnError)
        {
            if ((null != hierarchy) || (0 == cookie))
            {
                return false;
            }
            int hr = hierarchy.UnadviseHierarchyEvents(cookie);
            if (throwOnError)
            {
                Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(hr);
            }
            cookie = 0;
            return Microsoft.VisualStudio.ErrorHandler.Succeeded(hr);
        }

        /// <summary>
        /// Based in ItemId, check if the item is a file.
        /// Then check if it ends with ".prg"
        /// Retrieve its FullName
        /// </summary>
        /// <param name="itemId"></param>
        /// <param name="canonicalName"></param>
        /// <returns></returns>
        private bool IsPrgFile(uint itemId, out string canonicalName)
        {
            // Find out if this item is a physical file.
            Guid typeGuid = Guid.Empty;
            canonicalName = null;
            int hr = VSConstants.S_OK;
            try
            {
                hr = hierarchy.GetGuidProperty(itemId, (int)__VSHPROPID.VSHPROPID_TypeGuid, out typeGuid);
            }
            catch (System.Runtime.InteropServices.COMException)
            {
                //For WPF Projects, they will throw an exception when trying to access this property if the
                //guid is empty. These are caught and ignored here.
            }

            if (Microsoft.VisualStudio.ErrorHandler.Failed(hr) ||
                VSConstants.GUID_ItemType_PhysicalFile != typeGuid)
            {
                // It is not a file, we can exit now.
                return false;
            }

            // This item is a file; find if it is a PRG file.
            hr = hierarchy.GetCanonicalName(itemId, out canonicalName);
            if (Microsoft.VisualStudio.ErrorHandler.Failed(hr))
            {
                return false;
            }
            string extension = System.IO.Path.GetExtension(canonicalName);
            return (0 == string.Compare(extension, ".prg", StringComparison.OrdinalIgnoreCase));
        }

        /// <summary>
        /// Do a recursive walk on the hierarchy to find all the PRG files in it.
        /// It will generate an event for every file found.
        /// </summary>
        private void InternalScanHierarchy(uint itemId)
        {
            uint currentItem = itemId;
            while (VSConstants.VSITEMID_NIL != currentItem)
            {
                // If this item is a PRG file, then send the add item event.
                string itemName;
                if ((null != onItemAdded) && IsPrgFile(currentItem, out itemName))
                {
                    HierarchyEventArgs args = new HierarchyEventArgs(currentItem, itemName);
                    onItemAdded(hierarchy, args);
                }

                // NOTE: At the moment we skip the nested hierarchies, so here  we look for the 
                // children of this node.
                // Before looking at the children we have to make sure that the enumeration has not
                // side effects to avoid unexpected behavior.
                object propertyValue;
                bool canScanSubitems = true;
                int hr = hierarchy.GetProperty(currentItem, (int)__VSHPROPID.VSHPROPID_HasEnumerationSideEffects, out propertyValue);
                if ((VSConstants.S_OK == hr) && (propertyValue is bool))
                {
                    canScanSubitems = !(bool)propertyValue;
                }
                // If it is allow to look at the sub-items of the current one, lets do it.
                if (canScanSubitems)
                {
                    object child;
                    hr = hierarchy.GetProperty(currentItem, (int)__VSHPROPID.VSHPROPID_FirstChild, out child);
                    if (VSConstants.S_OK == hr)
                    {
                        // There is a sub-item, call this same function on it.
                        InternalScanHierarchy(GetItemId(child));
                    }
                }

                // Move the current item to its first visible sibling.
                object sibling;
                hr = hierarchy.GetProperty(currentItem, (int)__VSHPROPID.VSHPROPID_NextSibling, out sibling);
                if (VSConstants.S_OK != hr)
                {
                    currentItem = VSConstants.VSITEMID_NIL;
                }
                else
                {
                    currentItem = GetItemId(sibling);
                }
            }
        }

        /// <summary>
        /// Gets the item id.
        /// </summary>
        /// <param name="variantValue">VARIANT holding an itemid.</param>
        /// <returns>Item Id of the concerned node</returns>
        private static uint GetItemId(object variantValue)
        {
            if (variantValue == null) return VSConstants.VSITEMID_NIL;
            if (variantValue is int) return (uint)(int)variantValue;
            if (variantValue is uint) return (uint)variantValue;
            if (variantValue is short) return (uint)(short)variantValue;
            if (variantValue is ushort) return (uint)(ushort)variantValue;
            if (variantValue is long) return (uint)(long)variantValue;
            return VSConstants.VSITEMID_NIL;
        }
    }
}
