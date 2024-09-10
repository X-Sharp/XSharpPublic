//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/*****************************************************************************
* Based on IronStudio/IronPythonTools/IronPythonTools/Navigation
****************************************************************************/


using System;
using System.Collections.Generic;
using System.Diagnostics;

using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;

using VSConstants = Microsoft.VisualStudio.VSConstants;
using Microsoft.VisualStudio.Shell;
using XSharpModel;

namespace XSharp.LanguageService
{

    internal class HierarchyEventArgs : EventArgs
    {
        internal uint ItemID { get; private set; }
        internal XFile File { get; private set; }

        public HierarchyEventArgs(uint itemId, XFile file)
        {
            this.ItemID = itemId;
            this.File = file;
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
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                if (hierarchy != null)
                {
                    Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(
                        hierarchy.AdviseHierarchyEvents(this, out cookie));
                }
            });
            //
            //if (doInitialScan)
            {
                InternalScanHierarchy(VSConstants.VSITEMID_ROOT);
            }
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
            string name = getItemName(itemidAdded);
            // if (!IsPrgFile(itemidAdded, out name))
            //{
            //    return VSConstants.S_OK;
            //}

            // This item is a PRG file, so we can notify that it is added to the hierarchy.
            if (null != onItemAdded)
            {
                var file = XSolution.FindFile(name);
                HierarchyEventArgs args = new HierarchyEventArgs(itemidAdded, file);
                onItemAdded(hierarchy, args);
            }
            return VSConstants.S_OK;
        }

        public int OnItemDeleted(uint itemid)
        {
            Debug.WriteLine("--> OnItemDeleted");
            // Notify that the item is deleted only if it is a PRG file.
            string name = getItemName(itemid);
            //if (! IsPrgFile(itemid, out name))
            //{
            //    return VSConstants.S_OK;
            //}
            if (null != onItemDeleted)
            {
                var file = XSolution.FindFile(name);
                HierarchyEventArgs args = new HierarchyEventArgs(itemid, file);
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
            return ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            if ((null == hierarchy) || (0 == cookie))
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
            });
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
            canonicalName = getItemName(itemId);
            string extension = System.IO.Path.GetExtension(canonicalName);
            return (0 == string.Compare(extension, ".prg", StringComparison.OrdinalIgnoreCase));
        }
		
        private string getItemName(uint itemId)
        {
            Guid typeGuid = Guid.Empty;
            int hr = VSConstants.S_OK;
            try
            {
                if (hierarchy == null)
                    return string.Empty;
                ThreadHelper.JoinableTaskFactory.Run(async () =>
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    hr = hierarchy.GetGuidProperty(itemId, (int)__VSHPROPID.VSHPROPID_TypeGuid, out typeGuid);
                });
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
                return string.Empty;
            }

            //hierarchy.GetProperty(itemId, (int)__VSHPROPID.VSHPROPID_Name, out var name);

            // This item is a file; find if it is a PRG file.
            string canonicalName = null;
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                hr = hierarchy.GetCanonicalName(itemId, out canonicalName);
            });
            return canonicalName;
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
                string itemName = getItemName(itemId);
                // IsPrgFile(currentItem, out itemName);
                if ((null != onItemAdded) && ! string.IsNullOrEmpty(itemName))
                {
                    var file = XSolution.FindFile(itemName);
                    HierarchyEventArgs args = new HierarchyEventArgs(currentItem, file);
                    onItemAdded(hierarchy, args);
                }

                // NOTE: At the moment we skip the nested hierarchies, so here  we look for the 
                // children of this node.
                // Before looking at the children we have to make sure that the enumeration has not
                // side effects to avoid unexpected behavior.
                bool canScanSubitems = true;
                int hr = 0;
                object propertyValue = null;
                ThreadHelper.JoinableTaskFactory.Run(async () =>
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    hr = hierarchy.GetProperty(currentItem, (int)__VSHPROPID.VSHPROPID_HasEnumerationSideEffects, out propertyValue);
                });

                if (VSConstants.S_OK == hr && propertyValue is bool ok)
                {
                    canScanSubitems = !ok;
                }
                // If it is allow to look at the sub-items of the current one, lets do it.
                if (canScanSubitems)
                {
                    object child = null;
                    ThreadHelper.JoinableTaskFactory.Run(async () =>
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        hr = hierarchy.GetProperty(currentItem, (int)__VSHPROPID.VSHPROPID_FirstChild, out child);
                    });
                    if (VSConstants.S_OK == hr)
                    {
                        // There is a sub-item, call this same function on it.
                        InternalScanHierarchy(GetItemId(child));
                    }
                }

                // Move the current item to its first visible sibling.
                object sibling = null;
                ThreadHelper.JoinableTaskFactory.Run(async () =>
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    if (hierarchy != null)
                        hr = hierarchy.GetProperty(currentItem, (int)__VSHPROPID.VSHPROPID_NextSibling, out sibling);
                });
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
