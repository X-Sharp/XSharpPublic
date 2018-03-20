/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation. 
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
 * copy of the license can be found in the License.html file at the root of this distribution. If 
 * you cannot locate the Apache License, Version 2.0, please send an email to 
 * ironpy@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Apache License, Version 2.0.
 *
 * You must not remove this notice, or any other, from this software.
 *
 * ***************************************************************************/
/*****************************************************************************
* XSharp.BV
* Based on IronStudio/IronPythonTools/IronPythonTools/Navigation
*
****************************************************************************/

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Threading;

using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;

using VSConstants = Microsoft.VisualStudio.VSConstants;
using IServiceProvider = System.IServiceProvider;
using Microsoft.VisualStudio.TextManager.Interop;
using XSharpModel;

namespace XSharp.Project
{

    /// <summary>
    /// This is the Interface that defines the service that search for Files and expose them.
    /// </summary>
    [Guid(XSharpConstants.LibraryManagerService)]
    public interface IXSharpLibraryManager
    {
        void RegisterHierarchy(IVsHierarchy hierarchy, XProject Prj, XSharpProjectNode ProjectNode);
        void UnregisterHierarchy(IVsHierarchy hierarchy);
        void RegisterLineChangeHandler(uint document, TextLineChangeEvent lineChanged, Action<IVsTextLines> onIdle);
    }

    public delegate void TextLineChangeEvent(object sender, TextLineChange[] changes, int last);

    /// <summary>
    /// This is the implementation of the service.
    /// </summary>
    [Guid(XSharpConstants.LibraryManager)]
    internal class XSharpLibraryManager : IXSharpLibraryManager, IVsRunningDocTableEvents, IDisposable
    {

        /// <summary>
        /// Class storing the data about a parsing task on a module.
        /// A module is a source file, so here we use the file name as UID
        /// </summary>
        private class LibraryTask
        {

            private string fileName;
            //private string text;
            private XSharpModuleId moduleId;
            //private object _sourceLock;

            public LibraryTask(string fileName, XSharpModuleId ModuleID)
            {
                this.fileName = fileName;
                this.moduleId = ModuleID;
                //this.text = text;
            }

            public string FileName
            {
                get { return fileName; }
            }
            public XSharpModuleId ModuleID
            {
                get { return moduleId; }
                set { moduleId = value; }
            }
            //public string Text
            //{
            //    get { return text; }
            //}
            //public object sourceLock
            //{
            //    get { return _sourceLock; }
            //    set { _sourceLock = value; }
            //}
        }


        private IServiceProvider provider;
        private uint objectManagerCookie;
        private uint runningDocTableCookie;
        private Dictionary<uint, TextLineEventListener> documents;
        private Dictionary<IVsHierarchy, HierarchyListener> hierarchies;
        private MultiValueDictionary<XSharpModuleId, XSharpLibraryNode> files;
        private Library library;


        private Thread walkerThread;
        private ManualResetEvent requestPresent;
        private ManualResetEvent shutDownStarted;
        private Queue<LibraryTask> requests;

        //private String _defaultNameSpace;

        //public String DefaultNameSpace
        //{
        //    get
        //    {
        //        return this._defaultNameSpace;
        //    }
        //}

        public XSharpLibraryManager(IServiceProvider provider)
        {
            this.provider = provider;
            //
            documents = new Dictionary<uint, TextLineEventListener>();
            hierarchies = new Dictionary<IVsHierarchy, HierarchyListener>();
            library = new Library(new Guid(XSharpConstants.Library));
            library.LibraryCapabilities = (_LIB_FLAGS2)_LIB_FLAGS.LF_PROJECT;
            // A Dictionary with :
            // ModuleId : The ID of a File in the Project Hierarchy
            // A library Node
            // --> For a single ModuleId we can have Multiple Library Node
            files = new MultiValueDictionary<XSharpModuleId, XSharpLibraryNode>();
            //
            requests = new Queue<LibraryTask>();
            requestPresent = new ManualResetEvent(false);
            shutDownStarted = new ManualResetEvent(false);
            // Changes in the Model ??
            walkerThread = new Thread(new ThreadStart(WalkerThread));
            walkerThread.Start();
        }

        private void RegisterForRDTEvents()
        {
            if (0 != runningDocTableCookie)
            {
                return;
            }
            IVsRunningDocumentTable rdt = provider.GetService(typeof(SVsRunningDocumentTable)) as IVsRunningDocumentTable;
            if (null != rdt)
            {
                // Do not throw here in case of error, simply skip the registration.
                rdt.AdviseRunningDocTableEvents(this, out runningDocTableCookie);
            }
        }
        private void UnregisterRDTEvents()
        {
            if (0 == runningDocTableCookie)
            {
                return;
            }
            IVsRunningDocumentTable rdt = provider.GetService(typeof(SVsRunningDocumentTable)) as IVsRunningDocumentTable;
            if (null != rdt)
            {
                // Do not throw in case of error.
                rdt.UnadviseRunningDocTableEvents(runningDocTableCookie);
            }
            runningDocTableCookie = 0;
        }

        #region IDisposable Members
        public void Dispose()
        {
            // Make sure that the parse thread can exit.
            if (null != shutDownStarted)
            {
                shutDownStarted.Set();
            }
            if ((null != walkerThread) && walkerThread.IsAlive)
            {
                walkerThread.Join(500);
                if (walkerThread.IsAlive)
                {
                    walkerThread.Abort();
                }
                walkerThread = null;
            }

            requests.Clear();

            // Dispose all the listeners.
            foreach (HierarchyListener listener in hierarchies.Values)
            {
                listener.Dispose();
            }
            hierarchies.Clear();

            foreach (TextLineEventListener textListener in documents.Values)
            {
                textListener.Dispose();
            }
            documents.Clear();

            // Remove this library from the object manager.
            if (0 != objectManagerCookie)
            {
                IVsObjectManager2 mgr = provider.GetService(typeof(SVsObjectManager)) as IVsObjectManager2;
                if (null != mgr)
                {
                    mgr.UnregisterLibrary(objectManagerCookie);
                }
                objectManagerCookie = 0;
            }

            // Unregister this object from the RDT events.
            //UnregisterRDTEvents();

            // Dispose the events used to syncronize the threads.
            if (null != requestPresent)
            {
                requestPresent.Close();
                requestPresent = null;
            }
            if (null != shutDownStarted)
            {
                shutDownStarted.Close();
                shutDownStarted = null;
            }
        }
        #endregion

        #region IXSharpLibraryManager
        /// <summary>
        /// Called when a project is loaded
        /// </summary>
        /// <param name="hierarchy"></param>
        public void RegisterHierarchy(IVsHierarchy hierarchy, XProject Prj, XSharpProjectNode ProjectNode)
        {
            // No Hierarchy or... Hierarchy already registered ?
            if ((null == hierarchy) || hierarchies.ContainsKey(hierarchy))
            {
                return;
            }
            // 
            if (0 == objectManagerCookie)
            {
                IVsObjectManager2 objManager = provider.GetService(typeof(SVsObjectManager)) as IVsObjectManager2;
                if (null == objManager)
                {
                    return;
                }
                Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(
                    objManager.RegisterSimpleLibrary(library, out objectManagerCookie));
            }
            // The project is the Root of the Library
            XSharpLibraryProject prjNode = new XSharpLibraryProject(Prj, hierarchy);
            library.AddNode(prjNode);

            //this._defaultNameSpace = prjNode.DefaultNameSpace;
            //Define Callback
            ProjectNode.ProjectModel.FileWalkComplete = new XProject.OnFileWalkComplete(OnFileWalkComplete);

            // Attach a listener to the Project/Hierachy,so any change is raising an event
            HierarchyListener listener = new HierarchyListener(hierarchy);
            //listener.OnAddItem += new EventHandler<HierarchyEventArgs>(OnNewFile);
            listener.OnDeleteItem += new EventHandler<HierarchyEventArgs>(OnDeleteFile);
            listener.StartListening();
            hierarchies.Add(hierarchy, listener);
            // and ask for any change in the files that are opened in Source editor.
            RegisterForRDTEvents();
        }

        public void UnregisterHierarchy(IVsHierarchy hierarchy)
        {
            if ((null == hierarchy) || !hierarchies.ContainsKey(hierarchy))
            {
                return;
            }
            // Retrieve the listener for that Tree/Hierarchy
            HierarchyListener listener = hierarchies[hierarchy];
            if (null != listener)
            {
                listener.Dispose();
            }
            hierarchies.Remove(hierarchy);
            if (0 == hierarchies.Count)
            {
                UnregisterRDTEvents();
            }
            // Now remove all nodes
            // for all files
            lock (files)
            {
                XSharpModuleId[] keys = new XSharpModuleId[files.Keys.Count];
                // Get all Keys (ModuleId)
                files.Keys.CopyTo(keys, 0);
                foreach (XSharpModuleId id in keys)
                {
                    // The file is owned by the Hierarchy ?
                    if (hierarchy.Equals(id.Hierarchy))
                    {
                        HashSet<XSharpLibraryNode> values = null;
                        // Ok, now remove ALL nodes for that key
                        if (files.TryGetValue(id, out values))
                        {
                            foreach (XSharpLibraryNode node in values)
                            {
                                if (node.parent != null)
                                {
                                    node.parent.RemoveNode(node);
                                }
                            }
                        }
                        // and then remove the key
                        files.Remove(id);
                    }
                }
            }
            //
            LibraryNode prjNode = this.library.SearchHierarchy(hierarchy);
            if (prjNode is XSharpLibraryProject)
            {
                library.RemoveNode(prjNode);
            }
            // Remove the document listeners.
            uint[] docKeys = new uint[documents.Keys.Count];
            documents.Keys.CopyTo(docKeys, 0);
            foreach (uint id in docKeys)
            {
                TextLineEventListener docListener = documents[id];
                if (hierarchy.Equals(docListener.FileID.Hierarchy))
                {
                    documents.Remove(id);
                    docListener.Dispose();
                }
            }
        }

        public void RegisterLineChangeHandler(uint document,
            TextLineChangeEvent lineChanged, Action<IVsTextLines> onIdle)
        {
            documents[document].OnFileChangedImmediate += delegate (object sender, TextLineChange[] changes, int fLast)
            {
                lineChanged(sender, changes, fLast);
            };
            documents[document].OnFileChanged += delegate (object sender, HierarchyEventArgs args)
            {
                onIdle(args.TextBuffer);
            };
        }

        #endregion

        #region Watcher Thread
        /// <summary>
        /// Main function of the parsing thread.
        /// This function waits on the queue of the parsing requests and build the parsing tree for
        /// a specific file. The resulting tree is built using LibraryNode objects so that it can
        /// be used inside the class view or object browser.
        /// </summary>
        private void WalkerThread()
        {
            const int waitTimeout = 500;
            // Define the array of events this function is interest in.
            WaitHandle[] eventsToWait = new WaitHandle[] { requestPresent, shutDownStarted };
            // Execute the tasks.
            while (true)
            {
                // Wait for a task or a shutdown request.
                int waitResult = WaitHandle.WaitAny(eventsToWait, waitTimeout, false);
                if (waitResult == 1)
                {
                    // The shutdown of this component is started, so exit the thread.
                    return;
                }
                if (waitResult == WaitHandle.WaitTimeout)
                {
                    continue;
                }
                //
                LibraryTask task = null;
                lock (requests)
                {
                    if (0 != requests.Count)
                    {
                        task = requests.Dequeue();
                    }
                    if (0 == requests.Count)
                    {
                        requestPresent.Reset();
                    }
                }
                if (null == task)
                {
                    continue;
                }
                //
                XFile scope = null;
                if (System.IO.File.Exists(task.FileName))
                {
                    //scope = ScopeWalker.GetScopesFromFile(task.FileName);
                    scope = XSharpModel.XSolution.FindFile(task.FileName);
                    if (scope == null)
                        continue;
                }
                // If the file already exist
                lock (files)
                {
                    // These are the existing Modules
                    XSharpModuleId[] aTmp = new XSharpModuleId[files.Keys.Count];
                    files.Keys.CopyTo(aTmp, 0);
                    // Does this module already exist ?
                    XSharpModuleId found = Array.Find<XSharpModuleId>(aTmp, (x => x.Equals(task.ModuleID)));
                    if (found != null)
                    {
                        // Doesn't it have the same members?
                        if (found.ContentHashCode == task.ModuleID.ContentHashCode)
                            continue;
                        //
                        HashSet<XSharpLibraryNode> values = null;
                        // Ok, now remove ALL nodes for that key
                        if (files.TryGetValue(task.ModuleID, out values))
                        {
                            foreach (XSharpLibraryNode node in values)
                            {
                                if (node.Freeing(task.ModuleID.ItemID) == 0)
                                    if (node.parent != null)
                                    {
                                        node.parent.RemoveNode(node);
                                    }
                            }
                            // and then remove the key
                            files.Remove(task.ModuleID);
                        }
                    }
                    //
                    LibraryNode prjNode = this.library.SearchHierarchy(task.ModuleID.Hierarchy);
                    if (prjNode is XSharpLibraryProject)
                    {
                        //
                        CreateModuleTree((XSharpLibraryProject)prjNode, scope, task.ModuleID);
                        //
                        prjNode.updateCount += 1;
                        //this.prjNode.AddNode(node);
                        //library.AddNode(node);
                        this.library.Refresh();
                    }
                }
            }
        }

        private void CreateModuleTree(XSharpLibraryProject prjNode, XFile scope, XSharpModuleId moduleId)
        {
            if ((null == scope))
            {
                return;
            }
            // Retrieve all Types
            var elements = scope.TypeList;
            // 
            // First search for NameSpaces
            foreach (KeyValuePair<string, XType> pair in elements)
            {
                XType xType = pair.Value;
                if (xType.Kind == Kind.Namespace)
                {
                    // Does that NameSpave already exist ?
                    // Search for the corresponding NameSpace
                    XSharpLibraryNode newNode;
                    LibraryNode nsNode = prjNode.SearchNameSpace(xType.Name);
                    if (nsNode is XSharpLibraryNode)
                    {
                        newNode = (XSharpLibraryNode)nsNode;
                        newNode.Depends(moduleId.ItemID);
                    }
                    else
                    {
                        newNode = new XSharpLibraryNode(xType, "", moduleId.Hierarchy, moduleId.ItemID);

                        // NameSpaces are always added to the root.
                        prjNode.AddNode(newNode);
                        newNode.parent = prjNode;
                    }
                    // Handle Global Scope here
                    // It contains Function/Procedure/etc...
                    if (newNode.Name == "(Global Scope)")
                    {
                        CreateGlobalTree(newNode, xType, moduleId);
                    }
                    lock (files)
                    {
                        files.Add(moduleId, newNode);
                    }
                }
            }

            // Now, look for Classes
            foreach (KeyValuePair<string, XType> pair in elements)
            {
                XType xType = pair.Value;
                // Is it a kind of Type ?
                if ((xType.Kind == Kind.Class) || (xType.Kind == Kind.Structure) ||
                      (xType.Kind == Kind.Union) || (xType.Kind == Kind.VOStruct))
                {
                    string nSpace = prjNode.DefaultNameSpace;
                    if (!String.IsNullOrEmpty(xType.NameSpace))
                        nSpace = xType.NameSpace;
                    // Search for the corresponding NameSpace
                    LibraryNode nsNode = prjNode.SearchNameSpace(nSpace);
                    if (nsNode is XSharpLibraryNode)
                    {
                        XSharpLibraryNode xsNSNode = (XSharpLibraryNode)nsNode;
                        // So the Class node will belong to that NameSpace Node
                        // Now, try to check if such Type already exist
                        XSharpLibraryNode newNode;
                        LibraryNode newTmpNode;
                        newTmpNode = xsNSNode.SearchClass(xType.Name);
                        if (newTmpNode is XSharpLibraryNode)
                        {
                            newNode = (XSharpLibraryNode)newTmpNode;
                            newNode.Depends(moduleId.ItemID);
                        }
                        else
                        {
                            newNode = new XSharpLibraryNode(xType, "", moduleId.Hierarchy, moduleId.ItemID);
                            nsNode.AddNode(newNode);
                            newNode.parent = nsNode;
                        }
                        // Insert Members
                        CreateMembersTree(newNode, xType, moduleId);
                        //
                        lock (files)
                        {
                            files.Add(moduleId, newNode);
                        }
                    }
                    else
                    {
                        // Not found !?
                    }
                }
            }
        }

        private void CreateGlobalTree(LibraryNode current, XType scope, XSharpModuleId moduleId)
        {
            if (null == scope)
            {
                return;
            }

            foreach (XTypeMember member in scope.Members)
            {
                XSharpLibraryNode newNode = new XSharpLibraryNode(member, "", moduleId.Hierarchy, moduleId.ItemID);
                // Functions ?
                if ((newNode.NodeType & LibraryNode.LibraryNodeType.Members) != LibraryNode.LibraryNodeType.None)
                {
                    current.AddNode(newNode);
                    newNode.parent = current;
                    lock (files)
                    {
                        files.Add(moduleId, newNode);
                    }
                }
            }
        }

        private void CreateMembersTree(LibraryNode current, XType scope, XSharpModuleId moduleId)
        {
            if (null == scope)
            {
                return;
            }

            foreach (XTypeMember member in scope.Members)
            {
                XSharpLibraryNode newNode = new XSharpLibraryNode(member, "", moduleId.Hierarchy, moduleId.ItemID);

                // The classes are always added to the root node, the functions to the
                // current node.
                if ((newNode.NodeType & LibraryNode.LibraryNodeType.Members) != LibraryNode.LibraryNodeType.None)
                {
                    current.AddNode(newNode);
                    newNode.parent = current;
                    lock (files)
                    {
                        files.Add(moduleId, newNode);
                    }
                }
            }
        }
        #endregion

        // 
        /// <summary>
        /// We come here after a FileWlak
        /// </summary>
        private void OnFileWalkComplete(XFile xfile)
        {
            // Retrieve the corresponding node
            XSharpProjectNode prjNode = (XSharpProjectNode)xfile.Project.ProjectNode;
            Microsoft.VisualStudio.Project.HierarchyNode node = prjNode.FindURL(xfile.FullPath);
            if (node != null)
            {
                XSharpModuleId module = new XSharpModuleId(prjNode.InteropSafeHierarchy, node.ID);
                module.ContentHashCode = xfile.ContentHashCode;
                CreateParseRequest(xfile.FullPath, module);
            }
        }

        private void CreateParseRequest(string file, XSharpModuleId id)
        {
            LibraryTask task = new LibraryTask(file, id);
            task.ModuleID = id;
            lock (requests)
            {
                requests.Enqueue(task);
            }
            requestPresent.Set();
        }

        #region Hierarchy Events
        /// <summary>
        /// Called when a new file is added in the Project Hierarchy
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="args"></param>
        private void OnNewFile(object sender, HierarchyEventArgs args)
        {
            IVsHierarchy hierarchy = sender as IVsHierarchy;
            if (null == hierarchy)
            {
                return;
            }
            string fileText = null;
            if (null != args.TextBuffer)
            {
                int lastLine;
                int lastIndex;
                int hr = args.TextBuffer.GetLastLineIndex(out lastLine, out lastIndex);
                if (Microsoft.VisualStudio.ErrorHandler.Failed(hr))
                {
                    return;
                }
                hr = args.TextBuffer.GetLineText(0, 0, lastLine, lastIndex, out fileText);
                if (Microsoft.VisualStudio.ErrorHandler.Failed(hr))
                {
                    return;
                }
            }
            CreateParseRequest(args.CanonicalName, new XSharpModuleId(hierarchy, args.ItemID));
        }

        /// <summary>
        /// Handle the deletion of a file in the Solution Hierarchy
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="args"></param>
        private void OnDeleteFile(object sender, HierarchyEventArgs args)
        {
            IVsHierarchy hierarchy = sender as IVsHierarchy;
            if (null == hierarchy)
            {
                return;
            }
            XSharpModuleId id = new XSharpModuleId(hierarchy, args.ItemID);
            lock (files)
            {
                HashSet<XSharpLibraryNode> values = null;
                // Ok, now remove ALL nodes for that key
                if (files.TryGetValue(id, out values))
                {
                    foreach (XSharpLibraryNode node in values)
                    {
                        if (node.Freeing(id.ItemID) == 0)
                        {
                            if (node.parent != null)
                            {
                                node.parent.RemoveNode(node);
                            }
                        }
                    }
                }
                // and then remove the key
                files.Remove(id);
            }
            //
        }
        #endregion

        #region IVsRunningDocTableEvents Members

        public int OnAfterAttributeChange(uint docCookie, uint grfAttribs)
        {
            if ((grfAttribs & (uint)(__VSRDTATTRIB.RDTA_MkDocument)) == (uint)__VSRDTATTRIB.RDTA_MkDocument)
            {
                IVsRunningDocumentTable rdt = provider.GetService(typeof(SVsRunningDocumentTable)) as IVsRunningDocumentTable;
                if (rdt != null)
                {
                    uint flags, readLocks, editLocks, itemid;
                    IVsHierarchy hier;
                    IntPtr docData = IntPtr.Zero;
                    string moniker;
                    int hr;
                    try
                    {
                        hr = rdt.GetDocumentInfo(docCookie, out flags, out readLocks, out editLocks, out moniker, out hier, out itemid, out docData);
                        TextLineEventListener listner;
                        if (documents.TryGetValue(docCookie, out listner))
                        {
                            listner.FileName = moniker;
                        }
                    }
                    finally
                    {
                        if (IntPtr.Zero != docData)
                        {
                            Marshal.Release(docData);
                        }
                    }
                }
            }
            return VSConstants.S_OK;
        }

        public int OnAfterDocumentWindowHide(uint docCookie, IVsWindowFrame pFrame)
        {
            return VSConstants.S_OK;
        }

        public int OnAfterFirstDocumentLock(uint docCookie, uint dwRDTLockType, uint dwReadLocksRemaining, uint dwEditLocksRemaining)
        {
            return VSConstants.S_OK;
        }

        public int OnAfterSave(uint docCookie)
        {
            return VSConstants.S_OK;
        }

        public int OnBeforeDocumentWindowShow(uint docCookie, int fFirstShow, IVsWindowFrame pFrame)
        {
            // Check if this document is in the list of the documents.
            if (documents.ContainsKey(docCookie))
            {
                return VSConstants.S_OK;
            }
            // Get the information about this document from the RDT.
            IVsRunningDocumentTable rdt = provider.GetService(typeof(SVsRunningDocumentTable)) as IVsRunningDocumentTable;
            if (null != rdt)
            {
                // Note that here we don't want to throw in case of error.
                uint flags;
                uint readLocks;
                uint writeLoks;
                string documentMoniker;
                IVsHierarchy hierarchy;
                uint itemId;
                IntPtr unkDocData;
                int hr = rdt.GetDocumentInfo(docCookie, out flags, out readLocks, out writeLoks,
                                             out documentMoniker, out hierarchy, out itemId, out unkDocData);
                try
                {
                    if (Microsoft.VisualStudio.ErrorHandler.Failed(hr) || (IntPtr.Zero == unkDocData))
                    {
                        return VSConstants.S_OK;
                    }
                    // Check if the herarchy is one of the hierarchies this service is monitoring.
                    if (!hierarchies.ContainsKey(hierarchy))
                    {
                        // This hierarchy is not monitored, we can exit now.
                        return VSConstants.S_OK;
                    }

                    // Check the extension of the file to see if a listener is required.
                    string extension = System.IO.Path.GetExtension(documentMoniker);
                    if (0 != string.Compare(extension, ".prg", StringComparison.OrdinalIgnoreCase))
                    {
                        return VSConstants.S_OK;
                    }

                    // Create the module id for this document.
                    XSharpModuleId docId = new XSharpModuleId(hierarchy, itemId);

                    // Try to get the text buffer.
                    IVsTextLines buffer = Marshal.GetObjectForIUnknown(unkDocData) as IVsTextLines;

                    // Create the listener.
                    // So we are informed in case of Buffer change
                    TextLineEventListener listener = new TextLineEventListener(buffer, documentMoniker, docId);
                    // Set the event handler for the change event. Note that there is no difference
                    // between the AddFile and FileChanged operation, so we can use the same handler.
                    listener.OnFileChanged += new EventHandler<HierarchyEventArgs>(OnNewFile);
                    // Add the listener to the dictionary, so we will not create it anymore.
                    documents.Add(docCookie, listener);
                }
                finally
                {
                    if (IntPtr.Zero != unkDocData)
                    {
                        Marshal.Release(unkDocData);
                    }
                }
            }
            // Always return success.
            return VSConstants.S_OK;
        }

        public int OnBeforeLastDocumentUnlock(uint docCookie, uint dwRDTLockType, uint dwReadLocksRemaining, uint dwEditLocksRemaining)
        {
            if ((0 != dwEditLocksRemaining) || (0 != dwReadLocksRemaining))
            {
                return VSConstants.S_OK;
            }
            TextLineEventListener listener;
            if (!documents.TryGetValue(docCookie, out listener) || (null == listener))
            {
                return VSConstants.S_OK;
            }
            using (listener)
            {
                documents.Remove(docCookie);
                // Now make sure that the information about this file are up to date (e.g. it is
                // possible that Class View shows something strange if the file was closed without
                // saving the changes).
                HierarchyEventArgs args = new HierarchyEventArgs(listener.FileID.ItemID, listener.FileName);
                OnNewFile(listener.FileID.Hierarchy, args);
            }
            return VSConstants.S_OK;
        }

        #endregion

        public void OnIdle()
        {
            foreach (TextLineEventListener listener in documents.Values)
            {
                listener.OnIdle();
            }
        }
    }
}
