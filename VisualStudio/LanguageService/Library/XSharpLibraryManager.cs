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

using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using System;
using System.Linq;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Threading;
using XSharpModel;
using IServiceProvider = System.IServiceProvider;
using Microsoft.VisualStudio;
using System.Diagnostics;
using XSharp.Settings;
namespace XSharp.LanguageService
{

    /// <summary>
    /// This is the Interface that defines the service that search for Files and expose them.
    /// </summary>
    [Guid(XSharpConstants.LibraryManagerService)]
    public interface IXSharpLibraryManager
    {
        void RegisterHierarchy(IVsHierarchy hierarchy, XProject Prj, IXSharpProject ProjectNode);
        void UnregisterHierarchy(IVsHierarchy hierarchy);
        void RegisterLineChangeHandler(uint document, TextLineChangeEvent lineChanged, Action<IVsTextLines> onIdle);
    }

    public delegate void TextLineChangeEvent(object sender, TextLineChange[] changes, int last);

    /// <summary>
    /// This is the implementation of the service.
    /// </summary>
    [Guid(XSharpConstants.LibraryManager)]
    internal class XSharpLibraryManager : IXSharpLibraryManager, IDisposable
    {

        /// <summary>
        /// Class storing the data about a parsing task on a module.
        /// A module is a source file, so here we use the file name as UID
        /// </summary>
        [DebuggerDisplay("{FileName}")]
        private class LibraryTask
        {
            public string FileName { get; }
            public XSharpModuleId ModuleID { get; }
            public LibraryTask(string fileName, XSharpModuleId ModuleID)
            {
                this.FileName = fileName;
                this.ModuleID = ModuleID;
            }
        }


        private readonly IServiceProvider provider;
        private uint objectManagerCookie;
       // private uint runningDocTableCookie;
        private readonly Dictionary<IVsHierarchy, HierarchyListener> hierarchies;
        private readonly MultiValueDictionary<XSharpModuleId, XSharpLibraryNode> files;
        private readonly Dictionary<string, XSharpModuleId> filedict;
        private readonly Library library;
        private readonly Dictionary<string, XSharpLibraryProject> projectdict;


        private Thread updateTreeThread;
        private ManualResetEvent requestPresent;
        private ManualResetEvent shutDownStarted;
        private readonly Queue<LibraryTask> requests;
        private readonly Dictionary<string, LibraryTask> tasks;


        public XSharpLibraryManager(IServiceProvider provider)
        {
            this.provider = provider;
            //
            hierarchies = new Dictionary<IVsHierarchy, HierarchyListener>();
            library = new Library(new Guid(XSharpConstants.Library))
            {
                LibraryCapabilities = (_LIB_FLAGS2)_LIB_FLAGS.LF_PROJECT
            };
            // A Dictionary with :
            // ModuleId : The ID of a File in the Project Hierarchy
            // A library Node
            // --> For a single ModuleId we can have Multiple Library Node
            files = new MultiValueDictionary<XSharpModuleId, XSharpLibraryNode>();
            filedict = new Dictionary<string, XSharpModuleId>(StringComparer.OrdinalIgnoreCase);
            projectdict = new Dictionary<string, XSharpLibraryProject>(StringComparer.OrdinalIgnoreCase);
            //
            requests = new Queue<LibraryTask>();
            tasks = new Dictionary<string, LibraryTask>(StringComparer.OrdinalIgnoreCase);
            requestPresent = new ManualResetEvent(false);
            shutDownStarted = new ManualResetEvent(false);
            // Changes in the Model ??
            updateTreeThread = new Thread(new ThreadStart(UpdateTreeThread));
            updateTreeThread.Start();
        }

 
        #region IDisposable Members
        public void Dispose()
        {
            // Make sure that the parse thread can exit.
            if (null != shutDownStarted)
            {
                shutDownStarted.Set();
            }
            if ((null != updateTreeThread) && updateTreeThread.IsAlive)
            {
                updateTreeThread.Join(500);
                if (updateTreeThread.IsAlive)
                {
                    updateTreeThread.Abort();
                }
                updateTreeThread = null;
            }

            requests.Clear();
            filedict.Clear();

            // Dispose all the listeners.
            foreach (HierarchyListener listener in hierarchies.Values)
            {
                listener.Dispose();
            }
            hierarchies.Clear();
            // Remove this library from the object manager.
            if (0 != objectManagerCookie)
            {
                ThreadHelper.JoinableTaskFactory.Run(async () =>
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                    if (provider.GetService(typeof(SVsObjectManager)) is IVsObjectManager2 mgr)
                    {
                        mgr.UnregisterLibrary(objectManagerCookie);
                    }
                });
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
        public void RegisterHierarchy(IVsHierarchy hierarchy, XProject Prj, IXSharpProject ProjectNode)
        {
            // No Hierarchy or... Hierarchy already registered ?
            // disable classview for now
            if ( XSettings.DisableClassViewObjectView)
            {
                return;
            }

            if ((null == hierarchy) || hierarchies.ContainsKey(hierarchy))
            {
                return;
            }
            //
            if (0 == objectManagerCookie)
            {
                ThreadHelper.JoinableTaskFactory.Run(async () =>
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                    if (!(provider.GetService(typeof(SVsObjectManager)) is IVsObjectManager2 objManager))
                    {
                        return;
                    }
                    Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(
                        objManager.RegisterSimpleLibrary(library, out objectManagerCookie));
                });
            }
            // The project is the Root of the Library
            XSharpLibraryProject prjNode = new XSharpLibraryProject(Prj, hierarchy);
            library.AddNode(prjNode);
            projectdict.Add(Prj.FileName, prjNode);

            //this._defaultNameSpace = prjNode.DefaultNameSpace;
            //Define Callback
            var model = XSolution.FindProject(prjNode.Name);
            if (model != null)
            {
                model.FileWalkComplete += OnFileWalkComplete;
                model.ProjectWalkComplete += OnProjectWalkComplete;
            }
            // Attach a listener to the Project/Hierarchy,so any change is raising an event
            HierarchyListener listener = new HierarchyListener(hierarchy);
            listener.OnAddItem += new EventHandler<HierarchyEventArgs>(OnNewFile);
            listener.OnDeleteItem += new EventHandler<HierarchyEventArgs>(OnDeleteFile);
            listener.StartListening();
            hierarchies.Add(hierarchy, listener);
            // and ask for any change in the files that are opened in Source editor.
            //RegisterForRDTEvents();
        }


        private void AddNode(XSharpModuleId moduleId, XSharpLibraryNode newNode)
        {
            lock (files)
            {
                files.Add(moduleId, newNode);
                if (!filedict.ContainsKey(moduleId.Path))
                {
                    filedict.Add(moduleId.Path, moduleId);
                }
            }
        }

        private void RemoveNode(XSharpModuleId moduleId)
        {
            lock (files)
            {
                files.Remove(moduleId);
                if (filedict.ContainsKey(moduleId.Path))
                    filedict.Remove(moduleId.Path);
            }
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
                listener.StopListening();
                listener.Dispose();
                listener = null;
            }
            hierarchies.Remove(hierarchy);
            XSharpModuleId[] keys;
            lock (files)
            {
                keys = files.Keys.ToArray();
            }

            foreach (XSharpModuleId id in keys)
            {
                // The file is owned by the Hierarchy ?
                if (hierarchy.Equals(id.Hierarchy))
                {
                    // Ok, now remove ALL nodes for that key
                    if (files.TryGetValue(id, out HashSet<XSharpLibraryNode> values))
                    {
                        foreach (XSharpLibraryNode node in values)
                        {
                            if (node.parent != null)
                            {
                                node.parent.RemoveNode(node);
                            }
                        }
                    }
                    RemoveNode(id);
                }
            }
            //
            LibraryNode lnode = this.library.SearchHierarchy(hierarchy);
            if (lnode is XSharpLibraryProject prjNode)
            {
                library.RemoveNode(prjNode);
                if (projectdict.ContainsKey(prjNode.FullPath))
                {
                    projectdict.Remove(prjNode.FullPath);
                }
            }

        }
        public void RegisterLineChangeHandler(uint document,
            TextLineChangeEvent lineChanged, Action<IVsTextLines> onIdle)
        {
        }
        #endregion

        #region Watcher Thread
        /// <summary>
        /// Main function of the parsing thread.
        /// This function waits on the queue of the parsing requests and build the parsing tree for
        /// a specific file. The resulting tree is built using LibraryNode objects so that it can
        /// be used inside the class view or object browser.
        /// </summary>

        private XSharpModuleId FindFileByFileName(string name)
        {
            if (filedict.TryGetValue(name, out var value))
                return value;
            return null;
        }


        private void ProcessTask(LibraryTask task, XFile xfile, bool forceRefresh)
        {
            // If the file already exist
            XSharpModuleId found = FindFileByFileName(task.FileName);
            if (found != null)
            {
                //Logger.Information("Library scanning: "+task.FileName);
                // Doesn't it have the same members?
                // if (found.ContentHashCode == task.ModuleID.ContentHashCode)
                //    continue;
                //
                // Ok, now remove ALL nodes for that key
                if (files.TryGetValue(task.ModuleID, out HashSet<XSharpLibraryNode> values))
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
                    RemoveNode(task.ModuleID);
                }
            }
            //
            LibraryNode prjNode = this.library.SearchHierarchy(task.ModuleID.Hierarchy);
            if (prjNode is XSharpLibraryProject project)
            {
                CreateModuleTree(project, xfile, task.ModuleID);
                prjNode.updateCount += 1;
                if (forceRefresh)
                {
                    this.library.Refresh();
                    //XSolution.SetStatusBarAnimation(false, 0);
                }
            }
        }

        private void UpdateTreeThread()
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
                if (!XSharpModel.XSolution.IsOpen && requests.Count > 0)
                {
                    requests.Clear();
                    return;
                }
                //

                LibraryTask task = null;
                bool forceRefresh = false;
                lock (requests)
                {
                    if (0 != requests.Count)
                    {
                        //if (requests.Count % 25 == 0)
                        //{
                        //    Logger.Information("Remaining requests " + requests.Count.ToString());
                        //}
                        task = requests.Dequeue();
                        if (tasks.ContainsKey(task.FileName))
                            tasks.Remove(task.FileName);
                        if (requests.Count == 0)
                        {
                            forceRefresh = true; 
                        }
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
                if (System.IO.File.Exists(task.FileName))
                {
                    var xfile = XSharpModel.XSolution.FindFile(task.FileName);
                    if (xfile != null )
                    {
                        ProcessTask(task, xfile, forceRefresh);
                    }
                }
            }
        }

        private void AddNamespace(XSourceTypeSymbol xType, XSharpLibraryProject prjNode, XSharpModuleId moduleId)
        {
            string nodeName = xType.Name;
            XSharpLibraryNode node;
            if (String.IsNullOrEmpty(xType.Name))
            {
                nodeName = "Default NameSpace";
            }
            // Does that NameSpace already exist ?
            // Search for the corresponding NameSpace
            var nsNode = prjNode.SearchNameSpace(nodeName);
            if (nsNode is XSharpLibraryNode node1)
            {
                node = node1;
                node.Depends(moduleId.ItemID);
            }
            else
            {
                node = new XSharpLibraryNode(xType, nodeName, moduleId.Hierarchy, moduleId.ItemID);

                // NameSpaces are always added to the root.
                prjNode.AddNode(node);
                node.parent = prjNode;
            }
            AddNode(moduleId, node);
            return ;
        }

        private void AddType(XSourceTypeSymbol xType, XSharpLibraryProject prjNode, XSharpModuleId moduleId)
        {
            LibraryNode nsNode;
            XSharpLibraryNode newNode;
            string nSpace = "Default NameSpace"; // prjNode.DefaultNameSpace;
            if (!String.IsNullOrEmpty(xType.Namespace))
                nSpace = xType.Namespace;
            // Search for the corresponding NameSpace
            nsNode = prjNode.SearchNameSpace(nSpace);
            if (nsNode == null)
            {
                // should we search for a class ????
                nsNode = prjNode.SearchClass(nSpace);
            }
            if (nsNode is XSharpLibraryNode xsNSNode)
            {
                // So the Class node will belong to that NameSpace Node
                // Now, try to check if such Type already exist
                LibraryNode newTmpNode;
                newTmpNode = xsNSNode.SearchClass(xType.FullName);
                if (newTmpNode is XSharpLibraryNode node)
                {
                    newNode = node;
                    newNode.Depends(moduleId.ItemID);
                }
                else
                {
                    newNode = new XSharpLibraryNode(xType, "", moduleId.Hierarchy, moduleId.ItemID);
                    nsNode.AddNode(newNode);
                    newNode.parent = nsNode;
                }
                //
                // Insert Members
                CreateMembersTree(newNode, xType, moduleId);
                //
                AddNode(moduleId, newNode);
            }
            else
            {
                // Not found !?
            }
            return;
        }

        private void CreateModuleTree(XSharpLibraryProject prjNode, XFile file, XSharpModuleId moduleId)
        {
            if (null == file || XSolution.IsClosing)
            {
                return;
            }

            if (!file.HasCode)
                return;
            //Logger.Information("CreateModuleTree " + file.FullPath);
            //
            XSharpLibraryNode newNode;
            LibraryNode nsNode;
            try
            {
                // Retrieve all Types
                // !!! WARNING !!! The XFile object (scope) comes from the DataBase
                // We should retrieve TypeList from the DataBase.....
                var namespaces = XSharpModel.XDatabase.GetNamespacesInFile(file.Id.ToString(), true);
                if (namespaces == null)
                    return;
                //
                var elements = XDbResultHelpers.BuildTypesInFile(file, namespaces);
                // First search for NameSpaces
                foreach (XSourceTypeSymbol xType in elements)
                {

                    if (xType.Kind == Kind.Namespace)
                    {
                        AddNamespace(xType, prjNode, moduleId);
                    }
                }

                // Retrieve Classes from the file
                var types = XSharpModel.XDatabase.GetTypesInFile(file.Id.ToString());
                if (types == null)
                    return;
                var model = file.Project;
                model.FileWalkComplete -= OnFileWalkComplete;
                elements = XDbResultHelpers.BuildFullTypesInFile(file, types);
                model.FileWalkComplete += OnFileWalkComplete;
                // Now, look for Classes
                foreach (XSourceTypeSymbol xType in elements)
                {
                    if ((xType.Kind.IsType()))
                    {
                        AddType(xType, prjNode, moduleId);
                    }
                }

                // Ok, we need a (Global Scope) Now
                nsNode = prjNode.SearchNameSpace(XLiterals.GlobalName);
                if (nsNode is XSharpLibraryNode node)
                {
                    newNode = node;
                    newNode.Depends(moduleId.ItemID);
                }
                else
                {
                    newNode = new XSharpLibraryNode(XLiterals.GlobalName, LibraryNode.LibraryNodeType.Namespaces, "");
                    // NameSpaces are always added to the root.
                    prjNode.AddNode(newNode);
                    newNode.parent = prjNode;
                }
                //
                AddNode(moduleId, newNode);

                // Finally, any Function/Procedure ??
                var funcs = XSharpModel.XDatabase.GetFunctions(file.Id.ToString());
                IList<XSourceMemberSymbol> elts;
                if (funcs != null)
                {
                    elts = XDbResultHelpers.BuildFullFuncsInFile(file, funcs);
                    CreateGlobalTree(newNode, elts, moduleId);
                }

            }
            catch (Exception e)
            {
                Debug.WriteLine(e.Message);
            }
        }

        private void CreateGlobalTree(LibraryNode globalScope, IList<XSourceMemberSymbol> XMembers, XSharpModuleId moduleId)
        {
            if (XSolution.IsClosing)
            {
                return;
            }
            foreach (XSourceMemberSymbol member in XMembers)
            {
                XSharpLibraryNode newNode = new XSharpLibraryNode(member, "", moduleId.Hierarchy, moduleId.ItemID);
                // Functions ?
                if (newNode.NodeType.HasFlag(LibraryNode.LibraryNodeType.Members) )
                {
                    globalScope.AddNode(newNode);
                    newNode.parent = globalScope;
                    AddNode(moduleId, newNode);
                }
            }
        }

        private void CreateMembersTree(LibraryNode current, XSourceTypeSymbol scope, XSharpModuleId moduleId)
        {
            if (null == scope || XSolution.IsClosing)
            {
                return;
            }

            foreach (XSourceMemberSymbol member in scope.Members)
            {
                if (member.File.FullPath != moduleId.Path)
                    continue;
                XSharpLibraryNode newNode = new XSharpLibraryNode(member, "", moduleId.Hierarchy, moduleId.ItemID);

                // The classes are always added to the root node, the functions to the current node.
                if (newNode.NodeType.HasFlag(LibraryNode.LibraryNodeType.Members) )
                {
                    current.AddNode(newNode);
                    newNode.parent = current;
                    AddNode(moduleId, newNode);
                }
            }
        }
        #endregion

        //
        /// <summary>
        /// We come here : After a Project load (xFile == NULL), or after a File Save (xFile == the Saved file)
        /// </summary>
        private void OnFileWalkComplete(XFile xfile)
        {
            if (XSolution.IsClosing || xfile == null || !xfile.HasCode || xfile.Virtual)
            {
                return;
            }
            //Logger.Information("OnFileWalkComplete " + xfile.FullPath);
            if (filedict.TryGetValue(xfile.FullPath, out var module))
            {
                CreateUpdateTreeRequest(xfile.SourcePath, module.Hierarchy, module.ItemID);
            }
            else
            {
                if (projectdict.TryGetValue(xfile.Project.FileName, out var prjNode))
                {
                    var hierarchyId = prjNode.ownerHierarchy;
                    uint itemid = 0;
                    int hr = 0;
                    ThreadHelper.JoinableTaskFactory.Run(async () =>
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        hr = hierarchyId.ParseCanonicalName(xfile.FullPath, out itemid);
                    });

                    if (itemid != 0 && hr == VSConstants.S_OK)
                    {
                        var args = new HierarchyEventArgs(itemid, xfile.FullPath);
                        OnNewFile(hierarchyId, args);
                    }
                }
            }
            return;
        }

        private void OnProjectWalkComplete(XProject xsProject)
        {
            if (xsProject != null)
            {
                if (projectdict.TryGetValue(xsProject.FileName, out var prjNode))
                {
					// Add all source files.
                    var hierarchyId = prjNode.ownerHierarchy;
                    var listener = hierarchies[hierarchyId];
                    foreach (var path in xsProject.SourceFiles)
                    {
                        var xfile = xsProject.FindXFile(path);
                        uint itemid = 0;
                        int hr = 0;
                        ThreadHelper.JoinableTaskFactory.Run(async ( )=>
                        {
                            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                            hr = hierarchyId.ParseCanonicalName(xfile.FullPath, out itemid);
                        });
                        if (itemid != 0 && hr == VSConstants.S_OK)
                        {
                            var args = new HierarchyEventArgs(itemid, xfile.FullPath);
                            OnNewFile(hierarchyId, args);
                        }
                    }

                    listener.StartListening();
                }
            }
        }

        private void CreateUpdateTreeRequest(string file, IVsHierarchy owner, uint itemId)
        {
            if (XSolution.IsClosing)
                return;
            lock (requests)
            {
                if (! tasks.ContainsKey(file))
                {
                    //Logger.Information("CreateUpdateTreeRequest Library Enqueue " + file);
                    var id = new XSharpModuleId(owner, itemId, file);
                    LibraryTask task = new LibraryTask(file, id);
                    requests.Enqueue(task);
                    tasks.Add(file, task);
                }
            }
            requestPresent.Set();
        }

        //private string getFileNameFromCookie(uint docCookie)
        //{

        //    string fileName = "";
        //    ThreadHelper.JoinableTaskFactory.Run(async ( )=>
        //    {
        //        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();


        //        if (provider.GetService(typeof(SVsRunningDocumentTable)) is IVsRunningDocumentTable rdt)
        //        {
        //            IntPtr docData = IntPtr.Zero;
        //            try
        //            {
        //                var hr = rdt.GetDocumentInfo(docCookie, out uint flags, out uint readLocks, out uint editLocks, out fileName, out IVsHierarchy hier, out uint itemid, out docData);
        //                if (hierarchies.ContainsKey(hier))
        //                {

        //                }

        //            }
        //            finally
        //            {
        //                if (IntPtr.Zero != docData)
        //                {
        //                    Marshal.Release(docData);
        //                }
        //            }
        //        }
        //    });
        //    return fileName;

        //}

        #region Hierarchy Events
        /// <summary>
        /// Called when a new file is added in the Project Hierarchy
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="args"></param>
        private void OnNewFile(object sender, HierarchyEventArgs args)
        {
            ThreadHelper.JoinableTaskFactory.Run(async ( )=>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                if (!(sender is IVsHierarchy hierarchy))
                {
                    return;
                }
                var type = XFileTypeHelpers.GetFileType(args.CanonicalName);
                if (type == XFileType.SourceCode)
                {
                    CreateUpdateTreeRequest(args.CanonicalName, hierarchy, args.ItemID);
                }
            });
        }

        /// <summary>
        /// Handle the deletion of a file in the Solution Hierarchy
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="args"></param>
        private void OnDeleteFile(object sender, HierarchyEventArgs args)
        {
            ThreadHelper.JoinableTaskFactory.Run(async ( )=>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                if (!(sender is IVsHierarchy hierarchy))
                {
                    return;
                }
                XSharpModuleId id = new XSharpModuleId(hierarchy, args.ItemID, args.CanonicalName);
                Logger.Information("OnDeleteFile " + args.ItemID.ToString() + " " + args.CanonicalName);
                // Ok, now remove ALL nodes for that key
                lock (files)
                {
                    if (files.TryGetValue(id, out var values))
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
                }
                // and then remove the key
                RemoveNode(id);
                //
            });
        }
        #endregion


        public void OnIdle()
        {
        }
    }
}
