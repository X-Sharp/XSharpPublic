//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/*****************************************************************************
* Based on IronStudio/IronPythonTools/IronPythonTools/Navigation
****************************************************************************/

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using System;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Threading;
using XSharp.Settings;
using XSharpModel;
using IServiceProvider = System.IServiceProvider;
using System.Windows.Markup;
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
        internal const string Functions = nameof(Functions);
        /// <summary>
        /// Class storing the data about a parsing task on a module.
        /// A module is a source file, so here we use the file name as UID
        /// </summary>
        [DebuggerDisplay("{FileName}")]
        private class LibraryTask
        {
            public XFile File{ get; }
            public string FileKey => File.Project.Id.ToString() + "|" + File.FullPath;
            public string FileName => File.FullPath;    
            public XSharpModuleId ModuleID { get; }
            public LibraryTask(XFile file, XSharpModuleId ModuleID)
            {
                this.File = file;
                this.ModuleID = ModuleID;
            }
        }


        private readonly IServiceProvider provider;
        private uint objectManagerCookie;
        // private uint runningDocTableCookie;
        private readonly Dictionary<IVsHierarchy, HierarchyListener> hierarchies;
        private readonly MultiValueDictionary<XSharpModuleId, XSharpLibraryNode> files;
        private readonly ConcurrentDictionary<string, XSharpModuleId> fileDict;
        private readonly Library library;
        private readonly ConcurrentDictionary<string, XSharpLibraryProject> projectDict;


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
                LibraryCapabilities = (_LIB_FLAGS2)_LIB_FLAGS.LF_PROJECT |
                                          (_LIB_FLAGS2)_LIB_FLAGS.LF_EXPANDABLE |
                                          _LIB_FLAGS2.LF_SUPPORTSFILTERING |
                                          _LIB_FLAGS2.LF_SUPPORTSBASETYPES |
                                          _LIB_FLAGS2.LF_SUPPORTSINHERITEDMEMBERS |
                                          _LIB_FLAGS2.LF_SUPPORTSPRIVATEMEMBERS |
                                        _LIB_FLAGS2.LF_SUPPORTSPROJECTREFERENCES |
                                    _LIB_FLAGS2.LF_SUPPORTSCLASSDESIGNER

            };
            // A Dictionary with :
            // ModuleId : The ID of a File in the Project Hierarchy
            // A library Node
            // --> For a single ModuleId we can have Multiple Library Node
            files = new MultiValueDictionary<XSharpModuleId, XSharpLibraryNode>();
            fileDict = new ConcurrentDictionary<string, XSharpModuleId>(StringComparer.OrdinalIgnoreCase);
            projectDict = new ConcurrentDictionary<string, XSharpLibraryProject>(StringComparer.OrdinalIgnoreCase);
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
            fileDict.Clear();

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
        public void RegisterHierarchy(IVsHierarchy hierarchy, XProject project, IXSharpProject ProjectNode)
        {
            // No Hierarchy or... Hierarchy already registered ?
            // disable classview for now
            if (XSettings.DisableClassViewObjectView)
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
            XSharpLibraryProject prjNode = new XSharpLibraryProject(project, hierarchy);
            library.AddNode(prjNode);
            projectDict.TryAdd(project.FileName, prjNode);

            //this._defaultNameSpace = prjNode.DefaultNameSpace;
            //Define Callback
            project.FileWalkComplete += OnFileWalkComplete;
            project.ProjectWalkComplete += OnProjectWalkComplete;
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
                fileDict.TryAdd(moduleId.FileKey, moduleId);
            }
        }

        private void RemoveNode(XSharpModuleId moduleId)
        {
            lock (files)
            {
                files.TryRemove(moduleId, out var _);
                fileDict.TryRemove(moduleId.FileKey, out var _);
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
            keys = files.Keys.ToArray();

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
                projectDict.TryRemove(prjNode.FullPath, out var _);
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

        private void ProcessTask(LibraryTask task, bool forceRefresh)
        {
            // If the file already exist
            if (fileDict.TryGetValue(task.FileKey, out var found))
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
                CreateModuleTree(project, task.File, task.ModuleID);
                prjNode.updateCount += 1;
                if (forceRefresh)
                {
                    this.library.Refresh();
                    //XSolution.SetStatusBarAnimation(false, 0);
                }
            }
            else
            {
                requests.Enqueue(task);
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
                //if (waitResult == WaitHandle.WaitTimeout)
                //{
                //    continue;
                //}
                if (!XSharpModel.XSolution.IsOpen && requests.Count > 0)
                {
                    requests.Clear();
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
                            tasks.Remove(task.FileKey);
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
                    ProcessTask(task, forceRefresh);
                }
            }
        }

        private void AddNamespace(XSourceTypeSymbol xType, XSharpLibraryProject prjNode, XSharpModuleId moduleId)
        {
            string nodeName = xType.Name;
            XSharpLibraryNode node;
            if (String.IsNullOrEmpty(xType.Name))
            {
                nodeName = prjNode.Name;
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
            return;
        }

        private void AddType(XSourceTypeSymbol xType, XSharpLibraryProject prjNode, XSharpModuleId moduleId)
        {
            LibraryNode nsNode;
            XSharpLibraryNode newNode;
            string nSpace = prjNode.Name;
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
            if (file.IsHidden)
                return;

            if (!file.HasCode)
                return;
            //Logger.Information("CreateModuleTree " + file.FullPath);
            //
            XSharpLibraryNode newNode;
            LibraryNode nsNode;
            try
            {
                // get all files for the project
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
                var types = XSharpModel.XDatabase.GetTypesInFile(file);
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
                nsNode = prjNode.SearchNameSpace(Functions);
                if (nsNode is XSharpLibraryNode node)
                {
                    newNode = node;
                    newNode.Depends(moduleId.ItemID);
                }
                else
                {
                    newNode = new XSharpLibraryNode(Functions, LibraryNode.LibraryNodeType.Namespaces, "");
                    // NameSpaces are always added to the root.
                    prjNode.AddNode(newNode);
                    newNode.parent = prjNode;
                }
                //
                AddNode(moduleId, newNode);

                // Finally, any Function/Procedure ??
                var functions = XSharpModel.XDatabase.GetFunctions(file.Id, prjNode.XProject.Id);
                if (functions != null)
                {
                    var members = XDbResultHelpers.BuildFullFuncsInFile(file, functions);
                    CreateGlobalTree(newNode, members, moduleId);
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
                if (newNode.NodeType.HasFlag(LibraryNode.LibraryNodeType.Members))
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
                if (member.File.FullPath != moduleId.File.FullPath)
                    continue;
                XSharpLibraryNode newNode = new XSharpLibraryNode(member, "", moduleId.Hierarchy, moduleId.ItemID);

                // The classes are always added to the root node, the functions to the current node.
                if (newNode.NodeType.HasFlag(LibraryNode.LibraryNodeType.Members))
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
            if (fileDict.TryGetValue(xfile.FullPath, out var module))
            {
                CreateUpdateTreeRequest(xfile, module.Hierarchy, module.ItemID);
            }
            else
            {
                if (projectDict.TryGetValue(xfile.Project.FileName, out var prjNode))
                {
                    var hierarchyId = prjNode.ownerHierarchy;
                    uint itemId = 0;
                    int hr = 0;
                    ThreadHelper.JoinableTaskFactory.Run(async () =>
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        hr = hierarchyId.ParseCanonicalName(xfile.FullPath, out itemId);
                    });

                    if (itemId != 0 && hr == VSConstants.S_OK)
                    {
                        var args = new HierarchyEventArgs(itemId, xfile);
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
                if (projectDict.TryGetValue(xsProject.FileName, out var prjNode))
                {
                    // Add all source files.
                    var hierarchyId = prjNode.ownerHierarchy;
                    var listener = hierarchies[hierarchyId];
                    foreach (var path in xsProject.SourceFiles)
                    {
                        var xFile = xsProject.FindXFile(path);
                        uint itemId = 0;
                        int hr = 0;
                        ThreadHelper.JoinableTaskFactory.Run(async () =>
                        {
                            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                            hr = hierarchyId.ParseCanonicalName(xFile.FullPath, out itemId);
                        });
                        if (itemId != 0 && hr == VSConstants.S_OK)
                        {
                            var args = new HierarchyEventArgs(itemId, xFile);
                            OnNewFile(hierarchyId, args);
                        }
                    }

                    listener.StartListening();
                }
            }
        }

        private void CreateUpdateTreeRequest(XFile xfile, IVsHierarchy owner, uint itemId)
        {
            if (XSolution.IsClosing)
                return;
            var filekey = xfile.Project.Id.ToString() + "|" + xfile.FullPath;
            lock (requests)
            {
                bool newTask = true;
                if (tasks.TryGetValue(filekey, out var task))
                {
                    //Logger.Information("CreateUpdateTreeRequest Library Update " + file);
                    if (task.ModuleID.Hierarchy != owner || task.ModuleID.ItemID != itemId)
                    {
                        tasks.Remove(filekey);
                    }
                    else
                    {
                        newTask = false;
                    }
                }
                if (newTask)
                {   
                    var id = new XSharpModuleId(owner, itemId, xfile);
                    task = new LibraryTask(xfile, id);
                    tasks.Add(task.FileKey, task);
                }
                requests.Enqueue(task);
                requestPresent.Set();
            }
        }



        #region Hierarchy Events
        /// <summary>
        /// Called when a new file is added in the Project Hierarchy
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="args"></param>
        private void OnNewFile(object sender, HierarchyEventArgs args)
        {
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                if (!(sender is IVsHierarchy hierarchy))
                {
                    return;
                }
                var type = XFileTypeHelpers.GetFileType(args.File.FullPath);
                if (type == XFileType.SourceCode)
                {
                    CreateUpdateTreeRequest(args.File, hierarchy, args.ItemID);
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
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                if (!(sender is IVsHierarchy hierarchy))
                {
                    return;
                }
                XSharpModuleId id = new XSharpModuleId(hierarchy, args.ItemID, args.File);
                Logger.Information("OnDeleteFile " + args.ItemID.ToString() + " " + args.File.FullPath);
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
