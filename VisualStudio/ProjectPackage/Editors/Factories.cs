//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;

namespace XSharp.Project
{

    /// <summary>
    /// Factory for creating our editor object. Extends from the IVsEditoryFactory interface
    /// </summary>
    public abstract class VOEditorFactoryBase : IVsEditorFactory, IDisposable
    {
        private ServiceProvider vsServiceProvider;
        protected XSharpProjectPackage editorPackage;

        internal VOEditorFactoryBase()
        {
        }

        /// <summary>
        /// Since we create a ServiceProvider which implements IDisposable we
        /// also need to implement IDisposable to make sure that the ServiceProvider's
        /// Dispose method gets called.
        /// </summary>
        public void Dispose()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (vsServiceProvider != null)
            {
                vsServiceProvider.Dispose();
            }
        }

        #region IVsEditorFactory Members

        /// <summary>
        /// Used for initialization of the editor in the environment
        /// </summary>
        /// <param name="psp">pointer to the service provider. Can be used to obtain instances of other interfaces
        /// </param>
        /// <returns></returns>
        public int SetSite(Microsoft.VisualStudio.OLE.Interop.IServiceProvider psp)
        {
            vsServiceProvider = new ServiceProvider(psp);
            return VSConstants.S_OK;
        }

        public object GetService(Type serviceType)
        {
            return ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                return vsServiceProvider.GetService(serviceType);
            });
        }

        // This method is called by the Environment (inside IVsUIShellOpenDocument::
        // OpenStandardEditor and OpenSpecificEditor) to map a LOGICAL view to a
        // PHYSICAL view. A LOGICAL view identifies the purpose of the view that is
        // desired (e.g. a view appropriate for Debugging [LOGVIEWID_Debugging], or a
        // view appropriate for text view manipulation as by navigating to a find
        // result [LOGVIEWID_TextView]). A PHYSICAL view identifies an actual type
        // of view implementation that an IVsEditorFactory can create.
        //
        // NOTE: Physical views are identified by a string of your choice with the
        // one constraint that the default/primary physical view for an editor
        // *MUST* use a NULL string as its physical view name (*pbstrPhysicalView = NULL).
        //
        // NOTE: It is essential that the implementation of MapLogicalView properly
        // validates that the LogicalView desired is actually supported by the editor.
        // If an unsupported LogicalView is requested then E_NOTIMPL must be returned.
        //
        // NOTE: The special Logical Views supported by an Editor Factory must also
        // be registered in the local registry hive. LOGVIEWID_Primary is implicitly
        // supported by all editor types and does not need to be registered.
        // For example, an editor that supports a ViewCode/ViewDesigner scenario
        // might register something like the following:
        //        HKLM\Software\Microsoft\VisualStudio\9.0\Editors\
        //            {...guidEditor...}\
        //                LogicalViews\
        //                    {...LOGVIEWID_TextView...} = s ''
        //                    {...LOGVIEWID_Code...} = s ''
        //                    {...LOGVIEWID_Debugging...} = s ''
        //                    {...LOGVIEWID_Designer...} = s 'Form'
        //
        public int MapLogicalView(ref Guid rguidLogicalView, out string pbstrPhysicalView)
        {
            pbstrPhysicalView = null;    // initialize out parameter
            rguidLogicalView = VSConstants.LOGVIEWID_Designer;
            return VSConstants.S_OK;
        }

        public int Close()
        {
            return VSConstants.S_OK;
        }

        /// <summary>
        /// Used by the editor factory to create an editor instance. the environment first determines the
        /// editor factory with the highest priority for opening the file and then calls
        /// IVsEditorFactory.CreateEditorInstance. If the environment is unable to instantiate the document data
        /// in that editor, it will find the editor with the next highest priority and attempt to so that same
        /// thing.
        /// NOTE: The priority of our editor is 32 as mentioned in the attributes on the package class.
        ///
        /// Since our editor supports opening only a single view for an instance of the document data, if we
        /// are requested to open document data that is already instantiated in another editor, or even our
        /// editor, we return a value VS_E_INCOMPATIBLEDOCDATA.
        /// </summary>
        /// <param name="grfCreateDoc">Flags determining when to create the editor. Only open and silent flags
        /// are valid
        /// </param>
        /// <param name="pszMkDocument">path to the file to be opened</param>
        /// <param name="pszPhysicalView">name of the physical view</param>
        /// <param name="pvHier">pointer to the IVsHierarchy interface</param>
        /// <param name="itemid">Item identifier of this editor instance</param>
        /// <param name="punkDocDataExisting">This parameter is used to determine if a document buffer
        /// (DocData object) has already been created
        /// </param>
        /// <param name="ppunkDocView">Pointer to the IUnknown interface for the DocView object</param>
        /// <param name="ppunkDocData">Pointer to the IUnknown interface for the DocData object</param>
        /// <param name="pbstrEditorCaption">Caption mentioned by the editor for the doc window</param>
        /// <param name="pguidCmdUI">the Command UI Guid. Any UI element that is visible in the editor has
        /// to use this GUID. This is specified in the .vsct file
        /// </param>
        /// <param name="pgrfCDW">Flags for CreateDocumentWindow</param>
        /// <returns></returns>
        abstract public int CreateEditorInstance(
                        uint grfCreateDoc,
                        string pszMkDocument,
                        string pszPhysicalView,
                        IVsHierarchy pvHier,
                        uint itemid,
                        System.IntPtr punkDocDataExisting,
                        out System.IntPtr ppunkDocView,
                        out System.IntPtr ppunkDocData,
                        out string pbstrEditorCaption,
                        out Guid pguidCmdUI,
                        out int pgrfCDW);

        #endregion
        #region Helper Methods
        //protected bool CalledFromVulcanDLL()
        //{
        //    var trace = new System.Diagnostics.StackTrace(false);
        //    int i = 0;
        //    foreach (var frame in trace.GetFrames())
        //    {
        //        if (i > 0)
        //        {
        //            var dll = frame.GetMethod().Module.Assembly;
        //            String name = dll.GetName().Name.ToLower();
        //            if (name.Contains("vulcanproject2015"))
        //            {
        //                return true;
        //            }
        //        }
        //        i++;
        //    }
        //    return false;

        //}
        //protected object GetProjectNode(string fileName)
        //{
        //    var dte = this.GetService(typeof(Microsoft.VisualStudio.Shell.Interop.SDTE)) as EnvDTE80.DTE2;
        //    var projectitem = dte.Solution.FindProjectItem(fileName);
        //    if (projectitem != null)
        //    {
        //        var project = projectitem.ContainingProject.Object as object;
        //        if (project != null)
        //        {
        //            System.Type type2 = project.GetType();
        //            var property = type2.GetProperty("Project", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public);
        //            if (property != null)
        //            {
        //                // Now get Vulcans OAProjectNode
        //                object vulproject = property.GetValue(project);
        //                if (vulproject != null)     // Vulcans OAProject
        //                {
        //                    // Now get Vulcans ProjectNode
        //                    type2 = vulproject.GetType();
        //                    property = type2.GetProperty("Project", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public);
        //                    if (property != null)
        //                        return property.GetValue(vulproject);
        //                }
        //            }
        //        }
        //    }
        //    return null;
        //}
        //protected object GetVulcanFactory(string className, string fileName)
        //{
        //    object projectNode = GetProjectNode(fileName);
        //    object package = null;
        //    object factory = null;
        //    if (projectNode != null)
        //    {
        //        var type = projectNode.GetType();
        //        if (type != null)
        //        {
        //            var projectDLL = type.Assembly;
        //            var property = type.GetProperty("Package", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public);
        //            if (property != null)
        //            {
        //                type = projectDLL.GetType(className);
        //                package = property.GetValue(projectNode);
        //                factory = Activator.CreateInstance(type, new object[] { package });
        //            }
        //        }
        //    }
        //    return factory;
        //}
        #endregion
    }

    [Guid(PackageGuids.guidVOFormEditorCmdSetString)]
    [ProvideView(LogicalView.Designer,"")]
    public sealed class VOFormEditorFactory : VOEditorFactoryBase
    {
        public VOFormEditorFactory(XSharpProjectPackage package)
           : base()
        {
            this.editorPackage = package;
        }

        [SecurityPermission(SecurityAction.Demand, Flags = SecurityPermissionFlag.UnmanagedCode)]
        public override int CreateEditorInstance(
                        uint grfCreateDoc,
                        string pszMkDocument,
                        string pszPhysicalView,
                        IVsHierarchy pvHier,
                        uint itemid,
                        System.IntPtr punkDocDataExisting,
                        out System.IntPtr ppunkDocView,
                        out System.IntPtr ppunkDocData,
                        out string pbstrEditorCaption,
                        out Guid pguidCmdUI,
                        out int pgrfCDW)
        {

            // Initialize to null
            ppunkDocView = IntPtr.Zero;
            ppunkDocData = IntPtr.Zero;
            pguidCmdUI = PackageGuids.guidVOFormEditorCmdSet;
            pgrfCDW = 0;
            pbstrEditorCaption = null;

            // Validate inputs
            if ((grfCreateDoc & (VSConstants.CEF_OPENFILE | VSConstants.CEF_SILENT)) == 0)
            {
                return VSConstants.E_INVALIDARG;
            }
            if (punkDocDataExisting != IntPtr.Zero)
            {
                return VSConstants.VS_E_INCOMPATIBLEDOCDATA;
            }

            // Create the Document (editor)
            var editor = new VOFormEditorPane(editorPackage);
            ppunkDocView = Marshal.GetIUnknownForObject(editor);
            ppunkDocData = Marshal.GetIUnknownForObject(editor);
            pbstrEditorCaption = "";
            return VSConstants.S_OK;
        }
    }

    [Guid(XSharpConstants.guidXSharpVOMenuEditor)]
    [ProvideView(LogicalView.Designer, "")]
    public sealed class VOMenuEditorFactory : VOEditorFactoryBase
    {
        public VOMenuEditorFactory(XSharpProjectPackage package)
           : base()
        {

            this.editorPackage = package;
        }

        [SecurityPermission(SecurityAction.Demand, Flags = SecurityPermissionFlag.UnmanagedCode)]
        public override int CreateEditorInstance(
                        uint grfCreateDoc,
                        string pszMkDocument,
                        string pszPhysicalView,
                        IVsHierarchy pvHier,
                        uint itemid,
                        System.IntPtr punkDocDataExisting,
                        out System.IntPtr ppunkDocView,
                        out System.IntPtr ppunkDocData,
                        out string pbstrEditorCaption,
                        out Guid pguidCmdUI,
                        out int pgrfCDW)
        {

            // Initialize to null
            ppunkDocView = IntPtr.Zero;
            ppunkDocData = IntPtr.Zero;
            pguidCmdUI = XSharpConstants.guidVOMenuEditorFactory;
            pgrfCDW = 0;
            pbstrEditorCaption = null;

            // Validate inputs
            if ((grfCreateDoc & (VSConstants.CEF_OPENFILE | VSConstants.CEF_SILENT)) == 0)
            {
                return VSConstants.E_INVALIDARG;
            }
            if (punkDocDataExisting != IntPtr.Zero)
            {
                return VSConstants.VS_E_INCOMPATIBLEDOCDATA;
            }

            // Create the Document (editor)
            var editor = new VOMenuEditorPane(editorPackage);
            ppunkDocView = Marshal.GetIUnknownForObject(editor);
            ppunkDocData = Marshal.GetIUnknownForObject(editor);
            pbstrEditorCaption = "";
            XSharpModel.XFile file = XSharpModel.XSolution.FindFile(pszMkDocument);
            if (file != null)
            {
                editor.Project = file.Project.ProjectNode;
            }
            return VSConstants.S_OK;
        }
    }

    [Guid(XSharpConstants.guidXSharpVOFieldSpecEditor)]
    [ProvideView(LogicalView.Designer, "")]
    public sealed class VOFieldSpecEditorFactory : VOEditorFactoryBase
    {

        public VOFieldSpecEditorFactory(XSharpProjectPackage package)
            : base()
        {

            this.editorPackage = package;
        }

        [SecurityPermission(SecurityAction.Demand, Flags = SecurityPermissionFlag.UnmanagedCode)]
        public override int CreateEditorInstance(
                        uint grfCreateDoc,
                        string pszMkDocument,
                        string pszPhysicalView,
                        IVsHierarchy pvHier,
                        uint itemid,
                        System.IntPtr punkDocDataExisting,
                        out System.IntPtr ppunkDocView,
                        out System.IntPtr ppunkDocData,
                        out string pbstrEditorCaption,
                        out Guid pguidCmdUI,
                        out int pgrfCDW)
        {

            // Initialize to null
            ppunkDocView = IntPtr.Zero;
            ppunkDocData = IntPtr.Zero;
            pguidCmdUI = XSharpConstants.guidVOFieldSpecEditorFactory;
            pgrfCDW = 0;
            pbstrEditorCaption = null;

            // Validate inputs
            if ((grfCreateDoc & (VSConstants.CEF_OPENFILE | VSConstants.CEF_SILENT)) == 0)
            {
                return VSConstants.E_INVALIDARG;
            }
            if (punkDocDataExisting != IntPtr.Zero)
            {
                return VSConstants.VS_E_INCOMPATIBLEDOCDATA;
            }

            // Create the Document (editor)
            var editor = new VOFieldSpecEditorPane(editorPackage);

            ppunkDocView = Marshal.GetIUnknownForObject(editor);
            ppunkDocData = Marshal.GetIUnknownForObject(editor);
            pbstrEditorCaption = "";
            return VSConstants.S_OK;
        }
    }


    [Guid(XSharpConstants.guidXSharpVODbServerEditor)]
    [ProvideView(LogicalView.Designer, "")]
    public sealed class VODBServerEditorFactory : VOEditorFactoryBase
    {

        public VODBServerEditorFactory(XSharpProjectPackage package)
            : base()
        {

            this.editorPackage = package;
        }

        [SecurityPermission(SecurityAction.Demand, Flags = SecurityPermissionFlag.UnmanagedCode)]
        public override int CreateEditorInstance(
                        uint grfCreateDoc,
                        string pszMkDocument,
                        string pszPhysicalView,
                        IVsHierarchy pvHier,
                        uint itemid,
                        System.IntPtr punkDocDataExisting,
                        out System.IntPtr ppunkDocView,
                        out System.IntPtr ppunkDocData,
                        out string pbstrEditorCaption,
                        out Guid pguidCmdUI,
                        out int pgrfCDW)
        {

            // Initialize to null
            ppunkDocView = IntPtr.Zero;
            ppunkDocData = IntPtr.Zero;
            pguidCmdUI = XSharpConstants.guidVODbServerEditorFactory;
            pgrfCDW = 0;
            pbstrEditorCaption = null;

            // Validate inputs
            if ((grfCreateDoc & (VSConstants.CEF_OPENFILE | VSConstants.CEF_SILENT)) == 0)
            {
                return VSConstants.E_INVALIDARG;
            }
            if (punkDocDataExisting != IntPtr.Zero)
            {
                return VSConstants.VS_E_INCOMPATIBLEDOCDATA;
            }

            // Create the Document (editor)
            var editor = new VOServerEditorPane(editorPackage);


            ppunkDocView = Marshal.GetIUnknownForObject(editor);
            ppunkDocData = Marshal.GetIUnknownForObject(editor);
            pbstrEditorCaption = "";

            return VSConstants.S_OK;
        }
    }
}
