//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Globalization;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Designer.Interfaces;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;

using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;

using System.ComponentModel.Design;
using System.Diagnostics;


namespace XSharp.Project
{
    /// <summary>
    /// Factory for creating our editor
    /// </summary>
    [Guid(XSharpConstants.EditorFactoryGuidString)]
    [ProvideView(LogicalView.Code, "")]
    public class XSharpEditorFactory : IVsEditorFactory
    {
        #region fields
        private XSharpProjectPackage _package;
        private ServiceProvider _serviceProvider;
        #endregion

        #region ctors
        public XSharpEditorFactory(XSharpProjectPackage package)
        {
            _package = package;
        }
        #endregion

        #region Helpers
        protected bool CalledFromVulcanDLL()
        {
            var trace = new System.Diagnostics.StackTrace(false);
            foreach (var frame in trace.GetFrames())
            {
                var dll = frame.GetMethod().Module.Assembly;
                String name = dll.GetName().Name.ToLower();
                if (name.Contains("vulcanproject2015"))
                {
                    return true;
                }
            }
            return false;

        }
        #endregion

        #region IVsEditorFactory Members

        public virtual int SetSite(Microsoft.VisualStudio.OLE.Interop.IServiceProvider psp)
        {
            _serviceProvider = new ServiceProvider(psp);
            return VSConstants.S_OK;
        }

        public virtual object GetService(Type serviceType)
        {
            // This is were we will load the IVSMDProvider interface
            return _serviceProvider.GetService(serviceType);
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
        //        HKLM\Software\Microsoft\VisualStudio\8.0\Editors\
        //            {...guidEditor...}\
        //                LogicalViews\
        //                    {...LOGVIEWID_TextView...} = s ''
        //                    {...LOGVIEWID_Code...} = s ''
        //                    {...LOGVIEWID_Debugging...} = s ''
        //                    {...LOGVIEWID_Designer...} = s 'Form'
        //
        public virtual int MapLogicalView(ref Guid logicalView, out string physicalView)
        {
            physicalView = null;
            if (logicalView == VSConstants.LOGVIEWID_Code ||
                logicalView == VSConstants.LOGVIEWID_TextView ||
                logicalView == VSConstants.LOGVIEWID_Debugging ||
                logicalView == VSConstants.LOGVIEWID_Primary)
            {
                physicalView = null;
                return VSConstants.S_OK;
            }
            return VSConstants.E_NOTIMPL;
        }

        public virtual int Close()
        {
            return VSConstants.S_OK;
        }

        public virtual int CreateEditorInstance(
                       uint createEditorFlags,
                       string documentMoniker,
                       string physicalView,
                       IVsHierarchy hierarchy,
                       uint itemid,
                       System.IntPtr docDataExisting,
                       out System.IntPtr docView,
                       out System.IntPtr docData,
                       out string editorCaption,
                       out Guid commandUIGuid,
                       out int createDocumentWindowFlags)
        {
            // Initialize output parameters
            bool vulcan = CalledFromVulcanDLL();
            docView = IntPtr.Zero;
            docData = IntPtr.Zero;
            createDocumentWindowFlags = 0;
            commandUIGuid = Guid.Empty;
            editorCaption = null;

            // Validate inputs
            if ((createEditorFlags & (VSConstants.CEF_OPENFILE | VSConstants.CEF_SILENT)) == 0)
                return VSConstants.E_INVALIDARG;

            // Get a text buffer
            IVsTextLines textLines = GetTextBuffer(docDataExisting);
            IVsTextBuffer textbuffer = textLines as IVsTextBuffer;
            if (vulcan)
            {
                /*
                    These properties can be set on IVsUserData

                    public static readonly Guid VsBufferMoniker = new Guid(0x978A8E17, 0x4DF8, 0x432A, 0x96, 0x23, 0xD5, 0x30, 0xA2, 0x64, 0x52, 0xBC);
                    public static readonly Guid VsBufferIsDiskFile = new Guid(0xd9126592, 0x1473, 0x11d3, 0xbe, 0xc6, 0x0, 0x80, 0xc7, 0x47, 0xd9, 0xa0);
                    public static readonly Guid VsBufferDetectLangSID = new Guid(0x17f375ac, 0xc814, 0x11d1, 0x88, 0xad, 0x0, 0x0, 0xf8, 0x75, 0x79, 0xd2);
                    public static readonly Guid VsBufferEncodingVSTFF = new Guid(0x16417f39, 0xa6b7, 0x4c90, 0x89, 0xfa, 0x77, 0xd, 0x2c, 0x60, 0x44, 0xb );
                    public static readonly Guid VsBufferEncoding = new Guid(0x212729ac, 0xd6bb, 0x11d0, 0xae, 0x75, 0x0, 0xc0, 0x4f, 0xb6, 0x80, 0x6);
                    public static readonly Guid VsBufferEncodingPromptOnLoad = new Guid(0x99ec03f0, 0xc843, 0x4c09, 0xbe, 0x74, 0xcd, 0xca, 0x51, 0x58, 0xd3, 0x6c);
                    // from textmgr100.idl; guids for IVsUserData.{Get/Set}Data()
                    public static readonly Guid VsBufferContentType = new Guid(0x1beb4195, 0x98f4, 0x4589, 0x80, 0xe0, 0x48, 0xc, 0xe3, 0x2f, 0xf0, 0x59);
                    public static readonly Guid VsTextViewRoles = new Guid(0x297078ff, 0x81a2, 0x43d8, 0x9c, 0xa3, 0x44, 0x89, 0xc5, 0x3c, 0x99, 0xba);
                    public static readonly Guid UseLazyInitialization = new Guid(0xfea19c13, 0x32ce, 0x447b, 0x8c, 0xc3, 0x72, 0x0d, 0xdf, 0x13, 0x8b, 0xb8);

                 */
                Guid vsCoreLanguageService = new Guid("{8239bec4-ee87-11d0-8c98-00c04fc2ab22}");
                Guid activeLanguageService;
                textbuffer.GetLanguageServiceID(out activeLanguageService);
                if (activeLanguageService == vsCoreLanguageService)
                {
                    Guid guidVulcanLanguageService = GuidStrings.guidVulcanLanguageService;
                    int result = textbuffer.SetLanguageServiceID(ref guidVulcanLanguageService);
                    IVsUserData vud = (IVsUserData)textbuffer;
                    Guid bufferDetectLang = Microsoft.VisualStudio.Package.EditorFactory.GuidVSBufferDetectLangSid;
                    vud.SetData(ref bufferDetectLang, false);
                }
            }
            if (docDataExisting != IntPtr.Zero)
            {
                docData = docDataExisting;
                Marshal.AddRef(docData);
            }
            else
            {
                docData = Marshal.GetIUnknownForObject(textLines);
            }

            try
            {
                docView = CreateDocumentView(
                    physicalView, hierarchy, itemid, textLines, out editorCaption, ref commandUIGuid);
            }
            finally
            {
                if (docView == IntPtr.Zero && docDataExisting != docData && docData != IntPtr.Zero)
                {
                    // Cleanup the instance of the docData that we have addref'ed
                    Marshal.Release(docData);
                    docData = IntPtr.Zero;
                }
            }
            return VSConstants.S_OK;
        }


        #endregion

        #region Helper methods
        private IVsTextLines GetTextBuffer(System.IntPtr docDataExisting)
        {
            IVsTextLines textLines;
            if (docDataExisting == IntPtr.Zero)
            {
                // Create a new IVsTextLines buffer.
                Type textLinesType = typeof(IVsTextLines);
                Guid riid = textLinesType.GUID;
                Guid clsid = typeof(VsTextBufferClass).GUID;
                textLines = _package.CreateInstance(ref clsid, ref riid, textLinesType) as IVsTextLines;

                // set the buffer's site
                ((IObjectWithSite)textLines).SetSite(_serviceProvider.GetService(typeof(IOleServiceProvider)));
            }
            else
            {
                // Use the existing text buffer
                Object dataObject = Marshal.GetObjectForIUnknown(docDataExisting);
                textLines = dataObject as IVsTextLines;
                if (textLines == null)
                {
                    // Try get the text buffer from textbuffer provider
                    IVsTextBufferProvider textBufferProvider = dataObject as IVsTextBufferProvider;
                    if (textBufferProvider != null)
                    {
                        textBufferProvider.GetTextBuffer(out textLines);
                    }
                }
                if (textLines == null)
                {
                    // Unknown docData type then, so we have to force VS to close the other editor.
                    ErrorHandler.ThrowOnFailure((int)VSConstants.VS_E_INCOMPATIBLEDOCDATA);
                }

            }
            return textLines;
        }

        private IntPtr CreateDocumentView(
            string physicalView, 
            IVsHierarchy hierarchy, 
            uint itemid, 
            IVsTextLines textLines, 
            out string editorCaption, 
            ref Guid cmdUI)
        {
            //Init out params
            editorCaption = string.Empty;
           // cmdUI = Guid.Empty;

            if (string.IsNullOrEmpty(physicalView))
            {
                // create code window as default physical view
                return CreateCodeView(textLines, ref editorCaption, ref cmdUI);
            }
            else if (string.Compare(physicalView, "design", true, CultureInfo.InvariantCulture) == 0)
            {
                // Create Form view
                return CreateFormView(hierarchy, itemid, textLines, ref editorCaption, ref cmdUI);
            }

            // We couldn't create the view
            // Return special error code so VS can try another editor factory.
            ErrorHandler.ThrowOnFailure((int)VSConstants.VS_E_UNSUPPORTEDFORMAT);

            return IntPtr.Zero;
        }

        private IntPtr CreateFormView(
            IVsHierarchy hierarchy, 
            uint itemid, 
            IVsTextLines textLines, 
            ref string editorCaption, 
            ref Guid cmdUI)
        {
            // Request the Designer Service
            IVSMDDesignerService designerService = (IVSMDDesignerService)GetService(typeof(IVSMDDesignerService));

            // Create loader for the designer
            IVSMDDesignerLoader designerLoader = 
                (IVSMDDesignerLoader)designerService.CreateDesignerLoader(
                    "Microsoft.VisualStudio.Designer.Serialization.VSDesignerLoader");

            bool loaderInitalized = false;
            try
            {
                var service = _serviceProvider.GetService(typeof(IOleServiceProvider)) as IOleServiceProvider;

                // Initialize designer loader 
                designerLoader.Initialize(service, hierarchy, (int)itemid, textLines);
                loaderInitalized = true;

                // Create the designer
                IVSMDDesigner designer = designerService.CreateDesigner(service, designerLoader);

                // Get editor caption
                editorCaption = designerLoader.GetEditorCaption((int)READONLYSTATUS.ROSTATUS_Unknown);

                // Get view from designer
                object docView = designer.View;

                // Get command guid from designer
                cmdUI = designer.CommandGuid;

                return Marshal.GetIUnknownForObject(docView);

            }
            catch
            {
                // The designer loader may have created a reference to the shell or the text buffer.
                // In case we fail to create the designer we should manually dispose the loader
                // in order to release the references to the shell and the textbuffer
                if (loaderInitalized)
                {
                    designerLoader.Dispose();
                }
                throw;
            }
        }

        private IntPtr CreateCodeView(IVsTextLines textLines, ref string editorCaption, ref Guid cmdUI)
        {
            Type codeWindowType = typeof(IVsCodeWindow);
            Guid riid = codeWindowType.GUID;
            Guid clsid = typeof(VsCodeWindowClass).GUID;
            IVsCodeWindow window = (IVsCodeWindow)_package.CreateInstance(ref clsid, ref riid, codeWindowType);

            ErrorHandler.ThrowOnFailure(window.SetBuffer(textLines));
            ErrorHandler.ThrowOnFailure(window.SetBaseEditorCaption(null));
            ErrorHandler.ThrowOnFailure(window.GetEditorCaption(READONLYSTATUS.ROSTATUS_Unknown, out editorCaption));

            cmdUI = VSConstants.GUID_TextEditorFactory;
            return Marshal.GetIUnknownForObject(window);
        }

        #endregion
    }
}
