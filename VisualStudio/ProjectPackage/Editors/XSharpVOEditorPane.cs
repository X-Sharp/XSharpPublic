//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System;
using System.Collections;
using System.ComponentModel.Design;
using System.Diagnostics;
using System.IO;
using System.Globalization;
using System.Windows.Forms;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Shell;
using EnvDTE;
using XSharp.VOEditors;
namespace XSharp.Project
{
    /// <summary>
    /// This control hosts the editor and is responsible for
    /// handling the commands targeted to the editor as well as saving and loading
    /// the document. This control also implements the search and replace functionalities.
    /// </summary>

    ///////////////////////////////////////////////////////////////////////////////
    // Having an entry in the new file dialog.
    //
    // For our file type should appear under "General" in the new files dialog, we need the following:-
    //     - A .vsdir file in the same directory as NewFileItems.vsdir (generally under Common7\IDE\NewFileItems).
    //       In our case the file name is Editor.vsdir but we only require a file with .vsdir extension.
    //     - An empty <myext> file in the same directory as NewFileItems.vsdir. In
    //       our case we chose myfile.<myext>. Note this file name appears in Editor.vsdir
    //       (see vsdir file format below)
    //     - Three text strings in our language specific resource. File Resources.resx :-
    //          - "My File Type" - this is shown next to our icon.
    //          - "A blank my file type" - shown in the description window
    //             in the new file dialog.
    //          - "myfile" - This is the base file name. New files will initially
    //             be named as myfile.myext, myfile.myext... etc.
    ///////////////////////////////////////////////////////////////////////////////
    // Editor.vsdir contents:-
    //    myfile.myext|{guid}|#106|80|#109|0|401|0|#107
    // The fields in order are as follows:-
    //    - form.vnfs - our empty vnfs file
    //    - {guid} - our Editor package guid
    //    - #106 - the ID of "My File Type" in the resource
    //    - 80 - the display ordering priority
    //    - #109 - the ID of "A blank my file type" in the resource
    //    - 0 - resource dll string (we don't use this)
    //    - 401 - the ID of our icon
    //    - 0 - various flags (we don't use this - se vsshell.idl)
    //    - #107 - the ID of "myext"
    ///////////////////////////////////////////////////////////////////////////////

    //This is required for Find In files scenario to work properly. This provides a connection point 
    //to the event interface
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Maintainability", "CA1506:AvoidExcessiveClassCoupling")]
    //   [ComSourceInterfaces( typeof( IVsTextViewEvents ) )]
    [ComVisible(true)]
    public abstract class VOEditorPane : WindowPane,
                                IVsPersistDocData,  //to Enable persistence functionality for document data
                                IPersistFileFormat, //to enable the programmatic loading or saving of an object 
                                                    //in a format specified by the user.
                                IVsFileChangeEvents, //to notify the client when file changes on disk
                                IVsDocDataFileChangeControl, //to Determine whether changes to files made outside 
                                                             //of the editor should be ignored
                                IVsFileBackup,      //to support backup of files. Visual Studio File Recovery 
                                                    //backs up all objects in the Running Document Table that 
                                                    //support IVsFileBackup and have unsaved changes.
                                                    //                               IVsFindTarget,      //to implement find and replace capabilities within the editor
                                IExtensibleObject  //so we can get the automation object
    {
        private uint MyFormat = 0;
        protected string MyExtension = ".*";
        private XSharpModel.IXSharpProject project;

        private class VOEditorProperties
        {
            private VOEditorPane editor;
            public VOEditorProperties(VOEditorPane Editor)
            {
                editor = Editor;
            }

            [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
            public string FileName
            {
                get { return editor.FileName; }
            }

            [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
            public bool DataChanged
            {
                get { return editor.DataChanged; }
            }
        }

        #region Fields
        private XSharpProjectPackage  myPackage;

        private string fileName = string.Empty;
        private bool isDirty;
        private bool loading;
        private bool gettingCheckoutStatus;
        protected IVOWEDControl editorControl;             // All VO Editors have this common parent
        private bool isLoaded;

        private Microsoft.VisualStudio.Shell.SelectionContainer selContainer;
        private ITrackSelection trackSel;
        private IVsFileChangeEx vsFileChangeEx;

        private Timer FileChangeTrigger = new Timer();

        private Timer FNFStatusbarTrigger = new Timer();

        private bool fileChangedTimerSet;
        private int ignoreFileChangeLevel;
        private bool backupObsolete = true;
        private uint vsFileChangeCookie;

        private IExtensibleObjectSite extensibleObjectSite;

        #endregion

        #region "Window.Pane Overrides"
        /// <summary>
        /// </summary>
        /// <param name="package">Our Package instance.</param>
        public VOEditorPane(XSharpProjectPackage package)
           : base(null)
        {
            PrivateInit(package);
            
        }

        protected override void OnClose()
        {
            editorControl.StopRecorder();

            base.OnClose();
        }

        /// <summary>
        /// This is a required override from the Microsoft.VisualStudio.Shell.WindowPane class.
        /// It returns the control that we host.
        /// </summary>
        public override IWin32Window Window
        {
            get
            {
                return this.editorControl.IWin32Window;
            }
        }
        public XSharpModel.IXSharpProject Project
        {
            get
            {
                return project;
            }
            set
            {
                project = value;
            }

        }

        #endregion

        /// <summary>
        /// Initialization routine for the Editor. Loads the list of properties for the document 
        /// which will show up in the properties window 
        /// </summary>
        /// <param name="package"></param>
        private void PrivateInit(XSharpProjectPackage package)
        {
            myPackage = package;
            loading = false;
            gettingCheckoutStatus = false;
            trackSel = null;

            Control.CheckForIllegalCrossThreadCalls = false;
            // Create an ArrayList to store the objects that can be selected
            ArrayList listObjects = new ArrayList();

            // Create the object that will show the document's properties
            // on the properties window.
            VOEditorProperties prop = new VOEditorProperties(this);
            listObjects.Add(prop);

            // Create the SelectionContainer object.
            selContainer = new Microsoft.VisualStudio.Shell.SelectionContainer(true, false);
            selContainer.SelectableObjects = listObjects;
            selContainer.SelectedObjects = listObjects;

            // Create and initialize the editor

            this.editorControl = (IVOWEDControl) Activator.CreateInstance(typeof(XSharp_VOWEDControl));
            this.editorControl.IsDirtyChanged = new EventHandler(IsDirtyChangedHandler);
            this.editorControl.TriggerSave = new EventHandler(TriggerSaveHandler);

            setupCommands();
            this.editorControl.StatusMessage = new StatusMessageDelegate(StatusBarMessageHandler);
        }

        private IVsStatusbar oStatusBar;
        void StatusBarMessageHandler(string cText)
        {
            if (this.oStatusBar == null)
                oStatusBar = (IVsStatusbar)GetService(typeof(IVsStatusbar));
            if (this.oStatusBar != null)
                this.oStatusBar.SetText(cText);
        }
        void IsDirtyChangedHandler(object o, EventArgs e)
        {
            if (this.loading)
                this.isDirty = false;
            else
                this.isDirty = this.editorControl.IsDirty;
        }
        void TriggerSaveHandler(object o, EventArgs e)
        {
            if (!this.loading)
                ((IPersistFileFormat)this).Save(fileName, 0, 0);
        }

        public string FileName
        {
            get { return fileName; }
        }

        /// <summary>
        /// returns whether the contents of file have changed since the last save
        /// </summary>
        public bool DataChanged
        {
            get { return isDirty; }
        }

        /// <summary>
        /// returns an instance of the ITrackSelection service object
        /// </summary>
        private ITrackSelection TrackSelection
        {
            get
            {
                if (trackSel == null)
                {
                    trackSel = (ITrackSelection)GetService(typeof(ITrackSelection));
                }
                return trackSel;
            }
        }

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA1816:CallGCSuppressFinalizeCorrectly")]
        protected override void Dispose(bool disposing)
        {
            try
            {
                if (disposing)
                {
                    // Dispose the timers
                    if (null != FileChangeTrigger)
                    {
                        FileChangeTrigger.Dispose();
                        FileChangeTrigger = null;
                    }
                    if (null != FNFStatusbarTrigger)
                    {
                        FNFStatusbarTrigger.Dispose();
                        FNFStatusbarTrigger = null;
                    }

                    SetFileChangeNotification(null, false);

                    if (editorControl != null)
                    {
                        editorControl.Dispose();
                        editorControl = null;
                    }
                    if (FileChangeTrigger != null)
                    {
                        FileChangeTrigger.Dispose();
                        FileChangeTrigger = null;
                    }
                    if (extensibleObjectSite != null)
                    {
                        extensibleObjectSite.NotifyDelete(this);
                        extensibleObjectSite = null;
                    }
                    GC.SuppressFinalize(this);
                }
            }
            finally
            {
                base.Dispose(disposing);
            }
        }

        /// <summary>
        /// Gets an instance of the RunningDocumentTable (RDT) service which manages the set of currently open 
        /// documents in the environment and then notifies the client that an open document has changed
        /// </summary>
        private void NotifyDocChanged()
        {
            // Make sure that we have a file name
            if (fileName.Length == 0)
                return;

            // Get a reference to the Running Document Table
            IVsRunningDocumentTable runningDocTable = (IVsRunningDocumentTable)GetService(typeof(SVsRunningDocumentTable));

            // Lock the document
            uint docCookie;
            IVsHierarchy hierarchy;
            uint itemID;
            IntPtr docData;
            int hr = runningDocTable.FindAndLockDocument(
                (uint)_VSRDTFLAGS.RDT_ReadLock,
                fileName,
                out hierarchy,
                out itemID,
                out docData,
                out docCookie
            );
            ErrorHandler.ThrowOnFailure(hr);

            // Send the notification
            hr = runningDocTable.NotifyDocumentChanged(docCookie, (uint)__VSRDTATTRIB.RDTA_DocDataReloaded);

            // Unlock the document.
            // Note that we have to unlock the document even if the previous call failed.
            ErrorHandler.ThrowOnFailure(runningDocTable.UnlockDocument((uint)_VSRDTFLAGS.RDT_ReadLock, docCookie));

            // Check ff the call to NotifyDocChanged failed.
            ErrorHandler.ThrowOnFailure(hr);
        }

        void OnSelectionChanged(object sender, EventArgs e)
        {
            ITrackSelection track = TrackSelection;
            if (null != track)
            {
                ErrorHandler.ThrowOnFailure(track.OnSelectChange((ISelectionContainer)selContainer));
            }
        }

        #region Command Handling Functions

        protected virtual void setupCommands()
        {
        }

        protected  void onQueryUnimplemented(object sender, EventArgs e)
        {
            OleMenuCommand command = (OleMenuCommand)sender;
            command.Enabled = false;
            command.Supported = true;
            command.Visible = true;
        }

        //protected void onUnimplemented(object sender, EventArgs e)
        //{
        //}

        /// <summary>
        /// Helper function used to add commands using IMenuCommandService
        /// </summary>
        /// <param name="mcs"> The IMenuCommandService interface.</param>
        /// <param name="menuGroup"> This guid represents the menu group of the command.</param>
        /// <param name="cmdID"> The command ID of the command.</param>
        /// <param name="commandEvent"> An EventHandler which will be called whenever the command is invoked.</param>
        /// <param name="queryEvent"> An EventHandler which will be called whenever we want to query the status of
        /// the command.  If null is passed in here then no EventHandler will be added.</param>
        protected static void addCommand(IMenuCommandService mcs, Guid menuGroup, int cmdID,
                                       EventHandler commandEvent, EventHandler queryEvent)
        {
            // Create the OleMenuCommand from the menu group, command ID, and command event
            CommandID menuCommandID = new CommandID(menuGroup, cmdID);
            OleMenuCommand command = new OleMenuCommand(commandEvent, menuCommandID);

            // Add an event handler to BeforeQueryStatus if one was passed in
            if (null != queryEvent)
            {
                command.BeforeQueryStatus += queryEvent;
            }

            // Add the command using our IMenuCommandService instance
            mcs.AddCommand(command);
        }


        #endregion

        #region IEditor Implementation

        // Note that all functions implemented here call functions from the rich
        // edit control's text object model.

        /// <summary>
        /// This property gets/sets the default tab width.
        /// </summary>
        public float DefaultTabStop
        {
            get { return 0.0f; }
            set { }
        }


        /// <summary>
        /// This function performs the cut operation in the editor.
        /// </summary>
        /// <returns> HResult that indicates success/failure.</returns>
        public int Cut()
        {
            //object o = null;
            //editorControl.TextSelection.Cut(out o);
            editorControl.Action(Actions.Cut);
            return VSConstants.S_OK;
        }

        /// <summary>
        /// This function performs the copy operation in the editor.
        /// </summary>
        /// <returns> HResult that indicates success/failure.</returns>
        public int Copy()
        {
            //object o = null;
            //editorControl.TextSelection.Copy(out o);
            editorControl.Action(Actions.Copy);
            return VSConstants.S_OK;
        }

        /// <summary>
        /// This function performs the paste operation in the editor.
        /// </summary>
        /// <returns> HResult that indicates success/failure.</returns>
        public int Paste()
        {
            //object o = null;
            //editorControl.TextSelection.Paste(ref o, 0);
            editorControl.Action(Actions.Paste);
            return VSConstants.S_OK;
        }

        /// <summary>
        /// This function performs a delete in the editor.
        /// </summary>
        /// <param name="unit"> The type of units that we are going to delete.  The two valid options
        /// for this are TOMWord and TOMCharacter, which are defined in the TOMConstants enumeration.</param>
        /// <param name="count"> The number of units that we are going to delete.  Passing in a negative number
        /// will be similar to pressing backspace and passing in a positive number will be similar to
        /// pressing delete.</param>
        /// <returns> HResult that indicates success/failure.</returns>
        public int Delete(long unit, long count)
        {
            //editorControl.TextSelection.Delete((int)unit, (int)count);
            editorControl.Action(Actions.RemoveSelected);
            return VSConstants.S_OK;
        }

 
        #endregion

        #region IExtensibleObject Implementation

        /// <summary>
        /// This function is used for Macro playback.  Whenever a macro gets played this funtion will be
        /// called and then the IEditor functions will be called on the object that ppDisp is set to.
        /// Since VOFormEditorPane implements IEditor we will just set it to "this".
        /// </summary>
        /// <param name="Name"> Passing in either null, empty string or "Document" will work.  Anything
        /// else will result in ppDisp being set to null.</param>
        /// <param name="pParent"> An object of type IExtensibleObjectSite.  We will keep a reference to this
        /// so that in the Dispose method we can call the NotifyDelete function.</param>
        /// <param name="ppDisp"> The object that this is set to will act as the automation object for macro
        /// playback.  In our case since IEditor is the automation interface and VOFormEditorPane
        /// implements it we will just be setting this parameter to "this".</param>
        void IExtensibleObject.GetAutomationObject(string Name, IExtensibleObjectSite pParent, out Object ppDisp)
        {
            // null or empty string just means the default object, but if a specific string
            // is specified, then make sure it's the correct one, but don't enforce case
            if (!string.IsNullOrEmpty(Name) && !Name.Equals("Document", StringComparison.CurrentCultureIgnoreCase))
            {
                ppDisp = null;
                return;
            }

            // Set the out value to this
            ppDisp = this;

            // Store the IExtensibleObjectSite object, it will be used in the Dispose method
            extensibleObjectSite = pParent;
        }

        #endregion

        int Microsoft.VisualStudio.OLE.Interop.IPersist.GetClassID(out Guid pClassID)
        {
            // This is a bug in the sample code and causes infinite recursion
            //ErrorHandler.ThrowOnFailure(((Microsoft.VisualStudio.OLE.Interop.IPersist)this).GetClassID(out pClassID));
            // Note This must be implemented in the subclass
            pClassID = _GetClassID();
            return VSConstants.S_OK;
        }
        protected virtual Guid _GetClassID()
        {
            return Guid.Empty;
        }

        #region IPersistFileFormat Members

        /// <summary>
        /// Notifies the object that it has concluded the Save transaction
        /// </summary>
        /// <param name="strFileName">Pointer to the file name</param>
        /// <returns>S_OK if the funtion succeeds</returns>
        int IPersistFileFormat.SaveCompleted(string strFileName)
        {
            // TODO:  Add Editor.SaveCompleted implementation
            return VSConstants.S_OK;
        }

        /// <summary>
        /// Returns the path to the object's current working file 
        /// </summary>
        /// <param name="pstrFileName">Pointer to the file name</param>
        /// <param name="pnFormatIndex">Value that indicates the current format of the file as a zero based index
        /// into the list of formats. Since we support only a single format, we need to return zero. 
        /// Subsequently, we will return a single element in the format list through a call to GetFormatList.</param>
        /// <returns></returns>
        int IPersistFileFormat.GetCurFile(out string pstrFileName, out uint pnFormatIndex)
        {
            // We only support 1 format so return its index
            pnFormatIndex = MyFormat;
            pstrFileName = fileName;
            return VSConstants.S_OK;
        }
        /// <summary>
        /// Initialization for the object 
        /// </summary>
        /// <param name="nFormatIndex">Zero based index into the list of formats that indicates the current format 
        /// of the file</param>
        /// <returns>S_OK if the method succeeds</returns>
        int IPersistFileFormat.InitNew(uint nFormatIndex)
        {
            if (nFormatIndex != MyFormat)
            {
                return VSConstants.E_INVALIDARG;
            }
            // until someone change the file, we can consider it not dirty as
            // the user would be annoyed if we prompt him to save an empty file
            isDirty = false;
            return VSConstants.S_OK;
        }

        /// <summary>
        /// Returns the class identifier of the editor type
        /// </summary>
        /// <param name="pClassID">pointer to the class identifier</param>
        /// <returns>S_OK if the method succeeds</returns>
        int IPersistFileFormat.GetClassID(out Guid pClassID)
        {
            ErrorHandler.ThrowOnFailure(((Microsoft.VisualStudio.OLE.Interop.IPersist)this).GetClassID(out pClassID));
            return VSConstants.S_OK;
        }

        /// <summary>
        /// Provides the caller with the information necessary to open the standard common "Save As" dialog box. 
        /// This returns an enumeration of supported formats, from which the caller selects the appropriate format. 
        /// Each string for the format is terminated with a newline (\n) character. 
        /// The last string in the buffer must be terminated with the newline character as well. 
        /// The first string in each pair is a display string that describes the filter, such as "Text Only 
        /// (*.txt)". The second string specifies the filter pattern, such as "*.txt". To specify multiple filter 
        /// patterns for a single display string, use a semicolon to separate the patterns: "*.htm;*.html;*.asp". 
        /// A pattern string can be a combination of valid file name characters and the asterisk (*) wildcard character. 
        /// Do not include spaces in the pattern string. The following string is an example of a file pattern string: 
        /// "HTML File (*.htm; *.html; *.asp)\n*.htm;*.html;*.asp\nText File (*.txt)\n*.txt\n."
        /// </summary>
        /// <param name="ppszFormatList">Pointer to a string that contains pairs of format filter strings</param>
        /// <returns>S_OK if the method succeeds</returns>
        int IPersistFileFormat.GetFormatList(out string ppszFormatList)
        {
            ppszFormatList = getFormatList();
            return VSConstants.S_OK;
        }

        protected virtual string getFormatList()
        {
            char Endline = (char)'\n';
            string FormatList = string.Format(CultureInfo.InvariantCulture, "My Editor (*{0}){1}*{0}{1}{1}", MyExtension, Endline);
            return FormatList;
        }


        internal abstract bool Open(string filename);

        /// <summary>
        /// Loads the file content into the editor
        /// </summary>
        /// <param name="filename">Pointer to the full path name of the file to load</param>
        /// <param name="grfMode">file format mode</param>
        /// <param name="fReadOnly">determines if the file should be opened as read only</param>
        /// <returns>S_OK if the method succeeds</returns>
        int IPersistFileFormat.Load(string filename, uint grfMode, int fReadOnly)
        {
            if (filename == null)
            {
                return VSConstants.E_INVALIDARG;
            }

            loading = true;
            int hr = VSConstants.S_OK;
            bool lSuccess = false;
            try
            {
                // Show the wait cursor while loading the file
                IVsUIShell VsUiShell = (IVsUIShell)GetService(typeof(SVsUIShell));
                if (VsUiShell != null)
                {
                    // Note: we don't want to throw or exit if this call fails, so
                    // don't check the return code.
                    hr = VsUiShell.SetWaitCursor();
                }

                if (this.Open(filename))
                {
                    this.isLoaded = true;
                    lSuccess = true;

                    FileAttributes fileAttrs = File.GetAttributes(filename);

                    int isReadOnly = (int)fileAttrs & (int)FileAttributes.ReadOnly;

                    //Set readonly if either the file is readonly for the user or on the file system
                    if (0 == isReadOnly && 0 == fReadOnly)
                        SetReadOnly(false);
                    else
                        SetReadOnly(true);
                }
                this.isDirty = false;

                // Hook up to file change notifications
                if (String.IsNullOrEmpty(fileName) || 0 != String.Compare(fileName, filename, true, CultureInfo.CurrentCulture))
                {
                    fileName = filename;
                    SetFileChangeNotification(filename, true);

                    // Notify the load or reload
                    NotifyDocChanged();
                }
            }
            catch (Exception e)
            {
                if (System.Diagnostics.Debugger.IsAttached)
                    System.Diagnostics.Debug.WriteLine(e.Message);
            }
            finally
            {
                loading = false;
            }

            if (!lSuccess)
            {
                return VSConstants.E_ABORT;
            }

            return VSConstants.S_OK;
        }

        /// <summary>
        /// Determines whether an object has changed since being saved to its current file
        /// </summary>
        /// <param name="pfIsDirty">true if the document has changed</param>
        /// <returns>S_OK if the method succeeds</returns>
        int IPersistFileFormat.IsDirty(out int pfIsDirty)
        {
            if (isDirty)
            {
                pfIsDirty = 1;
            }
            else
            {
                pfIsDirty = 0;
            }
            return VSConstants.S_OK;
        }

        /// <summary>
        /// Save the contents of the editor into the specified file. If doing the save on the same file, we need to
        /// suspend notifications for file changes during the save operation.
        /// </summary>
        /// <param name="strFileName">Pointer to the file name. If the strFileName parameter is a null reference 
        /// we need to save using the current file
        /// </param>
        /// <param name="remember">Boolean value that indicates whether the strFileName parameter is to be used 
        /// as the current working file.
        /// If remember != 0, strFileName needs to be made the current file and the dirty flag needs to be cleared after the save.
        ///                   Also, file notifications need to be enabled for the new file and disabled for the old file 
        /// If remember == 0, this save operation is a Save a Copy As operation. In this case, 
        ///                   the current file is unchanged and dirty flag is not cleared
        /// </param>
        /// <param name="nFormatIndex">Zero based index into the list of formats that indicates the format in which 
        /// the file will be saved</param>
        /// <returns>S_OK if the method succeeds</returns>
        int IPersistFileFormat.Save(string strFileName, int fRemember, uint nFormatIndex)
        {
            int hr = VSConstants.S_OK;
            bool doingSaveOnSameFile = false;
            bool lSuccess = false;
            // If file is null or same --> SAVE
            if (strFileName == null || strFileName == fileName)
            {
                fRemember = 1;
                doingSaveOnSameFile = true;
            }

            //Suspend file change notifications for only Save since we don't have notifications setup
            //for SaveAs and SaveCopyAs (as they are different files)
            if (doingSaveOnSameFile)
                this.SuspendFileChangeNotification(strFileName, 1);

            try
            {
                if (this.isLoaded)
                    lSuccess = editorControl.Save(strFileName);
            }
            catch (ArgumentException)
            {
                hr = VSConstants.E_FAIL;
            }
            catch (IOException)
            {
                hr = VSConstants.E_FAIL;
            }
            finally
            {
                //restore the file change notifications
                if (doingSaveOnSameFile)
                    this.SuspendFileChangeNotification(strFileName, 0);
            }

            if (VSConstants.E_FAIL == hr)
                return hr;

            if (lSuccess)
            {
                //Save and Save as
                if (fRemember != 0)
                {
                    //Save as
                    if (null != strFileName && !fileName.Equals(strFileName))
                    {
                        SetFileChangeNotification(fileName, false); //remove notification from old file
                        SetFileChangeNotification(strFileName, true); //add notification for new file
                        fileName = strFileName;     //cache the new file name
                    }
                    isDirty = false;
                    SetReadOnly(false);             //set read only to false since you were successfully able
                                                    //to save to the new file                                                    
                }
                /*
                ITrackSelection track = TrackSelection;
                if (null != track)
                {
                    hr = track.OnSelectChange((ISelectionContainer)selContainer);
                }
                */
                // Since all changes are now saved properly to disk, there's no need for a backup.
                backupObsolete = false;
            }
            return hr;
        }

        #endregion


        #region IVsPersistDocData Members

        /// <summary>
        /// Used to determine if the document data has changed since the last time it was saved
        /// </summary>
        /// <param name="pfDirty">Will be set to 1 if the data has changed</param>
        /// <returns>S_OK if the function succeeds</returns>
        int IVsPersistDocData.IsDocDataDirty(out int pfDirty)
        {
            return ((IPersistFileFormat)this).IsDirty(out pfDirty);
        }

        /// <summary>
        /// Saves the document data. Before actually saving the file, we first need to indicate to the environment
        /// that a file is about to be saved. This is done through the "SVsQueryEditQuerySave" service. We call the
        /// "QuerySaveFile" function on the service instance and then proceed depending on the result returned as follows:
        /// If result is QSR_SaveOK - We go ahead and save the file and the file is not read only at this point.
        /// If result is QSR_ForceSaveAs - We invoke the "Save As" functionality which will bring up the Save file name 
        ///                                dialog 
        /// If result is QSR_NoSave_Cancel - We cancel the save operation and indicate that the document could not be saved
        ///                                by setting the "pfSaveCanceled" flag
        /// If result is QSR_NoSave_Continue - Nothing to do here as the file need not be saved
        /// </summary>
        /// <param name="dwSave">Flags which specify the file save options:
        /// VSSAVE_Save        - Saves the current file to itself.
        /// VSSAVE_SaveAs      - Prompts the User for a filename and saves the file to the file specified.
        /// VSSAVE_SaveCopyAs  - Prompts the user for a filename and saves a copy of the file with a name specified.
        /// VSSAVE_SilentSave  - Saves the file without prompting for a name or confirmation.  
        /// </param>
        /// <param name="pbstrMkDocumentNew">Pointer to the path to the new document</param>
        /// <param name="pfSaveCanceled">value 1 if the document could not be saved</param>
        /// <returns></returns>
        int IVsPersistDocData.SaveDocData(Microsoft.VisualStudio.Shell.Interop.VSSAVEFLAGS dwSave, out string pbstrMkDocumentNew, out int pfSaveCanceled)
        {
            pbstrMkDocumentNew = null;
            pfSaveCanceled = 0;
            int hr = VSConstants.S_OK;

            switch (dwSave)
            {
                case VSSAVEFLAGS.VSSAVE_Save:
                case VSSAVEFLAGS.VSSAVE_SilentSave:
                    {
                        IVsQueryEditQuerySave2 queryEditQuerySave = (IVsQueryEditQuerySave2)GetService(typeof(SVsQueryEditQuerySave));

                        // Call QueryEditQuerySave
                        uint result = 0;
                        hr = queryEditQuerySave.QuerySaveFile(
                                fileName,        // filename
                                0,    // flags
                                null,            // file attributes
                                out result);    // result
                        if (ErrorHandler.Failed(hr))
                            return hr;

                        // Process according to result from QuerySave
                        switch ((tagVSQuerySaveResult)result)
                        {
                            case tagVSQuerySaveResult.QSR_NoSave_Cancel:
                                // Note that this is also case tagVSQuerySaveResult.QSR_NoSave_UserCanceled because these
                                // two tags have the same value.
                                pfSaveCanceled = ~0;
                                break;

                            case tagVSQuerySaveResult.QSR_SaveOK:
                                {
                                    // Call the shell to do the save for us
                                    IVsUIShell uiShell = (IVsUIShell)GetService(typeof(SVsUIShell));
                                    hr = uiShell.SaveDocDataToFile(dwSave, (IPersistFileFormat)this, fileName, out pbstrMkDocumentNew, out pfSaveCanceled);
                                    if (ErrorHandler.Failed(hr))
                                        return hr;
                                }
                                break;

                            case tagVSQuerySaveResult.QSR_ForceSaveAs:
                                {
                                    // Call the shell to do the SaveAS for us
                                    IVsUIShell uiShell = (IVsUIShell)GetService(typeof(SVsUIShell));
                                    hr = uiShell.SaveDocDataToFile(VSSAVEFLAGS.VSSAVE_SaveAs, (IPersistFileFormat)this, fileName, out pbstrMkDocumentNew, out pfSaveCanceled);
                                    if (ErrorHandler.Failed(hr))
                                        return hr;
                                }
                                break;

                            case tagVSQuerySaveResult.QSR_NoSave_Continue:
                                // In this case there is nothing to do.
                                break;

                            default:
                                throw new NotSupportedException("Unsupported result from QEQS");
                        }
                        break;
                    }
                case VSSAVEFLAGS.VSSAVE_SaveAs:
                case VSSAVEFLAGS.VSSAVE_SaveCopyAs:
                    {
                        // Make sure the file name as the right extension
                        if (String.Compare(MyExtension, System.IO.Path.GetExtension(fileName), true, CultureInfo.CurrentCulture) != 0)
                        {
                            fileName += MyExtension;
                        }
                        // Call the shell to do the save for us
                        IVsUIShell uiShell = (IVsUIShell)GetService(typeof(SVsUIShell));
                        hr = uiShell.SaveDocDataToFile(dwSave, (IPersistFileFormat)this, fileName, out pbstrMkDocumentNew, out pfSaveCanceled);
                        if (ErrorHandler.Failed(hr))
                            return hr;
                        break;
                    }
                default:
                    throw new ArgumentException("Unsupported Save flag");
            };

            return VSConstants.S_OK;
        }

        /// <summary>
        /// Loads the document data from the file specified
        /// </summary>
        /// <param name="pszMkDocument">Path to the document file which needs to be loaded</param>
        /// <returns>S_Ok if the method succeeds</returns>
        int IVsPersistDocData.LoadDocData(string pszMkDocument)
        {
            return ((IPersistFileFormat)this).Load(pszMkDocument, 0, 0);
        }

        /// <summary>
        /// Used to set the initial name for unsaved, newly created document data
        /// </summary>
        /// <param name="pszDocDataPath">String containing the path to the document. We need to ignore this parameter
        /// </param>
        /// <returns>S_OK if the mthod succeeds</returns>
        int IVsPersistDocData.SetUntitledDocPath(string pszDocDataPath)
        {
            return ((IPersistFileFormat)this).InitNew(MyFormat);
        }

        /// <summary>
        /// Returns the Guid of the editor factory that created the IVsPersistDocData object
        /// </summary>
        /// <param name="pClassID">Pointer to the class identifier of the editor type</param>
        /// <returns>S_OK if the method succeeds</returns>
        int IVsPersistDocData.GetGuidEditorType(out Guid pClassID)
        {
            return ((IPersistFileFormat)this).GetClassID(out pClassID);
        }

        /// <summary>
        /// Close the IVsPersistDocData object
        /// </summary>
        /// <returns>S_OK if the function succeeds</returns>
        int IVsPersistDocData.Close()
        {
            return VSConstants.S_OK;
        }

        /// <summary>
        /// Determines if it is possible to reload the document data
        /// </summary>
        /// <param name="pfReloadable">set to 1 if the document can be reloaded</param>
        /// <returns>S_OK if the method succeeds</returns>
        int IVsPersistDocData.IsDocDataReloadable(out int pfReloadable)
        {
            // Allow file to be reloaded
            pfReloadable = 1;
            return VSConstants.S_OK;
        }

        /// <summary>
        /// Renames the document data
        /// </summary>
        /// <param name="grfAttribs"></param>
        /// <param name="pHierNew"></param>
        /// <param name="itemidNew"></param>
        /// <param name="pszMkDocumentNew"></param>
        /// <returns></returns>
        int IVsPersistDocData.RenameDocData(uint grfAttribs, IVsHierarchy pHierNew, uint itemidNew, string pszMkDocumentNew)
        {
            // TODO:  Add VOFormEditorPane.RenameDocData implementation
            return VSConstants.S_OK;
        }

        /// <summary>
        /// Reloads the document data
        /// </summary>
        /// <param name="grfFlags">Flag indicating whether to ignore the next file change when reloading the document data.
        /// This flag should not be set for us since we implement the "IVsDocDataFileChangeControl" interface in order to 
        /// indicate ignoring of file changes
        /// </param>
        /// <returns>S_OK if the mthod succeeds</returns>
        int IVsPersistDocData.ReloadDocData(uint grfFlags)
        {
            return ((IPersistFileFormat)this).Load(fileName, grfFlags, 0);
        }

        /// <summary>
        /// Called by the Running Document Table when it registers the document data. 
        /// </summary>
        /// <param name="docCookie">Handle for the document to be registered</param>
        /// <param name="pHierNew">Pointer to the IVsHierarchy interface</param>
        /// <param name="itemidNew">Item identifier of the document to be registered from VSITEM</param>
        /// <returns></returns>
        int IVsPersistDocData.OnRegisterDocData(uint docCookie, IVsHierarchy pHierNew, uint itemidNew)
        {
            //Nothing to do here
            return VSConstants.S_OK;
        }

        #endregion

        #region IVsFileChangeEvents Members

        /// <summary>
        /// Notify the editor of the changes made to one or more files
        /// </summary>
        /// <param name="cChanges">Number of files that have changed</param>
        /// <param name="rgpszFile">array of the files names that have changed</param>
        /// <param name="rggrfChange">Array of the flags indicating the type of changes</param>
        /// <returns></returns>
        int IVsFileChangeEvents.FilesChanged(uint cChanges, string[] rgpszFile, uint[] rggrfChange)
        {
            XSharpProjectPackage.Instance.DisplayOutPutMessage("**** Inside FilesChanged ****");

            //check the different parameters
            if (0 == cChanges || null == rgpszFile || null == rggrfChange)
                return VSConstants.E_INVALIDARG;

            //ignore file changes if we are in that mode
            if (ignoreFileChangeLevel != 0)
                return VSConstants.S_OK;

            for (uint i = 0; i < cChanges; i++)
            {
                if (!String.IsNullOrEmpty(rgpszFile[i]) && String.Compare(rgpszFile[i], fileName, true, CultureInfo.CurrentCulture) == 0)
                {
                    // if the readonly state (file attributes) have changed we can immediately update
                    // the editor to match the new state (either readonly or not readonly) immediately
                    // without prompting the user.
                    if (0 != (rggrfChange[i] & (int)_VSFILECHANGEFLAGS.VSFILECHG_Attr))
                    {
                        FileAttributes fileAttrs = File.GetAttributes(fileName);
                        int isReadOnly = (int)fileAttrs & (int)FileAttributes.ReadOnly;
                        SetReadOnly(isReadOnly != 0);
                    }
                    // if it looks like the file contents have changed (either the size or the modified
                    // time has changed) then we need to prompt the user to see if we should reload the
                    // file. it is important to not syncronisly reload the file inside of this FilesChanged
                    // notification. first it is possible that there will be more than one FilesChanged 
                    // notification being sent (sometimes you get separate notifications for file attribute
                    // changing and file size/time changing). also it is the preferred UI style to not
                    // prompt the user until the user re-activates the environment application window.
                    // this is why we use a timer to delay prompting the user.
                    if (0 != (rggrfChange[i] & (int)(_VSFILECHANGEFLAGS.VSFILECHG_Time | _VSFILECHANGEFLAGS.VSFILECHG_Size)))
                    {
                        if (!fileChangedTimerSet)
                        {
                            FileChangeTrigger = new Timer();
                            fileChangedTimerSet = true;
                            FileChangeTrigger.Interval = 1000;
                            FileChangeTrigger.Tick += new EventHandler(this.OnFileChangeEvent);
                            FileChangeTrigger.Enabled = true;
                        }
                    }
                }
            }

            return VSConstants.S_OK;
        }

        /// <summary>
        /// Notify the editor of the changes made to a directory
        /// </summary>
        /// <param name="pszDirectory">Name of the directory that has changed</param>
        /// <returns></returns>
        int IVsFileChangeEvents.DirectoryChanged(string pszDirectory)
        {
            //Nothing to do here
            return VSConstants.S_OK;
        }
        #endregion

        #region IVsDocDataFileChangeControl Members

        /// <summary>
        /// Used to determine whether changes to DocData in files should be ignored or not
        /// </summary>
        /// <param name="fIgnore">a non zero value indicates that the file changes should be ignored
        /// </param>
        /// <returns></returns>
        int IVsDocDataFileChangeControl.IgnoreFileChanges(int fIgnore)
        {
            XSharpProjectPackage.Instance.DisplayOutPutMessage("**** Inside IgnoreFileChanges ****");

            if (fIgnore != 0)
            {
                ignoreFileChangeLevel++;
            }
            else
            {
                if (ignoreFileChangeLevel > 0)
                    ignoreFileChangeLevel--;

                // We need to check here if our file has changed from "Read Only"
                // to "Read/Write" or vice versa while the ignore level was non-zero.
                // This may happen when a file is checked in or out under source
                // code control. We need to check here so we can update our caption.
                FileAttributes fileAttrs = File.GetAttributes(fileName);
                int isReadOnly = (int)fileAttrs & (int)FileAttributes.ReadOnly;
                SetReadOnly(isReadOnly != 0);
            }
            return VSConstants.S_OK;
        }
        #endregion

        #region File Change Notification Helpers

        /// <summary>
        /// In this function we inform the shell when we wish to receive 
        /// events when our file is changed or we inform the shell when 
        /// we wish not to receive events anymore.
        /// </summary>
        /// <param name="strFileName">File name string</param>
        /// <param name="fStart">TRUE indicates advise, FALSE indicates unadvise.</param>
        /// <returns>Result of teh operation</returns>
        private int SetFileChangeNotification(string strFileName, bool fStart)
        {
            XSharpProjectPackage.Instance.DisplayOutPutMessage("**** Inside SetFileChangeNotification ****");

            int result = VSConstants.E_FAIL;

            //Get the File Change service
            if (null == vsFileChangeEx)
                vsFileChangeEx = (IVsFileChangeEx)GetService(typeof(SVsFileChangeEx));
            if (null == vsFileChangeEx)
                return VSConstants.E_UNEXPECTED;

            // Setup Notification if fStart is TRUE, Remove if fStart is FALSE.
            if (fStart)
            {
                if (vsFileChangeCookie == VSConstants.VSCOOKIE_NIL)
                {
                    //Receive notifications if either the attributes of the file change or 
                    //if the size of the file changes or if the last modified time of the file changes
                    result = vsFileChangeEx.AdviseFileChange(strFileName,
                        (uint)(_VSFILECHANGEFLAGS.VSFILECHG_Attr | _VSFILECHANGEFLAGS.VSFILECHG_Size | _VSFILECHANGEFLAGS.VSFILECHG_Time),
                        (IVsFileChangeEvents)this,
                        out vsFileChangeCookie);
                    if (vsFileChangeCookie == VSConstants.VSCOOKIE_NIL)
                        return VSConstants.E_FAIL;
                }
            }
            else
            {
                if (vsFileChangeCookie != VSConstants.VSCOOKIE_NIL)
                {
                    result = vsFileChangeEx.UnadviseFileChange(vsFileChangeCookie);
                    vsFileChangeCookie = VSConstants.VSCOOKIE_NIL;
                }
            }
            return result;
        }

        /// <summary>
        /// In this function we suspend receiving file change events for
        /// a file or we reinstate a previously suspended file depending
        /// on the value of the given fSuspend flag.
        /// </summary>
        /// <param name="strFileName">File name string</param>
        /// <param name="fSuspend">TRUE indicates that the events needs to be suspended</param>
        /// <returns></returns>

        private int SuspendFileChangeNotification(string strFileName, int fSuspend)
        {
            XSharpProjectPackage.Instance.DisplayOutPutMessage("**** Inside SuspendFileChangeNotification ****");

            if (null == vsFileChangeEx)
                vsFileChangeEx = (IVsFileChangeEx)GetService(typeof(SVsFileChangeEx));
            if (null == vsFileChangeEx)
                return VSConstants.E_UNEXPECTED;

            if (0 == fSuspend)
            {
                // we are transitioning from suspended to non-suspended state - so force a
                // sync first to avoid asynchronous notifications of our own change
                if (vsFileChangeEx.SyncFile(strFileName) == VSConstants.E_FAIL)
                    return VSConstants.E_FAIL;
            }

            //If we use the VSCOOKIE parameter to specify the file, then pszMkDocument parameter 
            //must be set to a null reference and vice versa 
            return vsFileChangeEx.IgnoreFile(vsFileChangeCookie, null, fSuspend);
        }
        #endregion

        #region IVsFileBackup Members

        /// <summary>
        /// This method is used to Persist the data to a single file. On a successful backup this 
        /// should clear up the backup dirty bit
        /// </summary>
        /// <param name="strBackupFileName">Name of the file to persist</param>
        /// <returns>S_OK if the data can be successfully persisted.
        /// This should return STG_S_DATALOSS or STG_E_INVALIDCODEPAGE if there is no way to 
        /// persist to a file without data loss
        /// </returns>
        int IVsFileBackup.BackupFile(string strBackupFileName)
        {
            try
            {
                editorControl.Save(strBackupFileName, true);
                backupObsolete = false;
            }
            catch (ArgumentException)
            {
                return VSConstants.E_FAIL;
            }
            catch (IOException)
            {
                return VSConstants.E_FAIL;
            }
            return VSConstants.S_OK;
        }

        /// <summary>
        /// Used to set the backup dirty bit. This bit should be set when the object is modified 
        /// and cleared on calls to BackupFile and any Save method
        /// </summary>
        /// <param name="pbObsolete">the dirty bit to be set</param>
        /// <returns>returns 1 if the backup dirty bit is set, 0 otherwise</returns>
        int IVsFileBackup.IsBackupFileObsolete(out int pbObsolete)
        {
            if (backupObsolete)
                pbObsolete = 1;
            else
                pbObsolete = 0;
            return VSConstants.S_OK;
        }

        #endregion


        /// <summary>
        /// Used to ReadOnly property for the Rich TextBox and correspondingly update the editor caption
        /// </summary>
        /// <param name="_isFileReadOnly">Indicates whether the file loaded is Read Only or not</param>
        private void SetReadOnly(bool _isFileReadOnly)
        {
            this.editorControl.ReadOnly = _isFileReadOnly;

            //update editor caption with "[Read Only]" or "" as necessary
            IVsWindowFrame frame = (IVsWindowFrame)GetService(typeof(SVsWindowFrame));
            string editorCaption = "";
            if (_isFileReadOnly)
                editorCaption = this.GetResourceString("@80200");
            ErrorHandler.ThrowOnFailure(frame.SetProperty((int)__VSFPROPID.VSFPROPID_EditorCaption, editorCaption));
            backupObsolete = true;
        }

        /// <summary>
        /// This event is triggered when one of the files loaded into the environment has changed outside of the
        /// editor
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void OnFileChangeEvent(object sender, System.EventArgs e)
        {
            //Disable the timer
            FileChangeTrigger.Enabled = false;

            string message = this.GetResourceString("@80201");    //get the message string from the resource
            IVsUIShell VsUiShell = (IVsUIShell)GetService(typeof(SVsUIShell));
            int result = 0;
            Guid tempGuid = Guid.Empty;
            if (VsUiShell != null)
            {
                //Show up a message box indicating that the file has changed outside of VS environment
                ErrorHandler.ThrowOnFailure(VsUiShell.ShowMessageBox(0, ref tempGuid, fileName, message, null, 0,
                    OLEMSGBUTTON.OLEMSGBUTTON_YESNOCANCEL, OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST,
                    OLEMSGICON.OLEMSGICON_QUERY, 0, out result));
            }
            //if the user selects "Yes", reload the current file
            if (result == (int)DialogResult.Yes)
            {
                ErrorHandler.ThrowOnFailure(((IVsPersistDocData)this).ReloadDocData(0));
            }

            fileChangedTimerSet = false;
        }

        /// <summary>
        /// This method loads a localized string based on the specified resource.
        /// </summary>
        /// <param name="resourceName">Resource to load</param>
        /// <returns>String loaded for the specified resource</returns>
        internal string GetResourceString(string resourceName)
        {
            string resourceValue;
            IVsResourceManager resourceManager = (IVsResourceManager)GetService(typeof(SVsResourceManager));
            if (resourceManager == null)
            {
                throw new InvalidOperationException("Could not get SVsResourceManager service. Make sure the package is Sited before calling this method");
            }
            Guid packageGuid = myPackage.GetType().GUID;
            int hr = resourceManager.LoadResourceString(ref packageGuid, -1, resourceName, out resourceValue);
            Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(hr);
            return resourceValue;
        }

        /// <summary>
        /// This function asks to the QueryEditQuerySave service if it is possible to
        /// edit the file.
        /// </summary>
        private bool CanEditFile()
        {

            // Check the status of the recursion guard
            if (gettingCheckoutStatus)
                return false;

            try
            {
                // Set the recursion guard
                gettingCheckoutStatus = true;

                // Get the QueryEditQuerySave service
                IVsQueryEditQuerySave2 queryEditQuerySave = (IVsQueryEditQuerySave2)GetService(typeof(SVsQueryEditQuerySave));

                // Now call the QueryEdit method to find the edit status of this file
                string[] documents = { this.fileName };
                uint result;
                uint outFlags;

                // Note that this function can popup a dialog to ask the user to checkout the file.
                // When this dialog is visible, it is possible to receive other request to change
                // the file and this is the reason for the recursion guard.
                int hr = queryEditQuerySave.QueryEditFiles(
                    0,              // Flags
                    1,              // Number of elements in the array
                    documents,      // Files to edit
                    null,           // Input flags
                    null,           // Input array of VSQEQS_FILE_ATTRIBUTE_DATA
                    out result,     // result of the checkout
                    out outFlags    // Additional flags
                );
                if (ErrorHandler.Succeeded(hr) && (result == (uint)tagVSQueryEditResult.QER_EditOK))
                {
                    // In this case (and only in this case) we can return true from this function.
                    return true;
                }
            }

            finally
            {
                gettingCheckoutStatus = false;
            }
            return false;
        }
    }
    public class VOFormEditorPane : VOEditorPane
    {
        public VOFormEditorPane(XSharpProjectPackage package) : base(package)
        {
            MyExtension = ".xsfrm";
        }
        protected override Guid _GetClassID()
        {
            return GuidStrings.guidVOFormEditorFactory;
        }
        protected override string getFormatList()
        {
            char Endline = (char)'\n';
            string FormatList = string.Format(CultureInfo.InvariantCulture, "Window Editor (*{0}){1}*{0}{1}{1}", MyExtension, Endline);
            return FormatList;
        }

        internal override bool Open(string filename)
        {
            return this.editorControl.OpenWindow(filename);
        }

        #region Command Handling
        protected override void setupCommands()
        {
            // Now get the IMenuCommandService; this object is the one
            // responsible for handling the collection of commands implemented by the package.

            IMenuCommandService mcs = GetService(typeof(IMenuCommandService)) as IMenuCommandService;
            if (null != mcs)
            {
                // Now create one object derived from MenuCommnad for each command defined in
                // the .vsct file and add it to the command service.

                // For each command we have to define its id that is a unique Guid/integer pair, then
                // create the OleMenuCommand object for this command. The EventHandler object is the
                // function that will be called when the user will select the command. Then we add the 
                // OleMenuCommand to the menu service.  The addCommand helper function does all this for us.

                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.SelectAll,
                                new EventHandler(onSelectAll), null);
                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.Copy,
                                new EventHandler(onCopy), new EventHandler(onQueryCopy));
                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.Cut,
                                new EventHandler(onCut), new EventHandler(onQueryCutOrDelete));
                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.Paste,
                                new EventHandler(onPaste), new EventHandler(onQueryPaste));
                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.Delete,
                                new EventHandler(onDelete), new EventHandler(onQueryCutOrDelete));
                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.Undo,
                                new EventHandler(onUndo), new EventHandler(onQueryUndo));
                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.Redo,
                                new EventHandler(onRedo), new EventHandler(onQueryRedo));
                // These two commands enable Visual Studio's default undo/redo toolbar buttons.  When these
                // buttons are clicked it triggers a multi-level undo/redo (even when we are undoing/redoing
                // only one action.  Note that we are not implementing the multi-level undo/redo functionality,
                // we are just adding a handler for this command so these toolbar buttons are enabled (Note that
                // we are just reusing the undo/redo command handlers).  To implement multi-level functionality
                // we would need to properly handle these two commands as well as MultiLevelUndoList and
                // MultiLevelRedoList.
                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.MultiLevelUndo,
                                new EventHandler(onUndo), new EventHandler(onQueryUndo));
                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.MultiLevelRedo,
                                new EventHandler(onRedo), new EventHandler(onQueryRedo));

                EventHandler onQueryAlignEH = new EventHandler(onQueryAlign);
                EventHandler onQuerySpacingEH = new EventHandler(onQuerySpacing);

                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.AlignLeft,
                                new EventHandler(onAlignLeft), onQueryAlignEH);
                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.AlignRight,
                                new EventHandler(onAlignRight), onQueryAlignEH);

                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.AlignTop,
                                new EventHandler(onAlignTop), onQueryAlignEH);
                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.AlignBottom,
                                new EventHandler(onAlignBottom), onQueryAlignEH);

                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.AlignHorizontalCenters,
                                new EventHandler(onAlignHorizontalCenters), onQueryAlignEH);

                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.AlignVerticalCenters,
                                new EventHandler(onAlignVerticalCenters), onQueryAlignEH);

                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.SizeToControl,
                                new EventHandler(onSameSize), onQueryAlignEH);
                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.SizeToControlWidth,
                                new EventHandler(onSameHorzSize), onQueryAlignEH);
                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.SizeToControlHeight,
                                new EventHandler(onSameVertSize), onQueryAlignEH);

                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.TabOrder,
                                new EventHandler(onTabOrder), null);

                //addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.SizeToGrid,
                //                new EventHandler(onUnimplemented), new EventHandler(onQueryUnimplemented));

                //addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.LockControls,
                //                new EventHandler(onUnimplemented), new EventHandler(onQueryUnimplemented));

                //addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.AlignToGrid,
                //                new EventHandler(onUnimplemented), new EventHandler(onQueryUnimplemented));


                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.HorizSpaceMakeEqual,
                               new EventHandler(onHorizSpaceMakeEqual), new EventHandler(onQuerySpacingEH));

                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.HorizSpaceIncrease,
                                new EventHandler(onHorizSpaceIncrease), new EventHandler(onQuerySpacingEH));

                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.HorizSpaceDecrease,
                                new EventHandler(onHorizSpaceDecrease), new EventHandler(onQuerySpacingEH));

                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.HorizSpaceConcatenate,
                                new EventHandler(onHorizSpaceConcatenate), new EventHandler(onQuerySpacingEH));

                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.VertSpaceMakeEqual,
                                new EventHandler(onVertSpaceMakeEqual), new EventHandler(onQuerySpacingEH));

                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.VertSpaceIncrease,
                                new EventHandler(onVertSpaceIncrease), new EventHandler(onQuerySpacingEH));

                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.VertSpaceDecrease,
                                new EventHandler(onVertSpaceDecrease), new EventHandler(onQuerySpacingEH));

                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.VertSpaceConcatenate,
                                new EventHandler(onVertSpaceConcatenate), new EventHandler(onQuerySpacingEH));


                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.CenterHorizontally,
                                new EventHandler(onCenterHorizontally), new EventHandler(onQueryCopy));

                addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.CenterVertically,
                                new EventHandler(onCenterVertically), new EventHandler(onQueryCopy));


                //addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.BringToFront,
                //               new EventHandler(onUnimplemented), new EventHandler(onQueryUnimplemented));

                //addCommand(mcs, VSConstants.GUID_VSStandardCommandSet97, (int)VSConstants.VSStd97CmdID.SendToBack,
                //                new EventHandler(onUnimplemented), new EventHandler(onQueryUnimplemented));


                // our own commands
                addCommand(mcs, GuidStrings.guidVOFormEditorCmdSet, (int)GuidStrings.cmdidShowGrid,
                                new EventHandler(onToggleGrid), new EventHandler(onQueryViewGrid));

                addCommand(mcs, GuidStrings.guidVOFormEditorCmdSet, (int)GuidStrings.cmdidTestDialog,
                                new EventHandler(onTestDialog), null);
            }
        }
        /// <summary>
        /// Handler for out SelectAll command.
        /// </summary>
        /// <param name="sender">  This can be cast to an OleMenuCommand.</param>
        /// <param name="e">  Not used.</param>
        private void onSelectAll(object sender, EventArgs e)
        {
            editorControl.Action(Actions.SelectAll);
        }

        /// <summary>
        /// Handler for when we want to query the status of the copy command.  If there
        /// is any text selected then it will set the Enabled property to true.
        /// </summary>
        /// <param name="sender">  This can be cast to an OleMenuCommand.</param>
        /// <param name="e">  Not used.</param>
        private void onQueryCopy(object sender, EventArgs e)
        {
            OleMenuCommand command = (OleMenuCommand)sender;
            command.Enabled = editorControl.CanDoAction(Actions.Copy);
        }

        /// <summary>
        /// Handler for our Copy command.
        /// </summary>
        /// <param name="sender">  This can be cast to an OleMenuCommand.</param>
        /// <param name="e">  Not used.</param>
        private void onCopy(object sender, EventArgs e)
        {
            Copy();
            editorControl.RecordCommand("Copy");
        }

        /// <summary>
        /// Handler for when we want to query the status of the cut or delete
        /// commands.  If there is any selected text then it will set the 
        /// enabled property to true.
        /// </summary>
        /// <param name="sender">  This can be cast to an OleMenuCommand.</param>
        /// <param name="e">  Not used.</param>
        private void onQueryCutOrDelete(object sender, EventArgs e)
        {
            OleMenuCommand command = (OleMenuCommand)sender;
            command.Enabled = editorControl.CanDoAction(Actions.Cut);
        }

        /// <summary>
        /// Handler for our Cut command.
        /// </summary>
        /// <param name="sender">  This can be cast to an OleMenuCommand.</param>
        /// <param name="e">  Not used.</param>
        private void onCut(object sender, EventArgs e)
        {
            Cut();
            editorControl.RecordCommand("Cut");
        }

        /// <summary>
        /// Handler for our Delete command.
        /// </summary>
        private void onDelete(object sender, EventArgs e)
        {
            editorControl.Action(Actions.RemoveSelected);
            editorControl.RecordCommand("Delete");
        }

        /// <summary>
        /// Handler for when we want to query the status of the paste command.
        /// </summary>
        /// <param name="sender">  This can be cast to an OleMenuCommand.</param>
        /// <param name="e">  Not used.</param>
        private void onQueryPaste(object sender, EventArgs e)
        {
            OleMenuCommand command = (OleMenuCommand)sender;
            command.Enabled = editorControl.CanDoAction(Actions.Paste);
        }

        /// <summary>
        /// Handler for our Paste command.
        /// </summary>
        /// <param name="sender">  This can be cast to an OleMenuCommand.</param>
        /// <param name="e">  Not used.</param>
        private void onPaste(object sender, EventArgs e)
        {
            Paste();
            editorControl.RecordCommand("Paste");
        }

        private void onQueryAlign(object sender, EventArgs e)
        {
            OleMenuCommand command = (OleMenuCommand)sender;
            command.Enabled = editorControl.CanDoAction(Actions.AlignLeft);
        }
        private void onQuerySpacing(object sender, EventArgs e)
        {
            OleMenuCommand command = (OleMenuCommand)sender;
            command.Enabled = editorControl.CanDoAction(Actions.SpacingHorzEqual);
        }

        private void onAlignLeft(object sender, EventArgs e)
        {
            editorControl.Action(Actions.AlignLeft);
        }
        private void onAlignRight(object sender, EventArgs e)
        {
            editorControl.Action(Actions.AlignRight);
        }
        private void onAlignHorizontalCenters(object sender, EventArgs e)
        {
            editorControl.Action(Actions.AlignCenterVert);
        }
        private void onAlignTop(object sender, EventArgs e)
        {
            editorControl.Action(Actions.AlignTop);
        }
        private void onAlignBottom(object sender, EventArgs e)
        {
            editorControl.Action(Actions.AlignBottom);
        }
        private void onAlignVerticalCenters(object sender, EventArgs e)
        {
            editorControl.Action(Actions.AlignCenterHorz);
        }

        private void onSameSize(object sender, EventArgs e)
        {
            editorControl.Action(Actions.SameSize);
        }
        private void onSameHorzSize(object sender, EventArgs e)
        {
            editorControl.Action(Actions.SameHorSize);
        }
        private void onSameVertSize(object sender, EventArgs e)
        {
            editorControl.Action(Actions.SameVerSize);
        }

        private void onHorizSpaceMakeEqual(object sender, EventArgs e)
        {
            editorControl.Action(Actions.SpacingHorzEqual);
        }
        private void onHorizSpaceIncrease(object sender, EventArgs e)
        {
            editorControl.Action(Actions.SpacingHorzInc);
        }
        private void onHorizSpaceDecrease(object sender, EventArgs e)
        {
            editorControl.Action(Actions.SpacingHorzDec);
        }
        private void onHorizSpaceConcatenate(object sender, EventArgs e)
        {
            editorControl.Action(Actions.SpacingHorzRem);
        }
        private void onVertSpaceMakeEqual(object sender, EventArgs e)
        {
            editorControl.Action(Actions.SpacingVertEqual);
        }
        private void onVertSpaceIncrease(object sender, EventArgs e)
        {
            editorControl.Action(Actions.SpacingVertInc);
        }
        private void onVertSpaceDecrease(object sender, EventArgs e)
        {
            editorControl.Action(Actions.SpacingVertDec);
        }
        private void onVertSpaceConcatenate(object sender, EventArgs e)
        {
            editorControl.Action(Actions.SpacingVertRem);
        }

        private void onCenterHorizontally(object sender, EventArgs e)
        {
            editorControl.Action(Actions.CenterHorz);
        }
        private void onCenterVertically(object sender, EventArgs e)
        {
            editorControl.Action(Actions.CenterVert);
        }


        private void onQueryViewGrid(object sender, EventArgs e)
        {
            OleMenuCommand command = (OleMenuCommand)sender;
            command.Enabled = true;
            command.Checked = editorControl.IsGridEnabled;
        }

        private void onViewGrid(object sender, EventArgs e)
        {
            editorControl.ToggleGrid();
        }

        private void onTabOrder(object sender, EventArgs e)
        {
            editorControl.ShowTabOrder();
        }

        private void onTestDialog(object sender, EventArgs e)
        {
            editorControl.TestForm();
        }
        private void onToggleGrid(object sender, EventArgs e)
        {
            editorControl.ToggleGrid();
        }


        /// <summary>
        /// Handler for when we want to query the status of the Undo command.
        /// </summary>
        /// <param name="sender">  This can be cast to an OleMenuCommand.</param>
        /// <param name="e">  Not used.</param>
        private void onQueryUndo(object sender, EventArgs e)
        {
            OleMenuCommand command = (OleMenuCommand)sender;
            command.Enabled = editorControl.CanDoAction(Actions.Undo);
        }

        /// <summary>
        /// Handler for our Undo command.
        /// </summary>
        /// <param name="sender">  This can be cast to an OleMenuCommand.</param>
        /// <param name="e">  Not used.</param>
        private void onUndo(object sender, EventArgs e)
        {
            editorControl.Action(Actions.Undo);
        }

        /// <summary>
        /// Handler for when we want to query the status of the Redo command.
        /// </summary>
        /// <param name="sender">  This can be cast to an OleMenuCommand.</param>
        /// <param name="e">  Not used.</param>
        private void onQueryRedo(object sender, EventArgs e)
        {
            OleMenuCommand command = (OleMenuCommand)sender;
            command.Enabled = editorControl.CanDoAction(Actions.Redo);
        }

        /// <summary>
        /// Handler for our Redo command.
        /// </summary>
        /// <param name="sender">  This can be cast to an OleMenuCommand.</param>
        /// <param name="e">  Not used.</param>
        private void onRedo(object sender, EventArgs e)
        {
            editorControl.Action(Actions.Redo);
        }

#endregion




    }
    public class VOMenuEditorPane : VOEditorPane
    {
        public VOMenuEditorPane(XSharpProjectPackage package) : base(package)
        {
            MyExtension = ".xsmnu";
        }
        protected override Guid _GetClassID()
        {
            return GuidStrings.guidVOMenuEditorFactory;
        }
        protected override string getFormatList()
        {
            char Endline = (char)'\n';
            string FormatList = string.Format(CultureInfo.InvariantCulture, "Menu Editor (*{0}){1}*{0}{1}{1}", MyExtension, Endline);
            return FormatList;
        }
        internal override bool Open(string filename)
        {
            return this.editorControl.OpenMenu(filename);
        }

    }
    public class VOServerEditorPane : VOEditorPane
    {
        public VOServerEditorPane(XSharpProjectPackage package) : base(package)
        {
            MyExtension = ".xsdbs";
        }
        protected override Guid _GetClassID()
        {
            return GuidStrings.guidVODbServerEditorFactory;
        }
        protected override string getFormatList()
        {
            char Endline = (char)'\n';
            string FormatList = string.Format(CultureInfo.InvariantCulture, "DbServer Editor (*{0}){1}*{0}{1}{1}", MyExtension, Endline);
            return FormatList;
        }
        internal override bool Open(string filename)
        {
            return this.editorControl.OpenDBServer(filename);
        }

    }
    public class VOFieldSpecEditorPane : VOEditorPane
    {
        public VOFieldSpecEditorPane(XSharpProjectPackage package) : base(package)
        {
            MyExtension = ".xsfs";
        }
        protected override Guid _GetClassID()
        {
            return GuidStrings.guidVOFieldSpecEditorFactory;
        }
        protected override string getFormatList()
        {
            char Endline = (char)'\n';
            string FormatList = string.Format(CultureInfo.InvariantCulture, "FieldSpec Editor (*{0}){1}*{0}{1}{1}", MyExtension, Endline);
            return FormatList;
        }
        internal override bool Open(string filename)
        {
            return this.editorControl.OpenFieldSpec(filename);
        }

    }
}
