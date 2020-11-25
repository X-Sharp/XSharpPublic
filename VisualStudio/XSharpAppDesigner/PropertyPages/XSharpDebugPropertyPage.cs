//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Runtime.Versioning;
using System.Windows.Forms;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Project;
using EnvDTE;
using EnvDTE80;
using System.ComponentModel;
using System.Globalization;
using Microsoft.VisualStudio.Shell;

namespace XSharp.Project
{
    /// <summary>
    /// This class implements general property page for the project type.
    /// </summary>
    /// 
    [ComVisible(true)]
    [Guid(XSharpConstants.DebugPropertiesPage)]
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProvideObject(typeof(XSharpDebugPropertyPage))]
    public class XSharpDebugPropertyPage : XSharpSettingsPage
    {
        internal const string catGeneral = "General";
        internal const string catSpecial = "Special";
        internal const string captOutputPath = "Output Path";
        internal const string descOutputPath = "Output Path (macros are allowed)";
        internal const string captDebugType = "Generate Debug Information";
        internal const string descDebugType = "Generate Debug Information (none, full, pdbonly)";
        internal const string captDebuggerCommand = "Command";
        internal const string descDebuggerCommand = "The debug command to execute";
        internal const string captDebuggerCommandArguments = "Command Arguments";
        internal const string descDebuggerCommandArguments = "The command line arguments to pass to the application";
        internal const string captDebuggerWorkingDirectory = "Working Directory";
        internal const string descDebuggerWorkingDirectory = "The application's working directory. By default, the directory containing the project file.";
        internal const string captDebuggerAttach = "Attach";
        internal const string descDebuggerAttach = "Specifies whether the debugger should attempt to attach to an existing process when debugging starts.";
        internal const string captEnableUnmanagedDebugging = "Enable unmanaged debugging";
        internal const string descEnableUnmanagedDebugging = "Enable unmanaged debugging";
        internal const string captUseVSHostingProcess = "Enable the Visual Studio Hosting Process";
        internal const string descUseVSHostingProcess = "Enable the Visual Studio Hosting Process";


        #region Fields
        private DebugType debugtype;
        private string debuggercommand;
        private string debuggercommandarguments;
        private string debuggerworkingdirectory;
        //private bool debuggerattach;
        private bool enableunmanageddebugging;
        //private bool usevshostingprocess;

        #endregion Fields

        #region Constructors
        /// <summary>
        /// Explicitly defined default constructor.
        /// </summary>
        public XSharpDebugPropertyPage()
        {
            this.Name = "Debug";
        }
        #endregion


        [Category(catGeneral), DisplayName(captDebugType), Description(descDebugType)]
        public DebugType DebugType
        {
            get { return this.debugtype; }
            set { this.debugtype = value; this.IsDirty = true; }
        }


        [Category(catGeneral), DisplayName(captDebuggerCommand), Description(descDebuggerCommand)]
        [XSharpFileNameEditor("Select Debug Target", "Executable Files (*.exe)|*.exe|All files (*.*)|*.*", 01)]
        [Editor(typeof(XSharpSLEPropertyEditor), typeof(System.Drawing.Design.UITypeEditor))]

        public string DebuggerCommand
        {
            get { return this.debuggercommand; }
            set{ this.debuggercommand = value; this.IsDirty = true;}
        }
        [Category(catGeneral), DisplayName(captDebuggerCommandArguments), Description(descDebuggerCommandArguments)]
        public string DebuggerCommandArguments
        {
            get { return this.debuggercommandarguments; }
            set { this.debuggercommandarguments = value; this.IsDirty = true; }
        }

        [Category(catGeneral), DisplayName(captDebuggerWorkingDirectory), Description(descDebuggerWorkingDirectory)]
        [Editor(typeof(XSharpSLEPropertyEditor), typeof(System.Drawing.Design.UITypeEditor))]
        public string DebuggerWorkingDirectory
        {
            get { return this.debuggerworkingdirectory; }
            set { this.debuggerworkingdirectory = value; this.IsDirty = true; }
        }

        //[Category(catGeneral), DisplayName(captDebuggerAttach), Description(descDebuggerAttach)]
        //public bool DebuggerAttach
        //{
        //    get { return this.debuggerattach; }
        //    set { this.debuggerattach = value; this.IsDirty = true; }
        //}

        [Category(catSpecial),DisplayName(captEnableUnmanagedDebugging), Description(descEnableUnmanagedDebugging)]
        public bool EnableUnmanagedDebugging
        {
            get { return this.enableunmanageddebugging; }
            set { this.enableunmanageddebugging = value; this.IsDirty = true; }
        }


        //[Category(catSpecial), DisplayName(captUseVSHostingProcess), Description(descUseVSHostingProcess)]
        //public bool UseVSHostingProcess
        //{
        //    get { return this.usevshostingprocess; }
        //    set { this.usevshostingprocess  = value; this.IsDirty = true; }
        //}


        #region Overriden Implementation
        /// <summary>
        /// Returns class FullName property value.
        /// </summary>
        public override string GetClassName()
        {
            return this.GetType().FullName;
        }

        /// <summary>
        /// Bind properties.
        /// </summary>
        protected override void BindProperties()
        {
            if (this.ProjectMgr == null)
            {
                return;
            }
            string type = "";
            type= getCfgString(nameof(DebugType), DebugType.none.ToString());
            if (type != null && type.Length > 0)
            {
                if (! Enum.TryParse(type, true, out debugtype))
                {
                    this.debugtype = DebugType.none;
                }
            }
            debuggercommand = getCfgString(nameof(DebuggerCommand), "$(TargetPath)");
            debuggercommandarguments = getCfgString(nameof(DebuggerCommandArguments),  "");
            debuggerworkingdirectory= getCfgString(nameof(DebuggerWorkingDirectory),  "");
            //debuggerattach= getCfgLogic(nameof(DebuggerAttach),  false);
            enableunmanageddebugging = getCfgLogic(nameof(EnableUnmanagedDebugging),  true);
            //usevshostingprocess = getCfgLogic(nameof(UseVSHostingProcess),  true);

        }

        /// <summary>
        /// Apply Changes on project node.
        /// </summary>
        /// <returns>E_INVALIDARG if internal ProjectMgr is null, otherwise applies changes and return S_OK.</returns>
        protected override int ApplyChanges()
        {
            if (this.ProjectMgr == null)
            {
                return VSConstants.E_INVALIDARG;
            }
            this.SetConfigProperty(nameof(DebugType), this.debugtype.ToString().ToLower());
            this.SetConfigProperty(nameof(DebuggerCommand), this.debuggercommand?.ToString());
            this.SetConfigProperty(nameof(DebuggerCommandArguments), this.debuggercommandarguments?.ToString());
            this.SetConfigProperty(nameof(DebuggerWorkingDirectory), this.debuggerworkingdirectory?.ToString());
            //this.SetConfigProperty(nameof(DebuggerAttach), this.debuggerattach.ToString().ToLower());
            this.SetConfigProperty(nameof(EnableUnmanagedDebugging), this.enableunmanageddebugging.ToString().ToLower());
            //this.SetConfigProperty(nameof(UseVSHostingProcess), this.usevshostingprocess.ToString().ToLower());
            if (debugtype == DebugType.none) {
                this.SetConfigProperty("EmitDebugInformation", "false");
            } else {
                this.SetConfigProperty("EmitDebugInformation", "true");
            }
            this.IsDirty = false;

            return VSConstants.S_OK;
        }

        #endregion
    }
}


