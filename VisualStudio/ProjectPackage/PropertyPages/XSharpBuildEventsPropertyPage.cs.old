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

namespace XSharp.Project
{
    /// <summary>
    /// This class implements general property page for the project type.
    /// </summary>
    /// 
    [ComVisible(true)]
    [Guid("49306259-9119-466E-8780-486CFBE2597D")]
    [ClassInterface(ClassInterfaceType.AutoDual)]
    public class XSharpBuildEventsPropertyPage : XSharpSettingsPage
    {
        internal const string catEvents = "Build Events";
        internal const string captPreBuildEvent = "\tPre Build Event";
        internal const string descPreBuildEvent = "Pre Build Event Command Line (may use macros)";
        internal const string captPostBuildEvent = "Post Build Event";
        internal const string descPostBuildEvent = "Post Build Event Command Line (may use macros)";
        internal const string captRunPostBuildEvent = "Run the Post Build Event";
        internal const string descRunPostBuildEvent = "When to run the Post Build Event";

        #region Fields
        private string prebuildevent;
        private string postbuildevent;
        private RunPostBuildEvent runpostbuildevent;

        #endregion Fields

        #region Constructors
        /// <summary>
        /// Explicitly defined default constructor.
        /// </summary>
        public XSharpBuildEventsPropertyPage()
        {
            this.Name = "Build Events";
        }
        #endregion

        [Category(catEvents)]
        [DisplayName(captPreBuildEvent)]
        [Description(descPreBuildEvent)]
        [Editor(typeof(XSharpMLEPropertyEditor), typeof(System.Drawing.Design.UITypeEditor))]
        public string PreBuildEvent
        {
            get { return this.prebuildevent; }
            set { this.prebuildevent = value; this.IsDirty = true; }
        }

        [Category(catEvents)]
        [DisplayName(captPostBuildEvent)]
        [Description(descPostBuildEvent)]
        [Editor(typeof(XSharpMLEPropertyEditor), typeof(System.Drawing.Design.UITypeEditor))]
        public string PostBuildEvent
        {
            get { return this.postbuildevent; }
            set { this.postbuildevent = value; this.IsDirty = true; }
        }

        [Category(catEvents)]
        [DisplayName(captRunPostBuildEvent)]
        [Description(descRunPostBuildEvent)]
        public RunPostBuildEvent RunPostBuildEvent
        {
            get { return this.runpostbuildevent; }
            set { this.runpostbuildevent = value; this.IsDirty = true; }
        }

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
            prebuildevent = getCfgString(nameof(PreBuildEvent),"");
            postbuildevent = getCfgString(nameof(PostBuildEvent), "");
            string temp= "";
            temp = getCfgString(nameof(RunPostBuildEvent),  "Always");
            runpostbuildevent = (RunPostBuildEvent) new RunPostBuildEventConverter().ConvertFromString(temp);
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
            this.SetConfigProperty(nameof(PreBuildEvent), this.prebuildevent?.ToString());
            this.SetConfigProperty(nameof(PostBuildEvent), this.postbuildevent?.ToString());
            this.SetConfigProperty(nameof(RunPostBuildEvent), this.runpostbuildevent.ToString());

            this.IsDirty = false;

            return VSConstants.S_OK;
        }
        #endregion

    }
}


