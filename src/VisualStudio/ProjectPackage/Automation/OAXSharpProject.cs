//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Project.Automation;
using System;
using Microsoft.VisualStudio.Project;
using Community.VisualStudio.Toolkit;
using System.IO;

namespace XSharp.Project
{
    /// <summary>
    /// Represents automation object corresponding to a project.
    /// </summary>
    [CLSCompliant(false), ComVisible(true)]
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Interoperability", "CA1409:ComVisibleTypesShouldBeCreatable")]
    public class OAXSharpProject : OAProject
    {
        // =========================================================================================
        // Member variables
        // =========================================================================================

        /// <summary>
        /// Properties associated with the project.
        /// </summary>
        private OAProperties properties;
        #region Constructors
        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="OAXSharpProject"/> class.
        /// </summary>
        /// <param name="project">The node to which this project belongs.</param>
        public OAXSharpProject(XProjectNode xproject)
            : base(xproject)
        {
            if (xproject != null)
            {
                this.properties = new OAProperties(xproject.NodeProperties);
            }
        }
        #endregion
        /// <summary>
        /// Properties of the project
        /// </summary>
        /// <value>Collection of all project properties</value>
        public override EnvDTE.Properties Properties
        {
            get
            {
                return this.properties;
            }
        }
        // do not name this "NuGet" because then the stack will contain NuGet
        private bool MustReturnCSharp()
        {
            // HACK: Check to see if we are called from NuGet
            var stack = new System.Diagnostics.StackTrace();

            var max = stack.FrameCount;
            if (max > 5)
                max = 5;
            for (int iFrame = 0; iFrame < max; iFrame++)
            {
                var m = stack.GetFrame(iFrame).GetMethod();
                var t = m.DeclaringType;
                var a = t.Assembly;
                if (a.FullName.Contains("NuGet"))
                    return true;

            }
            return false;
        }
        const string CSharpProjectType = "{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}";
        /// <summary>
        /// Gets a GUID string indicating the kind or type of the object.
        /// </summary>

        public override string Kind
        {
            get
            {
                // When we are loading or building return the string fast
                if (XSharpProjectNode.InContextMenu &&
                    XSharpFileNode.CurrentItem != null &&
                    XSharpFileNode.CurrentItem.FileType == XSharpModel.XFileType.Config &&
                    XSharpFileNode.CurrentItem.FileName.ToLower().EndsWith("packages.config") &&
                    MustReturnCSharp() )
                {
                    // HACK: When NuGet is trying to convert packages.config to PackageReference, it
                    // looks at the project type and only supports C#, VB and F#. So we return C# here
                    // to make sure it work
                    return CSharpProjectType;
                }
                return Project.ProjectGuidString;
            }
        }

    }

#if DEV17
    /// <summary>
    /// Automation object for SDK-style XSharp projects.
    /// Overrides DTE to return a wrapper whose get_Properties("Environment","ProjectsandSolution")
    /// reports ShowAdvancedBuildConfigurations=false, allowing AppDesigner to enter
    /// simplified config mode and hide the Configuration/Platform toolbar.
    /// </summary>
    [CLSCompliant(false), ComVisible(true)]
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Interoperability", "CA1409:ComVisibleTypesShouldBeCreatable")]
    public class OAXSharpSdkProject : OAXSharpProject
    {
        public OAXSharpSdkProject(XProjectNode xproject) : base(xproject) { }

        public override EnvDTE.DTE DTE => new DteNoAdvancedConfigs(base.DTE);
    }

    /// <summary>
    /// Wraps a real EnvDTE.DTE and intercepts get_Properties("Environment","ProjectsandSolution")
    /// to patch ShowAdvancedBuildConfigurations to false.
    /// </summary>
    internal sealed class DteNoAdvancedConfigs : EnvDTE.DTE
    {
        private readonly EnvDTE.DTE _d;
        internal DteNoAdvancedConfigs(EnvDTE.DTE inner) { _d = inner; }

        public EnvDTE.Properties get_Properties(string Category, string Page)
        {
            var props = _d.get_Properties(Category, Page);
            if (string.Equals(Category, "Environment", StringComparison.OrdinalIgnoreCase) &&
                string.Equals(Page, "ProjectsandSolution", StringComparison.OrdinalIgnoreCase))
                return new PropsNoAdvancedConfigs(props);
            return props;
        }

        // All other members forward to _d
        public EnvDTE.Document ActiveDocument => _d.ActiveDocument;
        public object ActiveSolutionProjects => _d.ActiveSolutionProjects;
        public EnvDTE.Window ActiveWindow => _d.ActiveWindow;
#pragma warning disable CS0618 // AddIns obsolete
        public EnvDTE.AddIns AddIns => _d.AddIns;
#pragma warning restore CS0618
        public EnvDTE.DTE Application => _d.Application;
        public object CommandBars => _d.CommandBars;
        public string CommandLineArguments => _d.CommandLineArguments;
        public EnvDTE.Commands Commands => _d.Commands;
        public EnvDTE.ContextAttributes ContextAttributes => _d.ContextAttributes;
        public EnvDTE.Debugger Debugger => _d.Debugger;
        public EnvDTE.vsDisplay DisplayMode { get => _d.DisplayMode; set => _d.DisplayMode = value; }
        public EnvDTE.DTE DTE => _d.DTE;
        public EnvDTE.Documents Documents => _d.Documents;
        public string Edition => _d.Edition;
        public EnvDTE.Events Events => _d.Events;
        public string FileName => _d.FileName;
        public EnvDTE.Find Find => _d.Find;
        public string FullName => _d.FullName;
        public EnvDTE.Globals Globals => _d.Globals;
        public bool get_IsOpenFile(string ViewKind, string FileName) => _d.IsOpenFile[ViewKind, FileName];
        public EnvDTE.ItemOperations ItemOperations => _d.ItemOperations;
        public EnvDTE.wizardResult LaunchWizard(string VSZFile, ref object[] ContextParams) => _d.LaunchWizard(VSZFile, ref ContextParams);
        public int LocaleID => _d.LocaleID;
        public EnvDTE.Macros Macros => _d.Macros;
        public EnvDTE.DTE MacrosIDE => _d.MacrosIDE;
        public EnvDTE.Window MainWindow => _d.MainWindow;
        public EnvDTE.vsIDEMode Mode => _d.Mode;
        public string Name => _d.Name;
        public EnvDTE.ObjectExtenders ObjectExtenders => _d.ObjectExtenders;
        public EnvDTE.Window OpenFile(string ViewKind, string FileName) => _d.OpenFile(ViewKind, FileName);
        public string RegistryRoot => _d.RegistryRoot;
        public EnvDTE.SelectedItems SelectedItems => _d.SelectedItems;
        public EnvDTE.Solution Solution => _d.Solution;
        public EnvDTE.SourceControl SourceControl => _d.SourceControl;
        public EnvDTE.StatusBar StatusBar => _d.StatusBar;
        public bool SuppressUI { get => _d.SuppressUI; set => _d.SuppressUI = value; }
        public EnvDTE.UndoContext UndoContext => _d.UndoContext;
        public bool UserControl { get => _d.UserControl; set => _d.UserControl = value; }
        public string Version => _d.Version;
        public EnvDTE.WindowConfigurations WindowConfigurations => _d.WindowConfigurations;
        public EnvDTE.Windows Windows => _d.Windows;
        public string SatelliteDllPath(string Path, string Name) => _d.SatelliteDllPath(Path, Name);
        public object GetObject(string Name) => _d.GetObject(Name);
        public void ExecuteCommand(string CommandName, string CommandArgs) => _d.ExecuteCommand(CommandName, CommandArgs);
        public void Quit() => _d.Quit();
    }

    /// <summary>
    /// Wraps the "Environment/ProjectsandSolution" Properties collection and returns
    /// false for ShowAdvancedBuildConfigurations.
    /// </summary>
    internal sealed class PropsNoAdvancedConfigs : EnvDTE.Properties
    {
        private readonly EnvDTE.Properties _p;
        internal PropsNoAdvancedConfigs(EnvDTE.Properties inner) { _p = inner; }

        public EnvDTE.Property Item(object index)
        {
            var prop = _p.Item(index);
            if (index is string name &&
                string.Equals(name, "ShowAdvancedBuildConfigurations", StringComparison.OrdinalIgnoreCase))
                return new ConstantBoolProperty(prop, false);
            return prop;
        }

        public System.Collections.IEnumerator GetEnumerator() => _p.GetEnumerator();
        public int Count => _p.Count;
        public EnvDTE.DTE DTE => _p.DTE;
        public object Application => _p.Application;
        public object Parent => _p.Parent;
    }

    /// <summary>
    /// Wraps an EnvDTE.Property and overrides Value to return a constant.
    /// </summary>
    internal sealed class ConstantBoolProperty : EnvDTE.Property
    {
        private readonly EnvDTE.Property _p;
        private readonly object _value;
        internal ConstantBoolProperty(EnvDTE.Property inner, object value) { _p = inner; _value = value; }

        public object Value { get => _value; set { /* intentionally ignore */ } }
        public string Name => _p.Name;
        public object Application => _p.Application;
        public short NumIndices => _p.NumIndices;
        public object Object { get => _p.Object; set => _p.Object = value; }
        public EnvDTE.Properties Collection => _p.Collection;
        public EnvDTE.Properties Parent => _p.Parent;
        public EnvDTE.DTE DTE => _p.DTE;
        public object get_IndexedValue(object Index1, object Index2, object Index3, object Index4) =>
            _p.get_IndexedValue(Index1, Index2, Index3, Index4);
        public void let_Value(object lppvReturn) => _p.let_Value(lppvReturn);
        public void set_IndexedValue(object Index1, object Index2, object Index3, object Index4, object Val) =>
            _p.set_IndexedValue(Index1, Index2, Index3, Index4, Val);
    }
#endif
 }
