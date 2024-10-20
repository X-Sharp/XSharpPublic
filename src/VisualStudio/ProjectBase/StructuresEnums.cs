/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation.
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A
 * copy of the license can be found in the License.txt file at the root of this distribution. 
 * 
 * You must not remove this notice, or any other, from this software.
 *
 * ***************************************************************************/

using System;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

namespace Microsoft.VisualStudio.Project
{
    #region structures
    [StructLayoutAttribute(LayoutKind.Sequential)]
    internal struct _DROPFILES
    {
        public Int32 pFiles;
        public Int32 X;
        public Int32 Y;
        public Int32 fNC;
        public Int32 fWide;
    }
    #endregion

    #region enums

    /// <summary>
    /// The type of build performed.
    /// </summary>
    public enum BuildKind
    {
        Sync,
        Async
    }

    /// <summary>
    /// Defines possible types of output that can produced by a language project
    /// </summary>
    [PropertyPageTypeConverterAttribute(typeof(OutputTypeConverter))]
    public enum OutputType
    {
        /// <summary>
        /// The output type is a class library.
        /// </summary>
        Library,

        /// <summary>
        /// The output type is a windows executable.
        /// </summary>
        WinExe,

        /// <summary>
        /// The output type is an executable.
        /// </summary>
        Exe,
        /// <summary>
        /// The output type is an application Container.
        /// </summary>
        AppContainerExe,
        /// <summary>
        /// The output type is an WinMD Assembly
        /// </summary>
        WinMDObj,

    }

    /// <summary>
    /// Debug values used by DebugModeConverter.
    /// </summary>
    [PropertyPageTypeConverterAttribute(typeof(DebugModeConverter))]
    public enum DebugMode
    {
        Project,
        Program,
        [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "URL")]
        URL
    }

    [PropertyPageTypeConverterAttribute(typeof(CopyToOutputDirectoryConverter))]
    public enum CopyToOutputDirectory
    {
        DoNotCopy,
        Always,
        PreserveNewest,
    }

    /// <summary>
    /// An enumeration that describes the type of action to be taken by the build.
    /// </summary>
    //[PropertyPageTypeConverterAttribute(typeof(BuildActionConverter))]
    //public enum BuildAction
    //{
    //    None,
    //    Compile,
    //    Content,
    //    EmbeddedResource
    //}

    /// <summary>
    /// Defines the currect state of a property page.
    /// </summary>
    [Flags]
    public enum PropPageStatus
    {

        Dirty = 0x1,

        Validate = 0x2,

        Clean = 0x4
    }

    [Flags]
    [SuppressMessage("Microsoft.Design", "CA1008:EnumsShouldHaveZeroValue")]
    public enum ModuleKindFlags
    {

        ConsoleApplication,

        WindowsApplication,

        DynamicallyLinkedLibrary,

        ManifestResourceFile,

        UnmanagedDynamicallyLinkedLibrary
    }

    /// <summary>
    /// Defines the status of the command being queried
    /// </summary>
    [Flags]
    [SuppressMessage("Microsoft.Naming", "CA1714:FlagsEnumsShouldHavePluralNames")]
    [SuppressMessage("Microsoft.Design", "CA1008:EnumsShouldHaveZeroValue")]
    public enum QueryStatusResult
    {
        /// <summary>
        /// The command is not supported.
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "NOTSUPPORTED")]
        NOTSUPPORTED = 0,

        /// <summary>
        /// The command is supported
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "SUPPORTED")]
        SUPPORTED = 1,

        /// <summary>
        /// The command is enabled
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "ENABLED")]
        ENABLED = 2,

        /// <summary>
        /// The command is toggled on
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "LATCHED")]
        LATCHED = 4,

        /// <summary>
        /// The command is toggled off (the opposite of LATCHED).
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "NINCHED")]
        NINCHED = 8,

        /// <summary>
        /// The command is invisible.
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "INVISIBLE")]
        INVISIBLE = 16
    }

    /// <summary>
    /// Defines the type of item to be added to the hierarchy.
    /// </summary>
    public enum HierarchyAddType
    {
        AddNewItem,
        AddExistingItem
    }

    /// <summary>
    /// Defines the component from which a command was issued.
    /// </summary>
    public enum CommandOrigin
    {
        [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "Ui")]
        UiHierarchy,
        OleCommandTarget
    }

    /// <summary>
    /// Defines the current status of the build process.
    /// </summary>
    public enum MSBuildResult
    {
        /// <summary>
        /// The build is currently suspended.
        /// </summary>
        Suspended,

        /// <summary>
        /// The build has been restarted.
        /// </summary>
        Resumed,

        /// <summary>
        /// The build failed.
        /// </summary>
        Failed,

        /// <summary>
        /// The build was successful.
        /// </summary>
        Successful,
    }

    /// <summary>
    /// Defines the type of action to be taken in showing the window frame.
    /// </summary>
    public enum WindowFrameShowAction
    {
        DoNotShow,
        Show,
        ShowNoActivate,
        Hide,
    }

    /// <summary>
    /// Defines drop types
    /// </summary>
    internal enum DropDataType
    {
        None,
        Shell,
        VsStg,
        VsRef
    }

    /// <summary>
    /// Used by the hierarchy node to decide which element to redraw.
    /// </summary>
    [Flags]
    [SuppressMessage("Microsoft.Naming", "CA1714:FlagsEnumsShouldHavePluralNames")]
    public enum UIHierarchyElement
    {
        None = 0,

        /// <summary>
        /// This will be translated to VSHPROPID_IconIndex
        /// </summary>
        Icon = 1,

        /// <summary>
        /// This will be translated to VSHPROPID_StateIconIndex
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Scc")]
        SccState = 2,

        /// <summary>
        /// This will be translated to VSHPROPID_Caption
        /// </summary>
        Caption = 4,

        /// <summary>
        /// This will be translated to VSHPROPID_OverlayIconIndex
        /// </summary>
        OverlayIcon = 8
    }

    /// <summary>
    /// Defines the global propeties used by the msbuild project.
    /// </summary>
    public enum GlobalProperty
    {
        /// <summary>
        /// Property specifying that we are building inside VS.
        /// </summary>
        BuildingInsideVisualStudio,

        /// <summary>
        /// The VS installation directory. This is the same as the $(DevEnvDir) macro.
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Env")]
        DevEnvDir,

        /// <summary>
        /// The name of the solution the project is created. This is the same as the $(SolutionName) macro.
        /// </summary>
        SolutionName,

        /// <summary>
        /// The file name of the solution. This is the same as $(SolutionFileName) macro.
        /// </summary>
        SolutionFileName,

        /// <summary>
        /// The full path of the solution. This is the same as the $(SolutionPath) macro.
        /// </summary>
        SolutionPath,

        /// <summary>
        /// The directory of the solution. This is the same as the $(SolutionDir) macro.
        /// </summary>
        SolutionDir,

        /// <summary>
        /// The extension of teh directory. This is the same as the $(SolutionExt) macro.
        /// </summary>
        SolutionExt,

        /// <summary>
        /// The fxcop installation directory.
        /// </summary>
        FxCopDir,

        /// <summary>
        /// The ResolvedNonMSBuildProjectOutputs msbuild property
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "VSIDE")]
        VSIDEResolvedNonMSBuildProjectOutputs,

        /// <summary>
        /// The Configuartion property.
        /// </summary>
        Configuration,

        /// <summary>
        /// The platform property.
        /// </summary>
        Platform,

        /// <summary>
        /// The RunCodeAnalysisOnce property
        /// </summary>
        RunCodeAnalysisOnce,

        /// <summary>
        /// The VisualStudioStyleErrors property
        /// </summary>
        VisualStudioStyleErrors,

        UseHostCompilerIfAvailable
    }
    #endregion

    public class AfterProjectFileOpenedEventArgs : EventArgs
    {
        #region fields
        private bool added;
        #endregion

        #region properties
        /// <summary>
        /// True if the project is added to the solution after the solution is opened. false if the project is added to the solution while the solution is being opened.
        /// </summary>
        [SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
        internal bool Added
        {
            get { return this.added; }
        }
        #endregion

        #region ctor
        internal AfterProjectFileOpenedEventArgs(bool added)
        {
            this.added = added;
        }
        #endregion
    }

    public class BeforeProjectFileClosedEventArgs : EventArgs
    {
        #region fields
        private bool _removed;
        private IVsHierarchy _hierarchy;
        #endregion

        #region properties
        /// <summary>
        /// true if the project was removed from the solution before the solution was closed. false if the project was removed from the solution while the solution was being closed.
        /// </summary>
        internal bool Removed
		{
            get { return _removed; }
        }

        internal IVsHierarchy Hierarchy
		{
            get {
                return _hierarchy;
            }
        }

        #endregion

        #region ctor
        internal BeforeProjectFileClosedEventArgs(IVsHierarchy hierarchy, bool removed)
		{
            this._removed = removed;
            _hierarchy = hierarchy;
        }
        #endregion
    }

    /// <summary>
    /// Argument of the event raised when a project property is changed.
    /// </summary>
	[SuppressMessage("Microsoft.Naming", "CA1710:IdentifiersShouldHaveCorrectSuffix")]
    public class ProjectPropertyChangedArgs : EventArgs {
        private string propertyName;
        private string oldValue;
        private string newValue;

        internal ProjectPropertyChangedArgs(string propertyName, string oldValue, string newValue) {
            this.propertyName = propertyName;
            this.oldValue = oldValue;
            this.newValue = newValue;
        }

        public string NewValue {
            get { return newValue; }
        }

        public string OldValue {
            get { return oldValue; }
        }

        public string PropertyName {
            get { return propertyName; }
        }
    }

    /// <summary>
    /// This class is used for the events raised by a HierarchyNode object.
    /// </summary>
    public class HierarchyNodeEventArgs : EventArgs
    {
        private HierarchyNode child;

        internal HierarchyNodeEventArgs(HierarchyNode child)
        {
            this.child = child;
        }

        public HierarchyNode Child
        {
            get { return this.child; }
        }
    }

    /// <summary>
    /// Event args class for triggering file change event arguments.
    /// </summary>
    internal class FileChangedOnDiskEventArgs : EventArgs
    {
        #region Private fields
        /// <summary>
        /// File name that was changed on disk.
        /// </summary>
        private string fileName;

        /// <summary>
        /// The item ide of the file that has changed.
        /// </summary>
        private uint itemID;

        /// <summary>
        /// The reason the file has changed on disk.
        /// </summary>
        private _VSFILECHANGEFLAGS fileChangeFlag;
        #endregion

        /// <summary>
        /// Constructs a new event args.
        /// </summary>
        /// <param name="fileName">File name that was changed on disk.</param>
        /// <param name="id">The item id of the file that was changed on disk.</param>
        internal FileChangedOnDiskEventArgs(string fileName, uint id, _VSFILECHANGEFLAGS flag)
        {
            this.fileName = fileName;
            this.itemID = id;
            this.fileChangeFlag = flag;
        }

        /// <summary>
        /// Gets the file name that was changed on disk.
        /// </summary>
        /// <value>The file that was changed on disk.</value>
        internal string FileName
        {
            get
            {
                return this.fileName;
            }
        }

        /// <summary>
        /// Gets item id of the file that has changed
        /// </summary>
        /// <value>The file that was changed on disk.</value>
        internal uint ItemID
        {
            get
            {
                return this.itemID;
            }
        }

        /// <summary>
        /// The reason while the file has chnaged on disk.
        /// </summary>
        /// <value>The reason while the file has chnaged on disk.</value>
        internal _VSFILECHANGEFLAGS FileChangeFlag
        {
            get
            {
                return this.fileChangeFlag;
            }
        }
    }

    /// <summary>
    /// Defines the event args for the active configuration chnage event.
    /// </summary>
    public class ActiveConfigurationChangedEventArgs : EventArgs
    {
        #region Private fields
        /// <summary>
        /// The hierarchy whose configuration has changed
        /// </summary>
        private IVsHierarchy hierarchy;
        #endregion

        /// <summary>
        /// Constructs a new event args.
        /// </summary>
        /// <param name="fileName">The hierarchy that has changed its configuration.</param>
        internal ActiveConfigurationChangedEventArgs(IVsHierarchy hierarchy)
        {
            this.hierarchy = hierarchy;
        }

        /// <summary>
        /// The hierarchy whose configuration has changed
        /// </summary>
        internal IVsHierarchy Hierarchy
        {
            get
            {
                return this.hierarchy;
            }
        }
    }

}
