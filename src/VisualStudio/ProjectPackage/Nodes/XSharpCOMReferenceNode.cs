//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.IO;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Project;
using System.Reflection;
using System.Diagnostics;
using Microsoft.VisualStudio.Shell;

namespace XSharp.Project
{
    /// <summary>
    /// Knows about special requirements for project to project references
    /// </summary>
    [DebuggerDisplay("{Caption}")]
    public class XSharpComReferenceNode : ComReferenceNode
    {

        #region External Methods
        [DllImport("oleaut32.dll", CharSet = CharSet.Unicode, ExactSpelling = true, PreserveSig = false)]
        [return: MarshalAs(UnmanagedType.BStr)]
        static extern string QueryPathOfRegTypeLib([MarshalAs(UnmanagedType.LPStruct)]
            Guid guid,
            [MarshalAs(UnmanagedType.U2)] short wVerMajor,
            [MarshalAs(UnmanagedType.U2)] short wVerMinor,
            [MarshalAs(UnmanagedType.U4)] int lcid);

        [DllImport("oleaut32.dll", CharSet = CharSet.Unicode, ExactSpelling = true)]
        static extern int LoadTypeLib(string fileName, out System.Runtime.InteropServices.ComTypes.ITypeLib typeLib);

        #endregion
        #region Fields
        private Assembly assembly = null;
        private string wrapperFileName;
        private string description;
        private IVsTypeLibraryWrapper typelibwrapper = null;
        #endregion


        #region properties
        bool IsPrimary => string.Equals(WrapperTool, WrapperToolAttributeValue.Primary.ToString(), StringComparison.OrdinalIgnoreCase);
        bool IsActiveX => string.Equals(WrapperTool, WrapperToolAttributeValue.AxImp.ToString(), StringComparison.OrdinalIgnoreCase);
        bool IsTypeLib => string.Equals(WrapperTool, WrapperToolAttributeValue.TlbImp.ToString(), StringComparison.OrdinalIgnoreCase);
        #endregion

        internal Assembly Assembly => assembly;
        public XSharpComReferenceNode(ProjectNode root, ProjectElement element)
           : base(root, element)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            BindReferenceData();
        }
        
        protected override void Dispose(bool disposing)
        {
            if (this.ProjectMgr is XSharpProjectNode)
            {
                XSharpProjectNode projectNode = (XSharpProjectNode)this.ProjectMgr;
                if (projectNode != null)
                    projectNode.RemoveURL(this);
                ProjectMgr = null;

            }
            base.Dispose(disposing);
        }

        public XSharpComReferenceNode(ProjectNode root, VSCOMPONENTSELECTORDATA selectorData, string wrapperTool)
         : base(root, selectorData, wrapperTool)
        {
            Community.VisualStudio.Toolkit.VS.StatusBar.ShowMessageAsync("Binding COM reference").FireAndForget();
            if (String.IsNullOrEmpty(wrapperTool))
                wrapperTool = WrapperToolAttributeValue.TlbImp.ToString();
            ThreadHelper.ThrowIfNotOnUIThread();

            this.description = selectorData.bstrTitle;
            this.EmbedInteropTypes = false;
            BindReferenceData();
            Community.VisualStudio.Toolkit.VS.StatusBar.ClearAsync().FireAndForget();
        }


        protected override void BindReferenceData()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            base.BindReferenceData();
            GetNamesFromWrapper();
            XSharpProjectNode projectNode = (XSharpProjectNode)this.ProjectMgr;
            projectNode.ProjectModel.AddAssemblyReference(this.Url);
        }
        public override string Description
        {
            get { return this.description; }
        }

        public override void Remove(bool removeFromStorage)
        {
            XSharpProjectNode projectNode = (XSharpProjectNode)this.ProjectMgr;
            projectNode.ProjectModel.RemoveAssemblyReference(this.Url);
            base.Remove(!IsPrimary);  // when not Primary, then remove from storage
        } 

        protected override void DeleteFromStorage(string path)
        {
            if (File.Exists(path))
            {
                var projectPath = this.ProjectMgr.ProjectFolder;
                // only delete when inside the project path
                if ( string.Compare(path, 0, projectPath,0,projectPath.Length,true) == 0)
                {
                    File.SetAttributes(path, FileAttributes.Normal); // make sure it's not readonly.
                    OurNativeMethods.ShellDelete(path, OurNativeMethods.RecycleOption.SendToRecycleBin,
                       OurNativeMethods.UICancelOption.DoNothing, OurNativeMethods.FileOrDirectory.Directory);
                }
            }
        }

        public override string Url
        {
            get
            {
                if (IsPrimary)
                {
                    return wrapperFileName;
                }
                if (this.ProjectMgr == null)
                    return String.Empty;
                string result = this.ProjectMgr.GetProjectProperty("IntermediateOutputPath") + this.wrapperFileName;
                result = Path.Combine(this.ProjectMgr.ProjectFolder, result);
                return result;
            }
        }
        protected override NodeProperties CreatePropertiesObject()
        {
            return new XSharpComReferenceNodeProperties(this);

        }
        internal bool Matches(VSCOMPONENTSELECTORDATA selectorData, string wrapperTool)
        {
            ushort wMajorVerNum = (ushort)this.MajorVersionNumber;
            ushort wMinorVerNum = (ushort)this.MinorVersionNumber;
            UInt32 LCID = Convert.ToUInt32(this.LCID);
            return (selectorData.wTypeLibraryMajorVersion == wMajorVerNum
               && selectorData.wTypeLibraryMinorVersion == wMinorVerNum
               && selectorData.lcidTypeLibrary == LCID
               && selectorData.guidTypeLibrary == this.TypeGuid
               && string.Compare(wrapperTool, this.WrapperTool, true) == 0);
        }
        XSharpOAComReference comReference;
        public override object Object
        {
            get
            {
                if (null == comReference)
                {
                    comReference = new XSharpOAComReference(this);
                }
                return comReference;
            }
        }

        private IVsTypeLibraryWrapper TypeLibWrapper
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();

                if (typelibwrapper == null)
                {
                    Guid wrapperGuid = typeof(IVsTypeLibraryWrapper).GUID;
                    Guid CLSID_VSTypeLibraryImporter = new Guid(0x1f411263, 0x3a1d, 0x43f5, 0x96, 0xaf, 0xf5, 0x64, 0x8c, 0xb8, 0x91, 0x86);
                    Guid CLSID_VSAxImporter = new Guid(0x7D7D0D7B, 0xA0D5, 0x4BFE, 0xA2, 0xCF, 0x04, 0xB3, 0x72, 0xA4, 0x46, 0xBB);
                    Guid CLSID_PrimaryImporter = new Guid(0x0c075ae9, 0x42ac, 0x4bef, 0x87, 0xa1, 0x85, 0xc1, 0xbf, 0xed, 0x9f, 0x1f);
                    Guid Importer = new Guid();
                    if (IsActiveX)
                    {
                        Importer = CLSID_VSAxImporter;
                    }
                    else if (IsTypeLib)
                    {
                        Importer = CLSID_VSTypeLibraryImporter;
                    }
                    else if (IsPrimary)
                    {
                        Importer = CLSID_PrimaryImporter;
                    }
                    if (Importer != Guid.Empty)
                    {
                        ILocalRegistry localReg = this.GetService(typeof(SLocalRegistry)) as ILocalRegistry;
                        IntPtr result;
                        ErrorHandler.ThrowOnFailure(localReg.CreateInstance(Importer, null, ref wrapperGuid, (uint)Microsoft.VisualStudio.OLE.Interop.CLSCTX.CLSCTX_INPROC_SERVER, out result));
                        typelibwrapper = Marshal.GetObjectForIUnknown(result) as IVsTypeLibraryWrapper;
                    }
                }
                return typelibwrapper;
            }
        }
        private void GetNamesFromWrapper()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (this.TypeLibWrapper != null)
            {
                TLIBATTR[] attr = new TLIBATTR[1];
                attr[0].guid = this.TypeGuid;
                attr[0].wMajorVerNum = (ushort)this.MajorVersionNumber;
                attr[0].wMinorVerNum = (ushort)this.MinorVersionNumber;
                attr[0].LCID = Convert.ToUInt32(this.LCID);
                string name;
                if (TypeLibWrapper.GetMainWrapperFriendlyName(attr, out name) == VSConstants.S_OK)
                {
                    this.typeName = name;
                    this.ItemNode.Rename(name);
                }
                if (TypeLibWrapper.GetMainWrapperFilename(attr, out name) == VSConstants.S_OK)
                {
                    this.wrapperFileName = name;
                }
                else
                {
                    Version version = new Version(MajorVersionNumber, MinorVersionNumber);

                    var key = Microsoft.Win32.Registry.ClassesRoot.OpenSubKey("Typelib\\" + TypeGuid.ToString("B") + "\\" + version.ToString());
                    if (key != null)
                    {
                        name = (string)key.GetValue("PrimaryInteropAssemblyName");
                        var asmName = new AssemblyName(name);
                        this.assembly = Assembly.Load(asmName);
                        this.wrapperFileName = assembly.Location;
                    }

                }
            }
            // get the typelib description
            string typeLibPath = QueryPathOfRegTypeLib(this.TypeGuid, (short)MajorVersionNumber, (short)MinorVersionNumber, Convert.ToInt32(this.LCID));
            typeLibPath = typeLibPath.Replace("\0", "");
            if (File.Exists(typeLibPath))
            {
                int iresult = LoadTypeLib(typeLibPath, out var itypelib);
                if (iresult == VSConstants.S_OK && itypelib != null)
                {
                    itypelib.GetDocumentation(-1, out _, out var strDocString, out _, out _);
                    description = strDocString;
                }
            }

        }
    }
    [CLSCompliant(false), ComVisible(true)]
    internal class XSharpOAComReference : Microsoft.VisualStudio.Project.Automation.OAComReference
    {
        private XSharpComReferenceNode comref;
        private bool loaded = false;
        private string location;
        private bool tryLoad = false;
        private string asmName = null;

        internal XSharpOAComReference(XSharpComReferenceNode comReference) : base(comReference)
        {
            comref = comReference;
        }

        private void LoadAssembly()
        {
            if (!loaded && !tryLoad)
            {
                if (comref.Assembly != null)
                {
                    var assembly = comref.Assembly;
                    if (!String.IsNullOrEmpty(assembly.Location))
                    {
                        loaded = true;
                        location = assembly.Location;
                        asmName = assembly.GetName().Name;
                        return;
                    }
                }
                try
                {

                    if (BaseReferenceNode.WrapperTool?.ToLower() == "primary")
                    {
                        var key = Microsoft.Win32.Registry.ClassesRoot.OpenSubKey("Typelib\\" + BaseReferenceNode.TypeGuid.ToString("B") + "\\" + this.Version);
                        if (key != null)
                        {
                            string asm = (string)key.GetValue("PrimaryInteropAssemblyName");
                            var name = new AssemblyName(asm);
                            var assembly = Assembly.Load(name);
                            location = assembly.Location;
                            asmName = assembly.GetName().Name;
                            loaded = true;
                        }
                    }
                }
                catch (Exception)
                {
                    loaded = false;
                }
                if (! loaded)
                {
                    try
                    {
                        string path = base.Path;
                        if (File.Exists(path))
                        {
                            tryLoad = true;
                            var bytes = File.ReadAllBytes(path);
                            var assembly = Assembly.Load(bytes);
                            location = path;
                            asmName = assembly.GetName().Name;
                        }
                    }
                    catch (Exception)
                    {
                        loaded = false;
                    }
                }
            }

        }
        public override string Name
        {
            get
            {
                // this needs to return the name as defined in the assembly
                // Otherwise the form editor will not be able to load a saved activeX control
                // the safest thing to do is to load the assembly and retrieve its name
                if (!loaded)
                    LoadAssembly();
                if (loaded)
                {
                    return asmName;
                }
                return System.IO.Path.GetFileNameWithoutExtension(this.Path);
            }

        }
        public override string Path
        {
            get
            {
                try
                {
                    if (!loaded)
                        LoadAssembly();
                    if (loaded)
                        return location;
                }
                catch
                {

                }
                return base.Path;
            }
        }
    }

}
