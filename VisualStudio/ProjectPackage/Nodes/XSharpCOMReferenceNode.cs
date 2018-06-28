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

namespace XSharp.Project
{
    /// <summary>
    /// Knows about special requirements for project to project references
    /// </summary>
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
      static extern int LoadTypeLib(string fileName, out ITypeLib typeLib);





      #endregion
      #region Fields
      private string wrapperFileName;
      private string description;
      private Microsoft.VisualStudio.Shell.Interop.IVsTypeLibraryWrapper typelibwrapper = null;
      #endregion
      public XSharpComReferenceNode(ProjectNode root, ProjectElement element)
         : base(root, element)
      {
         BindReferenceData();
       }

        public override int ImageIndex
        {
            get
            {
                if (this.CanShowDefaultIcon())
                    return XSharpImageListIndex.Reference + XSharpProjectNode.imageOffset;
                else
                    return XSharpImageListIndex.DanglingReference + XSharpProjectNode.imageOffset;
            }
        }

        public XSharpComReferenceNode(ProjectNode root, VSCOMPONENTSELECTORDATA selectorData, string wrapperTool)
         : base(root, selectorData, wrapperTool)
      {
         if (String.IsNullOrEmpty(wrapperTool))
            wrapperTool = WrapperToolAttributeValue.TlbImp.ToString();

         this.description = selectorData.bstrTitle;
         this.EmbedInteropTypes = false;
         BindReferenceData();
      }


      protected override void BindReferenceData()
      {
         base.BindReferenceData();
         GetNamesFromWrapper();
      }
      public override string Description
      {
         get { return this.description; }
      }

      public override string Url
      {
         get
         {
            if (String.Compare(WrapperTool, WrapperToolAttributeValue.Primary.ToString(), true) == 0)
            {
               return wrapperFileName;
            }

            string result = this.ProjectMgr.GetProjectProperty("IntermediateOutputPath") + this.wrapperFileName;
            result = Path.Combine(this.ProjectMgr.ProjectFolder, result);
            return result;
         }
      }
      protected override NodeProperties CreatePropertiesObject()
      {
         return new XSharpComReferenceNodeProperties(this);

      }
      internal bool Matches(VSCOMPONENTSELECTORDATA selectorData, string wrapperTool )
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
        internal override object Object
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
            if (typelibwrapper == null)
            {
               Guid wrapperGuid = typeof(IVsTypeLibraryWrapper).GUID;
               Guid CLSID_VSTypeLibraryImporter = new Guid(0x1f411263, 0x3a1d, 0x43f5, 0x96, 0xaf, 0xf5, 0x64, 0x8c, 0xb8, 0x91, 0x86);
               Guid CLSID_VSAxImporter = new Guid(0x7D7D0D7B, 0xA0D5, 0x4BFE, 0xA2, 0xCF, 0x04, 0xB3, 0x72, 0xA4, 0x46, 0xBB);
               Guid CLSID_PrimaryImporter = new Guid(0x0c075ae9, 0x42ac, 0x4bef, 0x87, 0xa1, 0x85, 0xc1, 0xbf, 0xed, 0x9f, 0x1f);
               Guid Importer = new Guid();
               if (String.Equals(WrapperTool, WrapperToolAttributeValue.AxImp.ToString(), StringComparison.OrdinalIgnoreCase))
               {
                  Importer = CLSID_VSAxImporter;
               }
               else if (String.Equals(WrapperTool, WrapperToolAttributeValue.TlbImp.ToString(), StringComparison.OrdinalIgnoreCase))
               {
                  Importer = CLSID_VSTypeLibraryImporter;
               }
               else if (String.Equals(WrapperTool, WrapperToolAttributeValue.Primary.ToString(), StringComparison.OrdinalIgnoreCase))
               {
                  Importer = CLSID_PrimaryImporter;
               }
               if (Importer != Guid.Empty)
               {
                  IntPtr result = IntPtr.Zero;
                  ILocalRegistry localReg = this.GetService(typeof(SLocalRegistry)) as ILocalRegistry;
                  ErrorHandler.ThrowOnFailure(localReg.CreateInstance(Importer, null, ref wrapperGuid, (uint)Microsoft.VisualStudio.OLE.Interop.CLSCTX.CLSCTX_INPROC_SERVER, out result));
                  typelibwrapper = Marshal.GetObjectForIUnknown(result) as IVsTypeLibraryWrapper;
               }
            }
            return typelibwrapper;
         }
      }
      private void GetNamesFromWrapper()
      {
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
         }
         // get the typelib description
         string typeLibPath = QueryPathOfRegTypeLib(this.TypeGuid, (short)MajorVersionNumber, (short)MinorVersionNumber, Convert.ToInt32(this.LCID));
         typeLibPath = typeLibPath.Replace("\0", "");
         if (File.Exists(typeLibPath))
         { 
            ITypeLib itypelib;
            int iresult = LoadTypeLib(typeLibPath, out itypelib);
            if (iresult == VSConstants.S_OK && itypelib != null)
            {
               System.Runtime.InteropServices.ComTypes.ITypeLib  itl = itypelib as System.Runtime.InteropServices.ComTypes.ITypeLib ;
               if (itl != null)
               {
                  string strName, strDocString, strHelpFile;
                  int iHelpContext;
                  itl.GetDocumentation(-1, out strName, out strDocString, out iHelpContext, out strHelpFile);
                  description = strDocString;

               }
            }
         }

      }
   }
    [CLSCompliant(false), ComVisible(true)]
    internal class XSharpOAComReference : Microsoft.VisualStudio.Project.Automation.OAComReference
    {
        private Assembly assembly = null;
        private string assemblyPath = null;
        private bool tryLoad = false;

        internal XSharpOAComReference(ComReferenceNode comReference) : base(comReference)
        {

        }
        
        private void LoadAssembly()
        {
            if (assembly == null && !tryLoad)
            {
                try
                {

                    if (BaseReferenceNode.WrapperTool.ToLower() == "primary")
                    {
                        var key = Microsoft.Win32.Registry.ClassesRoot.OpenSubKey("Typelib\\" + BaseReferenceNode.TypeGuid.ToString("B") + "\\" + this.Version);
                        if (key != null)
                        {
                            string asmName = (string)key.GetValue("PrimaryInteropAssemblyName");
                            var name = new AssemblyName(asmName);
                            assembly = Assembly.Load(name);
                        }
                    }
                }
                catch (Exception)
                {
                    assembly = null;
                }
                if (assembly == null)
                {
                    try
                    {
                        string path = base.Path;
                        tryLoad = true;
                        assembly = XSharpModel.AssemblyInfo.LoadAssemblyFromFile(path);
                        assemblyPath = path;
                    }
                    catch (Exception)
                    {
                        assembly = null;
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
                LoadAssembly();
                if (assembly != null)
                {
                    return assembly.GetName().Name;
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
                    if (assembly == null)
                        LoadAssembly();
                    if (assembly != null)
                        return assemblyPath;
                }
                catch
                {

                }
                return base.Path;
            }
        }
    }

}
