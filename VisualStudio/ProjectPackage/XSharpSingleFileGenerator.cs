//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Text;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.Win32;
using Microsoft.VisualStudio.Shell;

namespace XSharp.Project
{
    class XSharpSingleFileGenerator : SingleFileGenerator, ISingleFileGenerator, ISingleFileGenerator2, IVsGeneratorProgress
    {

        #region fields
        private ProjectNode myProjectMgr;
        private static int anyGeneratorsRunning = 0;
        #endregion


    	#region ctors
		/// <summary>
		/// </summary>
		/// <param name="ProjectNode">The associated project</param>
		internal XSharpSingleFileGenerator(ProjectNode ProjectMgr) : base(ProjectMgr)
		{
			this.myProjectMgr = ProjectMgr;
		}
		#endregion

        #region IVsGeneratorProgress Members

        public override int GeneratorError(int warning, uint level, string err, uint line, uint col)
        {
            // TODO: this should create a task list item
            return VSConstants.S_OK; // BUG !!! MORE SHIT that doesn't work right!!!   return VSConstants.E_NOTIMPL;
        }

        public override int Progress(uint complete, uint total)
        {
            return VSConstants.S_OK; // BUG !!! MORE SHIT that doesn't work right!!!   return VSConstants.E_NOTIMPL;
        }

        #endregion

        #region properties

        static internal bool AnyGeneratorsRunning
        {
            get
            {
                return anyGeneratorsRunning > 0;
            }
        }

        protected ProjectNode ProjectMgr
        {
            get
            {
                return myProjectMgr;
            }
        }
        #endregion
        #region ISingleFileGenerator
        /// <summary>
        /// Runs the generator on the current project item.
        /// </summary>
        /// <param name="document"></param>
        /// <returns></returns>
        public override void RunGenerator(string document)
        {
            // so the first thing we should do is to see if there's any generator defined for this node.
            // in our project system we try to call the ISingleFileGenerator2.RunGeneratorEx
            base.RunGenerator(document);
        }
        #endregion

        #region ISingleFileGenerator2
        /// <summary>
        /// Runs the generator on the current project item.
        /// </summary>
        /// <param name="document"></param>
        /// <returns></returns>
        public virtual void RunGeneratorEx(string document, bool runEvenIfNotDirty)
        {
            // Go run the generator on that node, but only if the file is dirty
            // in the running document table.  Otherwise there is no need to rerun
            // the generator because if the original document is not dirty then
            // the generated output should be already up to date.

            // the original implementation is crappy.  This method is called zillions of times by the environment,
            // so the first thing we should do is to see if there's any generator defined for this node.  Why see if the file
            // is dirty if there's no generator even defined???

            uint itemid = VSConstants.VSITEMID_NIL;
            IVsHierarchy hier = (IVsHierarchy)this.ProjectMgr;
            if (document != null && hier != null && ErrorHandler.Succeeded(hier.ParseCanonicalName((string)document, out itemid)))
            {
                FileNode node = (FileNode)this.ProjectMgr.NodeFromItemId(itemid);

                SingleFileGeneratorNodeProperties nodeproperties = node.NodeProperties as SingleFileGeneratorNodeProperties;

                // not every node is necessarily going to have a properties object derived from SFGNodeProperties
                if (nodeproperties != null && !string.IsNullOrEmpty(nodeproperties.CustomTool))
                {
                   try
                   {
                      this.InvokeGeneratorEx(node, document, runEvenIfNotDirty);
                   }
                   catch (Exception e)
                   {
                        if (System.Diagnostics.Debugger.IsAttached)
                            Debug.WriteLine(e.Message);
                   }
                }
            }
        }
        #endregion

        /// <summary>
        /// This is called after the single file generator has been invoked to create or update the code file.
        /// </summary>
        /// <param name="fileNode">The node associated to the generator</param>
        /// <param name="data">data to update the file with</param>
        /// <param name="size">size of the data</param>
        /// <param name="fileName">Name of the file to update or create</param>
        /// <returns>full path of the file</returns>
        protected override string UpdateGeneratedCodeFile(FileNode fileNode, byte[] data, int size, string fileName)
        {
            var path = System.IO.Path.GetDirectoryName(fileNode.Url);
            var depfile = Path.Combine(path, fileName);
            var depedentNode = ((XSharpProjectNode)this.ProjectMgr).FindURL(depfile);
            return UpdateGeneratedCodeFile(fileNode, data, size, fileName, depedentNode);
        }
        protected virtual string UpdateGeneratedCodeFile(FileNode fileNode, byte[] data, int size, string fileName, HierarchyNode dependentNode)
        {
            string filePath = Path.Combine(Path.GetDirectoryName(fileNode.GetMkDocument()), fileName);
            IVsRunningDocumentTable rdt = this.ProjectMgr.GetService(typeof(SVsRunningDocumentTable)) as IVsRunningDocumentTable;

            // (kberes) Shouldn't this be an InvalidOperationException instead with some not to annoying errormessage to the user?
            if (rdt == null)
            {
                ErrorHandler.ThrowOnFailure(VSConstants.E_FAIL);
            }

            IVsHierarchy hier;
            uint cookie;
            uint itemid;
            IntPtr docData = IntPtr.Zero;
            ErrorHandler.ThrowOnFailure(rdt.FindAndLockDocument((uint)(_VSRDTFLAGS.RDT_NoLock), filePath, out hier, out itemid, out docData, out cookie));
            if (docData != IntPtr.Zero)
            {
                Marshal.Release(docData);
                IVsTextStream srpStream;
                string inputFileContents = this.GetBufferContents(filePath, out srpStream);
                if (srpStream != null)
                {
                    int oldLen = 0;
                    int hr = srpStream.GetSize(out oldLen);
                    if (ErrorHandler.Succeeded(hr))
                    {
                        IntPtr dest = IntPtr.Zero;
                        try
                        {

                            if (data.Length > 2 && data[0] == 0xEF && data[1] == 0xBB && data[2] == 0xBF)
                            {
                                // the data is in UTF-8 format and must be converted to Unicode, since the IVsTextStream
                                // buffer is always Unicode
                                //
                                // The ResXCodeGenerator generates UTF-8 code, other SingleFileGenerators might do the same thing, who knows?

                                string txt = new System.Text.UTF8Encoding().GetString(data, 3, data.Length - 3);  // skip over encoding preamble

                                UnicodeEncoding enc = new UnicodeEncoding();
                                int len = enc.GetByteCount(txt);
                                Byte[] unicodeData = new Byte[len];
                                enc.GetBytes(txt, 0, txt.Length, unicodeData, 0);

                                dest = Marshal.AllocCoTaskMem(len);
                                Marshal.Copy(unicodeData, 0, dest, unicodeData.Length);
                                ErrorHandler.ThrowOnFailure(srpStream.ReplaceStream(0, oldLen, dest, len / 2)); // Note: 4th param is # of chars, not bytes!
                            }
                            // end of changes
                            else
                            {
                                dest = Marshal.AllocCoTaskMem(data.Length);
                                Marshal.Copy(data, 0, dest, data.Length);
                                ErrorHandler.ThrowOnFailure(srpStream.ReplaceStream(0, oldLen, dest, size / 2));
                            }

                            // for now, always save the generated file.  Otherwise we have issues when the source file is dirty and the user
                            // builds the project.  The generator runs (because of the overridden SaveItem() in the project node) but the generated
                            // file ends up dirty and when the project is built, everything should be saved.
                            // We could probably force a save on that node, but it seems cleaner to always save the generated file.

                            int canceled;
                            ErrorHandler.ThrowOnFailure(this.ProjectMgr.SaveItem(VSSAVEFLAGS.VSSAVE_SilentSave, filePath, dependentNode.ID, docData, out canceled));
                        }
                        finally
                        {
                            if (dest != IntPtr.Zero)
                            {
                                Marshal.Release(dest);
                            }
                        }
                    }
                }
            }
            else
            {
                using (FileStream generatedFileStream = File.Open(filePath, FileMode.OpenOrCreate))
                {
                    generatedFileStream.Write(data, 0, size);
                    // adjust length, in case the file shrinks in size..
                    generatedFileStream.SetLength(size);

                }

                EnvDTE.ProjectItem projectItem = fileNode.GetAutomationObject() as EnvDTE.ProjectItem;
                if (projectItem != null && (this.ProjectMgr.FindChild(fileNode.FileName) == null))
                {
                    projectItem.ProjectItems.AddFromFile(filePath);
                }
            }
            return filePath;
        }
        protected internal virtual void InvokeGeneratorEx(FileNode fileNode, String document, bool runEvenIfNotDirty)
        {
            base.InvokeGenerator(fileNode);
            Utilities.ArgumentNotNull("fileNode", fileNode);

            SingleFileGeneratorNodeProperties nodeproperties = fileNode.NodeProperties as SingleFileGeneratorNodeProperties;
            if (nodeproperties == null)
            {
                throw new InvalidOperationException();
            }

            string customToolProgID = nodeproperties.CustomTool;
            if (string.IsNullOrEmpty(customToolProgID))
            {
                return;
            }

            // not sure what to do if BuildAction is set to "MSBuild:Compile".  IronPython doesn't handle this correctly either.
            // This build action is set for .xaml files but doing nothing doesn't seem to cause problems, so for now we ignore this
            // generator type and do nothing too.  If we don't do this, we get lots of message boxes when the clsid of this non-existent
            // single file generator isn't found.

            // We should try to find out what the proper action is here.  There is no "MSBuild:Compile" entry in the registry
            // for the C# or VB project system single file generators, so there must be some special handling for this build type
            // otherwise it would just be blank.  Naturally, the IronPython sample isn't any help here.

            if (customToolProgID.StartsWith("MSBuild:Compile", StringComparison.OrdinalIgnoreCase))
            {
                var project = fileNode.ProjectMgr as XSharpProjectNode;
                if (project.IsXamlFile(document))
                {
                    // need to generate the .g.prg file.
                    project.Build(project.CurrentConfig.ConfigCanonicalName, "BuildGenerateSources"); //  BuildGenerateSources
                }
                // The C# project system then calls a InvokeMsBuild method with the following contents
                /*
                HRESULT CVsProjBaseFileNode::InvokeMSBuildTarget(_In_z_ LPCWSTR wszMSBuildTarget)
                {
                    HRESULT hr = NOERROR;

                    if (wszMSBuildTarget != NULL)
                    {
                        CLangBuildMgr *pBuildMgr = GetProject()->GetBuildMgr();
                        CComPtr<MSBuildEngine::IProject> srpMSBuildProject = pBuildMgr->GetMSBuildProject();

                        if (CXMakeHelper::DoesTargetExist(srpMSBuildProject, wszMSBuildTarget) && pBuildMgr->HasPassedSecurityChecks())
                        {
                            // Initialize the build engine for this project and configuration.
                            CSmartMSBuildInitializer smsbi;
                            CLangProjectConfig *pProjConfig = pBuildMgr->GetConfigs()->GetActiveConfig();
                            hr = smsbi.Initialize(pBuildMgr, pProjConfig);

                            if (SUCCEEDED(hr))
                            {
                                // Build the specified target in the MSBuild project.
                                hr = pBuildMgr->BuildTarget(wszMSBuildTarget);
                            }

                            // Don't bother keeping the return value here--we want to
                            // return failure from Initialize or BuildTarget instead.
                            smsbi.Restore(pBuildMgr);
                        }
                    }

                    return hr;
                }
                 */
                return;
            }

            string customToolNamespace = nodeproperties.CustomToolNamespace;

            try
            {
                if (!this.runningGenerator)
                {
                    //Find if any dependent node exists
                    string dependentNodeName = null;
                    HierarchyNode dependentNode = fileNode.FirstChild;

                    while (dependentNode != null)
                    {
                        string dependentUpon = dependentNode.ItemNode.GetMetadata(ProjectFileConstants.DependentUpon);

                        if (!String.IsNullOrEmpty(dependentUpon))
                        {

                            if (!Path.IsPathRooted(dependentUpon))
                            {
                                dependentUpon = Path.Combine(Path.GetDirectoryName(fileNode.Url), dependentUpon);
                            }

                            if (string.Compare(dependentUpon, fileNode.Url, StringComparison.OrdinalIgnoreCase) == 0)
                            {
                                dependentNodeName = ((FileNode)dependentNode).FileName;
                                break;
                            }
                        }

                        dependentNode = dependentNode.NextSibling;
                    }

                    bool outputOutOfDate = dependentNodeName == null;

                    if ((!runEvenIfNotDirty) && !outputOutOfDate)
                    {
                        string generatedFile = Path.Combine(Path.GetDirectoryName(fileNode.GetMkDocument()), dependentNodeName);

                        outputOutOfDate = !File.Exists(generatedFile);

                        if (!outputOutOfDate)
                        {
                            outputOutOfDate = File.GetLastWriteTime(fileNode.GetMkDocument()) > File.GetLastWriteTime(generatedFile);

                            if (!outputOutOfDate)
                            {
                                IVsHierarchy rdtHier;
                                IVsPersistDocData perDocData;
                                uint cookie;
                                outputOutOfDate = this.VerifyFileDirtyInRdt(document, out rdtHier, out perDocData, out cookie);
                            }
                        }
                    }

                    //if (runEvenIfNotDirty || outputOutOfDate)
                    {
                        //Get the buffer contents for the current node
                        string moniker = fileNode.GetMkDocument();

                        this.runningGenerator = true;
                        anyGeneratorsRunning++;

                        //Get the generator
                        IVsSingleFileGenerator generator;
                        int generateDesignTimeSource;
                        int generateSharedDesignTimeSource;
                        int generateTempPE;
                        SingleFileGeneratorFactory factory = new SingleFileGeneratorFactory(this.ProjectMgr.ProjectGuid, this.ProjectMgr.Site);
                        ErrorHandler.ThrowOnFailure(factory.CreateGeneratorInstance(customToolProgID, out generateDesignTimeSource, out generateSharedDesignTimeSource, out generateTempPE, out generator));

                        //Check to see if the generator supports siting
                        IObjectWithSite objWithSite = generator as IObjectWithSite;
                        if (objWithSite != null)
                        {
                            objWithSite.SetSite(fileNode.OleServiceProvider);
                        }

                        //Determine the namespace
                        if (string.IsNullOrEmpty(customToolNamespace))
                        {
                            customToolNamespace = this.ComputeNamespace(moniker);
                        }

                        //Run the generator
                        IntPtr[] output = new IntPtr[1];
                        output[0] = IntPtr.Zero;
                        uint outPutSize;

                        if (dependentNodeName == null)
                        {
                            string extension = null;
                            try
                            {
                                generator.DefaultExtension(out extension);
                            }
                            catch (Exception e)
                            {
                                if (System.Diagnostics.Debugger.IsAttached)
                                    Debug.WriteLine(e);
                            }
                            if (!String.IsNullOrEmpty(extension))
                                dependentNodeName = Path.GetFileNameWithoutExtension(fileNode.FileName) + extension;
                            else
                                dependentNodeName = fileNode.FileName;
                        }


                        //If you found a dependent node.
                        if (dependentNode != null)
                        {
                            //Then check out the node and dependent node from SCC
                            if (!this.CanEditFile(dependentNode.GetMkDocument()))
                            {
                                throw Marshal.GetExceptionForHR(VSConstants.OLE_E_PROMPTSAVECANCELLED);
                            }
                        }
                        else //It is a new node to be added to the project
                        {
                            // Check out the project file if necessary.
                            if (!this.ProjectMgr.QueryEditProjectFile(false))
                            {
                                throw Marshal.GetExceptionForHR(VSConstants.OLE_E_PROMPTSAVECANCELLED);
                            }
                        }
                        IVsTextStream stream;
                        string inputFileContents = this.GetBufferContents(moniker, out stream);

                        var res = generator.Generate(moniker, inputFileContents, customToolNamespace, output, out outPutSize, this);
                        byte[] data = new byte[outPutSize];

                        if (output[0] != IntPtr.Zero)
                        {
                            Marshal.Copy(output[0], data, 0, (int)outPutSize);
                            Marshal.FreeCoTaskMem(output[0]);
                        }

                        //Todo - Create a file and add it to the Project
                        string fileToAdd = this.UpdateGeneratedCodeFile(fileNode, data, (int)outPutSize, dependentNodeName, dependentNode);
                    }
                }
            }
            finally
            {
                this.runningGenerator = false;
                anyGeneratorsRunning--;
            }
        }


        /// <summary>
        /// Computes the names space based on the folder for the ProjectItem. It just replaces DirectorySeparatorCharacter
        /// with "." for the directory in which the file is located.
        /// </summary>
        /// <returns>Returns the computed name space</returns>
        protected override string ComputeNamespace(string projectItemPath)
        {
            // Caution: the algorithm used to calculate the namespace must match the same algorithm
            // in the CreateManifestResourceName build task .

            // The algorithm for single file generators is to obtain the default namespace from the project.
            // Then, the relative path of the source file used by the generator is appended to the namespace.
            // The path of the source file is relative to the path of the project file.

            // So given a file named 'Properties\Resources.resx' in a project with a default namespace of 'foo',
            // the ResX single file generator would create a class named 'Resources' in the namespace 'foo.Properties'.
            // This is consistent with the way C# determines namespaces when it runs single file generators.

            // The CreateVulcanManifestResourceName build task will do the same thing, if the resource is not
            // dependent upon some other file.  For dependent resources, the class name and namespace is derived
            // from the first class in the dependee file, but that is not relevant here since such resources are not
            // generated by a single file generator, they're generated by the forms designer.

            if (String.IsNullOrEmpty(projectItemPath))
            {
                // This will throw an exception in the parent class, so we do not need to access the resource
                return base.ComputeNamespace(projectItemPath);
            }

            // The DefaultNamespace property has been changed to return nothing, to solve a problem in the generation
            // of the resource constructor.
            // Therefore we have to call the GetProjectProperty() directly now.
            //string nspace = ProjectMgr.GetProperty( (int) __VSHPROPID.VSHPROPID_DefaultNamespace ) as string;
            string nspace = this.ProjectMgr.GetProjectProperty(ProjectFileConstants.RootNamespace, true) as string;
            nspace += ".";

            string filePath = Path.GetDirectoryName(projectItemPath) + "\\";
            filePath = Microsoft.VisualStudio.Shell.PackageUtilities.GetPathDistance(this.ProjectMgr.BaseURI.Uri, new Uri(filePath));

            string[] toks = filePath.Split(new char[] { ':', '\\' }, StringSplitOptions.RemoveEmptyEntries);

            foreach (string tok in toks)
            {
                string temp = tok.Replace(" ", "");
                nspace += (temp + ".");
            }

            nspace = nspace.Remove(nspace.LastIndexOf(".", StringComparison.Ordinal), 1);

            return nspace;
        }

        #region Helper Methods
        /// <summary>
        /// Returns the buffer contents for a moniker.
        /// </summary>
        /// <returns>Buffer contents</returns>
        private string GetBufferContents(string fileName, out IVsTextStream srpStream)
        {
            Guid CLSID_VsTextBuffer = new Guid("{8E7B96A8-E33D-11d0-A6D5-00C04FB67F6A}");
            string bufferContents = "";
            srpStream = null;

            IVsRunningDocumentTable rdt = this.ProjectMgr.GetService(typeof(SVsRunningDocumentTable)) as IVsRunningDocumentTable;
            if (rdt != null)
            {
                IVsHierarchy hier;
                IVsPersistDocData persistDocData;
                uint itemid, cookie;
                bool docInRdt = true;
                IntPtr docData = IntPtr.Zero;
                int hr = NativeMethods.E_FAIL;
                try
                {
                    //Getting a read lock on the document. Must be released later.
                    hr = rdt.FindAndLockDocument((uint)_VSRDTFLAGS.RDT_ReadLock, fileName, out hier, out itemid, out docData, out cookie);
                    if (ErrorHandler.Failed(hr) || docData == IntPtr.Zero)
                    {
                        Guid iid = VSConstants.IID_IUnknown;
                        cookie = 0;
                        docInRdt = false;
                        ILocalRegistry localReg = this.ProjectMgr.GetService(typeof(SLocalRegistry)) as ILocalRegistry;
                        ErrorHandler.ThrowOnFailure(localReg.CreateInstance(CLSID_VsTextBuffer, null, ref iid, (uint)CLSCTX.CLSCTX_INPROC_SERVER, out docData));
                    }

                    persistDocData = Marshal.GetObjectForIUnknown(docData) as IVsPersistDocData;
                }
                finally
                {
                    if (docData != IntPtr.Zero)
                    {
                        Marshal.Release(docData);
                    }
                }

                //Try to get the Text lines
                IVsTextLines srpTextLines = persistDocData as IVsTextLines;
                if (srpTextLines == null)
                {
                    // Try getting a text buffer provider first
                    IVsTextBufferProvider srpTextBufferProvider = persistDocData as IVsTextBufferProvider;
                    if (srpTextBufferProvider != null)
                    {
                        hr = srpTextBufferProvider.GetTextBuffer(out srpTextLines);
                    }
                }

                if (ErrorHandler.Succeeded(hr))
                {
                    srpStream = srpTextLines as IVsTextStream;
                    if (srpStream != null)
                    {
                        // QI for IVsBatchUpdate and call FlushPendingUpdates if they support it
                        IVsBatchUpdate srpBatchUpdate = srpStream as IVsBatchUpdate;
                        if (srpBatchUpdate != null)
                        {
                            //ErrorHandler.ThrowOnFailure(srpBatchUpdate.FlushPendingUpdates(0));
                            // we don't want to throw here, this can return E_UNSPECIFIED_ERROR

                            srpBatchUpdate.FlushPendingUpdates(0);
                        }

                        int lBufferSize = 0;
                        hr = srpStream.GetSize(out lBufferSize);

                        if (ErrorHandler.Succeeded(hr))
                        {
                            IntPtr dest = IntPtr.Zero;
                            try
                            {
                                // Note that GetStream returns Unicode to us so we don't need to do any conversions
                                dest = Marshal.AllocCoTaskMem((lBufferSize + 1) * 2);
                                ErrorHandler.ThrowOnFailure(srpStream.GetStream(0, lBufferSize, dest));
                                //Get the contents
                                bufferContents = Marshal.PtrToStringUni(dest);
                            }
                            finally
                            {
                                if (dest != IntPtr.Zero)
                                    Marshal.FreeCoTaskMem(dest);
                            }
                        }
                    }

                }
                // Unlock the document in the RDT if necessary
                if (docInRdt && rdt != null)
                {
                    ErrorHandler.ThrowOnFailure(rdt.UnlockDocument((uint)(_VSRDTFLAGS.RDT_ReadLock | _VSRDTFLAGS.RDT_Unlock_NoSave), cookie));
                }

                if (ErrorHandler.Failed(hr))
                {
                    // If this failed then it's probably not a text file.  In that case,
                    // we just read the file as a binary
                    bufferContents = File.ReadAllText(fileName);
                }


            }
            return bufferContents;
        }
        #endregion
    }
}
