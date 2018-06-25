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
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.Build.Execution;
using System.Linq;
namespace Microsoft.VisualStudio.Project
{
    [CLSCompliant(false)]
    [ComVisible(true)]
    public class AssemblyReferenceNode : ReferenceNode
    {
        #region fieds
        /// <summary>
        /// The name of the assembly this refernce represents
        /// </summary>
        private System.Reflection.AssemblyName assemblyName;
        private AssemblyName resolvedAssemblyName;
        private string assemblyPath = string.Empty;
        private HashSet<string> resolvedProperties; // the names of the properties that MsBuild has resolved
        private ProjectItemInstance prjitem; // the project item that contains the properties from resolvedProperties
        /// <summary>
        /// Defines the listener that would listen on file changes on the nested project node.
        /// </summary>
        private FileChangeManager fileChangeListener;
        
        /// <summary>
        /// A flag for specifying if the object was disposed.
        /// </summary>
        private bool isDisposed;
        #endregion

        #region properties
        /// <summary>
        /// The name of the assembly this reference represents.
        /// </summary>
        /// <value></value>
        internal System.Reflection.AssemblyName AssemblyName
        {
            get
            {
                return this.assemblyName;
            }
        }

        /// <summary>
        /// Returns the name of the assembly this reference refers to on this specific
        /// machine. It can be different from the AssemblyName property because it can
        /// be more specific.
        /// </summary>
        internal System.Reflection.AssemblyName ResolvedAssembly
        {
            get { return resolvedAssemblyName; }
        }

        public override string Url
        {
            get
            {
                return this.assemblyPath;
            }
        }

        public override string Caption
        {
            get
            {
                return this.assemblyName.Name;
            }
        }

        private Automation.OAAssemblyReference assemblyRef;
        internal override object Object
        {
            get
            {
                if(null == assemblyRef)
                {
                    assemblyRef = new Automation.OAAssemblyReference(this);
                }
                return assemblyRef;
            }
        }
        #endregion

        #region ctors
        /// <summary>
        /// Constructor for the ReferenceNode
        /// </summary>
        public AssemblyReferenceNode(ProjectNode root, ProjectElement element)
            : base(root, element)
        {
            this.GetPathNameFromProjectFile();

            this.InitializeFileChangeEvents();

            string include = this.ItemNode.GetMetadata(ProjectFileConstants.Include);

            this.CreateFromAssemblyName(new System.Reflection.AssemblyName(include));
        }

        /// <summary>
        /// Constructor for the AssemblyReferenceNode
        /// </summary>
        public AssemblyReferenceNode(ProjectNode root, string assemblyPath)
            : base(root)
        {
            // Validate the input parameters.
            if(null == root)
            {
                throw new ArgumentNullException("root");
            }
			if (string.IsNullOrEmpty(assemblyPath))
            {
                throw new ArgumentNullException("assemblyPath");
            }

            this.InitializeFileChangeEvents();

            // The assemblyPath variable can be an actual path on disk or a generic assembly name.
            if(File.Exists(assemblyPath))
            {
                // The assemblyPath parameter is an actual file on disk; try to load it.
                this.assemblyName = System.Reflection.AssemblyName.GetAssemblyName(assemblyPath);
                this.assemblyPath = assemblyPath;

                // We register with listening to changes on the path here. 
                // The rest of the cases will call into resolving the assembly and registration is done there.
                this.fileChangeListener.ObserveItem(this.assemblyPath);
            }
            else
            {
                // The file does not exist on disk. This can be because the file / path is not
                // correct or because this is not a path, but an assembly name.
                // Try to resolve the reference as an assembly name.
                this.CreateFromAssemblyName(new System.Reflection.AssemblyName(assemblyPath));
            }
        }
        #endregion

        #region methods
        /// <summary>
        /// Closes the node.
        /// </summary>
        /// <returns></returns>
        public override int Close()
        {
            try
            {
                this.Dispose(true);
            }
            finally
            {
                base.Close();
            }

            return VSConstants.S_OK;
        }

        /// <summary>
        /// Links a reference node to the project and hierarchy.
        /// </summary>
        protected override void BindReferenceData()
        {
            Debug.Assert(this.assemblyName != null, "The AssemblyName field has not been initialized");

            // If the item has not been set correctly like in case of a new reference added it now.
            // The constructor for the AssemblyReference node will create a default project item. In that case the Item is null.
            // We need to specify here the correct project element.
            if(this.ItemNode == null || this.ItemNode.Item == null)
            {
                this.ItemNode = new ProjectElement(this.ProjectMgr, this.assemblyName.FullName, ProjectFileConstants.Reference);
            }

            // Set the basic information we know about
            this.ItemNode.SetMetadata(ProjectFileConstants.Name, this.assemblyName.Name);
            if (!string.IsNullOrEmpty(this.assemblyPath))
            {
                this.ItemNode.SetMetadata(ProjectFileConstants.AssemblyName, Path.GetFileName(this.assemblyPath));
            }
            else
            {
                this.ItemNode.SetMetadata(ProjectFileConstants.AssemblyName, null);
            }
			this.ItemNode.SetMetadata(ProjectFileConstants.SpecificVersion, "False");

            this.SetReferenceProperties();
        }

        /// <summary>
        /// Disposes the node
        /// </summary>
        /// <param name="disposing"></param>
        protected override void Dispose(bool disposing)
        {
            if(this.isDisposed)
            {
                return;
            }

            try
            {
                this.UnregisterFromFileChangeService();
            }
            finally
            {
                base.Dispose(disposing);
                this.isDisposed = true;
            }
        }

        private void CreateFromAssemblyName(AssemblyName name)
        {
            this.assemblyName = name;

            // Use MsBuild to resolve the assemblyname
            this.ResolveAssemblyReference();

            if(String.IsNullOrEmpty(this.assemblyPath) && (null != this.ItemNode.Item))
            {
                // Try to get the assembly name from the hintpath.
                this.GetPathNameFromProjectFile();
                if(this.assemblyPath == null)
                {
                    // Try to get the assembly name from the path
                    this.assemblyName = System.Reflection.AssemblyName.GetAssemblyName(this.assemblyPath);
                }
            }
            if(null == resolvedAssemblyName)
            {
                resolvedAssemblyName = assemblyName;
            }
        }

        /// <summary>
        /// Checks if an assembly is already added. The method parses all references and compares the full 
        /// assembly names, or the location of the assemblies to decide whether two assemblies are the same.
        /// </summary>
        /// <returns>true if the assembly has already been added.</returns>
        protected internal override bool IsAlreadyAdded(out ReferenceNode existingReference)
        {
            ReferenceContainerNode referencesFolder = this.ProjectMgr.FindChild(ReferenceContainerNode.ReferencesNodeVirtualName) as ReferenceContainerNode;
            Debug.Assert(referencesFolder != null, "Could not find the References node");
			bool shouldCheckPath = !string.IsNullOrEmpty(this.Url);

            for(HierarchyNode n = referencesFolder.FirstChild; n != null; n = n.NextSibling)
            {
                AssemblyReferenceNode assemblyReferenceNode = n as AssemblyReferenceNode;
                if(null != assemblyReferenceNode)
                {
                    // We will check if the full assembly names are the same or if the Url of the assemblies is the same.
                    if(String.Compare(assemblyReferenceNode.AssemblyName.FullName, this.assemblyName.FullName, StringComparison.OrdinalIgnoreCase) == 0 ||
                        (shouldCheckPath && NativeMethods.IsSamePath(assemblyReferenceNode.Url, this.Url)))
                    {
                        existingReference = assemblyReferenceNode;
                        return true;
                    }
                }
            }

            existingReference = null;
            return false;
        }

        /// <summary>
        /// Determines if this is node a valid node for painting the default reference icon.
        /// </summary>
        /// <returns></returns>
        protected override bool CanShowDefaultIcon()
        {
            if(String.IsNullOrEmpty(this.assemblyPath) || !File.Exists(this.assemblyPath))
            {
                return false;
            }

            return true;
        }

        private void GetPathNameFromProjectFile()
        {
            string result = this.ItemNode.GetMetadata(ProjectFileConstants.HintPath);
            if(String.IsNullOrEmpty(result))
            {
                result = this.ItemNode.GetMetadata(ProjectFileConstants.AssemblyName);
                if(String.IsNullOrEmpty(result))
                {
                    this.assemblyPath = String.Empty;
                }
                else if(!result.EndsWith(".dll", StringComparison.OrdinalIgnoreCase))
                {
                    result += ".dll";
                    this.assemblyPath = result;
                }
                else
                {
                    this.assemblyPath = result;
                }

            }
            else
            {
                this.assemblyPath = this.GetFullPathFromPath(result);
            }
        }

        private string GetFullPathFromPath(string path)
        {
            if(Path.IsPathRooted(path))
            {
                if (File.Exists(path))
                    return path;
                else
                {
                    this.ItemNode.SetMetadata(ProjectFileConstants.HintPath, null);
                    return string.Empty;
                }
            }
            else
            {
                Uri uri = new Uri(this.ProjectMgr.BaseURI.Uri, path);
                
                if(uri != null )
                {
                    string sPath = Microsoft.VisualStudio.Shell.Url.Unescape(uri.LocalPath, true);
                    if (File.Exists(sPath))
                        return sPath;
                    else
                    {
                        this.ItemNode.SetMetadata(ProjectFileConstants.HintPath, null);
                        return String.Empty;
                        }
                }
            }

            return String.Empty;
        }

        protected override void ResolveReference()
        {
            this.ResolveAssemblyReference();
        }


        internal string GetMsBuildProperty(string propName)
        {
            if (resolvedProperties != null && prjitem != null)
            {
                if (resolvedProperties.Contains(propName))
                    return prjitem.GetMetadataValue(propName);
            }
            return "";

        }

		private void SetHintPathAndPrivateValue(ProjectInstance instance)
		{

			// Private means local copy; we want to know if it is already set to not override the default
			string privateValue = this.ItemNode.GetMetadata(ProjectFileConstants.Private);

			// Get the list of items which require HintPath
			IEnumerable<ProjectItemInstance> references = MSBuildProjectInstance.GetItems(instance, MsBuildGeneratedItemType.ReferenceCopyLocalPaths);

			// Remove the HintPath, we will re-add it below if it is needed
			if (!String.IsNullOrEmpty(this.assemblyPath))
			{
				this.ItemNode.SetMetadata(ProjectFileConstants.HintPath, null);
			}

			// Now loop through the generated References to find the corresponding one
			foreach (ProjectItemInstance reference in references)
			{
				string fileName = Path.GetFileNameWithoutExtension(MSBuildItem.GetEvaluatedInclude(reference));
				if (String.Compare(fileName, this.assemblyName.Name, StringComparison.OrdinalIgnoreCase) == 0)
				{
					// We found it, now set some properties based on this.

					string hintPath = MSBuildItem.GetMetadataValue(reference, ProjectFileConstants.HintPath);
					if (!String.IsNullOrEmpty(hintPath))
					{
						if (Path.IsPathRooted(hintPath))
						{
							hintPath = PackageUtilities.GetPathDistance(this.ProjectMgr.BaseURI.Uri, new Uri(hintPath));
						}

						this.ItemNode.SetMetadata(ProjectFileConstants.HintPath, hintPath);
						// If this is not already set, we default to true
						if (String.IsNullOrEmpty(privateValue))
						{
							this.ItemNode.SetMetadata(ProjectFileConstants.Private, true.ToString());
						}
					}
					break;
				}

			}

		}

		// no loggers
		internal static bool BuildInstance(ProjectNode projectNode, ProjectInstance instance, string target)
		{
			BuildSubmission submission = projectNode.DoMSBuildSubmission(BuildKind.Sync, target, ref instance, null);
            if (submission == null)
                return false;
			return (submission.BuildResult.OverallResult == BuildResultCode.Success);
		}

        /// <summary>
        /// This function ensures that some properties of the reference are set.
        /// </summary>
		private void SetReferenceProperties()
		{
			// Set a default HintPath for msbuild to be able to resolve the reference.
			this.ItemNode.SetMetadata(ProjectFileConstants.HintPath, this.assemblyPath);

			// Resolve assembly references. This is needed to make sure that properties like the full path
			// to the assembly or the hint path are set.
			var instance = this.ProjectMgr.ProjectInstance;
			bool buildResultIsOk = BuildInstance(this.ProjectMgr, instance, MsBuildTarget.ResolveAssemblyReferences);
			if (!buildResultIsOk)
			{
				return;
			}

			// Check if we have to resolve again the path to the assembly.
			if (String.IsNullOrEmpty(this.assemblyPath))
			{
				ResolveReference();
			}

			// Make sure that the hint path if set (if needed).
			SetHintPathAndPrivateValue(instance);
		}

        /// <summary>
        /// Does the actual job of resolving an assembly reference. We need a private method that does not violate
        /// calling virtual method from the constructor.
        /// </summary>
        internal void ResolveAssemblyReference()
		{
			if (this.ProjectMgr == null || this.ProjectMgr.IsClosed)
			{
				return;
			}
            
			var instance = this.ProjectMgr.ProjectInstance;
            // must call MsBuild again
            var group = MSBuildProjectInstance.GetItems(instance, ProjectFileConstants.ReferencePath).ToArray();
            // RvdH Only call ResolveAsemblyReferences when we cannot find any items
            if (group == null || group.Length == 0)
            {
                BuildInstance(this.ProjectMgr, instance, MsBuildTarget.ResolveAssemblyReferences);
                group = MSBuildProjectInstance.GetItems(instance, ProjectFileConstants.ReferencePath).ToArray();

            }
            if (group != null)
			{
				foreach (var item in group)
				{
					string fullPath = item.GetMetadataValue("fullpath");
					AssemblyName name = AssemblyName.GetAssemblyName(fullPath);
                    
					// Try with full assembly name and then with weak assembly name.
					if (String.Compare(name.FullName, this.assemblyName.FullName, StringComparison.OrdinalIgnoreCase) == 0 || 
                        String.Compare(name.Name, this.assemblyName.Name, StringComparison.OrdinalIgnoreCase) == 0)
					{
						if (!NativeMethods.IsSamePath(fullPath, this.assemblyPath))
						{
							// set the full path now.
							this.assemblyPath = fullPath;

							// We have a new item to listen too, since the assembly reference is resolved from a different place.
							this.fileChangeListener.ObserveItem(this.assemblyPath);
						}
						this.resolvedAssemblyName = name;
                        // No hint path is needed since the assembly path will always be resolved.
                        
                        // cache the propertynames and the item.
                        resolvedProperties = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                        foreach (var str in item.MetadataNames)
                        {
                            resolvedProperties.Add(str);
                        }
                        prjitem = item;
                        return;
					}
				}
                // when we get here then the assembly was not resolved by MsBuild. Maybe the reference was not persisted yet ?
                return;
            }

        }

        /// <summary>
        /// Registers with File change events
        /// </summary>
        private void InitializeFileChangeEvents()
        {
            this.fileChangeListener = new FileChangeManager(this.ProjectMgr.Site);
            this.fileChangeListener.FileChangedOnDisk += this.OnAssemblyReferenceChangedOnDisk;
        }

        /// <summary>
        /// Unregisters this node from file change notifications.
        /// </summary>
        private void UnregisterFromFileChangeService()
        {
            this.fileChangeListener.FileChangedOnDisk -= this.OnAssemblyReferenceChangedOnDisk;
            this.fileChangeListener.Dispose();
        }

        /// <summary>
        /// Event callback. Called when one of the assembly file is changed.
        /// </summary>
        /// <param name="sender">The FileChangeManager object.</param>
        /// <param name="e">Event args containing the file name that was updated.</param>
        private void OnAssemblyReferenceChangedOnDisk(object sender, FileChangedOnDiskEventArgs e)
        {
            Debug.Assert(e != null, "No event args specified for the FileChangedOnDisk event");
            if (e == null) {
                return;
            }

            //// We only care about file deletes, so check for one before enumerating references.
            //if((e.FileChangeFlag & _VSFILECHANGEFLAGS.VSFILECHG_Del) == 0)
            //{
            //    return;
            //}


            if(NativeMethods.IsSamePath(e.FileName, this.assemblyPath))
            {
                this.OnInvalidateItems(this.Parent);
            }
        }

		protected override Guid GetBrowseLibraryGuid()
		{
			return VSConstants.guidCOMPLUSLibrary;
		}

        #endregion
    }
}
