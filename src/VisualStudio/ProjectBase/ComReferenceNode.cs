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
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.IO;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.Win32;
using System.Collections.Generic;
using Microsoft.Build.Evaluation;
using Microsoft.VisualStudio.Shell;

namespace Microsoft.VisualStudio.Project
{
    /// <summary>
    /// This type of node is used for references to COM components.
    /// </summary>
    [CLSCompliant(false)]
    [ComVisible(true)]
    public class ComReferenceNode : ReferenceNode
    {
        #region fields
        protected string typeName;
        private Guid typeGuid;
        private string projectRelativeFilePath;
        private string installedFilePath;
        private string minorVersionNumber;
        private string majorVersionNumber;
        private string lcid;
        private string tmpWrapper;

        #endregion

        #region properties
        public override string Caption
        {
            get { return this.typeName; }
        }

        public virtual string Description
        {
            get { return typeName;}
        }

        public override string Url
        {
            get
            {
                return this.projectRelativeFilePath;
            }
        }

        /// <summary>
        /// Returns the Guid of the COM object.
        /// </summary>
        public Guid TypeGuid
        {
            get { return this.typeGuid; }
        }

        /// <summary>
        /// Returns the path where the COM object is installed.
        /// </summary>
        public string InstalledFilePath
        {
            get { return this.installedFilePath; }
        }

        [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "LCID")]
        public string LCID
        {
            get { return lcid; }
        }

        public int MajorVersionNumber
        {
            get
            {
                if(string.IsNullOrEmpty(majorVersionNumber))
                {
                    return 0;
                }
                return int.Parse(majorVersionNumber, CultureInfo.CurrentCulture);
            }
        }



        public string WrapperTool
        {
            get { return this.ItemNode.GetMetadata(ProjectFileConstants.WrapperTool); }
            set {
                ThreadHelper.ThrowIfNotOnUIThread();
                this.ItemNode.SetMetadata(ProjectFileConstants.WrapperTool, value);
            }
        }

        public int MinorVersionNumber
        {
            get
            {
                if(string.IsNullOrEmpty(minorVersionNumber))
                {
                    return 0;
                }
                return int.Parse(minorVersionNumber, CultureInfo.CurrentCulture);
            }
        }
        private Automation.OAComReference comReference;
        public override object Object
        {
            get
            {
                if(null == comReference)
                {
                    comReference = new Automation.OAComReference(this);
                }
                return comReference;
            }
        }
        #endregion

        #region ctors
        /// <summary>
        /// Constructor for the ComReferenceNode.
        /// </summary>
        public ComReferenceNode(ProjectNode root, ProjectElement element)
            : base(root, element)
        {
            this.typeName = this.ItemNode.GetMetadata(ProjectFileConstants.Include);
            string typeGuidAsString = this.ItemNode.GetMetadata(ProjectFileConstants.Guid);
            if(typeGuidAsString != null)
            {
                this.typeGuid = new Guid(typeGuidAsString);
            }
            ThreadHelper.ThrowIfNotOnUIThread();

            this.majorVersionNumber = this.ItemNode.GetMetadata(ProjectFileConstants.VersionMajor);
            this.minorVersionNumber = this.ItemNode.GetMetadata(ProjectFileConstants.VersionMinor);
            this.lcid = this.ItemNode.GetMetadata(ProjectFileConstants.Lcid);
            this.SetProjectItemsThatRelyOnReferencesToBeResolved(false);
            this.SetInstalledFilePath();
        }
        protected override void Dispose(bool disposing)
        {
            try
            {
                if (this.comReference != null)
                {
                    this.comReference.Dispose();
                }
            }
            finally
            {
                this.comReference = null;
                base.Dispose(disposing);
            }
        }
        /// <summary>
        /// Overloaded constructor for creating a ComReferenceNode from selector data
        /// </summary>
        /// <param name="root">The Project node</param>
        /// <param name="selectorData">The component selctor data.</param>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2201:DoNotRaiseReservedExceptionTypes")]
        public ComReferenceNode(ProjectNode root, VSCOMPONENTSELECTORDATA selectorData, string wrapperTool = null)
            : base(root)
        {
            if(root == null)
            {
                throw new ArgumentNullException("root");
            }
            if(selectorData.type == VSCOMPONENTTYPE.VSCOMPONENTTYPE_Project
                || selectorData.type == VSCOMPONENTTYPE.VSCOMPONENTTYPE_ComPlus)
            {
                throw new ArgumentException("SelectorData cannot be of type VSCOMPONENTTYPE.VSCOMPONENTTYPE_Project or VSCOMPONENTTYPE.VSCOMPONENTTYPE_ComPlus", "selectorData");
            }

            // Initialize private state
            this.typeName = selectorData.bstrTitle;
            this.typeGuid = selectorData.guidTypeLibrary;
            int majorVersion = selectorData.wTypeLibraryMajorVersion != 0 ? selectorData.wTypeLibraryMajorVersion : selectorData.wFileMajorVersion;
            int minorVersion = selectorData.wTypeLibraryMinorVersion != 0 ? selectorData.wTypeLibraryMinorVersion : selectorData.wFileMinorVersion;
            this.majorVersionNumber = majorVersion.ToString(CultureInfo.InvariantCulture);
            this.minorVersionNumber = minorVersion.ToString(CultureInfo.InvariantCulture);

            this.lcid = selectorData.lcidTypeLibrary.ToString(CultureInfo.InvariantCulture);
            if (String.IsNullOrEmpty(wrapperTool))
               wrapperTool = WrapperToolAttributeValue.TlbImp.ToString();
           tmpWrapper = wrapperTool;

            // Check to see if the COM object actually exists.
            this.SetInstalledFilePath();
            // If the value cannot be set throw.
            if(String.IsNullOrEmpty(this.installedFilePath))
            {
                throw new InvalidOperationException(
                    $"Cannot locate the file that implements the type library for {selectorData.bstrTitle}");
            }
        }
        #endregion

        #region methods
        protected override NodeProperties CreatePropertiesObject()
        {
            return new ComReferenceProperties(this);
        }

        /// <summary>
        /// Links a reference node to the project and hierarchy.
        /// </summary>
        protected override void BindReferenceData()
        {
            Debug.Assert(this.ItemNode != null, "The AssemblyName field has not been initialized");

            // We need to create the project element at this point if it has not been created.
            // We cannot do that from the ctor if input comes from a component selector data, since had we been doing that we would have added a project element to the project file.
            // The problem with that approach is that we would need to remove the project element if the item cannot be added to the hierachy (E.g. It already exists).
            // It is just safer to update the project file now. This is the intent of this method.
            // Call MSBuild to build the target ResolveComReferences
            if(this.ItemNode == null || this.ItemNode.Item == null)
            {
                this.ItemNode = this.GetProjectElementBasedOnInputFromComponentSelectorData();
            }
            ThreadHelper.ThrowIfNotOnUIThread();

            this.SetProjectItemsThatRelyOnReferencesToBeResolved(true);
        }

        /// <summary>
        /// Checks if a reference is already added. The method parses all references and compares the the FinalItemSpec and the Guid.
        /// </summary>
        /// <returns>true if the assembly has already been added.</returns>
        protected internal override bool IsAlreadyAdded(out ReferenceNode existingReference)
        {
            ReferenceContainerNode referencesFolder = this.ProjectMgr.FindChild(ReferenceContainerNode.ReferencesNodeVirtualName) as ReferenceContainerNode;
            Debug.Assert(referencesFolder != null, "Could not find the References node");

            for(HierarchyNode n = referencesFolder.FirstChild; n != null; n = n.NextSibling)
            {
                ComReferenceNode referenceNode = n as ComReferenceNode;

                if(referenceNode != null)
                {
                    // We check if the name and guids are the same
                    if (referenceNode.TypeGuid == this.TypeGuid &&
                        referenceNode.WrapperTool == this.WrapperTool &&
                        referenceNode.MajorVersionNumber == this.MajorVersionNumber &&
                        referenceNode.MinorVersionNumber == this.MinorVersionNumber)
                    {
                        existingReference = referenceNode;
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
            return !String.IsNullOrEmpty(this.installedFilePath);
        }

        /// <summary>
        /// This is an helper method to convert the VSCOMPONENTSELECTORDATA recieved by the
        /// implementer of IVsComponentUser into a ProjectElement that can be used to create
        /// an instance of this class.
        /// This should not be called for project reference or reference to managed assemblies.
        /// </summary>
        /// <returns>ProjectElement corresponding to the COM component passed in</returns>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1308:NormalizeStringsToUppercase")]
        private ProjectElement GetProjectElementBasedOnInputFromComponentSelectorData()
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            ProjectElement element = new ProjectElement(this.ProjectMgr, this.typeName, ProjectFileConstants.COMReference);

            // Set the basic information regarding this COM component
            element.SetMetadata(ProjectFileConstants.Guid, this.typeGuid.ToString("B").ToUpper());
            element.SetMetadata(ProjectFileConstants.VersionMajor, this.majorVersionNumber);
            element.SetMetadata(ProjectFileConstants.VersionMinor, this.minorVersionNumber);
            element.SetMetadata(ProjectFileConstants.Lcid, this.lcid);
            element.SetMetadata(ProjectFileConstants.Isolated, false.ToString());

            // See if a PIA exist for this component
            TypeLibConverter typelib = new TypeLibConverter();
            string assemblyName;
            string assemblyCodeBase;
            if(typelib.GetPrimaryInteropAssembly(this.typeGuid, Int32.Parse(this.majorVersionNumber, CultureInfo.InvariantCulture), Int32.Parse(this.minorVersionNumber, CultureInfo.InvariantCulture), Int32.Parse(this.lcid, CultureInfo.InvariantCulture), out assemblyName, out assemblyCodeBase))
            {
                element.SetMetadata(ProjectFileConstants.WrapperTool, WrapperToolAttributeValue.Primary.ToString().ToLowerInvariant());
            }
            else
            {
                // MSBuild will have to generate an interop assembly
               element.SetMetadata(ProjectFileConstants.WrapperTool, tmpWrapper.ToLowerInvariant());
               //element.SetMetadata(ProjectFileConstants.Private, true.ToString());
            }
            return element;
        }

        private void SetProjectItemsThatRelyOnReferencesToBeResolved(bool renameItemNode)
        {
            // Call MSBuild to build the target ResolveComReferences
            bool success;
            ThreadHelper.ThrowIfNotOnUIThread();
            ErrorHandler.ThrowOnFailure(this.ProjectMgr.BuildTarget(MsBuildTarget.ResolveComReferences, out success));
            //if(!success)
                //throw new InvalidOperationException();

            // Now loop through the generated COM References to find the corresponding one
            //IEnumerable<ProjectItem> comReferences = this.ProjectMgr.BuildProject.GetItems(MsBuildGeneratedItemType.ComReferenceWrappers);
            IEnumerable<ProjectItem> comReferences = this.ProjectMgr.BuildProject.GetItems(ProjectFileConstants.COMReference);
            foreach (ProjectItem reference in comReferences)
            {
                if(string.Compare(reference.GetMetadataValue(ProjectFileConstants.Guid), this.typeGuid.ToString("B"), StringComparison.OrdinalIgnoreCase) == 0
                    && string.Compare(reference.GetMetadataValue(ProjectFileConstants.VersionMajor), this.majorVersionNumber, StringComparison.OrdinalIgnoreCase) == 0
                    && string.Compare(reference.GetMetadataValue(ProjectFileConstants.VersionMinor), this.minorVersionNumber, StringComparison.OrdinalIgnoreCase) == 0
                    && string.Compare(reference.GetMetadataValue(ProjectFileConstants.Lcid), this.lcid, StringComparison.OrdinalIgnoreCase) == 0
                    && string.Compare(reference.GetMetadataValue(ProjectFileConstants.WrapperTool), this.WrapperTool, StringComparison.OrdinalIgnoreCase) == 0)
                {

                    string name = reference.EvaluatedInclude;
                    if(Path.IsPathRooted(name))
                    {
                        this.projectRelativeFilePath = name;
                    }
                    else
                    {
                        this.projectRelativeFilePath = Path.Combine(this.ProjectMgr.ProjectFolder, name);
                    }

                    if(renameItemNode)
                    {
                        this.ItemNode.Rename(Path.GetFileNameWithoutExtension(name));
                    }
                    break;
                }
            }
        }

        /// <summary>
        /// Verify that the TypeLib is registered and set the the installed file path of the com reference.
        /// </summary>
        /// <returns></returns>
        private void SetInstalledFilePath()
        {
            string registryPath = string.Format(CultureInfo.InvariantCulture, @"TYPELIB\{0:B}\{1:x}.{2:x}", this.typeGuid, this.MajorVersionNumber, this.MinorVersionNumber);
            using(RegistryKey typeLib = Registry.ClassesRoot.OpenSubKey(registryPath))
            {
                if(typeLib != null)
                {
                    // Check if we need to set the name for this type.
					if (String.IsNullOrEmpty(this.typeName))
                    {
                        this.typeName = typeLib.GetValue(string.Empty) as string;
                    }
                    this.installedFilePath = null;
                    // Now get the path to the file that contains this type library.
                    using (RegistryKey installKey = typeLib.OpenSubKey(string.Format(CultureInfo.InvariantCulture, @"{0}\win32", this.LCID)))
                    {
                        if (installKey != null)
                        {
                            this.installedFilePath = installKey.GetValue(String.Empty) as String;
                        }
                    }
                    if (this.installedFilePath == null)
                    {
                        using (RegistryKey installKey = typeLib.OpenSubKey(string.Format(CultureInfo.InvariantCulture, @"{0}\win64", this.LCID)))
                        {
                            if (installKey != null)
                            {
                                this.installedFilePath = installKey.GetValue(String.Empty) as String;
                            }
                        }

                    }
                }
            }
        }

        #endregion
    }
}
