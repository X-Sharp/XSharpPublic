//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.Project;
using Microsoft.Build.Execution;
using System.Reflection;
using System.IO;
using Microsoft.VisualStudio.Shell;
using System.Diagnostics;

namespace XSharp.Project
{
    [DebuggerDisplay("{Caption}" )]
   class XSharpAssemblyReferenceNode : AssemblyReferenceNode
   {
        private HashSet<string> resolvedProperties; // the names of the properties that MsBuild has resolved
        private ProjectItemInstance prjitem; // the project item that contains the properties from resolvedProperties

        public XSharpAssemblyReferenceNode(ProjectNode root, ProjectElement element)
         : base(root, element)
      {
         //Binding reference data at startup will cause a 'project has changed' method
         //BindReferenceData();

      }
        public XSharpAssemblyReferenceNode(ProjectNode root, string assemblyPath)
           : base(root, assemblyPath)
        {
            //Binding reference data at startup will cause a 'project has changed' method
            BindReferenceData();
            ResolveAssemblyReference();

        }

        protected override void BindReferenceData()
        {
            base.BindReferenceData();
            XSharpProjectNode projectNode = (XSharpProjectNode)this.ProjectMgr;
            projectNode.ProjectModel.AddAssemblyReference(this.AssemblyPath);
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
        protected override NodeProperties CreatePropertiesObject()
        {
            return new XSharpAssemblyReferenceNodeProperties(this);
        }

        public override void Remove(bool removeFromStorage)
        {
            XSharpProjectNode projectNode = (XSharpProjectNode)this.ProjectMgr;
            projectNode.ProjectModel.RemoveAssemblyReference(this.AssemblyPath);
            base.Remove(removeFromStorage);
        }
        protected override void SetHintPathAndPrivateValue(ProjectInstance instance,ProjectItemInstance iteminstance)
        {

            // Private means local copy; we want to know if it is already set to not override the default
            string privateValue = this.ItemNode.GetMetadata(ProjectFileConstants.Private);
            string originalHintPath = this.ItemNode.GetMetadata(ProjectFileConstants.HintPath);


            this.ResolvedAssembly = AssemblyName.GetAssemblyName(this.AssemblyPath);
            resolvedProperties = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            foreach (var str in iteminstance.MetadataNames)
            {
                resolvedProperties.Add(str);
            }
            prjitem = iteminstance;

            string hintPath = iteminstance.GetMetadataValue(ProjectFileConstants.HintPath);
            if (hintPath != originalHintPath)
            {
                if (Path.IsPathRooted(hintPath))
                {
                    hintPath = PackageUtilities.GetPathDistance(this.ProjectMgr.BaseURI.Uri, new Uri(hintPath));
                }
                if (hintPath != originalHintPath)
                {
                    this.ItemNode.SetMetadata(ProjectFileConstants.HintPath, hintPath);
                }
                // If this is not already set, we default to true
                if (String.IsNullOrEmpty(privateValue))
                {
                    this.ItemNode.SetMetadata(ProjectFileConstants.Private, true.ToString());
                }
            }

        }
        protected override void ResolveAssemblyReference()
        {
            if (this.ProjectMgr == null || this.ProjectMgr.IsClosed)
            {
                return;
            }

            var instance = this.ProjectMgr.ProjectInstance;
            // do not call MsBuild again that will slow down things a lot.
            var group = MSBuildProjectInstance.GetItems(instance, ProjectFileConstants.ReferencePath);
            if (group != null)
            {
                foreach (var item in group)
                {
                    string fullPath = item.GetMetadataValue("fullpath");
                    if (! File.Exists(fullPath))
                        continue;
                    AssemblyName name = AssemblyName.GetAssemblyName(fullPath);

                    // Try with full assembly name and then with weak assembly name.
                    if (String.Compare(name.FullName, this.AssemblyName.FullName, StringComparison.OrdinalIgnoreCase) == 0 ||
                        String.Compare(name.Name, this.AssemblyName.Name, StringComparison.OrdinalIgnoreCase) == 0)
                    {
                        if (!NativeMethods.IsSamePath(fullPath, this.AssemblyPath))
                        {
                            // set the full path now.
                            this.AssemblyPath = fullPath;

                            // We have a new item to listen too, since the assembly reference is resolved from a different place.
                            base.ObserveItem(this.AssemblyPath);
                        }
                        this.ResolvedAssembly = name;
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
                var xnode = ProjectMgr as XSharpProjectNode;
                if (xnode != null && ! xnode.IsLoading)
                {
                    base.ResolveAssemblyReference();
                }

                return;
            }

        }

    }
}
