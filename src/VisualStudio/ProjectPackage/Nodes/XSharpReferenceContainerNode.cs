//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Project;
using System.IO;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;

namespace XSharp.Project
{
    /// <summary>
    /// Reference container node
    /// </summary>
    public class XSharpReferenceContainerNode : ReferenceContainerNode
    {
        public XSharpReferenceContainerNode(ProjectNode project) : base(project)
        {
        }

        protected virtual System.Type ProjectReferenceType => typeof(XSharpProjectReferenceNode);
        protected virtual System.Type AssemblyReferenceType => typeof(XSharpAssemblyReferenceNode);

        protected virtual System.Type ComReferenceType => typeof(XSharpComReferenceNode);

        protected override ProjectReferenceNode CreateProjectReferenceNode(ProjectElement element)
        {
            // Check to see if we have the Guid and Name in the ProjectElement
            var guid = element.GetMetadata(ProjectFileConstants.Project);
            var name = element.GetMetadata(ProjectFileConstants.Name);
            var path = element.Item.EvaluatedInclude;
            var parent = (XSharpProjectNode)this.ProjectMgr;
            path = Path.Combine(parent.ProjectFolder, path);
            path = Path.GetFullPath(path);
            var refnode = XSharpProjectNode.FindProject(path);
            bool changed = false;
            if (parent.IsSdkProject)
            {
                // already handled in the DependenciesContainer
                name = System.IO.Path.GetFileNameWithoutExtension(path);
                if (refnode != null)
                {
                    guid = refnode.ProjectIDGuid.ToString("B");
                }
                else
                {
                    guid = null;
                    parent.HasIncompleteReferences = true;
                }
            }
            else if (string.IsNullOrEmpty(guid) || string.IsNullOrEmpty(name))
            {
                // No guid, so it is probably an old style project reference
                // In that case we need to get the guid from the project file
                if (refnode != null)
                {
                    guid = refnode.ProjectIDGuid.ToString("B");
                    name = refnode.GetProjectProperty(ProjectFileConstants.AssemblyName);
                    element.SetMetadata(ProjectFileConstants.Project, guid);
                    element.SetMetadata(ProjectFileConstants.Name, name);
                    changed = true;
                }
                else
                {
                    parent.HasIncompleteReferences = true;
                }
                if (refnode != null)
                {
                    var refguid = refnode.ProjectIDGuid.ToString("B");
                    if (string.Compare(guid, refguid, StringComparison.OrdinalIgnoreCase) != 0)
                    {
                        // The guid's do not match, so update the project element
                        guid = refguid;
                        element.SetMetadata(ProjectFileConstants.Project, guid);
                        changed = true;
                    }
                }
            }
            if (changed)
            {
                parent.BuildProject.Save();
            }

            var node = (XSharpProjectReferenceNode) Activator.CreateInstance(ProjectReferenceType,this.ProjectMgr, element);
            ReferenceNode existing = null;
            if (isDuplicateNode(node, ref existing))
            {
                node = existing as XSharpProjectReferenceNode;
            }
            return node;
        }
        public override ReferenceNode AddReferenceFromSelectorData(VSCOMPONENTSELECTORDATA selectorData, string wrapperTool)
        {
            if (string.IsNullOrEmpty(wrapperTool))
                wrapperTool = WrapperToolAttributeValue.TlbImp.ToString().ToLowerInvariant();
            foreach (ReferenceNode child in this.EnumReferences())
            {
                XSharpComReferenceNode comnode = child as XSharpComReferenceNode;

                if (comnode != null && comnode.Matches(selectorData, wrapperTool))
                    return comnode;

            }
            return base.AddReferenceFromSelectorData(selectorData, wrapperTool);
        }
        protected override ProjectReferenceNode CreateProjectReferenceNode(VSCOMPONENTSELECTORDATA selectorData)
        {
            ProjectReferenceNode node = null;
            try
            {
                node = (XSharpProjectReferenceNode)Activator.CreateInstance(ProjectReferenceType, this.ProjectMgr, selectorData.bstrTitle, selectorData.bstrFile, selectorData.bstrProjRef);
            }
            catch (Exception e)
            {
                Logger.Exception(e, "CreateProjectReferenceNode");
            }
            ReferenceNode existing = null;
            if (isDuplicateNode(node, ref existing))
            {
                node = existing as ProjectReferenceNode;
            }
            return node;
        }
        protected override AssemblyReferenceNode CreateAssemblyReferenceNode(ProjectElement element)
        {
            AssemblyReferenceNode node = null;
            try
            {
                node = (AssemblyReferenceNode) Activator.CreateInstance(AssemblyReferenceType, this.ProjectMgr, element);
            }
            catch (Exception e)
            {
                Logger.Exception(e, "CreateAssemblyReferenceNode");
            }
            ReferenceNode existing = null;
            if (isDuplicateNode(node, ref existing))
            {
                node = existing as AssemblyReferenceNode;
            }
            return node;
        }

        // How to handle multiple references.
        public static int multiRefAutoCorrection = 0;

        protected override ComReferenceNode CreateComReferenceNode(ProjectElement reference)
        {
            ComReferenceNode node = null;
            try
            {
                node = (ComReferenceNode)Activator.CreateInstance(ComReferenceType, this.ProjectMgr, reference);
            }
            catch (Exception e)
            {
                Logger.Exception(e, "CreateComReferenceNode");
            }
            return node;
        }

        protected override ComReferenceNode CreateComReferenceNode(Microsoft.VisualStudio.Shell.Interop.VSCOMPONENTSELECTORDATA selectorData, string wrapperTool)
        {
            ComReferenceNode node = null;
            try
            {
                node = (ComReferenceNode)Activator.CreateInstance(ComReferenceType, this.ProjectMgr, selectorData, wrapperTool);
            }
            catch (Exception e)
            {
                Logger.Exception(e, "CreateComReferenceNode");
            }
            return node;
        }
#if DEV17
        protected override bool SupportsIconMonikers => true;
        protected override ImageMoniker GetIconMoniker(bool open)
        {
            return KnownMonikers.ReferenceGroup;
        }
#else
        // VS2019 does not have these image monikers
        protected override bool SupportsIconMonikers => false;
        public override int ImageIndex => XSharpImageListIndex.ReferenceGroup + XProjectNode.imageOffset;

#endif

        private bool isDuplicateNode(string nodeCaption, ref ReferenceNode ExistingNode)
        {
            if (nodeCaption != null)
            {
                foreach (ReferenceNode child in this.EnumReferences())
                {
                    // check for duplicate nodes
                    if (child.Caption == nodeCaption)
                    {
                        ExistingNode = child;
                        return true;
                    }
                }
            }
            ExistingNode = null;
            return false;

        }

        private bool isDuplicateNode(ReferenceNode node, ref ReferenceNode ExistingNode)
        {
            if (node != null)
            {
                return isDuplicateNode(node.Caption, ref ExistingNode);
            }
            ExistingNode = null;
            return false;

        }
        protected override ReferenceNode CreateFileComponent(VSCOMPONENTSELECTORDATA selectorData, string _wrapperTool = null)
        {
            ReferenceNode node = null;
            ReferenceNode existing = null;
            // To avoid the add of the Reference in the reference list
            // we will first check if it is already in there
            if (selectorData.bstrFile == null)
            {
                throw new ArgumentNullException("selectorData");
            }
            //
            if (selectorData.bstrFile[0] == '*')
            {
                selectorData.bstrFile = selectorData.bstrFile.Substring(1);
            }
            // We have a path to a file, it could be anything
            // First see if it is a managed assembly
            if (File.Exists(selectorData.bstrFile))
            {
                string assemblyPath = selectorData.bstrFile;
                System.Reflection.AssemblyName assemblyName = System.Reflection.AssemblyName.GetAssemblyName(assemblyPath);
                string caption = assemblyName.Name;
                if (isDuplicateNode(caption, ref existing))
                {
                    //
                    string existingUrl = existing.Url;
                    if (File.Exists(existingUrl))
                        return existing;
                    // file does not exist so this new node is better
                    existing.Remove(false);
                    var xProjectNode = this.ProjectMgr as XSharpProjectNode;
                    xProjectNode.ProjectModel.AddAssemblyReference(existingUrl);
                }
            }
            //
            // Ok, try to create and add the reference
            node = base.CreateFileComponent(selectorData, _wrapperTool);
            if (isDuplicateNode(node, ref existing))
            {
                // The CreateFileComponent create and Add the project element
                // but as it is duplicated..Remove it !
                node.Remove(false);
                var xProjectNode = this.ProjectMgr as XSharpProjectNode;
                xProjectNode.ProjectModel.AddAssemblyReference(existing.Url);
                return existing;
            }
            return node;
        }
        /// <summary>
        /// Creates an assembly reference node from a file path.
        /// </summary>
        protected override AssemblyReferenceNode CreateAssemblyReferenceNode(string fileName)
        {
            AssemblyReferenceNode node = null;
            try
            {
                // Ok when file name is a full path or when it doesn't have a DLL extension
                if (!File.Exists(fileName))
                {
                    if (fileName.EndsWith(".dll", StringComparison.OrdinalIgnoreCase))
                    {
                        fileName = Path.GetFileNameWithoutExtension(fileName);
                    }
                }
                node = (AssemblyReferenceNode)Activator.CreateInstance(AssemblyReferenceType, this.ProjectMgr, fileName);
            }
            catch (Exception e)
            {
                Logger.Exception(e, "CreateAssemblyReferenceNode");
            }
            return node;
        }

    }
}
