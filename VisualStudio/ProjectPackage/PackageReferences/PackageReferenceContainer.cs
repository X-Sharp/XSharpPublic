using Microsoft.Build.Evaluation;
using Microsoft.Build.Execution;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Shell.Interop;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace XSharp.Project
{
    public class XSharpPackageReferenceContainerNode : HierarchyNode
    {
        protected OAVSProjectItem vsProjectItem;

        private OAVSPackageReferences _vsPackageReferences;

        public const string PackageReferencesNodeVirtualName = "NuGet Packages";

        public List<XSharpPackageReferenceNode> PackageReferenceNodes 
        {
            get;
            set;
        }

        public override object Object => _vsPackageReferences;

        private XSharpProjectNode XSharpProjectNode
        {
            get;
            set;
        }

        public override string Caption => PackageReferencesNodeVirtualName;

        protected override bool SupportsIconMonikers => true;
        protected override ImageMoniker GetIconMoniker(bool open)
        {
            if (open)
                return KnownMonikers.PackageFolderOpened;
            else
                return KnownMonikers.PackageFolderClosed;
        }

        public override int SortPriority => DefaultSortOrderNode.NuGetPackagesNode;

        public override Guid ItemTypeGuid => VSConstants.GUID_ItemType_VirtualFolder;

        public override int MenuCommandId => VsMenus.IDM_VS_CTXT_PACKAGEREFERENCE_GROUP;

        public override string Url => base.VirtualNodeName;

        public XSharpPackageReferenceContainerNode(XSharpProjectNode parent)
            : base(parent)
        {
            base.Parent = parent;
            this.ExcludeNodeFromScc = true;
            _vsPackageReferences = null;
            _vsPackageReferences = new OAVSPackageReferences(this);
            PackageReferenceNodes = new List<XSharpPackageReferenceNode>();
            XSharpProjectNode = parent;
            base.VirtualNodeName = PackageReferencesNodeVirtualName;
        }
        protected override NodeProperties CreatePropertiesObject()
        {
            return new ReferenceContainerNodeProperties(this);

        }
        public override object GetProperty(int propId)
        {
            switch ((__VSHPROPID) propId)
            {
                //case __VSHPROPID.VSHPROPID_IsHiddenItem:
                //    var result = this.FirstChild != null;
                //    return result;
                case __VSHPROPID.VSHPROPID_NextVisibleSibling:
                case __VSHPROPID.VSHPROPID_NextSibling:
                    return base.NextSibling?.ID;
                default:
                    {
                        object baseVal = base.GetProperty(propId);
                        if (baseVal == null)
                        {
                            return base.ProjectMgr.GetProperty(propId);
                        }
                        return baseVal;
                    }
            }
        }

        public override object GetIconHandle(bool open)
        {
            int index = ImageIndex;
            if (-1 == index)
            {
                return base.GetIconHandle(open);
            }
            return base.ProjectMgr.ImageHandler.GetIconHandle(index);
        }

        public void AddOrUpdate(string bstrName, string bstrVersion, Array pbstrMetadataElements, Array pbstrMetadataValues)
        {
            var matches = PackageReferenceNodes.Where(node => string.Equals( node.Name, bstrName, StringComparison.InvariantCultureIgnoreCase));
            if (matches.Count() > 0)
            {
                XSharpPackageReferenceNode currentPackage = matches.First();
                currentPackage.ItemNode.Item.SetMetadataValue("Version", bstrVersion);
                for (int j = 0; j < pbstrMetadataElements.Length; j++)
                {
                    currentPackage.ItemNode.SetMetadata((string)pbstrMetadataElements.GetValue(j), (string)pbstrMetadataValues.GetValue(j));
                }
                base.ProjectMgr.SetProjectFileDirty(value: true);
                currentPackage.ReDraw(UIHierarchyElement.Caption);
                ReDraw(UIHierarchyElement.Caption);
            }
            else
            {
                XSharpPackageReferenceNode packageReference = CreatePackageReferenceNode(bstrName);
                packageReference.ItemNode.Item.SetMetadataValue("Version", bstrVersion);
                for (int i = 0; i < pbstrMetadataElements.Length; i++)
                {
                    packageReference.ItemNode.SetMetadata((string)pbstrMetadataElements.GetValue(i), (string)pbstrMetadataValues.GetValue(i));
                }
                PackageReferenceNodes.Add(packageReference);
                AddChild(packageReference);
            }
        }

        public void Remove(string bstrName)
        {
            var match = PackageReferenceNodes.Where( node => string.Equals(node.Name, bstrName, StringComparison.InvariantCultureIgnoreCase)).First();
            if (match != null)
            {
                PackageReferenceNodes.Remove(match);
                match.Remove(removeFromStorage: false);
            }
        }

        public bool TryGetReference(string bstrName, Array aDesiredMetadata, out string pbstrVersion, out Array aMetadataElements, out Array aMetadataValues)
        {
            // the array aDesiredMetadata contains the names of the properties
            // that they want.
            // we also always return the Version
            var nodes = PackageReferenceNodes.Where((n) => string.Equals(n.Name ,bstrName, StringComparison.InvariantCultureIgnoreCase));
            if (nodes.Count() == 0)
            {
                pbstrVersion = null;
                aMetadataElements = null;
                aMetadataValues = null;
                return false;
            }
            var node = nodes.FirstOrDefault();
            string verName = "Version";
            pbstrVersion = ((!string.IsNullOrEmpty(node?.ItemNode.GetMetadata(verName))) ? node?.ItemNode.GetMetadata(verName) : node?.Version);
            var metadata = new Dictionary<string, string>();
            var desiredMetadata = aDesiredMetadata.Cast<string>();
            foreach (ProjectMetadata mde in node?.ItemNode.Item.Metadata)
            {
                if (desiredMetadata.Contains(mde.Name))
                {
                    metadata.Add(mde.Name, mde.EvaluatedValue);
                }
                else if (string.Compare(mde.Name, verName, ignoreCase: true) == 0)
                {
                    metadata.Add(mde.Name, node.Version);
                }
            }
            aMetadataElements = metadata.Keys.ToArray();
            aMetadataValues = metadata.Values.ToArray();
            return true;
        }


        public IEnumerable<string> GetInstalledPackages()
        {
            return PackageReferenceNodes.Select((XSharpPackageReferenceNode node) => node.Name);
        }

        private XSharpPackageReferenceNode CreatePackageReferenceNode(string name)
        {
            return XSharpProjectNode.CreatePackageReferenceNode(name);
        }
        internal virtual void LoadReferencesFromBuildProject(XSharpProjectNode buildProject)
        {
            foreach (var item in base.ProjectMgr.BuildProject.GetItems(ProjectFileConstants.PackageReference))
            {
                var element = new ProjectElement(base.ProjectMgr, item, false);
                var referenceNode = new XSharpPackageReferenceNode(ProjectMgr, element);
                bool found = false;
                HierarchyNode i = base.FirstChild;
                while (i != null && !found)
                {
                    if (string.Compare(i.Caption, referenceNode.Caption, StringComparison.OrdinalIgnoreCase) == 0)
                    {
                        found = true;
                    }
                    i = i.NextSibling;
                }
                if (!found)
                {
                    AddChild(referenceNode);
                    PackageReferenceNodes.Add(referenceNode);
                }
            }
        }

        protected override string GetCanonicalName()
        {
            return Path.Combine(base.ProjectMgr.ProjectFolder, Caption);
        }
        
    }
}
