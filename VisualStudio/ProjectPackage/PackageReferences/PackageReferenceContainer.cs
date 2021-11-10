using Microsoft.Build.Evaluation;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Shell;
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


        public const string PackageReferencesNodeVirtualName = "NuGet";

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

        public override string Caption => "NuGet";

        public override int ImageIndex => 56;

        public override int SortPriority => 300;

        public override Guid ItemTypeGuid => VSConstants.GUID_ItemType_VirtualFolder;

        public override int MenuCommandId => 1104;

        public override string Url => base.VirtualNodeName;

        public XSharpPackageReferenceContainerNode(XSharpProjectNode parent)
            : base(parent)
        {
            base.Parent = parent;
            _vsPackageReferences = null;
            _vsPackageReferences = new OAVSPackageReferences(this);
            PackageReferenceNodes = new List<XSharpPackageReferenceNode>();
            XSharpProjectNode = parent;
            base.VirtualNodeName = "NuGet";
        }

        public override object GetProperty(int propId)
        {
            switch (propId)
            {
                case -2043:
                    return 1;
                case -2042:
                case -1002:
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
            IEnumerable<XSharpPackageReferenceNode> matches = PackageReferenceNodes.Where((XSharpPackageReferenceNode node) => node.Name.Equals(bstrName, StringComparison.InvariantCultureIgnoreCase));
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
            XSharpPackageReferenceNode match = PackageReferenceNodes.Where((XSharpPackageReferenceNode node) => node.Name.Equals(bstrName, StringComparison.InvariantCultureIgnoreCase)).First();
        }

        public bool TryGetReference(string bstrName, Array parrbstrDesiredMetadata, out string pbstrVersion, out Array pbstrMetadataElements, out Array pbstrMetadataValues)
        {
            IEnumerable<XSharpPackageReferenceNode> matches = PackageReferenceNodes.Where((XSharpPackageReferenceNode node) => node.Name.Equals(bstrName, StringComparison.InvariantCultureIgnoreCase));
            if (matches.Count() == 0)
            {
                pbstrVersion = null;
                pbstrMetadataElements = null;
                pbstrMetadataValues = null;
                return false;
            }
            XSharpPackageReferenceNode match = matches.FirstOrDefault();
            pbstrVersion = ((!string.IsNullOrEmpty(match?.ItemNode.GetMetadata("Version"))) ? match?.ItemNode.GetMetadata("Version") : match?.Version);
            Dictionary<string, string> metadata = new Dictionary<string, string>();
            IEnumerable<string> desiredMetadata = parrbstrDesiredMetadata.Cast<string>();
            foreach (ProjectMetadata mde in match?.ItemNode.Item.Metadata)
            {
                if (desiredMetadata.Contains(mde.Name))
                {
                    metadata.Add(mde.Name, mde.EvaluatedValue);
                }
                else if (string.Compare(mde.Name, "Version", ignoreCase: true) == 0)
                {
                    metadata.Add(mde.Name, match.Version);
                }
            }
            pbstrMetadataElements = metadata.Keys.ToArray();
            pbstrMetadataValues = metadata.Values.ToArray();
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

        public virtual void LoadReferencesFromBuildProject(XSharpProjectNode buildProject)
        {
            foreach (ProjectItem item in base.ProjectMgr.BuildProject.ThreadSafeGetItems("PackageReference"))
            {
                ProjectElement element = new ProjectElement(base.ProjectMgr, item, false);
                XSharpPackageReferenceNode referenceNode = new XSharpPackageReferenceNode(base.ProjectMgr, element);
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

        protected override int QueryStatusOnNode(Guid cmdGroup, uint cmd, IntPtr pCmdText, ref QueryStatusResult result)
        {
            if (cmdGroup == Microsoft.VisualStudio.Shell.VsMenus.guidStandardCommandSet2K)
            {
                if (cmd == 1113 || cmd == 1125 || cmd - 1129 <= 1)
                {
                    result |= QueryStatusResult.INVISIBLE;
                    return 0;
                }
            }
            return base.QueryStatusOnNode(cmdGroup, cmd, pCmdText, ref result);
        }

        protected override int ExecCommandOnNode(Guid cmdGroup, uint cmd, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            return base.ExecCommandOnNode(cmdGroup, cmd, nCmdexecopt, pvaIn, pvaOut);
        }
    }
}
