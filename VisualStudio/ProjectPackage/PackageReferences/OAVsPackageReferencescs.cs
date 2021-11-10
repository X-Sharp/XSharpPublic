using EnvDTE;
using Microsoft.VisualStudio.Shell;
using System;
using System.Linq;
using VSLangProj150;

namespace XSharp.Project
{
    internal class OAVSPackageReferences : PackageReferences
    {
        private XSharpPackageReferenceContainerNode PackageReferenceContainerNode
        {
            get;
            set;
        }

        public DTE DTE
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                return (EnvDTE.DTE)PackageReferenceContainerNode.ProjectMgr.GetService(typeof(EnvDTE.DTE));
            }
        }
        public dynamic Parent => PackageReferenceContainerNode.Parent.Object;

        public XSharpProjectNode ContainingProject => PackageReferenceContainerNode.ProjectMgr.GetAutomationObject() as XSharpProjectNode;

        public Array InstalledPackages => PackageReferenceContainerNode.GetInstalledPackages().ToArray();

        public OAVSPackageReferences(XSharpPackageReferenceContainerNode containerNode)
        {
            PackageReferenceContainerNode = containerNode;
            //PackageReferenceContainerNode.OnChildAdded += PackageReferenceContainerNode_OnChildAdded;
            //PackageReferenceContainerNode.OnChildRemoved += PackageReferenceContainerNode_OnChildRemoved;
        }

        //private void PackageReferenceContainerNode_OnChildRemoved(object sender, HierarchyNodeEventArgs e)
        //{
        //}

        //private void PackageReferenceContainerNode_OnChildAdded(object sender, HierarchyNodeEventArgs e)
        //{
        //}

        public void AddOrUpdate(string bstrName, string bstrVersion, Array pbstrMetadataElements, Array pbstrMetadataValues)
        {
            PackageReferenceContainerNode.AddOrUpdate(bstrName, bstrVersion, pbstrMetadataElements, pbstrMetadataValues);
        }

        public void Remove(string bstrName)
        {
            PackageReferenceContainerNode.Remove(bstrName);
        }

        public bool TryGetReference(string bstrName, Array parrbstrDesiredMetadata, out string pbstrVersion, out Array pbstrMetadataElements, out Array pbstrMetadataValues)
        {
            return PackageReferenceContainerNode.TryGetReference(bstrName, parrbstrDesiredMetadata, out pbstrVersion, out pbstrMetadataElements, out pbstrMetadataValues);
        }
    }
}
