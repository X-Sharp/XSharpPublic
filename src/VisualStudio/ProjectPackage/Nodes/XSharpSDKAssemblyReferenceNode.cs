using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using System;
using System.IO;
using System.Reflection;

namespace XSharp.Project
{
    internal class XSharpSDKAssemblyReferenceNode : AssemblyReferenceNode
    {
        public XSharpSDKAssemblyReferenceNode(ProjectNode root, string assemblyPath)
           : base(root, assemblyPath)
        {
            //Binding reference data at startup will cause a 'project has changed' method
            BindReferenceData();

        }
        protected override void BindReferenceData()
        {
            return;
        }
    }
}
