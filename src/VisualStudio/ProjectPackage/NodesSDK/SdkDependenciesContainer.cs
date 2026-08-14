#if DEV17
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell.Interop;

using System.Collections.Generic;
using System.Linq;

namespace XSharp.Project
{
    class XSharpDependenciesContainerNode : XSharpReferenceContainerNode
    {
        protected override System.Type ProjectReferenceType => typeof(XSharpSDKProjectReferenceNode);
        protected override System.Type AssemblyReferenceType => typeof(XSharpSDKAssemblyReferenceNode);
        protected override System.Type ComReferenceType => typeof(XSharpSDKComReferenceNode);

        internal List<XSharpSdkFrameworksNode> FrameworkNodes { get; private set; }
        private XSharpSdkProjectsNode _ProjectsNode = null;
        private XSharpSdkAssembliesNode _AssembliesNode = null;
        public XSharpDependenciesContainerNode(XSharpProjectNode root) : base(root)
        {
            // Create the FrameworkReference node
            FrameworkNodes = new List<XSharpSdkFrameworksNode>();
            CreateFrameworkReferenceNode();
        }
        internal XSharpSdkProjectsNode GetProjectsNode()
        {
            if (_ProjectsNode == null)
            {
                _ProjectsNode = new XSharpSdkProjectsNode((XSharpProjectNode)this.ProjectMgr);
                base.AddChild(_ProjectsNode);
            }
            return _ProjectsNode;
        }
        internal XSharpSdkAssembliesNode GetAssembliesNode()
        {
            if (_AssembliesNode == null)
            {
                _AssembliesNode = new XSharpSdkAssembliesNode((XSharpProjectNode)this.ProjectMgr);
                base.AddChild(_AssembliesNode);
            }
            return _AssembliesNode;
        }
        public override int SortPriority => DefaultSortOrderNode.DependenciesNode;

        public override string Caption => "Dependencies";

        private void CreateFrameworkReferenceNode()
        {
            var project = this.ProjectMgr as XSharpSdkProjectNode;
            var node = new XSharpSdkFrameworksNode((XSharpProjectNode)this.ProjectMgr);
            this.FrameworkNodes.Add(node);
            this.AddChild(node);
            if (project.SubProjects.Count == 1)
            {
                var subProject = project.SubProjects[0];
                // Create the node where the assembly dependencies will be stored
                subProject.FrameworksNode = node;
            }
            else
            {
                foreach (var subProject in project.SubProjects)
                {
                    var targetNode = new XSharpTargetFrameworkReferenceNode((XSharpProjectNode)this.ProjectMgr, subProject.TargetFramework);
                    subProject.TargetFrameworkReferenceNode = targetNode;
                    subProject.FrameworksNode = node;
                    node.AddChild(targetNode);
                }
            }
        }
        public override void AddChild(HierarchyNode node)
        {
            switch (node)
            {
                case XSharpIncludeContainerNode inc:
                    base.AddChild(inc);
                    break;
                case XSharpProjectReferenceNode pr:
                    var projects = GetProjectsNode();
                    projects.AddChild(pr);
                    break;
                case XSharpAssemblyReferenceNode ar:
                    var assemblies = GetAssembliesNode();
                    assemblies.AddChild(ar);
                    break;
                case XSharpTargetFrameworkReferenceNode fr:
                    var frameworkNode = FrameworkNodes.FirstOrDefault();
                    frameworkNode.AddChild(fr);
                    break;
                default:
                    base.AddChild(node);
                    break;
            }
        }

        public override void RemoveChild(HierarchyNode node)
        {
            base.RemoveChild(node);
        }

        /// <summary>
        /// Handle the guidVSStd16 "Add ... Reference..." commands of the IDM_VS_CTXT_REFERENCEROOT menu.
        /// </summary>
        /// <remarks>
        /// ReferenceContainerNode.QueryStatusOnNode returns OLECMDERR_E_UNKNOWNGROUP for every command
        /// group other than the 97 and 2K sets without calling its base, so we have to answer the
        /// guidVSStd16 commands here, before base is called.
        /// </remarks>
        protected override int QueryStatusOnNode(System.Guid cmdGroup, uint cmd, System.IntPtr pCmdText, ref QueryStatusResult result)
        {
            if (cmdGroup == XSharpSdkProjectNode.VsStd16 && this.ProjectMgr is XSharpSdkProjectNode sdk)
            {
                var hr = sdk.QueryStatusReferenceCommand(cmd, ref result);
                if (hr == Microsoft.VisualStudio.VSConstants.S_OK)
                    return hr;
            }
            return base.QueryStatusOnNode(cmdGroup, cmd, pCmdText, ref result);
        }

        protected override int ExecCommandOnNode(System.Guid cmdGroup, uint cmd, uint nCmdexecopt, System.IntPtr pvaIn, System.IntPtr pvaOut)
        {
            if (cmdGroup == XSharpSdkProjectNode.VsStd16 && this.ProjectMgr is XSharpSdkProjectNode sdk)
            {
                var hr = sdk.ExecReferenceCommand(cmd);
                if (hr != Microsoft.VisualStudio.VSConstants.E_NOTIMPL)
                    return hr;
            }
            return base.ExecCommandOnNode(cmdGroup, cmd, nCmdexecopt, pvaIn, pvaOut);
        }

        public void DeleteDependencies(string targetframework)
        {

            //    var nodes = new List<XSharpDependencyNode>();
            //    frameworkNode.FindNodesOfType(nodes);
            //    foreach (var child in nodes)
            //    {
            //        frameworkNode.RemoveChild(child);
            //        child.Dispose();
            //    }
        }

        protected override ProjectReferenceNode CreateProjectReferenceNode(ProjectElement element)
        {
            return base.CreateProjectReferenceNode(element);
        }


        protected override ProjectReferenceNode CreateProjectReferenceNode(VSCOMPONENTSELECTORDATA selectorData)
        {
            var result = base.CreateProjectReferenceNode(selectorData);
            return result;
        }

        protected override AssemblyReferenceNode CreateAssemblyReferenceNode(ProjectElement element)
        {
            return base.CreateAssemblyReferenceNode(element);

        }

        protected override ComReferenceNode CreateComReferenceNode(ProjectElement reference)
        {
            return base.CreateComReferenceNode(reference);
        }

        protected override ComReferenceNode CreateComReferenceNode(Microsoft.VisualStudio.Shell.Interop.VSCOMPONENTSELECTORDATA selectorData, string wrapperTool)
        {
            return base.CreateComReferenceNode(selectorData, wrapperTool);
        }
    }

}
#endif
