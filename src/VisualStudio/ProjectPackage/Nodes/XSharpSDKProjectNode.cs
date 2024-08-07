using Microsoft.Build.Tasks;
using Microsoft.VisualStudio.Project;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using MSBuild = Microsoft.Build.Evaluation;
namespace XSharp.Project
{
    internal class XSharpSDKProjectNode : XSharpProjectNode
    {
        public XSharpSDKProjectNode(XSharpProjectPackage package) : base(package)
        {

        }
        protected override bool IncludeProjectFileItem(MSBuild.ProjectItem item)
        {
            var path = item.EvaluatedInclude;
            if (path.StartsWith(".vs", StringComparison.OrdinalIgnoreCase))
                return false;
            if (path.StartsWith("obj", StringComparison.OrdinalIgnoreCase))
                return false;
            if (path.StartsWith("bin", StringComparison.OrdinalIgnoreCase))
                return false;
            if (!Path.IsPathRooted(path))
            {
                path = Path.Combine(this.ProjectFolder, path);
            }
            if (!File.Exists(path) && item.IsImported)
                return false;
            if (this.URLNodes.ContainsKey(path))
                return false;
            if (string.Equals(this.Url, path, StringComparison.OrdinalIgnoreCase))
                return false;
            if (string.Equals(this.Url + ".user", path, StringComparison.OrdinalIgnoreCase))
                return false;
            var ext = Path.GetExtension(path);
            if (ext.EndsWith("sln", StringComparison.OrdinalIgnoreCase))
            {
                return String.Equals(this.BuildProject.GlobalProperties["SolutionPath"], path, StringComparison.OrdinalIgnoreCase);
            }
            return true;

        }

        /// <summary>
        /// Factory method for reference container node
        /// </summary>
        /// <returns>ReferenceContainerNode created</returns>
        protected override ReferenceContainerNode CreateReferenceContainerNode()
        {
            // Todo: Create SDK style Dependencies node
            var node = new XSharpSDKReferencesContainerNode(this);
            return node;
        }

    }
    public class XSharpSDKReferencesContainerNode : XSharpReferenceContainerNode
    {
        public XSharpSDKReferencesContainerNode(ProjectNode project) : base(project)
        {
        }
        public override string Caption => "Dependencies";

    }
}
