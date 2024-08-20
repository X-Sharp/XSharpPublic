using Microsoft.Build.Execution;
using Microsoft.Build.Tasks;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Project;
using System;
using System.Collections.Generic;
using System.ComponentModel;
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
        protected ReferenceContainerNode CreateDependenciesContainerNode()
        {
            // Todo: Create SDK style Dependencies node
            var node = new XSharpDependenciesContainerNode(this);
            return node;
        }

        private ReferenceContainerNode CreateDependenciesNode()
        {
            var container = GetReferenceContainer() as ReferenceContainerNode;
            if (null == container)
            {
                // Process References
                ReferenceContainerNode referencesFolder = CreateDependenciesContainerNode();

                this.AddChild(referencesFolder);
                container = referencesFolder;
            }
            return container;

        }
        protected override void ProcessReferences()
        {
            var container = CreateDependenciesNode();
            var result = this.Build(MsBuildTarget.GetSuggestedWorkloads);
            result = this.Build(MsBuildTarget.GetTargetFrameworks);
            var project = result.ProjectInstance;
            var innerprojects = project.Items.Where(x => x.ItemType == "_InnerBuildProjects");
            var frameworks = project.Items.Where(x => x.ItemType == "_TargetFramework");
            foreach (var fw in frameworks)
            {
                var node = new XSharpFrameworkContainerNode(this, fw.EvaluatedInclude);
                container.AddChild(node);
            }
            result = this.Build(MsBuildTarget.ResolveReferences);
            project = result.ProjectInstance;
            DumpProperties(project);
            DumpItems(project);
            this.LoadFrameworkData(project);
            this.LoadPackageReferences();
        }
        private void DumpProperties(ProjectInstance project)
        {
            Logger.Information("# of Properties: " + project.Properties.Count.ToString());
            foreach (var prop in project.Properties)
            {
                Logger.Information("P:" + prop.Name + "=" + prop.EvaluatedValue);
            }
        }
        private void DumpItems(ProjectInstance project)
        {
            Logger.DoubleLine();
            Logger.Information("# of Items: " + project.Items.Count.ToString());
            foreach (var item in project.Items)
            {
                Logger.Information("I:" + item.ItemType + " " + item.EvaluatedInclude);
                foreach (var meta in item.Metadata)
                {
                    Logger.Information("   M:" + meta.Name + "=" + meta.EvaluatedValue);
                }
            }
        }
        private void LoadFrameworkData(ProjectInstance project)
        {
            //DumpProperties(project);
            //DumpItems(project);
            /*
            foreach (var target in project.Targets)
            {
                Logger.Information("T:" + target.Key+" " + target.Value.FullPath);
            }
            foreach (var p in project.GlobalProperties)
            {
                Logger.Information("GP:" + p.Key + " " + p.Value.ToString());
            }
#if DEV17
            foreach (var p in project.ImportPaths)
            {
                Logger.Information("IM:" + p);
            }
#endif
            */
        }
    }
    
    
}
