using EnvDTE;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.ComponentModelHost;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using System;
using System.IO;
using System.Linq;

namespace XSharp.Project
{
    public class XSharpPackageReferenceNode : HierarchyNode
    {
        protected OAVSProjectItem vsProjectItem;


        private static string CaptionFormat = "{0} ({1})";

        private string _caption;

        public override int ImageIndex => 56;

        public override object Object => this;

        public override string Url => string.Empty;

        public override string Caption
        {
            get
            {
                if (string.IsNullOrEmpty(_caption))
                {
                    _caption = GetCaption();
                }
                return _caption;
            }
        }

        public string Version
        {
            get
            {
                if (string.IsNullOrEmpty(base.ItemNode.GetMetadata("Version")))
                {
                    string version = "0";
                    if (string.Compare("Microsoft.NETCore.App", base.ItemNode.Item.UnevaluatedInclude, ignoreCase: true) == 0)
                    {
                        version = string.Concat(base.ProjectMgr.GetProjectProperty("TargetFramework").ToArray().Reverse()
                            .TakeWhile((char ch) => char.IsNumber(ch) || ch == '.')
                            .Reverse());
                    }
                    return version;
                }
                return base.ItemNode.GetMetadata("Version");
            }
        }

        public string Name => base.ItemNode.GetMetadata("Include");

        public override Guid ItemTypeGuid => VSConstants.PlatformReferenceProvider_Guid;

        public override int MenuCommandId => 1105;

        public XSharpPackageReferenceNode(ProjectNode projectManager, ProjectElement element)
            : base(projectManager, element)
        {
            XSharpProjectNode XSharpProject = projectManager as XSharpProjectNode;
            if (XSharpProject != null)
            {
                base.Parent = XSharpProject.PackageReferenceContainerNode;
            }
        }

        private string GetCaption()
        {
            if (base.ItemNode == null)
            {
                return Name;
            }
            return string.Format(CaptionFormat, Name, Version);
        }

        public override void ReDraw(UIHierarchyElement element)
        {
            if (element == UIHierarchyElement.Caption)
            {
                _caption = GetCaption();
            }
            base.ReDraw(element);
        }

        protected override string GetCanonicalName()
        {
            return Path.Combine(base.Parent.Caption, Name);
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

        protected override int QueryStatusOnNode(Guid cmdGroup, uint cmd, IntPtr pCmdText, ref QueryStatusResult result)
        {
            if (cmdGroup == VsMenus.guidStandardCommandSet97)
            {
                if (cmd == 17 || cmd == 168)
                {
                    result |= (QueryStatusResult.SUPPORTED | QueryStatusResult.ENABLED);
                    return 0;
                }
            }
            return base.QueryStatusOnNode(cmdGroup, cmd, pCmdText, ref result);
        }

        protected override int ExecCommandOnNode(Guid cmdGroup, uint cmd, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            if (cmdGroup == VsMenus.guidStandardCommandSet97)
            {
                if (cmd == 17 || cmd == 168)
                {
                    //((IComponentModel)GetService(typeof(SComponentModel))).GetService<IVsPackageUninstaller>()?.UninstallPackage(base.ProjectMgr.GetAutomationObject() as XSharpProjectNode, base.ItemNode.GetMetadata("Include"), removeDependencies: false);
                    return 0;
                }
            }
            return base.ExecCommandOnNode(cmdGroup, cmd, nCmdexecopt, pvaIn, pvaOut);
        }
    }
}
