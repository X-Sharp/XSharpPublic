using Microsoft.VisualStudio;
using Microsoft.VisualStudio.ComponentModelHost;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Shell;

using System;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
namespace XSharp.Project
{
    public class XSharpPackageReferenceNode : HierarchyNode
    {
        protected OAVSProjectItem vsProjectItem;


        private static string CaptionFormat = "{0} ({1})";

        private string _caption;

        protected override bool SupportsIconMonikers => true;
        protected override ImageMoniker GetIconMoniker(bool open)
        {
            return KnownMonikers.PackageReference;
        }

        public override object Object => this;

        public override string Url => string.Empty;

        public override string Caption
        {
            get
            {
                
                return GetCaption();
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

        public override int MenuCommandId => Microsoft.VisualStudio.Project.VsMenus.IDM_VS_CTXT_PACKAGEREFERENCE;

        public XSharpPackageReferenceNode(ProjectNode projectManager, ProjectElement element)
            : base(projectManager, element)
        {
            XSharpProjectNode XSharpProject = projectManager as XSharpProjectNode;
            if (XSharpProject != null)
            {
                base.Parent = XSharpProject.PackageReferenceContainerNode;
            }
            this.ExcludeNodeFromScc = true;
        }

        protected override NodeProperties CreatePropertiesObject()
        {
            return new XSharpPackageReferenceNodeProperties(this);

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
       
            if (cmdGroup == Microsoft.VisualStudio.Project.VsMenus.guidStandardCommandSet97)
            {
                if (cmd == (uint)VSConstants.VSStd97CmdID.Remove ||
                    cmd == (uint)VSConstants.VSStd97CmdID.Delete)
                {
                    // SUPPORTED is needed because otherwise the base class ignores our INVISIBLE flag !
                    result |=  QueryStatusResult.INVISIBLE | QueryStatusResult.SUPPORTED;
                    return VSConstants.S_OK;
                }
            }
            return base.QueryStatusOnNode(cmdGroup, cmd, pCmdText, ref result);
        }
       
    }
}
