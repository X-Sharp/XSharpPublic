using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Project;

namespace XSharp.Project
{
    public class XSharpFrameworkContainerNode : XSharpReferenceContainerNode
    {
        internal const string NodeVirtualName = "FrameworkNode";

        string _framework;
        public XSharpFrameworkContainerNode(ProjectNode project, string framework) : base(project)
        {
            _framework = framework.ToLower();
            this.VirtualNodeName = NodeVirtualName;
        }
        public override string Caption => _framework;
        protected override ImageMoniker GetIconMoniker(bool open)
        {
            return KnownMonikers.Library;
        }
    }
}
