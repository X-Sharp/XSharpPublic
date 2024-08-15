using Microsoft.VisualStudio.Project;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.Project
{
    public class XSharpDependenciesContainerNode : XSharpReferenceContainerNode
    {
        public XSharpDependenciesContainerNode(ProjectNode project) : base(project)
        {
        }
        public override string Caption => "Dependencies";
    }
}
