using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using XSharpModel;

namespace XSharp.Project
{
    class SolutionEvents : SolutionListener
    {
        public SolutionEvents(IServiceProvider serviceProviderParameter) : base(serviceProviderParameter)
        {
        }

        public override int OnAfterOpenSolution(object pUnkReserved, int fNewSolution)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            // refresh the references for all projects
            foreach (var xnode in XSharpProjectNode.AllProjects)
            {
                xnode.LoadPackageReferences();
                xnode.UpdateReferencesInProjectModel();
            }

            return VSConstants.S_OK;
        }
    }
}
