using Microsoft.Build.Tasks;
using Microsoft.Build.Utilities;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.Build
{

    public abstract class TaskExtension : Microsoft.Build.Utilities.Task
    {
        private TaskLoggingHelperExtension _logExtension;

        public new TaskLoggingHelper Log => _logExtension;

        internal TaskExtension()
            : base(Microsoft.Build.Shared.AssemblyResources.PrimaryResources, "MSBuild.")
        {
            _logExtension = new TaskLoggingHelperExtension(this, Microsoft.Build.Shared.AssemblyResources.PrimaryResources, Microsoft.Build.Shared.AssemblyResources.SharedResources, "MSBuild.");
        }
    }
}
