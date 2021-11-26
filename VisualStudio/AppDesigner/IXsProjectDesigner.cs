using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.Versioning;

namespace XSharp.Project
{
    public interface IXsProjectDesigner
    {
        void SetProjectProperty(string propertyName, string propertyValue);
        void SetProjectProperty(string propertyName, string propertyValue, string condition);
        string GetProjectProperty(string propertyName, bool resetCache, bool unevaluated = false);
        string ProjectFolder { get; }
        string ProjectFile { get; }
        string RootNameSpace { get; set; }
        string OutputFile { get; set; }
        FrameworkName TargetFrameworkMoniker { get; set; }
        void RemoveProjectProperty(string name);
        Guid GetCATIDForType(System.Type type );
        object GetService(System.Type type);
        IServiceProvider Site { get; }

    }
}
