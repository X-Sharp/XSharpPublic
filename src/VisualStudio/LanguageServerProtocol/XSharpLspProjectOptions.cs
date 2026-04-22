using System;
using System.Collections.Generic;

namespace XSharp.LanguageServerProtocol
{
    public sealed class XSharpLspProjectOptions
    {
        public string ProjectName { get; set; } = "XSharp.Lsp.Project";
        public string? ProjectFilePath { get; set; }
        public string? RootNamespace { get; set; }
        public IList<string> ParseOptions { get; } = new List<string> { "dialect:Core" };
        public IList<string> AssemblyReferences { get; } = new List<string>();

        internal string GetProjectFilePath()
        {
            if (!string.IsNullOrWhiteSpace(ProjectFilePath))
            {
                return ProjectFilePath!;
            }

            var safe = ProjectName.Replace(' ', '.');
            return System.IO.Path.Combine(System.IO.Path.GetTempPath(), "XSharpLsp", safe + ".xsproj");
        }
    }
}
