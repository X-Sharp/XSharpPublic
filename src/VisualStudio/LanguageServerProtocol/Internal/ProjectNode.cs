using System;
using XSharpModel;

namespace XSharp.LanguageServerProtocol.Internal
{
    internal sealed class ProjectNode : IXSharpProject
    {
        private readonly XParseOptions _parseOptions;
        private readonly string _rootNamespace;

        public ProjectNode(string projectPath, XParseOptions parseOptions, string rootNamespace)
        {
            Url = projectPath;
            _parseOptions = parseOptions;
            _rootNamespace = rootNamespace;
            DisplayName = System.IO.Path.GetFileNameWithoutExtension(projectPath);
        }

        public string DisplayName { get; }

        public string IntermediateOutputPath => string.Empty;

        public string OutputFile => string.Empty;

        public XParseOptions ParseOptions => _parseOptions;

        public bool PrefixClassesWithDefaultNamespace => false;

        public string RootNameSpace => _rootNamespace;

        public string Url { get; }

        public void AddFileNode(string fileName)
        {
        }

        public void DeleteFileNode(string fileName)
        {
        }

        public bool HasFileNode(string fileName)
        {
            return true;
        }
    }
}
