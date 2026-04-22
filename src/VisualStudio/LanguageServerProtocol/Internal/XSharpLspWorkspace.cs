using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using XSharp.Settings;
using XSharpModel;

namespace XSharp.LanguageServerProtocol.Internal
{
    internal sealed class XSharpLspWorkspace : IDisposable
    {
        private const int MaxSyntheticFileNameLength = 100;
        private static readonly object s_solutionGate = new object();
        private static int s_activeWorkspaceCount;
        private readonly Dictionary<string, DocumentState> _documents = new Dictionary<string, DocumentState>(StringComparer.OrdinalIgnoreCase);
        private readonly XProject _project;
        private bool _disposed;

        public XSharpLspWorkspace(XSharpLspProjectOptions options)
        {
            ConfigureCodeModel();
            AcquireSolution(options);

            var parseOptions = XParseOptions.FromVsValues(options.CompilerOptionValues);
            var projectPath = options.GetProjectFilePath();
            Directory.CreateDirectory(Path.GetDirectoryName(projectPath) ?? Path.GetTempPath());
            var node = new ProjectNode(projectPath, parseOptions, options.RootNamespace ?? string.Empty);
            _project = new XProject(node, string.Empty, projectPath);

            foreach (var reference in options.AssemblyReferences)
            {
                _project.AddAssemblyReference(reference);
            }

            _project.ResolveReferences();
        }

        public XProject Project => _project;

        public IReadOnlyCollection<DocumentState> Documents => _documents.Values;

        public DocumentState OpenOrUpdate(string uriText, string text)
        {
            var uri = new Uri(uriText, UriKind.Absolute);
            var key = uri.AbsoluteUri;
            if (!_documents.TryGetValue(key, out var state))
            {
                var filePath = ResolveFilePath(uri);
                _project.AddFile(filePath);
                var file = _project.FindXFile(filePath) ?? XSolution.AddOrphan(filePath);
                file.Project = _project;
                file.Interactive = true;
                state = new DocumentState(uri, filePath, file, text);
                _documents[key] = state;
            }
            else
            {
                state.UpdateText(text);
            }

            state.File.ParseContents(state.Text);
            state.File.SaveToDatabase();
            _project.ResolveReferences();
            return state;
        }

        public void Close(string uriText)
        {
            if (_documents.TryGetValue(uriText, out var state))
            {
                _project.RemoveFile(state.FilePath);
                _documents.Remove(uriText);
            }
        }

        public bool TryGet(string uriText, out DocumentState state)
        {
            return _documents.TryGetValue(uriText, out state!);
        }

        public void Dispose()
        {
            if (_disposed)
            {
                return;
            }

            try
            {
                _project.Close();
            }
            finally
            {
                _disposed = true;
                ReleaseSolution();
            }
        }

        private static void AcquireSolution(XSharpLspProjectOptions options)
        {
            lock (s_solutionGate)
            {
                if (s_activeWorkspaceCount == int.MaxValue)
                {
                    throw new InvalidOperationException("Too many active XSharp LSP workspaces.");
                }

                s_activeWorkspaceCount += 1;
                try
                {
                    if (s_activeWorkspaceCount == 1)
                    {
                        EnsureSolutionOpen(options);
                    }
                }
                catch
                {
                    s_activeWorkspaceCount -= 1;
                    throw;
                }
            }
        }

        private static void ReleaseSolution()
        {
            lock (s_solutionGate)
            {
                if (s_activeWorkspaceCount == 0)
                {
                    Debug.Fail("XSharpLspWorkspace.ReleaseSolution called with no active workspace.");
                    return;
                }

                s_activeWorkspaceCount -= 1;
                if (s_activeWorkspaceCount == 0)
                {
                    XSolution.Close();
                }
            }
        }

        private static void ConfigureCodeModel()
        {
            XSettings.EnableOutputWindowLogging = false;
            XSettings.EnableFileLogging = false;
            XSettings.EnableDebugLogging = false;
            XSettings.DisableForeignProjectReferences = false;
            XSettings.DisableXSharpProjectReferences = false;
            XSettings.DisableAssemblyReferences = false;
        }

        private static void EnsureSolutionOpen(XSharpLspProjectOptions options)
        {
            var root = Path.Combine(Path.GetTempPath(), "XSharpLsp");
            Directory.CreateDirectory(root);
            var solutionPath = Path.Combine(root, "XSharpLsp.sln");
            if (!File.Exists(solutionPath))
            {
                File.WriteAllText(solutionPath, "Microsoft Visual Studio Solution File, Format Version 12.00\n");
            }

            XSolution.Open(solutionPath);
            if (XSolution.OrphanedFilesProject == null)
            {
                XSolution.CreateOrphanedFilesProject();
            }
        }

        private static string ResolveFilePath(Uri uri)
        {
            if (uri.IsFile)
            {
                return uri.LocalPath;
            }

            var root = Path.Combine(Path.GetTempPath(), "XSharpLsp", "Documents");
            Directory.CreateDirectory(root);
            var stem = Uri.EscapeDataString(uri.AbsoluteUri).Replace("%", string.Empty);
            if (stem.Length > MaxSyntheticFileNameLength)
            {
                stem = stem.Substring(stem.Length - MaxSyntheticFileNameLength);
            }
            return Path.Combine(root, stem + ".prg");
        }
    }
}
