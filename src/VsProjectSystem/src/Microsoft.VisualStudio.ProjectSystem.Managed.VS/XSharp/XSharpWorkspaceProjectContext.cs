// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.
#if XSHARP
using System;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Markup;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.LanguageServices.ProjectSystem;
using Microsoft.Build.Execution;
using Microsoft.VisualStudio.Shell.Interop;
using XSharpModel;
using Microsoft.VisualStudio.Shell;
using XSharp;


namespace Microsoft.VisualStudio.ProjectSystem.LanguageServices
{
    internal class XSharpWorkspaceProjectContext : IWorkspaceProjectContext, IXSharpProject
    {
        private string _name;
        public Guid Guid { get; set; }
        public Guid ProjectGuid { get; set; }
        public string? ProjectFilePath { get; set; }
        public bool LastDesignTimeBuildSucceeded { get; set; }
        public string? BinOutputPath { get; set; }
        private string ? _IntermediateOutputPath;
        private string? _RootNamespace;
        private string? _version;
        private string? _TargetFrameworkIdentifier;
        public bool IsPrimary { get; set; }
        public string DisplayName { get; set; }
        private IVsHierarchy? _vsHierarchy;
        private readonly Stack<BatchScope> _batchScopes = new();
        private XParseOptions _parseOptions = XParseOptions.Default;
        private XProject? _xproject;

        public string FrameworkVersion => _version ?? "";
        public string FrameworkIdentifier => _TargetFrameworkIdentifier ?? "";
        internal XSharpWorkspaceProjectContext(Guid projectGuid, string? contextId, string languageName, 
            EvaluationData evaluationData, object? hostObject, CancellationToken cancellationToken)
        {
            Guid = Guid.NewGuid();
            ProjectGuid = projectGuid;
            BinOutputPath = evaluationData.GetRequiredPropertyAbsolutePathValue(BuildPropertyNames.TargetPath);
            _name = evaluationData.GetPropertyValue(BuildPropertyNames.AssemblyName);
            EnforceSelf = evaluationData.GetPropertyValue(XSharpProjectFileConstants.EnforceSelf)?.ToLower() == "true";
            ProjectFilePath = evaluationData.GetPropertyValue(BuildPropertyNames.MSBuildProjectFullPath);
            _IntermediateOutputPath = evaluationData.GetPropertyValue(XSharpProjectFileConstants.IntermediateOutputPath);
            _RootNamespace = evaluationData.GetPropertyValue(XSharpProjectFileConstants.RootNamespace);
            _version = evaluationData.GetPropertyValue(XSharpProjectFileConstants.TargetFrameworkVersion);
            DisplayName = $"{_name} ({_version})";
            LastDesignTimeBuildSucceeded = true;
            IsPrimary = true;
            _vsHierarchy = hostObject as IVsHierarchy;
            Id = ProjectId.CreateNewId(DisplayName);
            _xproject = XSolution.FindProject(ProjectFilePath, _version);
            if (_xproject is null)
            {
                _xproject = new XProject(this, _version);
            }
            else
            {
                ;
            }
        }


        public ProjectId Id { get; set; }

        public string IntermediateOutputPath => IntermediateOutputPath ?? "";

        public string OutputFile => BinOutputPath ?? "";
        public XParseOptions ParseOptions => XParseOptions.Default;

        public bool PrefixClassesWithDefaultNamespace => _parseOptions.ImplicitNamespace;

        public string RootNameSpace => _RootNamespace ?? "";

        public string Url => ProjectFilePath ?? "";

        public XDialect Dialect => _parseOptions.Dialect;


        public bool EnforceSelf { get; set; }

        #region Add files
        public void AddAdditionalFile(string filePath, bool isInCurrentContext = true)
        {
            _xproject?.AddFile(filePath);
        }

        public void AddAdditionalFile(string filePath, IEnumerable<string> folderNames, bool isInCurrentContext = true)
        {
            ;
        }

        public void AddAnalyzerConfigFile(string filePath)
        {
            ;
        }

        public void AddAnalyzerReference(string referencePath)
        {
            ;
        }

        public void AddDynamicFile(string filePath, IEnumerable<string>? folderNames = null)
        {
            _xproject?.AddFile(filePath); 
        }

        public void AddMetadataReference(string referencePath, MetadataReferenceProperties properties)
        {
            _xproject?.AddAssemblyReference(referencePath);
        }

        public void AddProjectReference(IWorkspaceProjectContext project, MetadataReferenceProperties properties)
        {
            _xproject?.AddProjectReference(project.ProjectFilePath);
        }

        public void AddSourceFile(string filePath, bool isInCurrentContext = true, IEnumerable<string>? folderNames = null, SourceCodeKind sourceCodeKind = SourceCodeKind.Regular)
        {
            _xproject?.AddFile(filePath);
        }
        #endregion
        #region Scoping
        private readonly SemaphoreSlim _gate = new SemaphoreSlim(initialCount: 1);
        private int _activeBatchScopes = 0;

        public IAsyncDisposable CreateBatchScope()
        {
            using (_gate.DisposableWait())
            {
                _activeBatchScopes++;
                return new BatchScope(this);
            }

        }

        public async ValueTask<IAsyncDisposable> CreateBatchScopeAsync(CancellationToken cancellationToken)
        {
            using (await _gate.DisposableWaitAsync(cancellationToken).ConfigureAwait(false))
            {
                _activeBatchScopes++;
                return new BatchScope(this);
            }
        }

        public void Dispose()
        {
            if (_xproject != null)
            {
                _xproject.Close();
                _xproject = null;
            }
            return;
        }

        public ValueTask EndBatchAsync()
        {
            var scope = _batchScopes.Pop();
            return scope.DisposeAsync();
        }
        #endregion
        #region Remove files

        public void RemoveAdditionalFile(string filePath)
        {
            _xproject?.RemoveFile(filePath);
        }

        public void RemoveAnalyzerConfigFile(string filePath)
        {
            ;
        }

        public void RemoveAnalyzerReference(string referencePath)
        {
            ;
        }

        public void RemoveDynamicFile(string filePath)
        {
            _xproject?.RemoveFile(filePath);
        }

        public void RemoveMetadataReference(string referencePath)
        {
            _xproject?.RemoveAssemblyReference(referencePath); 
        }

        public void RemoveProjectReference(IWorkspaceProjectContext project)
        {
            _xproject?.AddProjectReference(project.ProjectFilePath);  
        }

        public void RemoveSourceFile(string filePath)
        {
            _xproject?.RemoveFile(filePath);
        }
        #endregion
        #region Other
        public void ReorderSourceFiles(IEnumerable<string> filePaths)
        {
            ;
        }

        public void SetOptions(string commandLineForOptions)
        {
            ;
        }

        public void SetOptions(ImmutableArray<string> arguments)
        {
            var tmp = new List<string>();
            foreach (var arg in arguments)
            {
                switch (arg[0])
                {
                    case '/':
                        tmp.Add(arg.Substring(1));
                        break;
                    case '-':
                        tmp.Add(arg.Substring(1));
                        break;
                    default:
                        tmp.Add(arg);
                        break;
                }
            }
            var options = XParseOptions.FromVsValues(tmp);
            _parseOptions = options;
            _RootNamespace = options.DefaultNamespace;  
            EnforceSelf = options.EnforceSelf;
        }

        public void SetProperty(string name, string value)
        {
            switch (name.ToLower())
            {
                case "rootnamespace":
                    _RootNamespace = value;
                    break;
                case "targetframeworkidentifier":
                    _TargetFrameworkIdentifier = value;
                    break;
            }
        }

        public void StartBatch()
        {
            ;
        }
        #endregion
        #region IXSharpProject
        public void AddFileNode(string fileName)
        {
            return;
        }

        public void ClearIntellisenseErrors(string file)
        {
            return;
        }

        public void DeleteFileNode(string fileName)
        {
            return;
        }

        public string DocumentGetText(string file, ref bool IsOpen)
        {
            return "";
        }

        public bool DocumentInsertLine(string fileName, int line, string text)
        {
            return true;
        }

        public bool DocumentSetText(string fileName, string text)
        {
            return true;
        }

        public object? FindProject(string sUrl)
        {
            return null;
        }

        public List<IXErrorPosition> GetIntellisenseErrorPos(string fileName)
        {
            return new List<IXErrorPosition>();
        }

        public bool HasFileNode(string fileName)
        {
            return false;
        }

        public void ShowIntellisenseErrors()
        {
            return;
        }

        public string SynchronizeKeywordCase(string code, string fileName)
        {
            return "";
        }

        public void RunInForeGroundThread(Action a)
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                a();
            });

            return;
        }
#endregion
    }
    internal sealed class BatchScope : IDisposable, IAsyncDisposable
    {
        private readonly XSharpWorkspaceProjectContext _project;

        /// <summary>
        /// Flag to control if this has already been disposed. Not a boolean only so it can be used with Interlocked.CompareExchange.
        /// </summary>
        private volatile int _disposed = 0;

        internal BatchScope(XSharpWorkspaceProjectContext visualStudioProject)
            => _project = visualStudioProject;

        public void Dispose()
        {
            if (Interlocked.CompareExchange(ref _disposed, 1, 0) == 0)
            {
               // _project.OnBatchScopeDisposedMaybeAsync(useAsync: false).VerifyCompleted();
            }
        }

        public async ValueTask DisposeAsync()
        {
            if (Interlocked.CompareExchange(ref _disposed, 1, 0) == 0)
            {
                //await _project.OnBatchScopeDisposedMaybeAsync(useAsync: true).ConfigureAwait(false);
            }
        }
    }
    internal static class SemaphoreSlimExtensions
    {
        public static SemaphoreDisposer DisposableWait(this SemaphoreSlim semaphore, CancellationToken cancellationToken = default)
        {
            semaphore.Wait(cancellationToken);
            return new SemaphoreDisposer(semaphore);
        }

        public static async ValueTask<SemaphoreDisposer> DisposableWaitAsync(this SemaphoreSlim semaphore, CancellationToken cancellationToken = default)
        {
            await semaphore.WaitAsync(cancellationToken).ConfigureAwait(false);
            return new SemaphoreDisposer(semaphore);
        }

        internal struct SemaphoreDisposer : IDisposable
        {
            private SemaphoreSlim? _semaphore;

            public SemaphoreDisposer(SemaphoreSlim semaphore)
            {
                _semaphore = semaphore;
            }

            public void Dispose()
            {
                // Officially, Dispose() being called more than once is allowable, but in this case
                // if that were to ever happen that means something is very, very wrong. Since it's an internal
                // type, better to be strict.

                // Nulling this out also means it's a bit easier to diagnose some async deadlocks; if you have an
                // async deadlock where a SemaphoreSlim is held but you're unsure why, as long all the users of the
                // SemaphoreSlim used the Disposable helpers, you can search memory and find the instance that
                // is pointing to the SemaphoreSlim that hasn't nulled out this field yet; in that case you know
                // that's holding the lock and can figure out who is holding that SemaphoreDisposer.
                var semaphoreToDispose = Interlocked.Exchange(ref _semaphore, null);

                if (semaphoreToDispose is null)
                {
                    throw new ObjectDisposedException($"Somehow a {nameof(SemaphoreDisposer)} is being disposed twice.");
                }

                semaphoreToDispose.Release();
            }
        }
    }
}
#endif
