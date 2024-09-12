//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.LanguageServices.ProjectSystem;
using Microsoft.VisualStudio.Shell.Interop;
using XSharpModel;
using Microsoft.VisualStudio.Shell;
using XSharp;
using System.Diagnostics;
using Microsoft.VisualStudio.Threading;

namespace Microsoft.VisualStudio.ProjectSystem.LanguageServices
{
    [DebuggerDisplay("{XsDisplayName,nq}")]
    internal class XSharpWorkspaceProjectContext : IWorkspaceProjectContext, IXSharpProject
    {
        private readonly string _name;
        public Guid Guid { get; set; }
        public Guid ProjectGuid { get; set; }
        public string? ProjectFilePath { get; set; }
        public bool LastDesignTimeBuildSucceeded { get; set; }
        public string? BinOutputPath { get; set; }
        private string? _rootNamespace;
        private readonly string? _version;
        private string? _targetFrameworkIdentifier;
        public bool IsPrimary { get; set; }
        public string DisplayName { get; set; }
        public string XsDisplayName { get; set; }
        private readonly Stack<BatchScope> _batchScopes = new();
        private XParseOptions _parseOptions = XParseOptions.Default;
        private XProject? _xSharpProject;

        public string FrameworkVersion => _version ?? "";
        public string FrameworkIdentifier => _targetFrameworkIdentifier ?? "";

        static XSharpWorkspaceProjectContext()
        {
            var installDir = Environment.GetEnvironmentVariable("VSINSTALLDIR");
            if (string.IsNullOrEmpty(installDir))
            {
                installDir = Process.GetCurrentProcess().MainModule.FileName;
                var pos = installDir.IndexOf("Common7", StringComparison.OrdinalIgnoreCase);
                if (pos > 0)
                {
                    installDir = installDir.Substring(0, pos);
                }
                Environment.SetEnvironmentVariable("VSINSTALLDIR", installDir);
            }
        }

        internal XSharpWorkspaceProjectContext(Guid projectGuid, string? contextId, string languageName, 
            EvaluationData evaluationData, object? hostObject, CancellationToken cancellationToken)
        {
            Guid = Guid.NewGuid();
            ProjectGuid = projectGuid;
            BinOutputPath = evaluationData.GetRequiredPropertyAbsolutePathValue(BuildPropertyNames.TargetPath);
            _name = evaluationData.GetPropertyValue(BuildPropertyNames.AssemblyName);
            EnforceSelf = evaluationData.GetPropertyValue(XSharpProjectFileConstants.EnforceSelf)?.ToLower() == "true";
            ProjectFilePath = evaluationData.GetPropertyValue(BuildPropertyNames.MSBuildProjectFullPath);
            _rootNamespace = evaluationData.GetPropertyValue(XSharpProjectFileConstants.RootNamespace);
            _version = evaluationData.GetPropertyValue(XSharpProjectFileConstants.TargetFramework);
            XsDisplayName = $"{_name} ({_version})";
            DisplayName = _name;
            LastDesignTimeBuildSucceeded = true;
            IsPrimary = true;
            Id = ProjectId.CreateNewId(DisplayName);
            _xSharpProject = XSolution.FindProject(ProjectFilePath, _version);
            if (_xSharpProject is null)
            {
                _xSharpProject = new XProject(this, _version);
            }
            else
            {
                ;
            }
        }
        public ProjectId Id { get; set; }

        public string IntermediateOutputPath => IntermediateOutputPath ?? "";

        public string OutputFile => BinOutputPath ?? "";
        public XParseOptions ParseOptions => _parseOptions;

        public bool PrefixClassesWithDefaultNamespace => _parseOptions.ImplicitNamespace;

        public string RootNameSpace => _rootNamespace ?? "";

        public string Url => ProjectFilePath ?? "";

        public XDialect Dialect => _parseOptions.Dialect;
        public bool EnforceSelf { get; set; }

        #region Add files
        public void AddAdditionalFile(string filePath, bool isInCurrentContext = true)
        {
            _xSharpProject?.AddFile(filePath);
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
            _xSharpProject?.AddFile(filePath); 
        }

        public void AddMetadataReference(string referencePath, MetadataReferenceProperties properties)
        {
            _xSharpProject?.AddAssemblyReference(referencePath);
        }

        public void AddProjectReference(IWorkspaceProjectContext project, MetadataReferenceProperties properties)
        {
            _xSharpProject?.AddProjectReference(project.ProjectFilePath);
        }

        public void AddSourceFile(string filePath, bool isInCurrentContext = true, IEnumerable<string>? folderNames = null, SourceCodeKind sourceCodeKind = SourceCodeKind.Regular)
        {
            _xSharpProject?.AddFile(filePath);
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
            await TaskScheduler.Default;
            _activeBatchScopes++;
            return new BatchScope(this);
        }

        public void Dispose()
        {
            if (_xSharpProject is object)
            {
                _xSharpProject.Close();
                _xSharpProject = null;
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
            _xSharpProject?.RemoveFile(filePath);
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
            _xSharpProject?.RemoveFile(filePath);
        }

        public void RemoveMetadataReference(string referencePath)
        {
            _xSharpProject?.RemoveAssemblyReference(referencePath); 
        }

        public void RemoveProjectReference(IWorkspaceProjectContext project)
        {
            _xSharpProject?.AddProjectReference(project.ProjectFilePath);  
        }

        public void RemoveSourceFile(string filePath)
        {
            _xSharpProject?.RemoveFile(filePath);
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
                string cleanarg;
                switch (arg[0])
                {
                    case '/':
                    case '-':
                        cleanarg = arg.Substring(1);
                        break;
                    default:
                        cleanarg = arg;
                        break;
                }
                // we want /d: and not /define:
                if (cleanarg.ToLower().StartsWith("define:"))
                {
                    cleanarg = "d:" + cleanarg.Substring(7);
                }
                tmp.Add(cleanarg);
            }
            var options = XParseOptions.FromVsValues(tmp);
            _parseOptions = options;
            if (_xSharpProject is object)
            {
                _xSharpProject.ResetParseOptions(options);
            }
            _rootNamespace = options.DefaultNamespace;  
            EnforceSelf = options.EnforceSelf;
        }

        public void SetProperty(string name, string value)
        {
            switch (name.ToLower())
            {
                case "rootnamespace":
                    _rootNamespace = value;
                    break;
                case "targetframeworkidentifier":
                    _targetFrameworkIdentifier = value;
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
            if (! File.Exists(fileName))
            {
                File.WriteAllBytes(fileName, Array.Empty<byte>());
            }
        }
        public void DeleteFileNode(string fileName)
        {
            if (File.Exists(fileName))
            {
                File.Delete(fileName);
            }
        }

        public object? FindProject(string sUrl)
        {
            return null;
        }
        public bool HasFileNode(string fileName)
        {
            return false;
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
                await Task.Yield();
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
            await TaskScheduler.Default;
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

