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

namespace Microsoft.VisualStudio.ProjectSystem.LanguageServices
{
    internal class XSharpWorkspaceProjectContext : IWorkspaceProjectContext
    {
        public string DisplayName { get; set; }
        public Guid Guid { get; set; }
        public Guid ProjectGuid { get; set; }
        public string? ProjectFilePath { get; set; }
        public bool LastDesignTimeBuildSucceeded { get; set; }
        public string? BinOutputPath { get; set; }
        public bool IsPrimary { get; set; }
        private IVsHierarchy? _vsHierarchy;
        private readonly Stack<BatchScope> _batchScopes = new();

        internal XSharpWorkspaceProjectContext(Guid projectGuid, string? contextId, string languageName, 
            EvaluationData evaluationData, object? hostObject, CancellationToken cancellationToken)
        {
            Guid = Guid.NewGuid();
            ProjectGuid = projectGuid;
            BinOutputPath = evaluationData.GetRequiredPropertyAbsolutePathValue(BuildPropertyNames.TargetPath);
            DisplayName = evaluationData.GetPropertyValue(BuildPropertyNames.AssemblyName);
            ProjectFilePath = evaluationData.GetRequiredPropertyAbsolutePathValue(BuildPropertyNames.MSBuildProjectFullPath);
            LastDesignTimeBuildSucceeded = true;
            IsPrimary = true;
            _vsHierarchy = hostObject as IVsHierarchy;
            Id = ProjectId.CreateNewId(DisplayName);
        }


        public ProjectId Id { get; set; }

        public void AddAdditionalFile(string filePath, bool isInCurrentContext = true)
        {
            ;
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
            ;
        }

        public void AddMetadataReference(string referencePath, MetadataReferenceProperties properties)
        {
            ;
        }

        public void AddProjectReference(IWorkspaceProjectContext project, MetadataReferenceProperties properties)
        {
            ;
        }

        public void AddSourceFile(string filePath, bool isInCurrentContext = true, IEnumerable<string>? folderNames = null, SourceCodeKind sourceCodeKind = SourceCodeKind.Regular)
        {
            ;
        }
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
            ;
        }

        public ValueTask EndBatchAsync()
        {
            var scope = _batchScopes.Pop();
            return scope.DisposeAsync();
        }

        public void RemoveAdditionalFile(string filePath)
        {
            ;
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
            ;
        }

        public void RemoveMetadataReference(string referencePath)
        {
            ; 
        }

        public void RemoveProjectReference(IWorkspaceProjectContext project)
        {
            ;
        }

        public void RemoveSourceFile(string filePath)
        {
            ;
        }

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
            ;
        }

        public void SetProperty(string name, string value)
        {
            ;
        }

        public void StartBatch()
        {
            ;
        }
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
