//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.LanguageServices.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.VS;
using XSharpModel;
namespace Microsoft.VisualStudio.ProjectSystem.LanguageServices.Handlers
{
    /// <summary>
    ///     Handles changes to dynamic items, such as Razor CSHTML files.
    /// </summary>
    [Export(typeof(IWorkspaceUpdateHandler))]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    internal class XSharpItemHandler : IWorkspaceUpdateHandler, ISourceItemsHandler
    {
        private readonly UnconfiguredProject _project;
        private readonly HashSet<string> _paths = new(StringComparers.Paths);

        [ImportingConstructor]
        public XSharpItemHandler(UnconfiguredProject project)
        {
            _project = project;
        }

        public void Handle(IWorkspaceProjectContext context, IImmutableDictionary<string, IProjectChangeDescription> projectChanges, ContextState state, IManagedProjectDiagnosticOutputService logger)
        {
            foreach ((_, IProjectChangeDescription projectChange) in projectChanges)
            {
                if (!projectChange.Difference.AnyChanges)
                    continue;

                IProjectChangeDiff difference = projectChange.Difference.NormalizeRenames();

                foreach (string includePath in difference.RemovedItems)
                {
                    if (IsDynamicFile(includePath) || IsSourceFile(includePath))
                    {
                        RemoveFromContextIfPresent(context, includePath, logger);
                    }
                }

                foreach (string includePath in difference.AddedItems)
                {
                    if (IsDynamicFile(includePath) || IsSourceFile(includePath))
                    {
                        IImmutableDictionary<string, string> metadata = projectChange.After.Items.GetValueOrDefault(includePath, ImmutableStringDictionary<string>.EmptyOrdinal);

                        AddToContextIfNotPresent(context, includePath, metadata, logger);
                    }
                }

                // We Remove then Add changed items to pick up the Linked metadata
                foreach (string includePath in difference.ChangedItems)
                {
                    if (IsDynamicFile(includePath) || IsSourceFile(includePath))
                    {
                        IImmutableDictionary<string, string> metadata = projectChange.After.Items.GetValueOrDefault(includePath, ImmutableStringDictionary<string>.EmptyOrdinal);

                        RemoveFromContextIfPresent(context, includePath, logger);
                        AddToContextIfNotPresent(context, includePath, metadata, logger);
                    }
                }
            }
        }

        private void AddToContextIfNotPresent(IWorkspaceProjectContext context, string includePath, IImmutableDictionary<string, string> metadata, IManagedProjectDiagnosticOutputService logger)
        {
            string fullPath = _project.MakeRooted(includePath);

            if (!_paths.Contains(fullPath))
            {
                string[]? folderNames = FileItemServices.GetLogicalFolderNames(Path.GetDirectoryName(_project.FullPath), fullPath, metadata);

                if (IsSourceFile(includePath))
                {
                    logger.WriteLine("Adding source file '{0}'", fullPath);
                    context.AddSourceFile(fullPath, true, folderNames);
                }
                else
                {
                    logger.WriteLine("Adding dynamic file '{0}'", fullPath);
                    context.AddDynamicFile(fullPath, folderNames);
                }
                bool added = _paths.Add(fullPath);
                Assumes.True(added);
            }
        }

        private void RemoveFromContextIfPresent(IWorkspaceProjectContext context, string includePath, IManagedProjectDiagnosticOutputService logger)
        {
            string fullPath = _project.MakeRooted(includePath);

            if (_paths.Contains(fullPath))
            {
                if (IsSourceFile(includePath))
                {
                    logger.WriteLine("Removing source file '{0}'", fullPath);
                    context.RemoveSourceFile(fullPath);
                }
                else
                {
                    logger.WriteLine("Removing dynamic file '{0}'", fullPath);
                    context.RemoveDynamicFile(fullPath);
                }

                bool removed = _paths.Remove(fullPath);
                Assumes.True(removed);
            }
        }

        private static bool IsDynamicFile(string includePath)
        {
            var ft = XFileTypeHelpers.GetFileType(includePath);
            switch (ft)
            {
                case XFileType.VODBServer:
                case XFileType.VOFieldSpec:
                case XFileType.VOForm:
                case XFileType.VOIndex:
                case XFileType.VOMenu:
                case XFileType.VOOrder:
                case XFileType.VOReport:
                case XFileType.VOSqlTable:
                case XFileType.NativeResource:
                case XFileType.Header:
                    return true;
                default:
                    return false;
            }
        }
        private static bool IsSourceFile(string includePath)
        {
            var ft = XFileTypeHelpers.GetFileType(includePath);
            switch (ft)
            {
                case XFileType.SourceCode:
                    return true;
                default:
                    return false;
            }
        }
    }
}
