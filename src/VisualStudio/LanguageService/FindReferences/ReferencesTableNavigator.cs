//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.ComponentModel.Composition;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.TableControl;
using Microsoft.VisualStudio.Shell.TableManager;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;
using XSharp.Support;

namespace XSharp.LanguageService.FindReferences
{
    [Export(typeof(ITableControlEventProcessorProvider))]
    [DataSourceType(ReferencesTableDataSource.SourceType)]
    [DataSource(ReferencesTableDataSource.SourceType)]
    [Name("XSharpFindReferencesNavigator")]
    [Order(Before = "Default")]
    internal sealed class ReferencesTableNavigatorProvider : ITableControlEventProcessorProvider
    {
        public ITableControlEventProcessor GetAssociatedEventProcessor(IWpfTableControl tableControl)
        {
            Logger.Information("FindReferences navigator attached to a table control");
            return new ReferencesTableNavigator();
        }
    }

    /// <summary>Double click (or Enter) on a hit opens the file at the hit.</summary>
    internal sealed class ReferencesTableNavigator : TableControlEventProcessorBase
    {
        public override void PreprocessNavigate(ITableEntryHandle entry, TableEntryNavigateEventArgs e)
        {
            if (!entry.TryGetValue(StandardTableKeyNames.DocumentName, out string path) ||
                !entry.TryGetValue(StandardTableKeyNames.Line, out int line) ||
                !entry.TryGetValue(StandardTableKeyNames.Column, out int column))
            {
                Logger.Information("FindReferences navigate: entry without document/line/column");
                return;
            }
            Logger.Information($"FindReferences navigate to {path}:{line + 1}:{column + 1}");
            e.Handled = true;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                var view = await VS.Documents.OpenAsync(path);
                var textView = view?.TextView;
                if (textView == null)
                    return;
                var snapshot = textView.TextSnapshot;
                if (line >= snapshot.LineCount)
                    return;
                var textLine = snapshot.GetLineFromLineNumber(line);
                var position = System.Math.Min(textLine.Start.Position + column, textLine.End.Position);
                var point = new SnapshotPoint(snapshot, position);
                textView.Caret.MoveTo(point);
                textView.ViewScroller.EnsureSpanVisible(new SnapshotSpan(point, 0), EnsureSpanVisibleOptions.AlwaysCenter);
                (textView as Microsoft.VisualStudio.Text.Editor.IWpfTextView)?.VisualElement.Focus();
            });
        }
    }
}
