using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text.Formatting;
using Microsoft.VisualStudio.Text;
using System;
using System.Linq;
using System.Collections.ObjectModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService
{
    internal class Commenting
    {
        readonly static string[] CommentChars = new[] { "//", "&&", "*" };

        public static async Task InitializeAsync()
        {
            // We need to manually intercept the commenting command, because language services swallow these commands.
            await VS.Commands.InterceptAsync(VSConstants.VSStd2KCmdID.COMMENT_BLOCK, () => Execute(Comment));
            await VS.Commands.InterceptAsync(VSConstants.VSStd2KCmdID.UNCOMMENT_BLOCK, () => Execute(Uncomment));
        }

        private static CommandProgression Execute(Action<DocumentView> action)
        {
            return ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                DocumentView doc = await VS.Documents.GetActiveDocumentViewAsync();

                if (doc?.TextBuffer != null && doc.TextBuffer.ContentType.IsOfType(Constants.LanguageName))
                {
                    action(doc);
                    return CommandProgression.Stop;
                }

                return CommandProgression.Continue;
            });
        }

        private static SnapshotSpan GetSelection(DocumentView doc)
        {
            SnapshotSpan spans = doc.TextView.Selection.SelectedSpans.First();
            if (spans.Length == 0)
            {
                var line = doc.TextView.Caret.ContainingTextViewLine;
                var start = line.Start;
                var end = line.End;
                spans = new SnapshotSpan(start, end);
            }
            return spans;
        }
        private static void Comment(DocumentView doc)
        {
            var spans = GetSelection(doc);
            if (spans.Length > 0)
            {
                Collection<ITextViewLine> lines = doc.TextView.TextViewLines.GetTextViewLinesIntersectingSpan(spans);
                var editsession = doc.TextBuffer.CreateEdit();
                foreach (ITextViewLine line in lines.Reverse())
                {
                    editsession.Insert(line.Start.Position, CommentChars[0] + " ");
                }
                editsession.Apply();
            }
        }

        private static void Uncomment(DocumentView doc)
        {
            var spans = GetSelection(doc);
            if (spans.Length > 0)
            {
                Collection<ITextViewLine> lines = doc.TextView.TextViewLines.GetTextViewLinesIntersectingSpan(spans);
                var editsession = doc.TextBuffer.CreateEdit();
                foreach (ITextViewLine line in lines.Reverse())
                {
                    var span = Span.FromBounds(line.Start, line.End);
                    var originalText = doc.TextBuffer.CurrentSnapshot.GetText(span);
                    var trimmedText = originalText.TrimStart(new char[] { ' ', '\t' });
                    string leading = "";
                    if (trimmedText.Length < originalText.Length)
                    {
                        leading = originalText.Substring(0, originalText.Length - trimmedText.Length);
                    }
                    int lenToDelete = 0;
                    foreach (var str in CommentChars)
                    {
                        if (trimmedText.StartsWith(str))
                        {
                            lenToDelete = str.Length;
                            // delete whitespace after comment chars?
                            if (trimmedText.Length > str.Length && char.IsWhiteSpace(trimmedText[lenToDelete]))
                                lenToDelete++;
                            break;
                        }
                    }
                    if (lenToDelete != 0)
                    {
                        var start = span.Start + leading.Length;
                        Span commentCharSpan = new Span(start, lenToDelete);
                        editsession.Delete(commentCharSpan);
                    }

                }
                editsession.Apply();
            }
        }
    }

}

