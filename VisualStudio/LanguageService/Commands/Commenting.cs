
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using System;
using static LanguageService.SyntaxTree.Atn.SemanticContext;
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

        private static void Comment(DocumentView doc)
        {
            // Todo: add check to see if we have a block marked on a single line
            // in that case surround with /* */
            var snapshot = doc.TextBuffer.CurrentSnapshot;
            var sel = doc.TextView.Selection;
            var start = sel.Start.Position.Position;
            var end = sel.End.Position.Position;
            using (var editsession = doc.TextBuffer.CreateEdit())
            {
                do
                {
                    var line = snapshot.GetLineFromPosition(start);
                    editsession.Insert(line.Start.Position, CommentChars[0] + " ");
                    start = line.EndIncludingLineBreak.Position;
                    if (start == end)
                        break;
                } while (start < end);
                editsession.Apply();
            }
        }

        private static void Uncomment(DocumentView doc)
        {
            var snapshot = doc.TextBuffer.CurrentSnapshot;
            var sel = doc.TextView.Selection;
            var start = sel.Start.Position.Position;
            var end = sel.End.Position.Position;
            using (var editsession = doc.TextBuffer.CreateEdit())
            {
                do
                {
                    var line = snapshot.GetLineFromPosition(start);
                    var originalText = line.GetText();
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
                        var pos = line.Start.Position + leading.Length;
                        Span commentCharSpan = new Span(pos, lenToDelete);
                        editsession.Delete(commentCharSpan);
                    }
                    start = line.EndIncludingLineBreak.Position;
                    if (start == end)
                        break;
                } while (start < end);
                editsession.Apply();
            }
        }
    }

}

