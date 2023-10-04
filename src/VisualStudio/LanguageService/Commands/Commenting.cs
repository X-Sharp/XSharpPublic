
using Community.VisualStudio.Toolkit;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using System;
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
        private static (int, int) swapNumbers(int start, int end)
        {
            if (start > end)
            {
                return (end, start);
            }
            return (start, end);
        }

        private static void Comment(DocumentView doc)
        {
            // Todo: add check to see if we have a block marked on a single line
            // in that case surround with /* */
            var snapshot = doc.TextBuffer.CurrentSnapshot;
            var sel = doc.TextView.Selection;
            var (start, end) = swapNumbers(sel.Start.Position.Position, sel.End.Position.Position);
            using (var editsession = doc.TextBuffer.CreateEdit())
            {
                var startLine = snapshot.GetLineFromPosition(start);
                var endLine = snapshot.GetLineFromPosition(end);
                if (startLine.LineNumber == endLine.LineNumber && start != end)
                {
                    var text = snapshot.GetText(start, end - start);
                    text = "/*" + text + "*/";
                    var span = new SnapshotSpan(snapshot, start, end - start);
                    editsession.Replace(span, text);
                }
                else
                {
                    do
                    {
                        var line = snapshot.GetLineFromPosition(start);

                        editsession.Insert(line.Start.Position, CommentChars[0] + " ");
                        start = line.EndIncludingLineBreak.Position;
                        if (start == end)
                            break;
                    } while (start < end);
                }
                editsession.Apply();
            }
        }

        private static void Uncomment(DocumentView doc)
        {
            var snapshot = doc.TextBuffer.CurrentSnapshot;
            var sel = doc.TextView.Selection;
            var (start, end) = swapNumbers(sel.Start.Position.Position, sel.End.Position.Position);
            using (var editsession = doc.TextBuffer.CreateEdit())
            {
                bool done = false;
                // check to see if we are on a MultiLine commenttoken
                var xdoc = doc.TextBuffer.GetDocument();
                do
                {
                    var line = snapshot.GetLineFromPosition(start);
                    if (xdoc.LineState.Get(line.LineNumber, out var state))
                    {
                        if (state.HasFlag(LineFlags.MultiLineComments))
                        {
                            // remove multi line comments
                            do
                            {
                                var tokens = xdoc.GetTokensInSingleLine(line, true);
                                foreach (XSharpToken token in tokens)
                                {
                                    if (token.Type == XSharpLexer.ML_COMMENT)
                                    {
                                        if (token.StartIndex <= start && token.StopIndex >= end)
                                        {
                                            var text = token.Text;
                                            if (text.StartsWith("/*") && text.EndsWith("*/"))
                                            {
                                                text = text.Substring(2, text.Length - 4);
                                                editsession.Replace(token.StartIndex, token.Text.Length, text);
                                                done = true;
                                                break;
                                            }
                                        }
                                    }
                                }
                                if (!done)
                                {
                                    if (line.LineNumber > 0)
                                    {
                                        line = xdoc.GetLine(line.LineNumber - 1);
                                    }
                                    else
                                    {
                                        done = true;
                                    }
                                }
                            } while (!done);
                        }

                        if (!done)
                        {
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
                        }
                    }
                    start = line.EndIncludingLineBreak.Position;
                } while (!done && start < end);

                editsession.Apply();
            }
        }
    }

}

