
using Community.VisualStudio.Toolkit;

using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;

using Microsoft.VisualStudio.Text;

using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService.Commands
{
    internal class CommentCommand : AbstractCommand
    {

        readonly static string[] CommentChars = new[] { "//", "&&", "*" };

        public static async Task InitializeAsync()
        {
            var cmdToggleBlock = await VS.Commands.FindCommandAsync("Edit.ToggleBlockComment");
            var cmdToggleLine = await VS.Commands.FindCommandAsync("Edit.ToggleLineComment");
            var cmdCommentSelection = await VS.Commands.FindCommandAsync("Edit.CommentSelection");
            var cmdUnCommentSelection = await VS.Commands.FindCommandAsync("Edit.UncommentSelection");

            // We need to manually intercept the commenting command, because language services swallow these commands.
            await VS.Commands.InterceptAsync(cmdCommentSelection, () => Execute(CommentSelection));
            await VS.Commands.InterceptAsync(cmdUnCommentSelection, () => Execute(UncommentSelection));
            await VS.Commands.InterceptAsync(cmdToggleBlock, () => Execute(ToggleBlockComment));
            await VS.Commands.InterceptAsync(cmdToggleLine, () => Execute(ToggleLineComment));
        }

        private static (int, int) SortLowHigh(int start, int end)
        {
            if (start > end)
            {
                return (end, start);
            }
            return (start, end);
        }


        private static void UnCommentBlock(DocumentView doc, ITextEdit editsession, IToken token)
        {
            var text = token.Text;
            if (text.StartsWith("/*") && text.EndsWith("*/"))
            {
                text = text.Substring(2, text.Length - 4);
                editsession.Replace(token.StartIndex, token.Text.Length, text);
            }
        }
        private static void CommentBlock(DocumentView doc, ITextEdit editsession, SnapshotSpan selection)
        {
            var text = selection.GetText();
            text = "/*" + text + "*/";
            editsession.Replace(selection, text);
        }
        private static void CommentLine(DocumentView doc, ITextEdit editsession, SnapshotSpan selection)
        {
            var text = selection.GetText();
            text = "// " + text;
            editsession.Replace(selection, text);
        }
        private static void UnCommentLine(DocumentView doc, ITextEdit editsession, SnapshotSpan selection)
        {
            var text = selection.GetText();
            var ws = text.Substring(0, text.Length - text.TrimStart().Length);
            text = text.TrimStart();
            text = text.Substring(2).TrimStart();
            text = ws + text;
            editsession.Replace(selection, text);

        }

        private static void ToggleBlockComment(DocumentView doc)
        {
            var snapshot = doc.TextBuffer.CurrentSnapshot;
            var sel = doc.TextView.Selection;
            using (var editsession = doc.TextBuffer.CreateEdit())
            {
                foreach (var selection in sel.SelectedSpans)
                {
                    var (start, end) = SortLowHigh(selection.Start.Position, selection.End.Position);
                    if (OnMultiLineComment(doc, snapshot, start, out var token))
                    {
                        UnCommentBlock(doc, editsession, token);
                    }
                    else
                    {
                        CommentBlock(doc, editsession, selection);
                    }
                }
                editsession.Apply();
            }

        }

        private static bool OnMultiLineComment(DocumentView doc, ITextSnapshot snapshot, int start, out IToken token)
        {
            var xdoc = doc.TextBuffer.GetDocument();
            token = null;
            var line = snapshot.GetLineFromPosition(start);
            if (xdoc.LineState.Get(line.LineNumber, out var state))
            {
                if (state.HasFlag(LineFlags.MultiLineComments))
                {
                    do
                    {
                        // Find token that starts the ML comment
                        var tokens = xdoc.GetTokensInSingleLine(line, true);
                        foreach (XSharpToken t in tokens)
                        {
                            if (t.Type == XSharpLexer.ML_COMMENT)
                            {
                                token = t;
                                return true;
                            }
                        }
                        if (line.LineNumber > 0)
                        {
                            line = snapshot.GetLineFromLineNumber(line.LineNumber - 1);
                        }
                        else
                        {
                            break;
                        }
                    } while (true);
                }
            }
            return false;
        }

        private static void ToggleLineComment(DocumentView doc)
        {
            var snapshot = doc.TextBuffer.CurrentSnapshot;
            var sel = doc.TextView.Selection;
            using (var editsession = doc.TextBuffer.CreateEdit())
            {
                foreach (var selection in sel.SelectedSpans)
                {
                    bool done = false;
                    var (start, end) = SortLowHigh(selection.Start.Position, selection.End.Position);
                    do
                    {
                        var line = snapshot.GetLineFromPosition(start);
                        var span = line.Extent;
                        var text = line.GetText();
                        if (text.TrimStart().StartsWith("//"))
                        {
                            UnCommentLine(doc, editsession, span);
                        }
                        else
                        {
                            CommentLine(doc, editsession, span);
                        }
                        start = line.EndIncludingLineBreak.Position;
                    } while (!done && start < end);
                }
                editsession.Apply();
            }
        }

        private static void CommentSelection(DocumentView doc)
        {
            // Todo: add check to see if we have a block marked on a single line
            // in that case surround with /* */
            var snapshot = doc.TextBuffer.CurrentSnapshot;
            var sel = doc.TextView.Selection;
            using (var editsession = doc.TextBuffer.CreateEdit())
            {
                foreach (var selection in sel.SelectedSpans)
                {
                    var (start, end) = SortLowHigh(selection.Start.Position, selection.End.Position);
                    bool singleLine = start != end && selection.Start.GetContainingLine().LineNumber == selection.End.GetContainingLine().LineNumber;
                    if (singleLine)
                    {
                        CommentBlock(doc, editsession, selection);
                    }
                    else
                    {
                        // selection may have multiple lines
                        do
                        {
                            var line = snapshot.GetLineFromPosition(start);
                            CommentLine(doc, editsession, line.Extent);
                            start = line.EndIncludingLineBreak.Position;
                        } while (start < end);
                    }
                }
                editsession.Apply();
            }
        }
        private static void UncommentSelection(DocumentView doc)
        {
            var snapshot = doc.TextBuffer.CurrentSnapshot;
            var sel = doc.TextView.Selection;
            using (var editsession = doc.TextBuffer.CreateEdit())
            {
                foreach (var selection in sel.SelectedSpans)
                {
                    var (start, end) = SortLowHigh(selection.Start.Position, selection.End.Position);
                    if (OnMultiLineComment(doc, snapshot, start, out var token))
                    {
                        UnCommentBlock(doc, editsession, token);
                    }
                    else
                    {
                        // selection may have multiple lines
                        do
                        {
                            var line = snapshot.GetLineFromPosition(start);
                            UnCommentLine(doc, editsession, line.Extent);
                            start = line.EndIncludingLineBreak.Position;
                            if (start == end)
                                break;
                        } while (start <= end);

                    }
                }
                editsession.Apply();
            }
        }
    }

}

