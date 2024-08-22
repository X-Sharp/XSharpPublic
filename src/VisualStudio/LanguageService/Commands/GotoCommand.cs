
using Community.VisualStudio.Toolkit;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Windows.Forms;
using XSharp.LanguageService.Commands;
using XSharpModel;
using static LanguageService.SyntaxTree.Atn.SemanticContext;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService
{
    internal class GotoCommand : AbstractCommand
    {
        public static async Task InitializeAsync()
        {
            await VS.Commands.InterceptAsync(VSConstants.VSStd2KCmdID.GOTOBRACE, () => Execute(GotoBraceNormal));
            await VS.Commands.InterceptAsync(VSConstants.VSStd2KCmdID.GOTOBRACE_EXT, () => Execute(GotoBraceExt));
            await VS.Commands.InterceptAsync(VSConstants.VSStd97CmdID.GotoDefn, () => Execute(GotoDefinition));
            await VS.Commands.InterceptAsync(VSConstants.VSStd2KCmdID.ECMD_NEXTMETHOD, () => Execute(GotoNextMethod));
            await VS.Commands.InterceptAsync(VSConstants.VSStd2KCmdID.ECMD_PREVMETHOD, () => Execute(GotoPreviousMethod));
        }

        private static void GotoBraceExt(DocumentView doc)
        {
            GotoBraceWorker(doc, true);
        }
        private static void GotoBraceNormal(DocumentView doc)
        {
            GotoBraceWorker(doc, false);
        }

        private static void GotoBraceWorker(DocumentView doc, bool ext)
        {
            var xdoc = doc.TextBuffer.GetDocument();
            var currentChar = doc.TextView.Caret.Position.BufferPosition;
            int currentLine = currentChar.GetContainingLine().LineNumber;
            int tokenLine = currentLine + 1;// our tokens have 1 based line numbers
            var blocks = xdoc.Blocks.Where(b => b.Token.Line <= tokenLine && b.Last.Token.Line >= tokenLine);
            var foundSpans = KeywordMatchingTagger.GetBlockSpans(blocks, currentChar, doc.TextBuffer);
            if (foundSpans == null || foundSpans.Count == 0)
            {
                var ents = xdoc.Entities.Where(e => e.Range.StartLine <= currentLine && e.Range.EndLine >= currentLine);
                foundSpans = KeywordMatchingTagger.GetEntitySpans(ents, currentChar, doc.TextBuffer);
            }
            if (foundSpans != null)
            {
                // there can be multiple blocks  IF ELSEIF ELSE ENDIF
                // when the cursor is on the first then we want to go to the last
                // when the cursor is on a middle then we want to go to the next
                if (foundSpans.Count >= 2)
                {
                    SnapshotPoint end = currentChar;
                    SnapshotPoint start = currentChar;
                    SnapshotPoint target = currentChar;
                    bool reversed = false;
                    start = target = foundSpans[0].Start;
                    end = foundSpans[foundSpans.Count - 1].End;
                    if (foundSpans[0].Contains(currentChar))
                    {
                        target = end;
                    }
                    else if (foundSpans[foundSpans.Count - 1].Contains(currentChar))
                    {
                        target = start;
                        reversed = true;
                    }
                    else if (foundSpans.Count > 2)
                    {
                        for (int i = 0; i < foundSpans.Count - 1; i++)
                        {
                            if (foundSpans[i].Contains(currentChar))
                            {
                                if (foundSpans[i].GetText().ToLower() == "end" && i >= foundSpans.Count - 2)
                                {
                                    // end should be the word after the END keyword. This is already selected
                                    break;

                                }
                                start = foundSpans[i].Start;
                                target = end = foundSpans[i + 1].End;
                                break;
                            }
                        }
                    }
                    GotoBraceMoveTo(doc, ext, start, end, target, reversed);
                }
            }

            ;
            if (GetTaggerType(doc) != null)
            {
                object property = null;
                doc.TextView.Properties.TryGetProperty(TaggerType, out property);
                if (property != null)
                {
                    var span = new SnapshotSpan(doc.TextBuffer.CurrentSnapshot, currentChar.Position, 1);
                    NormalizedSnapshotSpanCollection spans = new NormalizedSnapshotSpanCollection(span);
                    var mi = TaggerType.GetMethods().Where(m => m.Name == "GetTags").FirstOrDefault();
                    if (mi != null)
                    {
                        var tags = mi.Invoke(property, new object[] { spans }) as IEnumerable<ITagSpan<TextMarkerTag>>;
                        if (tags != null)
                        {
                            var hasMatched = false;
                            foreach (var tag in tags)
                            {
                                var s = tag.Span;
                                if (s.Contains(currentChar))
                                {
                                    hasMatched = true;
                                    continue;
                                }
                                if (s.IntersectsWith(span) && !hasMatched)
                                    continue;
                                SnapshotPoint start, end, target;
                                bool reversed = false;
                                if (currentChar.Position < s.Start.Position)
                                {
                                    // cursor on or before opening paren / curly
                                    start = currentChar; ;
                                    end = s.End;
                                    target = end;
                                }
                                else
                                {
                                    // cursor on or after closing paren / curly
                                    end = currentChar;
                                    start = s.Start;
                                    target = start;
                                    reversed = true;
                                }
                                GotoBraceMoveTo(doc, ext, start, end, target, reversed);
                                break;
                            }
                        }
                    }
                }
            }
            return;
        }
        static System.Type _taggerType = null;

        public static Type TaggerType => _taggerType; 

        private static Type GetTaggerType(DocumentView doc)
        {
            if (_taggerType == null)
            {
                foreach (var prop in doc.TextView.Properties.PropertyList)
                {
                    if (prop.Key is Type type)
                    {
                        if (type.Name == "BraceMatchingTagger")
                        {
                            _taggerType = type;
                            break;
                        }
                    }
                }
            }
            return _taggerType;
        }

        private static void GotoBraceMoveTo(DocumentView doc, bool ext, SnapshotPoint start, SnapshotPoint end, SnapshotPoint target, bool reversed)
        {
            if (ext)
            {
                var selection = new SnapshotSpan(start, end);
                doc.TextView.Selection.Select(selection, reversed);
            }
            doc.TextView.Caret.MoveTo(target);
            doc.TextView.ViewScroller.EnsureSpanVisible(new SnapshotSpan(target, 1));
        }

        private static void GotoDefinition(DocumentView doc)
        {
            XSharpGotoDefinition.GotoDefn(doc.TextView);
        }

        private static int findCurrentEntity(DocumentView doc)
        {
            var xdoc = doc.TextBuffer.GetDocument();
            var currentEntity = xdoc.GetCurrentEntity(doc.TextView);
            if (currentEntity == null)
                return -1;
            return xdoc.Entities.IndexOf(currentEntity);
        }

        private static void GotoNextMethod(DocumentView doc)
        {
            var xdoc = doc.TextBuffer.GetDocument();
            var pos = findCurrentEntity(doc);
            var entities = xdoc.Entities;
            if (pos >= 0 && pos < entities.Count - 1)
            {
                GotoEntity(doc, entities[pos + 1]);
            }

        }
        private static void GotoEntity(DocumentView doc,XSourceEntity entity)
        {
            var line = entity.Range.StartLine;
            var lineSpan = doc.TextView.TextSnapshot.GetLineFromLineNumber(line).Extent;
            doc.TextView.Caret.MoveTo(lineSpan.Start);
            doc.TextView.ViewScroller.EnsureSpanVisible(lineSpan);
        }

        private static void GotoPreviousMethod(DocumentView doc)
        {
            var xdoc = doc.TextBuffer.GetDocument();
            var pos = findCurrentEntity(doc);
            if (pos > 0 )
            {
                var entities = xdoc.Entities;
                GotoEntity(doc, entities[pos - 1]);
            }
        }
    }

}

