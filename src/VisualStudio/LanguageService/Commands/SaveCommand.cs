
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using System;
using XSharp.LanguageService.Commands;
using XSharp.Settings;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService
{
    internal class SaveCommand : AbstractCommand
    {
        public static async Task InitializeAsync()
        {
            await VS.Commands.InterceptAsync(VSConstants.VSStd97CmdID.Save, () =>  Execute(Save,CommandProgression.Continue));
            await VS.Commands.InterceptAsync(VSConstants.VSStd97CmdID.SaveAs, () => Execute(Save, CommandProgression.Continue));
            await VS.Commands.InterceptAsync(VSConstants.VSStd97CmdID.SaveProjectItem, () => Execute(Save, CommandProgression.Continue));
        }

        private static void Save(DocumentView doc)
        {
            var Settings = doc.TextBuffer.GetSettings();
            if (Settings != null)
            {
                if (Settings.InsertFinalNewline || Settings.TrimTrailingWhiteSpace)
                {
                    adjustWhiteSpace(Settings, doc);
                }
            }
        }
        private static void adjustWhiteSpace(SourceCodeEditorSettings settings, DocumentView doc)
        {
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                formatbeforeSaving(settings, doc);
            });
        }
        private static void formatbeforeSaving(SourceCodeEditorSettings settings, DocumentView doc)
        {
            var buffer = doc.TextBuffer;
            using (var editSession = buffer.CreateEdit())
            {
                try
                {
                    var snapshot = editSession.Snapshot;
                    if (settings.InsertFinalNewline)
                    {
                        var text = snapshot.GetText();
                        if (!text.EndsWith(Environment.NewLine))
                        {
                            var line = snapshot.GetLineFromLineNumber(snapshot.LineCount - 1);
                            editSession.Insert(line.End.Position, Environment.NewLine);
                        }

                    }
                    if (settings.TrimTrailingWhiteSpace)
                    {
                        foreach (var line in snapshot.Lines)
                        {
                            var text = line.GetText();
                            if (text.Length > 0)
                            {
                                var last = text[text.Length - 1];
                                if (last == ' ' || last == '\t')
                                {
                                    editSession.Replace(line.Start.Position, line.Length, text.TrimEnd());
                                }
                            }
                        }
                    }
                }
                catch (Exception)
                {
                    editSession.Cancel();
                }
                finally
                {
                    ApplyChanges(editSession);
                }
            }
        }

        private static void ApplyChanges(ITextEdit editSession)
        {
            if (editSession.HasEffectiveChanges)
            {
                editSession.Apply();
            }
            else
            {
                editSession.Cancel();
            }
        }
    }

}



