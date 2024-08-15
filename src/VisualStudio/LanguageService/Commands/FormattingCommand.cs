
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using XSharp.LanguageService.Commands;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService
{
    internal class FormattingCommand : AbstractCommand
    {
        public static async Task InitializeAsync()
        {
            var cmd = new MyCommands();
            await cmd.InterceptAsync(VSConstants.VSStd2KCmdID.FORMATDOCUMENT, () => Execute(FormatDocument, CommandProgression.Continue));
            await cmd.InterceptAsync(VSConstants.VSStd2KCmdID.FORMATSELECTION, () => Execute(FormatSelection, CommandProgression.Continue));
        }

        private static void FormatDocument(DocumentView doc)
        {
            ;// Implementation is still in FormattingCommandHandler        }
        }
        private static void FormatSelection(DocumentView doc)
        {
            ;// Implementation is still in FormattingCommandHandler
        }

    }
}

