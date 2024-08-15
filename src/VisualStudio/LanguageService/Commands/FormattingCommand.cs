
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
            await cmd.InterceptAsync(VSConstants.VSStd2KCmdID.FORMATDOCUMENT, () => Execute(FormatDocument));
            await cmd.InterceptAsync(VSConstants.VSStd2KCmdID.FORMATSELECTION, () => Execute(FormatSelection));
        }

        private static void FormatDocument(DocumentView doc)
        {
            if (doc.TextView.Properties.TryGetProperty<XSharpFormattingCommandHandler>(typeof(XSharpFormattingCommandHandler), out var cmdHandler))
            {
                cmdHandler.FormatDocument();
            }

        }
        private static void FormatSelection(DocumentView doc)
        {
            if (doc.TextView.Properties.TryGetProperty<XSharpFormattingCommandHandler>(typeof(XSharpFormattingCommandHandler), out var cmdHandler))
            {
                cmdHandler.FormatSelection();
            }
        }

    }
}

