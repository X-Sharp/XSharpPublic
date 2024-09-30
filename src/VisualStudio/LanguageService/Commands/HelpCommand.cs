
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using System.Linq;
using XSharp.LanguageService.Commands;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService
{
    internal class HelpCommand : AbstractCommand
    {

        public static async Task InitializeAsync()
        {
            await VS.Commands.InterceptAsync(VSConstants.VSStd2KCmdID.HELPKEYWORD, () => Execute(Help));
            await VS.Commands.InterceptAsync(VSConstants.VSStd97CmdID.F1Help, () => Execute(Help));
        }
       
        private static void Help(DocumentView doc)
        {
            var textView = doc.TextView;
            var result = textView.GetSymbolUnderCursor(out _, out _, out var tokens); ;
            if (result.Count == 0 && tokens.Count > 0)
            {
                var token = tokens[0].Text;
                HelpViewer.ShowHelp(textView, token);
            }
            if (result.Count > 0)
                HelpViewer.ShowHelp(textView, result.First());
            else
                HelpViewer.ShowHelp(textView, "");
        }

    }

}

