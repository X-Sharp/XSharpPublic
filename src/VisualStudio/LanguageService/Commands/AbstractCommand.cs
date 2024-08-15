using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.LanguageService.Commands
{
    internal class AbstractCommand
    {
        protected static CommandProgression Execute(Action<DocumentView> action, CommandProgression result = CommandProgression.Stop )
        {
            return ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                DocumentView doc = await VS.Documents.GetActiveDocumentViewAsync();

                if (doc?.TextBuffer != null && doc.TextBuffer.ContentType.IsOfType(Constants.LanguageName))
                {
                    action(doc);
                    return result;
                }

                return CommandProgression.Continue;
            });
        }
    }
}
