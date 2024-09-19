using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;

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
        protected static CommandProgression Execute(Func<DocumentView, CommandProgression> action)
        {
            return ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                DocumentView doc = await VS.Documents.GetActiveDocumentViewAsync();

                if (doc?.TextBuffer != null && doc.TextBuffer.ContentType.IsOfType(Constants.LanguageName))
                {
                    return action(doc);
                }

                return CommandProgression.Continue;
            });
        }
        public static void InitializeCommands()
        {
            System.Threading.Tasks.Task result = null;
            foreach (var type in typeof(AbstractCommand).Assembly.GetTypes())
            {
                if (type.IsSubclassOf(typeof(AbstractCommand)))
                {
                    var m = type.GetMethod("InitializeAsync");
                    if (m != null)
                    {
                        result = (System.Threading.Tasks.Task)m.Invoke(null, null);
                    }
                }
            }
            return;
        }
        protected static void DoNothing(DocumentView view)
        {

        }
    }

}
