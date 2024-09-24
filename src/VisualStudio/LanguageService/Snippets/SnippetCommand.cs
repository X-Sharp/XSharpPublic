
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;
using MSXML;
using System;
using System.Windows.Diagnostics;
using XSharp.LanguageService.Commands;
using XSharp.Settings;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService
{
    internal class SnippetCommand : AbstractCommand
    {
        private static string[] type1 = { "Expansion" };
        private static string[] type2 = { "SurroundsWith" };


        public static async Task InitializeAsync()
        {
            // We need to manually intercept the commenting command, because language services swallow these commands.
            await VS.Commands.InterceptAsync(VSConstants.VSStd2KCmdID.INSERTSNIPPET, () => Execute(InsertSnippet));
            await VS.Commands.InterceptAsync(VSConstants.VSStd2KCmdID.SURROUNDWITH, () => Execute(SurroundWith));
            await VS.Commands.InterceptAsync(VSConstants.VSStd2KCmdID.TAB, () => Execute(Tab));
            await VS.Commands.InterceptAsync(VSConstants.VSStd2KCmdID.BACKTAB, () => Execute(BackTab));
        }
        private static void InsertSnippet(DocumentView doc)
        {
            doc.TextBuffer.Properties.TryGetProperty<XSharpCompletionProvider>(typeof(XSharpCompletionProvider), out var textView);
            var res = InvokeDialog(doc, type1, "Insert Snippet");
        }
        private static void SurroundWith(DocumentView doc)
        {
            var res = InvokeDialog(doc, type2, "Surround with");
        }

        private static CommandProgression BackTab(DocumentView doc)
        {
            var xDocument = doc.TextBuffer.GetDocument();
            if (xDocument != null && xDocument.ExpansionSession != null)
            {
                xDocument.ExpansionSession.GoToPreviousExpansionField();
                return CommandProgression.Stop;
            }
            return CommandProgression.Continue;
        }
        private static CommandProgression Tab(DocumentView doc)
        {
            var xDocument = doc.TextBuffer.GetDocument();
            if (xDocument != null && xDocument.ExpansionSession != null)
            {
                var res = xDocument.ExpansionSession.GoToNextExpansionField(1);
                if (res != VSConstants.S_OK)
                {
                    xDocument.ExpansionSession.EndCurrentExpansion(fLeaveCaret: 1);
                    xDocument.ExpansionSession = null;
                }
                return CommandProgression.Stop;
            }
            return CommandProgression.Continue;

        }

        private static int InvokeDialog(DocumentView doc, string[] type, string caption)
        {
            var client = new ExpansionClient(doc);
            return client.ShowExpansionDialog(type, caption);
        }

    }
    internal class ExpansionClient : IVsExpansionClient
    {
        IVsExpansionManager m_exManager = null;
        readonly IVsTextView _textViewAdapter = null;
        readonly XDocument _doc;
        internal ExpansionClient(DocumentView doc)
        {
            var tv = doc.TextView;
            _doc = tv.TextBuffer.GetDocument();
            this._textViewAdapter = tv.ToIVsTextView();
            GetManager();
        }
        internal void GetManager()
        {
            var textManager = VS.GetRequiredService<SVsTextManager, IVsTextManager2>();
            textManager.GetExpansionManager(out m_exManager);

        }
        internal ExpansionClient(XDocument doc, IVsTextView adapter)
        {
            _doc = doc;
            this._textViewAdapter = adapter;
            GetManager();
        }
        internal int ShowExpansionDialog(string[] type, string caption)
        {
            if (_doc == null)
                return VSConstants.E_FAIL;


            _doc.ExpansionSession = null;
            return m_exManager.InvokeInsertionUI(
                   _textViewAdapter,
                   this,      //the expansion client
                   XSharpConstants.guidLanguageService,
                   type,       //snippet types
                   1,          //number of types (0 for all)
                   0,          //ignored if iCountTypes == 0
                   null,       //use all snippet kinds
                   0,          //use all snippet kinds
                   0,          //ignored if iCountTypes == 0
                   caption,    //the text to show in the prompt
                   "");        //No extra keys to end handler

        }

        #region IVSExpansionClient

        public bool TryGetSnippetFunctionInfo(
        string xmlFunctionText,
        out string snippetFunctionName,
        out string param)
        {
            if (string.IsNullOrEmpty(xmlFunctionText))
            {
                snippetFunctionName = null;
                param = null;
                return false;
            }
            var pos1 = xmlFunctionText.IndexOf('(');
            var pos2 = xmlFunctionText.IndexOf(')');
            if (pos1 == -1 || pos2 == -1 || pos1 > pos2)
            {
                snippetFunctionName = null;
                param = null;
                return false;
            }

            snippetFunctionName = xmlFunctionText.Substring(0, pos1);

            var paramStart = xmlFunctionText.IndexOf('(') + 1;
            var paramLength = xmlFunctionText.LastIndexOf(')') - xmlFunctionText.IndexOf('(') - 1;
            param = xmlFunctionText.Substring(paramStart, paramLength);
            return true;
        }

        public int GetExpansionFunction(IXMLDOMNode xmlFunctionNode, string bstrFieldName, out IVsExpansionFunction pFunc)
        {

            if (!TryGetSnippetFunctionInfo(xmlFunctionNode.text, out var snippetFunctionName, out var param))
            {
                pFunc = null;
                return VSConstants.E_INVALIDARG;
            }

            switch (snippetFunctionName)
            {
                case "SimpleTypeName":
                    pFunc = new SimpleTypeNameExpansionFunction(this, bstrFieldName, param);
                    return VSConstants.S_OK;
                case "InitProcType":
                    pFunc = new InitProcTypeExpansionFunction(this);
                    return VSConstants.S_OK;
                default:
                    pFunc = null;
                    return VSConstants.E_INVALIDARG;
            }
        }

        public int FormatSpan(IVsTextLines pBuffer, TextSpan[] ts)
        {
            return VSConstants.S_OK;
        }

        public int EndExpansion()
        {
            _doc.ExpansionSession = null;
            return VSConstants.S_OK;
        }

        public int IsValidType(IVsTextLines pBuffer, TextSpan[] ts, string[] rgTypes, int iCountTypes, out int pfIsValidType)
        {
            pfIsValidType = 1;
            return VSConstants.S_OK;
        }

        public int IsValidKind(IVsTextLines pBuffer, TextSpan[] ts, string bstrKind, out int pfIsValidKind)
        {
            pfIsValidKind = 1;
            return VSConstants.S_OK;
        }

        public int OnBeforeInsertion(IVsExpansionSession pSession)
        {
            _doc.ExpansionSession = pSession;
            return VSConstants.S_OK;
        }

        public int OnAfterInsertion(IVsExpansionSession pSession)
        {
            return VSConstants.S_OK;
        }

        public int PositionCaretForEditing(IVsTextLines pBuffer, TextSpan[] ts)
        {
            return VSConstants.S_OK;
        }

        public int OnItemChosen(string pszTitle, string pszPath)
        {
            InsertAnyExpansion(null, pszTitle, pszPath);
            return VSConstants.S_OK;
        }
        #endregion

        internal bool InsertAnyExpansion(string shortcut, string title, string path)
        {
            //first get the location of the caret, and set up a TextSpan
            //get the column number from  the IVsTextView, not the ITextView
            _textViewAdapter.GetCaretPos(out var startLine, out var endColumn);

            TextSpan addSpan = new TextSpan();
            addSpan.iStartIndex = endColumn;
            addSpan.iEndIndex = endColumn;
            addSpan.iStartLine = startLine;
            addSpan.iEndLine = startLine;

            if (shortcut != null) //get the expansion from the shortcut
            {
                //reset the TextSpan to the width of the shortcut, 
                //because we're going to replace the shortcut with the expansion
                addSpan.iStartIndex = addSpan.iEndIndex - shortcut.Length;

                m_exManager.GetExpansionByShortcut(
                    this,
                    XSharpConstants.guidLanguageService,
                    shortcut,
                    _textViewAdapter,
                    new TextSpan[] { addSpan },
                    0,
                    out path,
                    out title);

            }
            if (title != null && path != null)
            {
                _textViewAdapter.GetBuffer(out var textLines);
                IVsExpansion bufferExpansion = (IVsExpansion)textLines;

                if (bufferExpansion != null)
                {
                    int hr = bufferExpansion.InsertNamedExpansion(
                        title,
                        path,
                        addSpan,
                        this,
                        XSharpConstants.guidLanguageService,
                        1,
                       out var session);
                    if (VSConstants.S_OK == hr)
                    {
                        _doc.ExpansionSession = session;
                        return true;
                    }
                }
            }
            return false;
        }

    }
}

