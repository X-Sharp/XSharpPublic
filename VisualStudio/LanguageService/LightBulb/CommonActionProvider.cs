using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using System;
using XSharpModel;

namespace XSharp.LanguageService.Editors.LightBulb
{
    internal class CommonActionProvider
    {

    }
    internal class CommonActionsSource
    {
        protected ITextBuffer m_textBuffer;
        protected ITextView m_textView;
        protected XFile _xfile;

        internal CommonActionsSource(ITextView textView, ITextBuffer textBuffer)
        {
            this.m_textView = textView;
            this.m_textBuffer = textBuffer;

        }
        public void Dispose()
        {
        }

        public bool TryGetTelemetryId(out Guid telemetryId)
        {
            // This is a sample provider and doesn't participate in LightBulb telemetry  
            telemetryId = Guid.Empty;
            return false;
        }
        /// <summary>
        /// Gets the file object from the buffer
        /// </summary>
        /// <returns></returns>
        internal bool GetFile()
        {
            _xfile = m_textBuffer.GetFile();
            if (_xfile == null)
                return false;
            return true;
        }
        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableLogging)
            {
                XSettings.LogMessage("XSharp.LightBulb:" + strMessage);
            }
        }


        /// <summary>
        /// Based on the Caret line position, check if this is a continuig line
        /// </summary>
        /// <returns></returns>
        protected int SearchRealStartLine()
        {
            //
            var xDocument = m_textBuffer.GetDocument();
            if (xDocument == null)
            {
                return -1;
            }
            var linesState = xDocument.LineState;
            SnapshotPoint caret = this.m_textView.Caret.Position.BufferPosition;
            ITextSnapshotLine line = caret.GetContainingLine();
            //
            var lineNumber = line.LineNumber;
            var lineState = linesState.Get(lineNumber);
            // Search the first line
            while (lineState == LineFlags.Continued)
            {
                // Move back
                lineNumber--;
                lineState = linesState.Get(lineNumber);
            }
            return lineNumber;
        }

    }
}
