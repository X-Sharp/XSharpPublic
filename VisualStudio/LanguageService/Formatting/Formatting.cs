//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//------------------------------------------------------------------------------

using LanguageService.CodeAnalysis.Text;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using System;
using XSharp.Parser;
using XSharpModel;
using static XSharp.Parser.VsParser;

namespace XSharp.LanguageService
{
    partial class XSharpFormattingCommandHandler
    {
        private bool WaitUntilBufferReady()
        {
            if (_buffer.EditInProgress)
            {
                var end = DateTime.Now.AddSeconds(5);
                while (_buffer.EditInProgress)
                {
                    System.Threading.Thread.Sleep(100);
                    if (DateTime.Now > end)
                    {
                        return false;
                    }
                }
            }
            return true;
        }
        private void FormatLine()
        {

            SnapshotPoint caret = this._textView.Caret.Position.BufferPosition;
            ITextSnapshotLine line = caret.GetContainingLine();
            // On what line are we ?
            int lineNumber = line.LineNumber;
            // we calculate the indent based on the previous line so we must be on the second line
            if (lineNumber > 0)
            {
                if (!WaitUntilBufferReady())
                    return;
                //var _ = _classifier.ClassifyWhenNeededAsync();
                var editSession = _buffer.CreateEdit();
                try
                {
                    switch ((EnvDTE.vsIndentStyle)Settings.IndentStyle)
                    {
                        case EnvDTE.vsIndentStyle.vsIndentStyleSmart:
                            if (lineNumber > 0)
                            {
                                _lineFormatter.FormatLine(editSession, line);
                            }
                            break;
                        case EnvDTE.vsIndentStyle.vsIndentStyleDefault:
                        case EnvDTE.vsIndentStyle.vsIndentStyleNone:
                            break;
                    }
                }
                finally
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

        private bool CanEdit
        {
            get
            {
                if (_buffer == null || _classifier == null)
                    return false;
                if (XSettings.DebuggerIsRunning)
                    return false;
                if (_buffer.CurrentSnapshot.Length == 0)
                {
                    return false;
                }
                if (!_buffer.CheckEditAccess())
                {
                    return false;
                }
                return true;

            }
        }
        /// <summary>
        /// Format document, evaluating line after line
        /// </summary>
        private void FormatDocument()
        {
            if (!CanEdit)
                return;
            WriteOutputMessage("FormatDocument() -->>");
            // Try to retrieve an already parsed list of Tags
            //
            if (!WaitUntilBufferReady())
                return;

            var endLine = _buffer.CurrentSnapshot.LineCount - 1;
            if (endLine < 1)
            {
                // Nothing to do
                return;
            }
            // Format the full text, with an first Indentation set to 0
            FormatSpan(0, endLine, 0);
            //
            WriteOutputMessage("FormatDocument() <<--");
        }

        private void FormatSpan(int startLine, int endLine, int startIndent)
        {
            var lines = _buffer.CurrentSnapshot.Lines;
            ITextEdit editSession = null;
            var settings = Settings;
            try
            {
                editSession = _buffer.CreateEdit();
                var document = _buffer.GetDocument();
                document.NeedsKeywords = true;
                ThreadHelper.JoinableTaskFactory.Run(async delegate
              {
                  await _classifier.ForceClassifyAsync();
              });

                document.NeedsKeywords = false;
                var formatter = new DocFormatter(document, settings);
                var expectedIndent = formatter.GetIndentSizes(lines, startLine, endLine, startIndent);

                // now process the lines
                foreach (var line in lines)
                {
                    var number = line.LineNumber;
                    if (number >= startLine && number <= endLine)
                    {
                        var indent = expectedIndent[number];
                        _lineFormatter.FormatLineIndent(editSession, line, indent * settings.IndentSize);
                        _lineFormatter.FormatLineCase(editSession, line);
                    }
                }
                if (editSession.HasEffectiveChanges)
                {
                    editSession.Apply();
                }
                else
                {
                    editSession.Cancel();
                }
            }
            catch
            {
                if (editSession != null && editSession.HasEffectiveChanges)
                {
                    editSession.Cancel();
                }
            }
        }

        /// <summary>
        /// Format the current selection
        /// </summary>
        private void FormatSelection()
        {
            if (!CanEdit)
                return;

            int startPosition = _textView.Selection.Start.Position.Position;
            int endPosition = _textView.Selection.End.Position.Position;
            //
            int startLine = _buffer.CurrentSnapshot.GetLineNumberFromPosition(startPosition);
            int endLine = _buffer.CurrentSnapshot.GetLineNumberFromPosition(endPosition);
            //
            FormatSpan(startLine, endLine, 0);
        }
    }


    public class ErrorIgnorer : IErrorListener
    {
        #region IErrorListener
        public void ReportError(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            ; //  _errors.Add(new XError(fileName, span, errorCode, message, args));
        }

        public void ReportWarning(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            ; //  _errors.Add(new XError(fileName, span, errorCode, message, args));
        }
        #endregion
    }


}
