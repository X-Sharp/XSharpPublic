﻿using Microsoft.VisualStudio.Language.Intellisense;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Imaging.Interop;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using Microsoft.VisualStudio.Text;
using System.Threading;
using XSharpModel;
using System.Reflection;
using Microsoft.VisualStudio.Text.Editor;
using XSharp.LanguageService;
using System.Collections.Immutable;
using Microsoft.VisualStudio.Text.Adornments;
using System.Text;
using System.Linq;

namespace XSharp.Project.Editors.LightBulb
{
    internal class PropertySuggestedAction : ISuggestedAction
    {
        private readonly ITextSnapshot m_snapshot;

        public PropertySuggestedAction(ITextSnapshot snapshot)
        {
            m_snapshot = snapshot;
        }

        private readonly ITextView m_textView;
        private readonly XSourceMemberSymbol _fieldEntity;
        private readonly XSharpModel.TextRange _range;
        private readonly bool _full;
        private readonly string _generateName;
        private readonly bool _renameField;

        public PropertySuggestedAction(ITextView textView, ITextBuffer m_textBuffer, XSourceMemberSymbol fieldToPropertize, XSharpModel.TextRange range, bool fullProperty, string genName, bool renField)
        {
            m_snapshot = m_textBuffer.CurrentSnapshot;
            m_textView = textView;
            //
            _fieldEntity = fieldToPropertize;
            _range = range;
            _full = fullProperty;
            _renameField = renField;
            _generateName = genName;
        }

        public Task<object> GetPreviewAsync(CancellationToken cancellationToken)
        {
            List<Inline> content = new List<Inline>();
            //
            foreach (string desc in CreateProperty("", 0))
            {
                Run temp = new Run(desc + Environment.NewLine);
                content.Add(temp);
            }
            // Create a "simple" list... Would be cool to have Colors...
            var textBlock = new TextBlock
            {
                Padding = new Thickness(5)
            };
            textBlock.Inlines.AddRange(content);
            return Task.FromResult<object>(textBlock);
        }

        public Task<IEnumerable<SuggestedActionSet>> GetActionSetsAsync(CancellationToken cancellationToken)
        {
            return Task.FromResult<IEnumerable<SuggestedActionSet>>(null);
        }

        public bool HasActionSets
        {
            get { return false; }
        }
        public string DisplayText
        {
            get { return "Encapsulate field:" + (_full ? "(full) " : " ") + _fieldEntity.Name.Replace("_", "__") + (_renameField ? "(and rename field) " : " "); }
        }
        public ImageMoniker IconMoniker
        {
            get { return default; }
        }
        public string IconAutomationText
        {
            get
            {
                return null;
            }
        }
        public string InputGestureText
        {
            get
            {
                return null;
            }
        }
        public bool HasPreview
        {
            get { return true; }
        }

        public void Dispose()
        {
        }

        public bool TryGetTelemetryId(out Guid telemetryId)
        {
            // This is a sample action and doesn't participate in LightBulb telemetry
            telemetryId = Guid.Empty;
            return false;
        }

        // The job is done here !!
        public void Invoke(CancellationToken cancellationToken)
        {
            var settings = m_textView.TextBuffer.Properties.GetProperty<SourceCodeEditorSettings>(typeof(SourceCodeEditorSettings));
            //m_span.TextBuffer.Replace(m_span.GetSpan(m_snapshot), ");
            StringBuilder insertText;
            // Last line of the Entity
            int lineNumber = Math.Min(_range.EndLine, m_snapshot.LineCount - 1);
            ITextSnapshotLine lastLine = m_snapshot.GetLineFromLineNumber(lineNumber);
            // Retrieve the text
            string lineText = lastLine.GetText();
            // and count how many spaces we have before
            int count = lineText.TakeWhile(Char.IsWhiteSpace).Count();
            // Get these as prefix
            string prefix = lineText.Substring(0, count);
            List<Inline> content = new List<Inline>();
            // Add a comment with the Interface name ??
            insertText = new StringBuilder();
            // Create an Edit Session
            var editSession = m_textView.TextBuffer.CreateEdit();
            try
            {
                foreach (string line in CreateProperty(prefix, settings.IndentSize))
                {
                    insertText.AppendLine(line);
                }
                // Inject code
                editSession.Insert(lastLine.Start.Position, insertText.ToString());
                if (_renameField)
                {
                    lineNumber = _fieldEntity.Range.StartLine;
                    ITextSnapshotLine fieldLine = m_snapshot.GetLineFromLineNumber(lineNumber);
                    string sourceCode = _fieldEntity.SourceCode;
                    sourceCode = sourceCode.Replace(_fieldEntity.Name, _generateName);
                    editSession.Insert( fieldLine.Start.Position, prefix);
                    editSession.Replace( fieldLine.Extent, sourceCode);
                }

            }
            catch (Exception e)
            {
                WriteOutputMessage("editSession : error " + e.Message);
            }
            finally
            {
                // Validate the Edit Session ?
                if (editSession.HasEffectiveChanges)
                {
                    editSession.Apply();

                }
                else
                {
                    editSession.Cancel();
                }
                //
            }
        }

        private List<String> CreateProperty(string prefix, int indentSize)
        {
            List<String> result = new List<string>();
            String indent = new string(' ', indentSize);
            //
            StringBuilder insertText = new StringBuilder();
            insertText.Append(prefix);
            //insertText.Append(_fieldEntity.ModVis);
            insertText.Append("PUBLIC "); // C# always set generated Properties as PUBLIC
            //insertText.Append(' '); ModVis already has a final space
            insertText.Append("PROPERTY ");
            if (_renameField)
                insertText.Append(_fieldEntity.Name);
            else
                insertText.Append(_generateName);
            insertText.Append(" AS ");
            insertText.Append(_fieldEntity.TypeName);
            insertText.Append(" ");
            if (_full)
            {
                result.Add(insertText.ToString());
                insertText.Clear();
                insertText.Append(prefix);
                insertText.Append(indent);
            }
            insertText.Append("GET ");
            if (_full)
            {
                result.Add(insertText.ToString());
                insertText.Clear();
                insertText.Append(prefix);
                insertText.Append(indent);
                insertText.Append(indent);
                insertText.Append("RETURN ");
            }
            if (!_renameField)
                insertText.Append(_fieldEntity.Name);
            else
                insertText.Append(_generateName);
            insertText.Append(" ");
            if (_full)
            {
                result.Add(insertText.ToString());
                insertText.Clear();
                insertText.Append(prefix);
                insertText.Append(indent);
                insertText.Append("END GET");
                result.Add(insertText.ToString());
                insertText.Clear();
                insertText.Append(prefix);
                insertText.Append(indent);
            }
            insertText.Append("SET ");
            if (_full)
            {
                result.Add(insertText.ToString());
                insertText.Clear();
                insertText.Append(prefix);
                insertText.Append(indent);
                insertText.Append(indent);
            }
            if (!_renameField)
                insertText.Append(_fieldEntity.Name);
            else
                insertText.Append(_generateName);
            insertText.Append(" := VALUE ");
            if (_full)
            {
                result.Add(insertText.ToString());
                insertText.Clear();
                insertText.Append(prefix);
                insertText.Append(indent);
                insertText.Append("END SET");
                result.Add(insertText.ToString());
                insertText.Clear();
                insertText.Append(prefix);
                insertText.Append("END PROPERTY");
            }
            //
            result.Add(insertText.ToString());
            return result;
        }

        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableParameterLog && XSettings.EnableLogging)
            {
                XSettings.LogMessage("ImplementInterfaceSuggestedAction:" + strMessage);
            }
        }


    }






}
