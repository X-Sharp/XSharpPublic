using Microsoft.VisualStudio.Language.Intellisense;
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
        private ITextSnapshot m_snapshot;
        private ITextView m_textView;
        private XSourceMemberSymbol _fieldEntity;
        private XSharpModel.TextRange _range;
        private bool _full;

        public PropertySuggestedAction(ITextView textView, ITextBuffer m_textBuffer, XSourceMemberSymbol fieldToPropertize, XSharpModel.TextRange range, bool fullProperty )
        {
            m_snapshot = m_textBuffer.CurrentSnapshot;
            m_textView = textView;
            _fieldEntity = fieldToPropertize;
            _range = range;
            _full = fullProperty;
        }

        public Task<object> GetPreviewAsync(CancellationToken cancellationToken)
        {
            List<Inline> content = new List<Inline>();
            //
            foreach (string desc in CreateProperty("",0))
            {
                Run temp = new Run(desc + Environment.NewLine );
                content.Add(temp);
            }
            // Create a "simple" list... Would be cool to have Colors...
            var textBlock = new TextBlock();
            textBlock.Padding = new Thickness(5);
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
            get{ return "Encapsulate field:" + (_full ? "(full) " : " ") + _fieldEntity.Name; }
        }
        public ImageMoniker IconMoniker
        {
            get { return default(ImageMoniker); }
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
            // First line of the Entity
            ITextSnapshotLine firstLine = m_snapshot.GetLineFromLineNumber(_range.EndLine);
            // Retrieve the text
            string lineText = firstLine.GetText();
            // and count how many spaces we have before
            int count = lineText.TakeWhile(Char.IsWhiteSpace).Count();
            // Get these as prefix
            string prefix = lineText.Substring(0, count);
            ITextSnapshotLine lastLine = m_snapshot.GetLineFromLineNumber(_range.EndLine);
            List<Inline> content = new List<Inline>();
            // Add a comment with the Interface name ??
            insertText = new StringBuilder();
            // Create an Edit Session
            var editSession = m_textView.TextBuffer.CreateEdit();
            try
            {
                foreach( string line in CreateProperty(prefix, settings.IndentSize))
                {
                    insertText.AppendLine(line);
                }
                // Inject code
                editSession.Insert(lastLine.Start.Position, insertText.ToString());
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

        private List<String> CreateProperty(string prefix, int indentSize )
        {
            List<String> result = new List<string>();
            String indent = new string(' ', indentSize);
            //
            StringBuilder insertText = new StringBuilder();
            insertText.Append(prefix);
            insertText.Append(_fieldEntity.ModVis);
            //insertText.Append(' '); ModVis already has a final space
            insertText.Append("PROPERTY ");
            insertText.Append(_fieldEntity.Name.Substring(1));
            insertText.Append(" AS ");
            insertText.Append(_fieldEntity.TypeName);
            insertText.Append(" ");
            if (_full )
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
            insertText.Append(_fieldEntity.Name);
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
            insertText.Append(_fieldEntity.Name);
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

        /// <summary>
        /// Try to infer a default value
        /// </summary>
        /// <param name="returnType"></param>
        /// <returns></returns>
        private string poorManDefaultValue(string returnType)
        {
            string ret = "";
            returnType = returnType.ToLower();
            switch (returnType)
            {
                case "int":
                case "long":
                case "float":
                case "double":
                    ret = "0";
                    break;
                case "boolean":
                case "logic":
                    ret = "false";
                    break;
                default:
                    ret = "null";
                    break;
            }
            return ret;
        }


        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableParameterLog && XSettings.EnableLogging)
            {
                XSettings.DisplayOutputMessage("ImplementInterfaceSuggestedAction:" + strMessage);
            }
        }


    }






}
