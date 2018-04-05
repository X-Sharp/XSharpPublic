using System;
using System.Linq;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using static XSharp.Project.XSharpConstants;
using XSharpColorizer;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text.Operations;
using System.Threading.Tasks;
using System.Threading;
using System.Collections.Immutable;
using Microsoft.VisualStudio.Text.Formatting;
using XSharpModel;
using System.Reflection;

namespace XSharp.Project.Editors.LightBulb
{
    [Export(typeof(ISuggestedActionsSourceProvider))]
    [Name("Implement Interface Suggested Action")]
    [ContentType(LanguageName)]
    internal class ImplementInterfaceSuggestedActionsSourceProvider : ISuggestedActionsSourceProvider
    {
        [Import(typeof(ITextStructureNavigatorSelectorService))]
        internal ITextStructureNavigatorSelectorService NavigatorService { get; set; }

        public ISuggestedActionsSource CreateSuggestedActionsSource(ITextView textView, ITextBuffer textBuffer)
        {
            var package = XSharp.Project.XSharpProjectPackage.Instance;
            var optionsPage = package.GetIntellisenseOptionsPage();
            if (optionsPage.DisableLightBulb)
                return null;
            if (textBuffer == null && textView == null)
            {
                return null;
            }
            return new ImplementInterfaceSuggestedActionsSource(this, textView, textBuffer);
        }
    }

    internal class ImplementInterfaceSuggestedActionsSource : ISuggestedActionsSource
    {
        private ImplementInterfaceSuggestedActionsSourceProvider m_factory;
        private ITextBuffer m_textBuffer;
        private ITextView m_textView;

#pragma warning disable CS0067
        public event EventHandler<EventArgs> SuggestedActionsChanged;

        public ImplementInterfaceSuggestedActionsSource(ImplementInterfaceSuggestedActionsSourceProvider ImplementInterfaceSuggestedActionsSourceProvider, ITextView textView, ITextBuffer textBuffer)
        {
            this.m_factory = ImplementInterfaceSuggestedActionsSourceProvider;
            this.m_textView = textView;
            this.m_textBuffer = textBuffer;
            //
        }

        private bool TryGetWordUnderCaret(out TextExtent wordExtent)
        {
            ITextCaret caret = m_textView.Caret;
            SnapshotPoint point;

            if (caret.Position.BufferPosition > 0)
            {
                point = caret.Position.BufferPosition - 1;
            }
            else
            {
                wordExtent = default(TextExtent);
                return false;
            }

            ITextStructureNavigator navigator = m_factory.NavigatorService.GetTextStructureNavigator(m_textBuffer);

            wordExtent = navigator.GetExtentOfWord(point);
            return true;
        }

        public Task<bool> HasSuggestedActionsAsync(ISuggestedActionCategorySet requestedActionCategories, SnapshotSpan range, CancellationToken cancellationToken)
        {
            //return Task.Factory.StartNew(() =>
            //{
            //    TextExtent extent;
            //    if (TryGetWordUnderCaret(out extent))
            //    {
            //        // don't display the action if the extent has whitespace  
            //        return extent.IsSignificant;
            //    }
            //    return false;
            //});

            return Task.Factory.StartNew(() =>
            {
                return SearchImplement();
            });
        }

        public IEnumerable<SuggestedActionSet> GetSuggestedActions(ISuggestedActionCategorySet requestedActionCategories, SnapshotSpan range, CancellationToken cancellationToken)
        {
            if (SearchImplement())
            {
                ITrackingSpan trackingSpan = range.Snapshot.CreateTrackingSpan(range, SpanTrackingMode.EdgeInclusive);
                var ImplementInterfaceAction = new ImplementInterfaceSuggestedAction(trackingSpan);
                return new SuggestedActionSet[] { new SuggestedActionSet(new ISuggestedAction[] { ImplementInterfaceAction }) };
            }
            return Enumerable.Empty<SuggestedActionSet>();
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

        public bool SearchImplement()
        {
            // Try to retrieve an already parsed list of Tags
             XSharpClassifier xsClassifier = null;
            if (m_textBuffer.Properties.ContainsProperty(typeof(XSharpClassifier)))
            {
                xsClassifier = m_textBuffer.Properties[typeof(XSharpClassifier)] as XSharpClassifier;
            }

            ITextCaret caret = m_textView.Caret;
            if (xsClassifier != null && xsClassifier.Snapshot.Version == caret.Position.BufferPosition.Snapshot.Version)
            {
                //
                ITextSnapshot snapshot = xsClassifier.Snapshot;
                // Note we should use the same snapshot everywhere in this code.
                // the snapshot in the classifier may be older than the current snapshot
                if (snapshot.Length == 0)
                    return false; // Should not happen : This means that the buffer is empty !!!
                //
                ITextViewLine iLine = caret.ContainingTextViewLine;
                SnapshotSpan Span = new SnapshotSpan(snapshot, iLine.Start.Position, iLine.Length);
                //
                IList<ClassificationSpan> classifications = xsClassifier.GetClassificationSpans(Span);
                //
                foreach (var classification in classifications)
                {
                    if (!classification.Span.Contains(caret.Position.BufferPosition))
                        continue;

                    var name = classification.ClassificationType.Classification.ToLower();
                    //
                    if (name.Contains("keyword"))
                    {
                        SnapshotSpan cspan = classification.Span;
                        string Keyword = snapshot.GetText(cspan);
                        Keyword = Keyword.ToLower();
                        // Search for Implements
                        if (Keyword.Equals("implements"))
                        {
                            return true;
                        }
                    }
                }
            }
            return false;
        }


        private void BuildMemberList(SnapshotSpan span)
        {
            // 
            ITrackingSpan trackingSpan = span.Snapshot.CreateTrackingSpan(span, SpanTrackingMode.EdgeInclusive);
            // Get the Name of the File
            XSharpModel.XFile file = this.m_textView.TextBuffer.GetFile();
            if (file != null)
            {
                //
                ITextSnapshotLine line = span.Start.GetContainingLine();
                int lineNumber = line.LineNumber;
                int columnNumber = span.Start.Position - line.Start.Position;
                //
                XType classDef = null;
                foreach (KeyValuePair<String, XType> kvp in file.TypeList)
                {
                    if (kvp.Value.Range.ContainsInclusive(lineNumber, columnNumber))
                    {
                        classDef = kvp.Value;
                        break;
                    }
                }
                if (classDef != null)
                {
                    // Get the Interfaces
                    // classDef.Implement DOESN'T exist currently :(
                    string[] interfaces = { }; 
                    // Clr Types
                    Type t = null;
                    // Our own types
                    XType ti = null;
                    IList<String> Usings = file.Usings;
                    // Search already implemented Members
                    bool FoundAll = true;
                    string FullName = "";
                    // Let's build a list of Elements to add to implement the Interface
                    List<XTypeMember> toAdd = new List<XTypeMember>();
                    CompletionType temp;
                    //
                    foreach (string iface in interfaces)
                    {
                        String iFace = iface.Trim();
                        // Search The interface
                        // --> Default NameSpace
                        temp = new CompletionType(iFace, file, "");
                        if (!temp.IsEmpty())
                        {
                            if (temp.XType != null)
                            {
                                ti = temp.XType;
                                if (ti.Kind == Kind.Interface)
                                {
                                    FullName = ti.Name;
                                    // Everything is here ?
                                    FoundAll = true;
                                    foreach (XTypeMember mbr in ti.Members)
                                    {
                                        if (!classDef.Members.Contains(mbr))
                                        {
                                            // No
                                            toAdd.Add(mbr);
                                        }
                                    }
                                    FoundAll = (toAdd.Count == 0);
                                }
                            }
                            else
                            {
                                t = temp.SType;
                                if (t.IsInterface)
                                {
                                    FullName = t.FullName;
                                    // Everything is here ?
                                    FoundAll = true;
                                    // Please create the array
                                    toAdd = BuildMissingMembers(classDef, t.GetMembers());
                                    FoundAll = (toAdd.Count == 0);
                                }
                            }
                        }
                        //
                    }
                }
            }
            // Sorry, nothing to do....
            return ;
        }

        private List<XTypeMember> BuildMissingMembers(XType currentClass, System.Reflection.MemberInfo[] members)
        {
            List<XTypeMember> elementsToAdd = new List<XTypeMember>();
            //
            foreach (System.Reflection.MemberInfo member in members)
            {
                System.Reflection.MemberTypes realType = member.MemberType;
                if (realType == System.Reflection.MemberTypes.Method)
                {
                    System.Reflection.MethodInfo method = (System.Reflection.MethodInfo)member;
                    // Check for Getter/Setter 
                    if ((method.Attributes & System.Reflection.MethodAttributes.SpecialName) == System.Reflection.MethodAttributes.SpecialName)
                    {
                        string getsetName = member.Name;
                        if (getsetName.StartsWith("get_") || getsetName.StartsWith("set_"))
                            // Oooppsss
                            continue;
                    }
                }
                // Now, We will have to check Parameters / Return Type
                if (!CheckForMember(currentClass, member))
                {
                    // and re-create our own prototype
                    elementsToAdd.Add( CreateMember(member, members));
                }
            }
            return elementsToAdd;
        }

        private XTypeMember CreateMember(MemberInfo member, MemberInfo[] members)
        {
            // NOOooooo
            return null;
        }

        private bool CheckForMember(XType currentClass, MemberInfo member)
        {
            return true;
        }
    }


}
