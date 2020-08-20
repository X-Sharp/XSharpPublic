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
using XSharpColorizer;
using System.Collections.Immutable;

namespace XSharp.Project.Editors.LightBulb
{
    internal class ImplementInterfaceSuggestedAction : ISuggestedAction
    {
        private ITrackingSpan m_span;
        private string m_display;
        private ITextSnapshot m_snapshot;
        private ITextView m_textView;

        public ImplementInterfaceSuggestedAction(ITrackingSpan span, ITextView textView)
        {
            m_span = span;
            m_snapshot = span.TextBuffer.CurrentSnapshot;
            m_display = string.Format("Implement Interfaces", span.GetText(m_snapshot));
            m_textView = textView;
        }

        public Task<object> GetPreviewAsync(CancellationToken cancellationToken)
        {
            var textBlock = new TextBlock();
            textBlock.Padding = new Thickness(5);
            textBlock.Inlines.Add(new Run() { Text = "Ooopsss" });
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
            get { return m_display; }
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
            //m_span.TextBuffer.Replace(m_span.GetSpan(m_snapshot), ");
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
                XTypeDefinition classDef = null;
                foreach (KeyValuePair<String, XTypeDefinition> kvp in file.TypeList)
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
                    // Our own types
                    XTypeDefinition ti = null;
                    IList<string> Usings = file.Usings;
                    // Search already implemented Members
                    bool FoundAll = true;
                    string FullName = "";
                    // Let's build a list of Elements to add to implement the Interface
                    List<XMemberDefinition> toAdd = new List<XMemberDefinition>();
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
                            if (temp.XTypeDef!= null)
                            {
                                ti = temp.XTypeDef;
                                if (ti.Kind == Kind.Interface)
                                {
                                    FullName = ti.Name;
                                    // Everything is here ?
                                    FoundAll = true;
                                    foreach (XMemberDefinition mbr in ti.Members)
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
                        }
                        //
                    }
                }
            }
            // Sorry, nothing to do....
            return;
        }

        private List<XMemberDefinition> BuildMissingMembers(XTypeDefinition currentClass, System.Reflection.MemberInfo[] members)
        {
            List<XMemberDefinition> elementsToAdd = new List<XMemberDefinition>();
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
                    elementsToAdd.Add(CreateMember(member, members));
                }
            }
            return elementsToAdd;
        }

        private XMemberDefinition CreateMember(MemberInfo member, MemberInfo[] members)
        {
            // NOOooooo
            return null;
        }

        private bool CheckForMember(XTypeDefinition currentClass, MemberInfo member)
        {
            return true;
        }


    }
}
