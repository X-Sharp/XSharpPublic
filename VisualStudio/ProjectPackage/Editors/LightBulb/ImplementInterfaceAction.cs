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
            return;
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
                    elementsToAdd.Add(CreateMember(member, members));
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
