using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using XSharpModel;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.VisualStudio.Text.Editor;
using System.Diagnostics;

namespace XSharp.LanguageService
{
    internal class XSharpSignatureProperties
    {
        internal int triggerLine { get; set; }
        internal int Start { get; set; }
        internal IXMemberSymbol Element { get; set; }
        internal int Length { get; set; }
        internal Modifiers Visibility { get; set; }
        internal char triggerChar { get; set; }
        internal int triggerPosition { get; set; }
        internal string triggerToken { get; set; }
        internal XSharpSearchLocation Location { get; set; }
        internal XSharpSignatureProperties(XSharpSearchLocation location)
        {
            Location = location;
        }
    }
    [DebuggerDisplay("{Name}")]
    internal class XSharpVsParameter : IParameter
    {
        public string Documentation { get; private set; }
        public Span Locus { get; private set; }
        public string Name { get; private set; }
        public ISignature Signature { get; private set; }
        public Span PrettyPrintedLocus { get; private set; }


        public XSharpVsParameter(string documentation, Span locus, string name, ISignature signature)
        {
            Documentation = documentation;
            Locus = locus;
            Name = name;
            Signature = signature;
        }
    }
    [DebuggerDisplay("{Content}")]
    internal class XSharpVsSignature : ISignature
    {
        private ITextView m_textview;
        private IParameter m_currentParameter;
        private string m_content;
        private string m_documentation;
        private ITrackingSpan m_applicableToSpan;
        private ReadOnlyCollection<IParameter> m_parameters;
        private string m_printContent;

        internal XSharpVsSignature(ITextView textView, string content, string doc, ReadOnlyCollection<IParameter> parameters)
        {
            m_textview = textView;
            m_content = content;
            m_documentation = doc;
            m_parameters = parameters;
            //m_subjectBuffer.Changed += new EventHandler<TextContentChangedEventArgs>(OnSubjectBufferChanged);
        }

        public event EventHandler<CurrentParameterChangedEventArgs> CurrentParameterChanged;


        #region

        private void RaiseCurrentParameterChanged(IParameter prevCurrentParameter, IParameter newCurrentParameter)
        {
            EventHandler<CurrentParameterChangedEventArgs> tempHandler = this.CurrentParameterChanged;
            if (tempHandler != null)
            {
               tempHandler(this, new CurrentParameterChangedEventArgs(prevCurrentParameter, newCurrentParameter));
            }
        }

        internal static int CalculateCommaPosition(string sigText, int lastPos, ITextBuffer buffer)
        {
            var doc = buffer.GetDocument();
            var tokens = doc.GetTokens(sigText);
            int commaCount = 0;
            foreach (var token in tokens)
            {
                if (token.Column > lastPos)
                    break;
                if (token.Type == XSharpLexer.COMMA)
                    commaCount += 1;
            }
            return commaCount;
        }

        internal void ComputeCurrentParameter(int atPosition = -1 )
        {
            if (Parameters.Count == 0)
            {
                this.CurrentParameter = null;
                return;
            }

            //the number of commas in the string is the index of the current parameter
            string sigText = ApplicableToSpan.GetText(m_textview.TextBuffer.CurrentSnapshot);
            if (atPosition == -1)
                atPosition = sigText.Length;
            var commaCount = CalculateCommaPosition(sigText, atPosition, m_textview.TextBuffer);

            if (commaCount < Parameters.Count)
            {
                this.CurrentParameter = Parameters[commaCount];
            }
        }

        internal void OnSubjectBufferChanged(object sender, TextContentChangedEventArgs e)
        {
            this.ComputeCurrentParameter();
        }

        #endregion

        public IParameter CurrentParameter
        {
            get { return m_currentParameter; }

            internal set
            {
                IParameter old = m_currentParameter;
                m_currentParameter = value;
                if (old != value)
                {
                    this.RaiseCurrentParameterChanged(old, value);
                }
            }
        }

        public ITrackingSpan ApplicableToSpan
        {
            get { return (m_applicableToSpan); }
            internal set { m_applicableToSpan = value; }
        }


        public string Content
        {
            get { return (m_content); }
            internal set { m_content = value; }
        }

        public string Documentation
        {
            get { return (m_documentation); }
            internal set { m_documentation = value; }
        }

        public ReadOnlyCollection<IParameter> Parameters
        {
            get { return (m_parameters); }
            internal set { m_parameters = value; }
        }

        public string PrettyPrintedContent
        {
            get { return (m_printContent); }
            internal set { m_printContent = value; }
        }


    }

    internal class XSharpSignatureHelpSource : ISignatureHelpSource
    {
        private ITextBuffer m_textBuffer;
        private ISignatureHelpSession m_session;
        private ITrackingSpan m_applicableToSpan;
        private XFile m_file;

        public XSharpSignatureHelpSource(ITextBuffer textBuffer, XFile file)
        {
            m_textBuffer = textBuffer;
            m_file = file;
        }
        internal void Debug(string strMessage)
        {
            if (XSettings.EnableParameterLog && XSettings.EnableLogging)
            {
                XSettings.LogMessage(strMessage);
            }
        }

        private string getProto(IXMemberSymbol xMember, XSharpSignatureProperties props)
        {
            var proto = xMember.Prototype;
            // Adjust SELF() and SUPER: remove typename and replace {} with ()
            if (xMember.Kind == Kind.Constructor && props.triggerChar == '(')
            {
                var parlist = proto.Substring(proto.IndexOf('{')+1);
                parlist = parlist.Substring(0, parlist.Length - 1);
                proto = props.triggerToken + "("+parlist+")";
            }
            return proto;
        }

        public void AugmentSignatureHelpSession(ISignatureHelpSession session, IList<ISignature> signatures)
        {
            try
            {
                Debug("XSharpSignatureHelpSource.AugmentSignatureHelpSession()");
                m_session = session;
                XSharpModel.ModelWalker.Suspend();
                ITextSnapshot snapshot = m_textBuffer.CurrentSnapshot;
                var props = session.GetSignatureProperties();
                if (props == null)
                    return;
                int position = session.GetTriggerPoint(m_textBuffer).GetPosition(snapshot);
                m_applicableToSpan = m_textBuffer.CurrentSnapshot.CreateTrackingSpan(new Span(props.Start, props.Length), SpanTrackingMode.EdgeInclusive, 0);
                object elt = props.Element;
                if (elt is IXSymbol)
                {
                    IXMemberSymbol element = elt as IXMemberSymbol;
                    var sigs = new List<ISignature>();
                    if (elt is IXMemberSymbol xMember)
                    {
                        var names = new List<string>();
                        var proto = getProto(xMember, props);
                        names.Add(proto);
                        sigs.Add(CreateSignature(session, xMember, proto, ApplicableToSpan, xMember.Kind == XSharpModel.Kind.Constructor, m_file));
                        var overloads = xMember.GetOverloads();

                        foreach (var member in overloads)
                        {
                            if (member.Visibility < props.Visibility)
                                continue;
                            // prevent duplicate prototypes in the list  (when a child has overriden a method)
                            proto = getProto(member, props);
                            if (!names.Contains(proto))
                            {
                                sigs.Add(CreateSignature(session, member, proto, ApplicableToSpan, member.Kind == XSharpModel.Kind.Constructor, m_file));
                                names.Add(proto);
                            }
                        }
                        sigs.Sort((x, y) => x.Parameters.Count - y.Parameters.Count);
                        foreach (var sig in sigs)
                        {
                            signatures.Add(sig);
                        }
                    }
                    else if (element != null)
                    {
                        // Type ??
                        signatures.Add(CreateSignature(session, null, element.Prototype, ApplicableToSpan, false, m_file));
                    }
                    // why not ?
                    int paramCount = int.MaxValue;
                    foreach (ISignature sig in signatures)
                    {
                        if (sig.Parameters.Count < paramCount)
                        {
                            paramCount = sig.Parameters.Count;
                        }
                    }
                    //
                    m_textBuffer.Changed += new EventHandler<TextContentChangedEventArgs>(OnSubjectBufferChanged);
                }
                session.Dismissed += OnSignatureHelpSessionDismiss;
            }
            catch (Exception ex)
            {
                XSettings.LogException(ex, "XSharpSignatureHelpSource.AugmentSignatureHelpSession Exception failed ");
            }
            finally
            {
                XSharpModel.ModelWalker.Resume();
            }
        }




        private XSharpVsSignature CreateSignature(ISignatureHelpSession session, IXMemberSymbol member, string methodSig, ITrackingSpan span, bool isCtor, XFile file )
        {
            var doc = "";
            string returns;
            string remarks;
            if (member != null)
            {
                doc = XSharpXMLDocMember.GetMemberSummary(member, file.Project, out returns, out remarks);
            }

            Debug($"XSharpSignatureHelpSource.CreateSignature( {methodSig})");
            var sig = new XSharpVsSignature(session.TextView, methodSig, doc, null);
            var names = new List<string>();
            var descriptions = new List<string>();
            if (member != null)
            {
                XSharpXMLDocMember.GetMemberParameters(member, file.Project, names, descriptions);
            }
            // Moved : Done in the XSharpSignature constructor
            // textBuffer.Changed += new EventHandler<TextContentChangedEventArgs>(sig.OnSubjectBufferChanged);

            // find the parameters in the method signature :
            // MyMethod( param1 AS TYPE1, param2 AS TYPE2 ) AS TYPE3 will turn to
            // 0 : MyMethod
            // 1 : param1 AS TYPE1
            // 2 : param2 AS TYPE2
            // 3 : AS TYPE3
            var pars = methodSig.Split(new char[] { '(', '{', ',', '}',')' });
            List<IParameter> paramList = new List<IParameter>();
            int locusSearchStart = 0;
            // the pars array has the method name and return type too:
            // i = 1 to skip the MethodName; Length-1 to Skip the ReturnType

            while (names.Count < pars.Length)
            {
                names.Add("");
                descriptions.Add("");
            }
            for (int i = 1; i < pars.Length-1; i++)
            {
                string param = pars[i].Trim();
                if (string.IsNullOrEmpty(param))
                    continue;

                //find where this parameter is located in the method signature
                int locusStart = methodSig.IndexOf(param, locusSearchStart);
                if (locusStart >= 0)
                {
                    Span locus = new Span(locusStart, param.Length);
                    locusSearchStart = locusStart + param.Length;
                    // paramList.Add(new XSharpParameter("Documentation for the parameter.", locus, param, sig));
                    if (!string.IsNullOrEmpty(names[i - 1]))
                        param = names[i - 1];
                    paramList.Add(new XSharpVsParameter(descriptions[i-1], locus, param, sig));
                }
            }

            sig.Parameters = new ReadOnlyCollection<IParameter>(paramList);
            sig.ApplicableToSpan = span;
            sig.ComputeCurrentParameter();
            return sig;
        }

        public ISignature GetBestMatch(ISignatureHelpSession session)
        {
            if (session.Signatures.Count > 0)
            {
                ITrackingSpan applicableToSpan = session.Signatures[0].ApplicableToSpan;
                string text = applicableToSpan.GetText(applicableToSpan.TextBuffer.CurrentSnapshot);

                return session.Signatures[0];
            }
            return null;
        }

        //private bool nameEquals(string name, string compareWith)
        //{
        //    return (name.ToLower().CompareTo(compareWith.ToLower()) == 0);
        //}

        private bool m_isDisposed;
        public void Dispose()
        {
            if (!m_isDisposed)
            {
                GC.SuppressFinalize(this);
                m_isDisposed = true;
            }
        }



        public ITrackingSpan ApplicableToSpan
        {
            get { return (m_applicableToSpan); }
            internal set { m_applicableToSpan = value; }
        }

        private void OnSignatureHelpSessionDismiss(object sender, EventArgs e)
        {
            m_textBuffer.Changed -= new EventHandler<TextContentChangedEventArgs>(OnSubjectBufferChanged);
        }

        internal void OnSubjectBufferChanged(object sender, TextContentChangedEventArgs e)
        {
            this.ComputeCurrentParameter();
        }

        internal void ComputeCurrentParameter()
        {
            //the number of commas in the string is the index of the current parameter
            string sigText = ApplicableToSpan.GetText(m_textBuffer.CurrentSnapshot);

            var commaCount = XSharpVsSignature.CalculateCommaPosition(sigText,sigText.Length, m_textBuffer);

            //
            List<ISignature> signatures = new List<ISignature>();
            foreach (ISignature sig in this.m_session.Signatures)
            {
                if (sig.Parameters.Count > commaCount)
                    signatures.Add(sig);
            }
            //
            if (signatures.Count == 0)
            {
                var sig = this.m_session.SelectedSignature as XSharpVsSignature;
                sig.CurrentParameter = null;
            }
            else
            {

               if (this.m_session.SelectedSignature != null && this.m_session.SelectedSignature.Parameters.Count < commaCount)
                {
                    this.m_session.SelectedSignature = signatures[0];
                    var sig = this.m_session.SelectedSignature as XSharpVsSignature;
                    sig.CurrentParameter = signatures[0].Parameters[commaCount];
                }
            }
        }
    }
}
