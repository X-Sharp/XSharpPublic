using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using System.Reflection;
using XSharpModel;
using XSharp.Project.OptionsPages;

namespace XSharp.Project
{
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
    internal class XSharpVsSignature : ISignature
    {
        private ITextBuffer m_subjectBuffer;
        private IParameter m_currentParameter;
        private string m_content;
        private string m_documentation;
        private ITrackingSpan m_applicableToSpan;
        private ReadOnlyCollection<IParameter> m_parameters;
        private string m_printContent;

        internal XSharpVsSignature(ITextBuffer subjectBuffer, string content, string doc, ReadOnlyCollection<IParameter> parameters)
        {
            m_subjectBuffer = subjectBuffer;
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

        internal static int CalculateCommaPosition(string sigText, int lastPos)
        {
            int commaCount = 0;
            bool instring = false;
            char endchar = '\0';
            int currentPos = 0;
            foreach (char ch in sigText)
            {
                if (currentPos > lastPos)
                {
                    break;
                }
                currentPos += 1;
                switch (ch)
                {
                    case '\'':
                        if (!instring)
                        {
                            instring = true;
                            endchar = ch;
                            continue;
                        }
                        break;
                    case '"':
                        if (!instring)
                        {
                            instring = true;
                            endchar = ch;
                            continue;
                        }
                        break;
                    case '[':
                        if (!instring)
                        {
                            instring = true;
                            endchar = ']';
                            continue;
                        }
                        break;
                    case ',':
                        if (!instring)
                        {
                            commaCount++;
                        }

                        break;
                }
                if (ch == endchar)
                {
                    instring = false;
                    continue;
                }
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
            string sigText = ApplicableToSpan.GetText(m_subjectBuffer.CurrentSnapshot);
            if ( atPosition == -1 )
                atPosition = sigText.Length;
            var commaCount = CalculateCommaPosition(sigText, atPosition);


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
                if (m_currentParameter != value)
                {
                    IParameter prevCurrentParameter = m_currentParameter;
                    m_currentParameter = value;
                    this.RaiseCurrentParameterChanged(prevCurrentParameter, m_currentParameter);
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
        IntellisenseOptionsPage optionsPage = null;
        XSharpProjectPackage package;

        public XSharpSignatureHelpSource(ITextBuffer textBuffer)
        {
            m_textBuffer = textBuffer;
            package = XSharpProjectPackage.Instance;
            optionsPage = package.GetIntellisenseOptionsPage();
        }
        internal void Debug(string strMessage)
        {
            if (optionsPage.EnableParameterLog && optionsPage.EnableOutputPane)
            {
                XSharpProjectPackage.Instance.DisplayOutPutMessage(strMessage);
            }
        }
        public void AugmentSignatureHelpSession(ISignatureHelpSession session, IList<ISignature> signatures)
        {
            try
            {
                Debug("XSharpSignatureHelpSource.AugmentSignatureHelpSession()");
                XSharpModel.ModelWalker.Suspend();
                ITextSnapshot snapshot = m_textBuffer.CurrentSnapshot;
                int position = session.GetTriggerPoint(m_textBuffer).GetPosition(snapshot);
                int start = (int)session.Properties["Start"];
                int length = (int)session.Properties["Length"];
                var comma = (bool)session.Properties["Comma"];
                var file = (XFile)session.Properties["File"];
                m_applicableToSpan = m_textBuffer.CurrentSnapshot.CreateTrackingSpan(
                 new Span(start, length), SpanTrackingMode.EdgeInclusive, 0);

                object elt = session.Properties["Element"];
                m_session = session;
                if (elt is IXElement)
                {
                    IXMember element = elt as IXMember;
                    //
                    if (elt is IXMember)
                    {
                        IXMember xMember = elt as IXMember;
                        var names = new List<string>();
                        var proto = xMember.Prototype;
                        names.Add(proto);
                        signatures.Add(CreateSignature(m_textBuffer, xMember, proto, "", ApplicableToSpan, comma, xMember.Kind == XSharpModel.Kind.Constructor, file));
                        var overloads = xMember.GetOverloads();
                        foreach (var member in overloads)
                        {
                            // prevent duplicate prototypes in the list  (when a child has overriden a method)
                            proto = member.Prototype;
                            if (!names.Contains(proto))
                            {
                                signatures.Add(CreateSignature(m_textBuffer, member, proto, "", ApplicableToSpan, comma, member.Kind == XSharpModel.Kind.Constructor, file));
                                names.Add(proto);
                            }
                        }
                    }
                    else if (element != null)
                    {
                        // Type ??
                        signatures.Add(CreateSignature(m_textBuffer, null, element.Prototype, "", ApplicableToSpan, comma, false, file));
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
                package.DisplayOutPutMessage("XSharpSignatureHelpSource.AugmentSignatureHelpSession Exception failed " );
                package.DisplayException(ex);
            }
            finally
            {
                XSharpModel.ModelWalker.Resume();
            }
        }


 

        private XSharpVsSignature CreateSignature(ITextBuffer textBuffer, IXMember member, string methodSig, string methodDoc, ITrackingSpan span, bool comma, bool isCtor, XFile file )
        {
            var doc = methodDoc;
            string returns;
            string remarks;
            if (member != null)
            {
                doc = XSharpXMLDocMember.GetMemberSummary(member, file.Project, out returns, out remarks);
            }

            Debug("XSharpSignatureHelpSource.CreateSignature()");
            var sig = new XSharpVsSignature(textBuffer, methodSig, doc, null);
            var names = new List<String>();
            var descriptions = new List<String>();
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
            string[] pars;
            if (isCtor)
                pars = methodSig.Split(new char[] { '{', ',', '}' });
            else
                pars = methodSig.Split(new char[] { '(', ',', ')' });
            List<IParameter> paramList = new List<IParameter>();
            int locusSearchStart = 0;
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

        private bool nameEquals(string name, string compareWith)
        {
            return (name.ToLower().CompareTo(compareWith.ToLower()) == 0);
        }

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

            var commaCount = XSharpVsSignature.CalculateCommaPosition(sigText,sigText.Length);

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
                this.m_session.SelectedSignature = signatures[0];
                var sig = this.m_session.SelectedSignature as XSharpVsSignature;
                sig.CurrentParameter = signatures[0].Parameters[commaCount];
            }
        }
    }
}
