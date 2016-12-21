using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Language.Intellisense;
using System.Collections.ObjectModel;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using System.Windows.Media;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Reflection;

namespace XSharpLanguage
{
    [Export(typeof(ICompletionSourceProvider))]
    [ContentType("XSharp")]
    [Name("XSharpCompletion")]
    class XSharpCompletionSourceProvider : ICompletionSourceProvider
    {
        [Import]
        internal SVsServiceProvider ServiceProvider = null;

        [Import]
        internal IGlyphService GlyphService = null;

        public ICompletionSource TryCreateCompletionSource(ITextBuffer textBuffer)
        {

            return new XSharpCompletionSource(this, textBuffer, GetFileName(textBuffer));
        }


        private string GetFileName(ITextBuffer buffer)
        {
            IVsTextBuffer bufferAdapter;
            buffer.Properties.TryGetProperty(typeof(IVsTextBuffer), out bufferAdapter);
            if (bufferAdapter != null)
            {
                var persistFileFormat = bufferAdapter as IPersistFileFormat;
                string ppzsFilename = null;
                uint iii;
                if (persistFileFormat != null)
                    persistFileFormat.GetCurFile(out ppzsFilename, out iii);
                return ppzsFilename;
            }
            return null;
        }
    }

    class XSharpCompletionSource : ICompletionSource
    {
        private ITextBuffer _buffer;
        private String _fileName;
        private bool _disposed = false;
        private XSharpCompletionSourceProvider _provider;
        public XSharpCompletionSource(XSharpCompletionSourceProvider provider, ITextBuffer buffer, String fileName)
        {
            _provider = provider;
            _buffer = buffer;
            _fileName = fileName;
        }

        public void AugmentCompletionSession(ICompletionSession session, IList<CompletionSet> completionSets)
        {
            if (_disposed)
                throw new ObjectDisposedException("XSharpCompletionSource");
            // Where does the StartSession has been triggered ?
            ITextSnapshot snapshot = _buffer.CurrentSnapshot;
            var triggerPoint = (SnapshotPoint)session.GetTriggerPoint(snapshot);
            if (triggerPoint == null)
                return;
            // What is the character were it starts ?
            var line = triggerPoint.GetContainingLine();
            SnapshotPoint start = triggerPoint;
            //while (start > line.Start && !char.IsWhiteSpace((start - 1).GetChar()))
            //{
            //    start -= 1;
            //}
            //
            var applicableTo = snapshot.CreateTrackingSpan(new SnapshotSpan(start, triggerPoint), SpanTrackingMode.EdgeInclusive);
            //
            XFile file = XSharpModel.XSolution.FindFullPath(this._fileName);
            if (file == null)
            {
                // Uhh !??, Something went wrong
                return;
            }
            // The Completion list we are building
            CompletionList compList = new CompletionList();
            // Build a list with the tokens we have from the TriggerPoint to the start of the line
            List<String> tokenList = this.GetTokenList(triggerPoint);
            // Check if we can get the member where we are
            XTypeMember member = this.FindMember(triggerPoint.Position);
            // LookUp for the BaseType, reading the TokenList (From left to right)
            CompletionType cType = this.RetrieveType(tokenList, member);
            if (cType != null)
            {
                Modifiers visibleAs = Modifiers.Public;
                if (member.ParentName == cType.FullName)
                {
                    visibleAs = Modifiers.Private;
                }
                // Now, Fill the CompletionList with the available members, from there
                BuildCompletionList(compList, cType, visibleAs);
            }
            else
            {
                if (member != null) // Fill with the context ( Parameters and Locals )
                {
                    BuildCompletionList(compList, member);
                }
                // Now Add Functions and Procedures
                BuildCompletionList(compList, file.Project.GlobalType);
            }
            //
            compList.Sort((comp1, comp2) => comp1.DisplayText.CompareTo(comp2.DisplayText));
            //
            completionSets.Add(new CompletionSet("All", "All", applicableTo, compList, Enumerable.Empty<Completion>()));
        }

        private CompletionType RetrieveType(List<string> tokenList, XTypeMember currentMember)
        {
            if (currentMember == null)
            {
                System.Diagnostics.Debug.WriteLine(String.Format("Retrieve current Type for {0} : Cannot find Member.", this._fileName));
                return null;
            }
            // we have to walk the tokenList, searching for the current Type
            // As we have separators every even token, we will walk by step 2
            int currentPos = 0;
            String currentToken = "";
            XElement element = null;
            CompletionType cType = null;
            if (tokenList.Count == 0)
                return null;
            //
            while (currentPos < tokenList.Count)
            {
                currentToken = tokenList[currentPos];
                // Search in Parameters
                element = currentMember.Parameters.Find(x => x.Name.ToLower() == currentToken.ToLower());
                if (element == null)
                {
                    // then Locals
                    element = currentMember.Locals.Find(x => x.Name.ToLower() == currentToken.ToLower());
                    if (element == null)
                    {
                        // then Properties
                        // The XTypeMember has a Parent, which has members (at least the currentMemeber !)
                        if (currentMember.Parent is XType)
                        {
                            XType prt = currentMember.Parent as XType;
                            element = prt.Members.Find(x =>
                            {
                                if ((x.Kind == Kind.Property) || (x.Kind == Kind.Access) || (x.Kind == Kind.Assign))
                                {
                                    return (x.Name.ToLower() == currentToken.ToLower());
                                }
                                return false;
                            });
                            //
                        }
                    }
                    else
                    {
                        cType = new CompletionType((XVariable)element);
                        if (!cType.IsInitialized)
                        {
                            cType = null;
                        }
                    }
                }
                else
                {
                    cType = new CompletionType(element);
                }
                // Next Token
                currentPos += 2;
                if (currentPos >= tokenList.Count)
                {
                    break;
                }
                //
            }
            return cType;
        }

        private void BuildCompletionList(CompletionList compList, XTypeMember currentMember)
        {
            if (currentMember == null)
            {
                System.Diagnostics.Debug.WriteLine(String.Format("Building Completion for {0} : Cannot find Member.", this._fileName));
                return;
            }
            // First, look after Parameters
            foreach (XVariable paramVar in currentMember.Parameters)
            {
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(paramVar.GlyphGroup, paramVar.GlyphItem);
                compList.Add(new XSCompletion(paramVar.Name, paramVar.Name, paramVar.Description, icon, null));
            }
            // Then, look for Locals
            // First, look after Parameters
            foreach (XVariable localVar in currentMember.Locals)
            {
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(localVar.GlyphGroup, localVar.GlyphItem);
                compList.Add(new XSCompletion(localVar.Name, localVar.Name, localVar.Description, icon, null));
            }
            // Ok, now look for Members of the Owner of the member... So, the class a Method
            //BuildCompletionList(compList, currentMember.Parent, Modifiers.Private);
            //
        }

        private void BuildCompletionList(CompletionList compList, XElement parent)
        {
            BuildCompletionList(compList, parent, Modifiers.Public);
        }
        private void BuildCompletionList(CompletionList compList, XElement parent, Modifiers minVisibility)
        {
            if (parent == null)
            {
                System.Diagnostics.Debug.WriteLine(String.Format("Building Completion for {0} : Cannot find Parent.", this._fileName));
                return;
            }
            if (!(parent is XType))
            {
                System.Diagnostics.Debug.WriteLine(String.Format("Building Completion for {0} : Parent {1} is NOT XType.", this._fileName, parent.Name));
                return;
            }
            //
            XType Owner = parent as XType;
            //
            foreach (XTypeMember elt in Owner.Members)
            {
                if (elt.Kind == Kind.Constructor)
                    continue;
                if (elt.IsStatic)
                    continue;
                if (elt.Visibility < minVisibility)
                    continue;
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(elt.GlyphGroup, elt.GlyphItem);
                String toAdd = "";
                if ((elt.Kind == Kind.Method) || (elt.Kind == Kind.Function) || (elt.Kind == Kind.Procedure))
                {
                    toAdd = "(";
                }
                compList.Add(new XSCompletion(elt.Name, elt.Name + toAdd, elt.Description, icon, null));
            }
            // Hummm, we should call for Owner of the Owner.. Super !
            if (Owner.Parent != null)
            {
                BuildCompletionList(compList, parent, Modifiers.Protected);
            }
        }

        private void BuildCompletionList(CompletionList compList, CompletionType cType)
        {
            BuildCompletionList(compList, cType, Modifiers.Public);
        }
        private void BuildCompletionList(CompletionList compList, CompletionType cType, Modifiers minVisibility)
        {
            if (cType == null)
            {
                System.Diagnostics.Debug.WriteLine(String.Format("Building Completion for {0} : Cannot find CompletionType.", this._fileName));
                return;
            }
            //
            if (cType.XType != null)
            {
                FillMembers(compList, cType.XType, minVisibility, false);
                // Hummm, we should call for Owner of the Owner.. Super !
                if (cType.XType.Parent != null)
                {
                    BuildCompletionList(compList, new CompletionType(cType.XType.Parent), Modifiers.Protected);
                }
                else if (cType.XType.ParentName != null)
                {
                    BuildCompletionList(compList, new CompletionType(cType.XType.ParentName, cType.XType.File.Usings), Modifiers.Protected);
                }
            }
            else if (cType.SType != null)
            {
                // Now add Members for System types
                FillMembers(compList, cType.SType, minVisibility, false);
            }
        }

        /// <summary>
        /// Add Members for our Project Types
        /// </summary>
        /// <param name="compList"></param>
        /// <param name="xType"></param>
        /// <param name="minVisibility"></param>
        private void FillMembers(CompletionList compList, XType xType, Modifiers minVisibility, bool staticOnly)
        {
            // Add Members for our Project Types
            foreach (XTypeMember elt in xType.Members)
            {
                if (elt.Kind == Kind.Constructor)
                    continue;
                if (elt.IsStatic)
                    continue;
                if (elt.Visibility < minVisibility)
                    continue;
                //
                ImageSource icon = _provider.GlyphService.GetGlyph(elt.GlyphGroup, elt.GlyphItem);
                String toAdd = "";
                if ((elt.Kind == Kind.Method) || (elt.Kind == Kind.Function) || (elt.Kind == Kind.Procedure))
                {
                    toAdd = "(";
                }
                compList.Add(new XSCompletion(elt.Name, elt.Name + toAdd, elt.Description, icon, null));
            }
        }

        /// <summary>
        /// Add Members from System Types
        /// </summary>
        /// <param name="compList"></param>
        /// <param name="sType"></param>
        /// <param name="minVisibility"></param>
        private void FillMembers(CompletionList compList, System.Type sType, Modifiers minVisibility, bool staticOnly)
        {
            MemberInfo[] members;
            //
            if (minVisibility < Modifiers.Public)
            {
                // Get Public, Internal, Protected & Private Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                members = sType.GetMembers(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly);
            }
            else
            {
                //  Get Public Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                members = sType.GetMembers();
            }
            //
            foreach (var member in members)
            {
                MemberAnalysis analysis = new MemberAnalysis(member);
                if ((analysis.IsInitialized) && (minVisibility <= analysis.Visibility))
                {
                    if (analysis.Kind == Kind.Constructor)
                        continue;
                    if ( analysis.IsStatic && !staticOnly)
                    {
                        continue;
                    }
                    String toAdd = "";
                    if ((analysis.Kind == Kind.Method))
                    {
                        toAdd = "(";
                    }
                    //
                    ImageSource icon = _provider.GlyphService.GetGlyph(analysis.GlyphGroup, analysis.GlyphItem);
                    compList.Add(new XSCompletion(analysis.Name, analysis.Name + toAdd, analysis.Description, icon, null));
                }
            }
        }

        private XTypeMember FindMember(int position)
        {
            XFile file = XSharpModel.XSolution.FindFullPath(this._fileName);
            if (file == null)
            {
                // Uhh !??, Something went wrong
                System.Diagnostics.Debug.WriteLine(String.Format("Cannot find file {0} .", this._fileName));
                return null;
            }
            //
            foreach (XType eltType in file.TypeList)
            {
                if (eltType.Interval.ContainsInclusive(position))
                {
                    foreach (XTypeMember elt in eltType.Members)
                    {
                        if (elt.Interval.ContainsInclusive(position))
                        {
                            return elt;
                        }
                    }
                }
            }
            //
            System.Diagnostics.Debug.WriteLine(String.Format("Cannot find member as position {0} in file {0} .", position, this._fileName));
            return null;
        }

        /// <summary>
        /// Starting at TriggerPoint, get the current Text, and using the XSharpLexer, build a TokenList :
        ///  First, walk the tokens to pass the TriggerPoint.
        ///  Then, walk back, build a TokenList upto the first WhiteSpace
        /// </summary>
        /// <param name="triggerPoint"></param>
        /// <returns></returns>
        private List<String> GetTokenList(SnapshotPoint triggerPoint)
        {
            List<String> tokenList = new List<string>();
            String token;
            // lex the entire document
            var stream = new AntlrInputStream(_buffer.CurrentSnapshot.GetText());
            var lexer = new XSharpLexer(stream);
            var tokens = new CommonTokenStream(lexer);
            tokens.Fill();

            // locate the last token before the trigger point
            IToken nextToken;
            while (true)
            {
                nextToken = tokens.Lt(1);
                if (nextToken.Type == XSharpLexer.Eof) // End Of File
                    break;

                if (nextToken.StartIndex > triggerPoint.Position)
                    break;

                tokens.Consume();
            }
            nextToken = tokens.Lt(-1);
            // Now, let's build the Token chain, so we can guess what to add in the CompletionList
            IToken triggerToken = GetPreviousToken(tokens, nextToken);
            while (triggerToken != null)
            {

                token = triggerToken.Text;
                switch (triggerToken.Type)
                {
                    case XSharpLexer.RPAREN:
                        // Search for the Left Parenthesis
                        triggerToken = ProcessBounds(tokens, triggerToken, XSharpLexer.LPAREN, XSharpLexer.RPAREN);
                        // we had a trouble in the previous process ?
                        if (triggerToken == null)
                            break;
                        // ...
                        token = "()";
                        break;
                    case XSharpLexer.RBRKT:
                        // Search for the Left Bracket
                        triggerToken = ProcessBounds(tokens, triggerToken, XSharpLexer.LBRKT, XSharpLexer.RBRKT);
                        // we had a trouble in the previous process ?
                        if (triggerToken == null)
                            break;
                        // ...
                        token = "[]";
                        break;
                    case XSharpLexer.RCURLY:
                        // Search for the Left Curly
                        triggerToken = ProcessBounds(tokens, triggerToken, XSharpLexer.LCURLY, XSharpLexer.RCURLY);
                        // we had a trouble in the previous process ?
                        if (triggerToken == null)
                            break;
                        // ...
                        token = "{}";
                        break;

                }
                //
                tokenList.Add(token);
                //
                triggerToken = GetPreviousToken(tokens, triggerToken);
            }
            // 
            tokenList.Reverse();
            return tokenList;
        }

        /// <summary>
        /// Walk back the CommonTokenStream from the currentToken, upto a non WhiteSpace token
        /// </summary>
        /// <param name="tokens">The Stream where to look at</param>
        /// <param name="currentToken">The Starting token</param>
        /// <returns></returns>
        private IToken GetPreviousToken(CommonTokenStream tokens, IToken currentToken)
        {
            IToken prev = null;
            if (currentToken != null)
            {
                prev = currentToken;
                int Line = prev.Line;
                do
                {
                    prev = tokens.Get(prev.TokenIndex - 1);
                    if (prev.Line != Line)
                    {
                        prev = null;
                    }
                } while ((prev != null) && (prev.Type == XSharpLexer.WS));
            }
            return prev;
        }

        /// <summary>
        /// Walk back the CommonTokenStream from the currentToken.
        /// We started with a RightElement ( { [, and we are looking for the corresponding LeftElement ] } )
        /// </summary>
        /// <param name="tokens"></param>
        /// <param name="currentToken"></param>
        /// <param name="LeftElement"></param>
        /// <param name="RightElement"></param>
        /// <returns></returns>
        private IToken ProcessBounds(CommonTokenStream tokens, IToken currentToken, int LeftElement, int RightElement)
        {
            // Count the elements
            int rightElt = 1;
            while ((currentToken != null) && (rightElt != 0))
            {
                //currentToken = tokens.Lt(-1);
                currentToken = GetPreviousToken(tokens, currentToken);
                if (currentToken != null)
                {
                    // Bump the Left ?
                    if (currentToken.Column == 0)
                    {
                        if (rightElt != 0)
                        {
                            // If the counter is not null, we have a open element
                            currentToken = null;
                        }
                    }
                    // A Left Element ?
                    else if (currentToken.Type == LeftElement)
                    {
                        rightElt--;
                    }
                    // Another Right Parenthesis
                    else if (currentToken.Type == RightElement)
                    {
                        rightElt++;
                    }
                }
            }
            //
            return currentToken;
        }

        public void Dispose()
        {
            _disposed = true;
        }



    }


    internal class MemberAnalysis
    {
        class ParamInfo
        {
            public String Name;
            public String TypeName;

            internal ParamInfo(String n, String t)
            {
                this.Name = n;
                this.TypeName = t;
            }
        }

        private String _name;
        private Modifiers _modifiers;
        private Modifiers _visibility;
        private Kind _kind;
        private bool _isStatic;
        private String _typeName;
        private List<ParamInfo> _parameters;

        internal MemberAnalysis(MemberInfo member)
        {
            Type declType;
            //
            this._name = member.Name;
            this._kind = Kind.Class;
            this._modifiers = Modifiers.None;
            this._visibility = Modifiers.Public;
            this._typeName = "";
            this._parameters = new List<ParamInfo>();
            //
            switch (member.MemberType)
            {
                case MemberTypes.Constructor:
                    this._kind = Kind.Constructor;
                    ConstructorInfo constInfo = member as ConstructorInfo;
                    this._isStatic = constInfo.IsStatic;
                    //
                    if (constInfo.IsAbstract)
                    {
                        this._modifiers = Modifiers.Abstract;
                    }
                    //
                    if (constInfo.IsPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    if (constInfo.IsAssembly)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    if (constInfo.IsFamily)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    declType = constInfo.DeclaringType;
                    this._typeName = declType.FullName;
                    break;
                case MemberTypes.Event:
                    this._kind = Kind.Event;
                    EventInfo evt = member as EventInfo;
                    MethodInfo methodInfo = evt.GetAddMethod(true);
                    if (methodInfo == null)
                    {
                        methodInfo = evt.GetRemoveMethod(true);
                    }
                    //
                    this._isStatic = methodInfo.IsStatic;
                    //
                    if (methodInfo.IsAbstract)
                    {
                        this._modifiers = Modifiers.Abstract;
                    }
                    //
                    if (methodInfo.IsPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    if (methodInfo.IsAssembly)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    if (methodInfo.IsFamily)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    declType = evt.EventHandlerType;
                    this._typeName = declType.FullName;
                    break;
                case MemberTypes.Field:
                    this._kind = Kind.ClassVar;
                    FieldInfo field = member as FieldInfo;
                    //
                    this._isStatic = field.IsStatic;
                    //
                    if (field.IsPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    if (field.IsAssembly)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    if (field.IsFamily)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    declType = field.FieldType;
                    this._typeName = declType.FullName;
                    break;
                case MemberTypes.Method:
                    this.Kind = Kind.Method;
                    MethodInfo method = member as MethodInfo;
                    if (method.IsSpecialName)
                    {
                        // The SpecialName bit is set to flag members that are treated in a special way by some compilers (such as property accessors and operator overloading methods).
                        this._name = null;
                        break;
                    }
                    //
                    this._isStatic = method.IsStatic;
                    //
                    if (method.IsPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    if (method.IsAssembly)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    if (method.IsFamily)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    ParameterInfo[] pars = method.GetParameters();
                    foreach (ParameterInfo p in pars)
                    {
                        this._parameters.Add(new ParamInfo(p.Name, p.ParameterType.FullName));
                    }
                    //
                    declType = method.ReturnType;
                    this._typeName = declType.FullName;
                    break;
                case MemberTypes.Property:
                    this.Kind = Kind.Property;
                    PropertyInfo prop = member as PropertyInfo;
                    MethodInfo propInfo = prop.GetGetMethod(true);
                    if (propInfo == null)
                    {
                        propInfo = prop.GetSetMethod(true);
                    }
                    //
                    this._isStatic = propInfo.IsStatic;
                    //
                    if (propInfo.IsPrivate)
                    {
                        this._visibility = Modifiers.Private;
                    }
                    if (propInfo.IsAssembly)
                    {
                        this._visibility = Modifiers.Internal;
                    }
                    if (propInfo.IsFamily)
                    {
                        this._visibility = Modifiers.Protected;
                    }
                    //
                    declType = prop.PropertyType;
                    this._typeName = declType.FullName;
                    break;
                default:
                    // Mark as Not-Initialized
                    this._name = null;
                    break;
            }
        }

        public bool IsInitialized
        {
            get
            {
                return (this.Name != null);
            }
        }

        public string Name
        {
            get
            {
                return _name;
            }
        }

        public String Description
        {
            get
            {
                String modVis = "";
                if (this.Modifiers != Modifiers.None)
                {
                    modVis += this.Modifiers.ToString() + " ";
                }
                modVis += this.Visibility.ToString() + " ";
                //
                String desc = modVis;
                //
                if (this.Kind != Kind.ClassVar)
                    desc += this.Kind.ToString() + " ";
                desc += this.Prototype;
                //
                if ((this.Kind == Kind.Method) || (this.Kind == Kind.Function) || (this.Kind == Kind.Property) || (this.Kind == Kind.ClassVar) || (this.Kind == Kind.Event))
                {
                    desc += " as " + this.TypeName;
                }
                //
                return desc;
            }
        }

        public String Prototype
        {
            get
            {
                if ((this.Kind == Kind.Property) || (this.Kind == Kind.Access) || (this.Kind == Kind.ClassVar) || (this.Kind == Kind.Event))
                    return this.Name;
                //
                String vars = "";
                foreach (var var in this.Parameters)
                {
                    if (vars.Length > 0)
                        vars += ", ";
                    vars += var.Name + " as " + var.TypeName;
                }
                //
                String desc = "";
                desc += this.Name;
                desc += "(";
                desc += vars;
                desc += ")";
                //
                return desc;
            }
        }

        public StandardGlyphGroup GlyphGroup
        {
            get
            {
                StandardGlyphGroup imgG = StandardGlyphGroup.GlyphGroupClass;
                //
                switch (this.Kind)
                {
                    case Kind.Class:
                        imgG = StandardGlyphGroup.GlyphGroupClass;
                        break;
                    case Kind.Constructor:
                    case Kind.Destructor:
                    case Kind.Method:
                    case Kind.Function:
                    case Kind.Procedure:
                        imgG = StandardGlyphGroup.GlyphGroupMethod;
                        break;
                    case Kind.Structure:
                        imgG = StandardGlyphGroup.GlyphGroupStruct;
                        break;
                    case Kind.Access:
                    case Kind.Assign:
                    case Kind.Property:
                        imgG = StandardGlyphGroup.GlyphGroupProperty;
                        break;
                    case Kind.Local:
                        imgG = StandardGlyphGroup.GlyphGroupProperty;
                        break;
                }
                return imgG;
            }
        }

        /// <summary>
        /// Glyph Item used by CompletionList in CompletionSource
        /// - See also GlyphGroup
        ///  http://glyphlist.azurewebsites.net/standardglyphgroup/
        /// </summary>
        public StandardGlyphItem GlyphItem
        {
            get
            {
                StandardGlyphItem imgI = StandardGlyphItem.GlyphItemPublic;
                //
                switch (this.Visibility)
                {
                    case Modifiers.Public:
                        imgI = StandardGlyphItem.GlyphItemPublic;
                        break;
                    case Modifiers.Protected:
                        imgI = StandardGlyphItem.GlyphItemProtected;
                        break;
                    case Modifiers.Private:
                        imgI = StandardGlyphItem.GlyphItemPrivate;
                        break;
                    case Modifiers.Internal:
                        imgI = StandardGlyphItem.GlyphItemInternal;
                        break;
                    case Modifiers.ProtectedInternal:
                        imgI = StandardGlyphItem.GlyphItemProtected;
                        break;

                }
                //
                return imgI;
            }
        }

        public Kind Kind
        {
            get
            {
                return _kind;
            }

            set
            {
                _kind = value;
            }
        }

        public Modifiers Modifiers
        {
            get
            {
                return _modifiers;
            }
        }

        public Modifiers Visibility
        {
            get
            {
                return _visibility;
            }
        }

        public string TypeName
        {
            get
            {
                return _typeName;
            }
        }

        private List<ParamInfo> Parameters
        {
            get
            {
                return _parameters;
            }

            set
            {
                _parameters = value;
            }
        }

        public bool IsStatic
        {
            get
            {
                return _isStatic;
            }

            set
            {
                _isStatic = value;
            }
        }
    }

    /// <summary>
    /// XSharp CompletionList
    /// Overload the Add() Method to support "overloads"
    /// </summary>
    internal class CompletionList : List<XSCompletion>
    {
        public new void Add( XSCompletion item)
        {
            int overloads = 0;
            //
            foreach ( XSCompletion comp in this)
            {
                // Search for the same Name
                if (comp.DisplayText == item.DisplayText)
                {
                    // Already exists in the List !!
                    // First Overload ?
                    if ( comp.Properties.ContainsProperty("overloads") )
                    {
                        // No ...
                        overloads = (int)comp.Properties.GetProperty("overloads");
                    }
                    else
                    {
                        comp.Properties.AddProperty("overloads", overloads);
                    }
                    overloads += 1;
                    // Set the number of Overload(s)
                    comp.Properties["overloads"] = overloads;
                    // Now, hack the Description text

                    // Ok, Forget about the newly added Completion please
                    return;
                }
            }
            // Unknown, Standard behaviour
            base.Add(item);
        }
    }

    /// <summary>
    /// XSharp Completion class.
    /// Overload the Description property in order to add "overload" text at the end
    /// </summary>
    public class XSCompletion : Completion
    {
        public XSCompletion(string displayText, string insertionText, string description, ImageSource iconSource, string iconAutomationText) 
            : base(displayText, insertionText, description, iconSource, iconAutomationText)
        {
        }

        public override string Description
        {
            get
            {
                string desc;
                int overloads=0;
                if (this.Properties.ContainsProperty("overloads"))
                {
                    // No ...
                    overloads = (int)this.Properties.GetProperty("overloads");
                }
                if ( overloads > 0 )
                {
                    desc = base.Description;
                    desc += " (+" + overloads + " overload";
                    if (overloads > 1)
                        desc += "s";
                    desc += ")";
                }
                else
                {
                    desc = base.Description;
                }
                //
                return desc;
            }
            set
            {
                base.Description = value;
            }
        }

    }
}

