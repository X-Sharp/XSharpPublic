/*****************************************************************************
 *
 * Copyright(c) Microsoft Corporation.
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A
* copy of the license can be found in the License.html file at the root of this distribution.If
* you cannot locate the Apache License, Version 2.0, please send an email to
* ironpy@microsoft.com.By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Apache License, Version 2.0.
 *
 * You must not remove this notice, or any other, from this software.
*
****************************************************************************/
/*****************************************************************************
* XSharp.BV
* Based on IronStudio/IronPythonTools/IronPythonTools/Navigation
*
****************************************************************************/

using System;
using System.Globalization;

using Microsoft.VisualStudio.Shell.Interop;
using ErrorHandler = Microsoft.VisualStudio.ErrorHandler;
using VSConstants = Microsoft.VisualStudio.VSConstants;
using XSharpModel;
using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.VisualStudio.Shell;
using XSharp.Settings;
namespace XSharp.LanguageService
{

    // Value are from OMGlyphs.h
    internal enum IconImageIndex
    {
        // access types
        AccessPublic = 0,
        AccessInternal = 1,
        AccessFriend = 2,
        AccessProtected = 3,
        AccessPrivate = 4,
        AccessShortcut = 5,

        Base = 6,
        // Each of the following icon type has 6 versions,
        //corresponding to the access types
        _Class = Base * 0,
        _Constant = Base * 1,
        _Delegate = Base * 2,
        _Enumeration = Base * 3,
        _EnumMember = Base * 4,
        _Event = Base * 5,
        _Exception = Base * 6,
        _Field = Base * 7,
        _Interface = Base * 8,
        _Macro = Base * 9,
        _Map = Base * 10,
        _MapItem = Base * 11,
        _Method = Base * 12,
        _OverloadedMethod = Base * 13,
        _Module = Base * 14,
        _Namespace = Base * 15,
        _Operator = Base * 16,
        _Property = Base * 17,
        _Struct = Base * 18,
        _Template = Base * 19,
        _Typedef = Base * 20,
        _Type = Base * 21,
        _Union = Base * 22,
        _VVariable = Base * 23,
        _ValueType = Base * 24,
        _Intrinsic = Base * 25,
        _JavaMethod = Base * 26,
        _JavaField = Base * 27,
        _JavaClass = Base * 28,
        _JavaNamespace = Base * 29,
        _JavaInterface = Base * 30,
        // Miscellaneous icons with one icon for each type.
        _Error = 187,
        _GreyedClass = 188,
        _GreyedPrivateMethod = 189,
        _GreyedProtectedMethod = 190,
        _GreyedPublicMethod = 191,
        _BrowseResourceFile = 192,
        _Reference = 193,
        _Library = 194,
        _VBProject = 195,
        _VBWebProject = 196,
        _CSProject = 197,
        _CSWebProject = 198,
        _VB6Project = 199,
        _CPlusProject = 200,
        _Form = 201,
        _OpenFolder = 201,
        _ClosedFolder = 202,
        _Arrow = 203,
        _CSClass = 205,
        _Snippet = 206,
        _Keyword = 206,
        _Info = 207,
        _CallBrowserCall = 208,
        _CallBrowserCallRecursive = 210,
        _XMLEditor = 211,
        _VJProject = 212,
        _VJClass = 213,
        _ForwardedType = 214,
        _CallsTo = 215,
        _CallsFrom = 216,
        _Warning = 217,
    };

    internal class SourcePosition
    {
        internal string FileName { get; set; }
        internal int Line { get; set; }
        internal int Column { get; set; }
    }
    [DebuggerDisplay("{Name}")]
    internal class XSharpLibraryNode : LibraryNode
    {
        internal IVsHierarchy ownerHierarchy;
        internal List<uint> filesId;
        private string fileMoniker;
        // Description Infos...
        private List<Tuple<string, VSOBDESCRIPTIONSECTION>> description;
        private readonly SourcePosition editorInfo;
        private string nodeText;
        // ClassName & Namespace
        private string nameSpace = "";
        private string className = "";
        private readonly Kind kind;
        private readonly Modifiers attributes;
        const string NEWLINE = "\n";
        const string CONSTRUCTOR = "Constructor";
        const string DEFAULTNAMESPACE = "Default NameSpace";
        const string SUMMARY = "\nSummary:\n";
        const string PARAMETERS = "\nParameters:";
        const string RETURNS = "\nReturns:\n";
        const string REMARKS = "\nRemarks:\n";
        internal string FullPath { get; } = "";
		
        internal XSharpLibraryNode(string namePrefix, LibraryNodeType nType, string path)
            : base(namePrefix)
        {
            //
            this.filesId = new List<uint>();
            FullPath = path;
            //
            this.ownerHierarchy = null;
            this.Depends(0);
            //this.member = null;
            this.NodeType = nType;
            if (this.NodeType == LibraryNodeType.Namespaces)
            {
                BuildImageData(Kind.Namespace, Modifiers.Public);
            }
            //
            description = new List<Tuple<string, VSOBDESCRIPTIONSECTION>>();
            editorInfo = null;
        }


        internal XSharpLibraryNode(XSourceEntity entity, string namePrefix, IVsHierarchy hierarchy, uint itemId)
            : base(String.IsNullOrEmpty(entity.FullName) ? namePrefix : entity.FullName)
        {
            kind = entity.Kind;
            attributes = entity.Attributes;
            if (kind == Kind.Namespace)
            {
                this.NodeType = LibraryNodeType.Namespaces;
            }
            else if (kind.IsType())
            {
                this.NodeType = LibraryNodeType.Classes;
            }
            else
            {
                this.NodeType = LibraryNodeType.Members;
            }
            //
            this.filesId = new List<uint>();
            //
            this.ownerHierarchy = hierarchy;
            this.Depends(itemId);
            //this.member = scope;
            // Can we Goto ?
            if ((ownerHierarchy != null) && (VSConstants.VSITEMID_NIL != itemId))
            {
                this.CanGoToSource = true;
                this.editorInfo = new SourcePosition()
                {
                    FileName = entity.File.FullPath,
                    Line = entity.Range.StartLine + 1,
                    Column = entity.Range.StartColumn,
                };
            }
            //
            this.BuildImageData(entity.Kind, entity.Visibility);
            this.InitText(entity);
            this.InitDescription(entity);
        }

        private void BuildImageData(Kind elementType, Modifiers accessType)
        {
            int iImage = 0;
            // First get the Icon
            switch (elementType)
            {
                case Kind.Class:
                    iImage = (int)IconImageIndex._Class;
                    break;
                case Kind.Structure:
                case Kind.VOStruct:
                    iImage = (int)IconImageIndex._Struct;
                    break;
                case Kind.Union:
                    iImage = (int)IconImageIndex._Union;
                    break;
                case Kind.Delegate:
                    iImage = (int)IconImageIndex._Delegate;
                    break;
                case Kind.Namespace:
                    iImage = (int)IconImageIndex._Namespace;
                    break;
                case Kind.Constructor:
                case Kind.Destructor:
                case Kind.Method:
                case Kind.Function:
                case Kind.Procedure:
                case Kind.LocalFunc:
                case Kind.LocalProc:
                case Kind.VODLL:
                    iImage = (int)IconImageIndex._Method;
                    break;
                case Kind.Property:
                case Kind.Access:
                case Kind.Assign:
                    iImage = (int)IconImageIndex._Property;
                    break;
                case Kind.Field:
                case Kind.VOGlobal:
                    iImage = (int)IconImageIndex._Field;
                    break;
                case Kind.Interface:
                    iImage = (int)IconImageIndex._Interface;
                    break;
                case Kind.Event:
                    iImage = (int)IconImageIndex._Event;
                    break;
                case Kind.Operator:
                    iImage = (int)IconImageIndex._Operator;
                    break;
                case Kind.Enum:
                    iImage = (int)IconImageIndex._Enumeration;
                    break;
                case Kind.VODefine:
                    iImage = (int)IconImageIndex._Constant;
                    break;
                case Kind.EnumMember:
                    iImage = (int)IconImageIndex._EnumMember;
                    break;
                case Kind.Local:
                case Kind.Parameter:
                case Kind.DbField:
                    iImage = (int)IconImageIndex._VVariable;
                    break;
                case Kind.Using:
                    iImage = (int)IconImageIndex._Reference;
                    break;
                default:
                    break;

            }
            //
            switch (accessType)
            {
                case Modifiers.Public:
                    iImage += (int)IconImageIndex.AccessPublic;
                    break;
                case Modifiers.Hidden:
                    iImage += (int)IconImageIndex.AccessPrivate;
                    break;
                case Modifiers.Protected:
                    iImage += (int)IconImageIndex.AccessProtected;
                    break;
                case Modifiers.Internal:
                    iImage += (int)IconImageIndex.AccessInternal;
                    break;
            }
            //
            this.displayData.Image = (ushort)iImage;
            this.displayData.SelectedImage = (ushort)iImage;
        }

        internal XSharpLibraryNode(XSharpLibraryNode node) :
            base(node)
        {
            this.filesId = new List<uint>();
            this.Depends(node.filesId);
            this.ownerHierarchy = node.ownerHierarchy;
            this.fileMoniker = node.fileMoniker;
            //this.member = node.member;
            this.NodeType = node.NodeType;
            this.editorInfo = node.editorInfo;
            this.description = node.description;
            this.kind = node.kind;
            this.attributes = node.attributes;


        }


        /// <summary>
        /// Indicate that the Node belongs to a file/Module
        /// </summary>
        /// <param name="itemId">The File/Module Id</param>
        /// <returns>The number of Files/Modules that points to that node</returns>
        public int Depends(uint itemId)
        {
            this.filesId.Add(itemId);
            return this.filesId.Count;
        }

        /// <summary>
        /// Indicate that the Node belongs to a list of files/Modules
        /// </summary>
        /// <param name="itemId">The List of Files/Modules Id</param>
        /// <returns>The number of Files/Modules that points to that node</returns>
        public int Depends(List<uint> itemsId)
        {
            this.filesId.AddRange(itemsId);
            return this.filesId.Count;
        }

        /// <summary>
        /// Remove that File/Module Id from the Node owner's
        /// </summary>
        /// <param name="itemId">The File/Module Id </param>
        /// <returns>The number of Files/Modules that continues to point to that node</returns>
        public int Freeing(uint itemId)
        {
            this.filesId.Remove(itemId);
            return this.filesId.Count;
        }

        private uint ClassAccess()
        {
            _LIBCAT_CLASSACCESS result = 0;
            if ((attributes & Modifiers.VisibilityMask) == Modifiers.None)
                result |= _LIBCAT_CLASSACCESS.LCCA_PUBLIC;
            else
            {
                if (attributes.HasFlag(Modifiers.Sealed))
                    result |= _LIBCAT_CLASSACCESS.LCCA_SEALED;
                if (attributes.HasFlag(Modifiers.Protected))
                    result |= _LIBCAT_CLASSACCESS.LCCA_PROTECTED;
                if (attributes.HasFlag(Modifiers.Private))
                    result |= _LIBCAT_CLASSACCESS.LCCA_PRIVATE;
                if (attributes.HasFlag(Modifiers.Public))
                    result |= _LIBCAT_CLASSACCESS.LCCA_PUBLIC;
                if (attributes.HasFlag(Modifiers.Internal))
                    result |= _LIBCAT_CLASSACCESS.LCCA_FRIEND;
            }
            return (uint)result;
        }

        private uint MemberAccess()
        {
            _LIBCAT_MEMBERACCESS result = 0;
            if ((attributes & Modifiers.VisibilityMask) == Modifiers.None)
                result |= _LIBCAT_MEMBERACCESS.LCMA_PUBLIC;
            else
            {
                if (attributes.HasFlag(Modifiers.Sealed))
                    result |= _LIBCAT_MEMBERACCESS.LCMA_SEALED;
                if (attributes.HasFlag(Modifiers.Protected))
                    result |= _LIBCAT_MEMBERACCESS.LCMA_PROTECTED;
                if (attributes.HasFlag(Modifiers.Private))
                    result |= _LIBCAT_MEMBERACCESS.LCMA_PRIVATE;
                if (attributes.HasFlag(Modifiers.Public))
                    result |= _LIBCAT_MEMBERACCESS.LCMA_PUBLIC;
                if (attributes.HasFlag(Modifiers.Internal))
                    result |= _LIBCAT_MEMBERACCESS.LCMA_FRIEND;
            }
            return (uint) result;
        }
        private _LIBCAT_MEMBERTYPE MemberType()
        {
            switch (kind)
            {
                case Kind.Method:
                case Kind.Constructor:
                case Kind.Destructor:
                    return _LIBCAT_MEMBERTYPE.LCMT_METHOD;
                case Kind.Function:
                case Kind.LocalFunc:
                case Kind.Procedure:
                case Kind.LocalProc:
                    return _LIBCAT_MEMBERTYPE.LCMT_FUNCTION;
                case Kind.Operator:
                    return _LIBCAT_MEMBERTYPE.LCMT_OPERATOR;
                case Kind.Property:
                case Kind.Access:
                case Kind.Assign:
                    return _LIBCAT_MEMBERTYPE.LCMT_PROPERTY;
                case Kind.Field:
                case Kind.VOGlobal:
                    return _LIBCAT_MEMBERTYPE.LCMT_FIELD;
                case Kind.Local:
                case Kind.Parameter:
                case Kind.DbField:
                case Kind.MemVar:
                    return _LIBCAT_MEMBERTYPE.LCMT_VARIABLE;
                case Kind.Event:
                    return _LIBCAT_MEMBERTYPE.LCMT_EVENT;
                case Kind.VODefine:
                    return _LIBCAT_MEMBERTYPE.LCMT_CONSTANT;
                case Kind.EnumMember:
                    return _LIBCAT_MEMBERTYPE.LCMT_ENUMITEM;
                case Kind.Class:
                case Kind.Structure:
                case Kind.VOStruct:
                case Kind.Interface:
                case Kind.Enum:
                case Kind.Delegate:
                case Kind.Union:
                    return _LIBCAT_MEMBERTYPE.LCMT_TYPEDEF;
                default:
                    return _LIBCAT_MEMBERTYPE.LCMT_ERROR;
            }
        }

        private _LIBCAT_CLASSTYPE ClassType()
        {
            switch (kind)
            {
                case Kind.Class:
                    return _LIBCAT_CLASSTYPE.LCCT_CLASS;
                case Kind.Interface:
                    return _LIBCAT_CLASSTYPE.LCCT_INTERFACE;
                case Kind.Structure:
                case Kind.VOStruct:
                case Kind.Union:
                    return _LIBCAT_CLASSTYPE.LCCT_STRUCT;
                case Kind.Enum:
                    return _LIBCAT_CLASSTYPE.LCCT_ENUM;
                // LCCT_MODULE
                // LCCT_INTRINSIC 
                case Kind.Delegate:
                    return _LIBCAT_CLASSTYPE.LCCT_DELEGATE;
                // LCCT_EXCEPTION
                // LCCT_MAP
                // LCCT_GLOBAL
                default:
                    return _LIBCAT_CLASSTYPE.LCCT_ERROR;
            }
        }

        private _LIBCAT_MODIFIERTYPE ModifierType()
        {
            _LIBCAT_MODIFIERTYPE result = (_LIBCAT_MODIFIERTYPE)0;
            if (attributes.HasFlag(Modifiers.Static))
                result |= _LIBCAT_MODIFIERTYPE.LCMDT_STATIC;
            if (attributes.HasFlag(Modifiers.Virtual))
                result |= _LIBCAT_MODIFIERTYPE.LCMDT_VIRTUAL;
            else if (attributes.HasFlag(Modifiers.Override))
                result |= _LIBCAT_MODIFIERTYPE.LCMDT_VIRTUAL;
            else
                result |= _LIBCAT_MODIFIERTYPE.LCMDT_NONVIRTUAL;
            if (attributes.HasFlag(Modifiers.Sealed))
                result |= _LIBCAT_MODIFIERTYPE.LCMDT_FINAL;

            return result;
        }

        protected override uint CategoryField(LIB_CATEGORY category)
        {
            switch (category)
            {
                case (LIB_CATEGORY)_LIB_CATEGORY2.LC_MEMBERINHERITANCE:
                    if (this.NodeType == LibraryNodeType.Members)
                    {
                        return (uint)_LIBCAT_MEMBERINHERITANCE.LCMI_IMMEDIATE;
                    }
                    break;
                case LIB_CATEGORY.LC_MEMBERTYPE:
                    return (uint)MemberType();

                case LIB_CATEGORY.LC_MEMBERACCESS:
                    return this.MemberAccess();
                case LIB_CATEGORY.LC_CLASSTYPE:
                    return (uint) this.ClassType();
                case LIB_CATEGORY.LC_CLASSACCESS:
                    return (uint)this.ClassAccess();
                case LIB_CATEGORY.LC_ACTIVEPROJECT:
                    return (uint)_LIBCAT_ACTIVEPROJECT.LCAP_SHOWALWAYS;
                case LIB_CATEGORY.LC_LISTTYPE:
                    break;
                case LIB_CATEGORY.LC_VISIBILITY:
                    return (uint) _LIBCAT_VISIBILITY.LCV_VISIBLE;
                case LIB_CATEGORY.LC_MODIFIER:
                    return (uint) ModifierType();
                case LIB_CATEGORY.LC_NODETYPE:
                    break;
                default:
                    break;
            }
            return base.CategoryField(category);
        }

        internal override LibraryNode Clone()
        {
            return new XSharpLibraryNode(this);
        }

        protected override void GotoSource(VSOBJGOTOSRCTYPE gotoType)
        {
            // We do not support the "Goto Reference"
            if (gotoType == VSOBJGOTOSRCTYPE.GS_REFERENCE)
            {
                return;
            }
            //
            if (this.CanGoToSource && this.editorInfo != null)
            {
                // Need to retrieve the Project, then the File...
                var file = XSolution.FindFile(editorInfo.FileName);
                XSettings.OpenDocument(file.FullPath, editorInfo.Line, editorInfo.Column, true);
            }
        }

        protected override void SourceItems(out IVsHierarchy hierarchy, out uint itemId, out uint itemsCount)
        {
            hierarchy = ownerHierarchy;
            itemId = this.filesId[0];
            itemsCount = 1;
        }

        protected override void SourceContext(out string pbstrFilename, out uint pulLineNum)
        {
            // default
            pbstrFilename = null;
            pulLineNum = 0;
            //
            if (this.editorInfo != null)
            {
                pbstrFilename = this.editorInfo.FileName;
                pulLineNum = (uint)this.editorInfo.Line - 1;
            }
        }

        protected override void Text(VSTREETEXTOPTIONS tto, out string pbstrText)
        {
            string descText = this.Name;
            switch (tto)
            {
                case VSTREETEXTOPTIONS.TTO_PREFIX2:
                    //  
                    descText = nameSpace;
                    break;
                case VSTREETEXTOPTIONS.TTO_PREFIX:
                    //   
                    descText = className;
                    break;

                default:
                    if (nodeText != null)
                    {
                        descText = nodeText;
                        // No description for Project
                        if ((tto == VSTREETEXTOPTIONS.TTO_SEARCHTEXT) && (this.NodeType == LibraryNodeType.Package))
                        {
                            descText = "";
                        }
                    }
                    break;
            }
            pbstrText = descText;
        }

        private void InitText(XSourceEntity member)
        {
            string descText = this.Name;
            //
            if (member != null)
            {
                if (member.Parent is XSourceTypeSymbol symbol)
                {
                    nameSpace = symbol.Namespace;
                    className = symbol.Name;
                }
                //
                descText = member.Name;
                if (member is XSourceMemberSymbol)
                {
                    var tm = member as XSourceMemberSymbol;
                    descText = tm.Kind == Kind.Constructor ? CONSTRUCTOR : member.Name;
                    if (tm.Kind.HasParameters())
                    {
                        descText +=  tm.Kind == Kind.Constructor ? "{" : "( ";
                        if (tm.HasParameters)
                        {

                            //
                            descText += tm.ParameterList;
                        }
                        descText +=  tm.Kind == Kind.Constructor ? "}" : ") ";
                    }
                }
                else if (member is XSourceTypeSymbol)
                {
                    var tm = member as XSourceTypeSymbol;
                    if ((tm.Kind == Kind.Namespace) && (String.IsNullOrEmpty(descText)))
                    {
                        descText = DEFAULTNAMESPACE;
                    }
                }

            }
            nodeText = descText;
        }

        public string NodeText => nodeText ?? "";

        protected override void buildDescription(_VSOBJDESCOPTIONS flags, IVsObjectBrowserDescription3 obDescription)
        {
            ThreadHelper.JoinableTaskFactory.Run(async ( )=>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                obDescription.ClearDescriptionText();
                try
                {
                    foreach (var element in description)
                    {
                        obDescription.AddDescriptionText3(element.Item1, element.Item2, null);
                    }
                }
                catch { }

            });
        }

        private Tuple<string, VSOBDESCRIPTIONSECTION> Item (string item1, VSOBDESCRIPTIONSECTION item2)
        {
            return new Tuple<string, VSOBDESCRIPTIONSECTION>(item1, item2);
        }

        private void InitDescription(XSourceEntity member) //, _VSOBJDESCOPTIONS flags, IVsObjectBrowserDescription3 description)
        {
            description = new List<Tuple<string, VSOBDESCRIPTIONSECTION>>();
            string descText ;
            if (member != null)
            {
                string modifier = "";
                string access = "";
                if ((member is XSourceTypeSymbol) && (member.Kind != Kind.Namespace))
                {
                    modifier = member.Modifiers.ToDisplayString() ;
                    access = member.Visibility.ToDisplayString() ;
                }
                else if ((member is XSourceMemberSymbol) && ((member.Kind != Kind.Function) && (member.Kind != Kind.Procedure)))
                {
                    modifier = member.Modifiers.ToDisplayString();
                    access = member.Visibility.ToDisplayString() ;
                }
                //
                if (!string.IsNullOrEmpty(modifier))
                {
                    description.Add(Item(modifier + " ", VSOBDESCRIPTIONSECTION.OBDS_ATTRIBUTE));
                }
                //
                if (!string.IsNullOrEmpty(access))
                {
                    description.Add(Item(access + " ", VSOBDESCRIPTIONSECTION.OBDS_ATTRIBUTE));
                }
                // 
                if (member.Kind != Kind.Field)
                {
                    VSOBDESCRIPTIONSECTION descName = VSOBDESCRIPTIONSECTION.OBDS_MISC;
                    descText = XLiterals.FormatKeyword(member.Kind.DisplayName()) + " ";
                    if (member.Kind == Kind.Constructor)
                    {
                        descName = VSOBDESCRIPTIONSECTION.OBDS_NAME;
                    }
                    description.Add(Item(descText, descName));
                }
                if (member.Kind != Kind.Constructor)
                {
                    descText = member.Name;
                    description.Add(Item(descText, VSOBDESCRIPTIONSECTION.OBDS_NAME));
                }
                // Parameters ?
                if (member.Kind.HasParameters())
                {
                    descText = "(";
                    description.Add(Item(descText, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                    XSourceMemberSymbol realmember;
                    XSourceTypeSymbol type = member as XSourceTypeSymbol;
                    if (member.Kind == Kind.Delegate && type?.XMembers.Count > 0)
                        realmember = type.XMembers[0] ;
                    else
                        realmember = member as XSourceMemberSymbol;

                    if (realmember != null && realmember.HasParameters)
                    {
                        //
                        int paramNum = 1;
                        foreach (IXParameterSymbol param in realmember.Parameters)
                        {
                            descText = param.Name;
                            description.Add(Item(descText, VSOBDESCRIPTIONSECTION.OBDS_PARAM));
                            descText = param.ParamTypeDesc;
                            description.Add(Item(descText, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                            descText = param.TypeName;
                            //
                            description.Add(Item(descText, VSOBDESCRIPTIONSECTION.OBDS_TYPE));
                            // Need a comma ?
                            if (paramNum < realmember.ParameterCount)
                            {
                                paramNum++;
                                descText = ",";
                                description.Add(Item(descText, VSOBDESCRIPTIONSECTION.OBDS_COMMA));
                            }
                        }
                    }
                    descText = ")";
                    description.Add(Item(descText, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                }
                if (member.Kind.HasReturnType())
                {
                    descText = XLiterals.AsKeyWord;
                    description.Add(Item(descText, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                    descText = member.TypeName;
                    description.Add(Item(descText, VSOBDESCRIPTIONSECTION.OBDS_TYPE));
                }

                description.Add(Item(null, VSOBDESCRIPTIONSECTION.OBDS_ENDDECL));
            }
            //
            if (member.File?.Project != null)
            {
                string summary=null, returns=null, remarks=null;
                List<string> pNames = new List<string>();
                List<string> pDescriptions = new List<string>();
                if (member is XSourceMemberSymbol symbol1)
                {
                    summary = XSharpXMLDocMember.GetMemberSummary(symbol1, member.File?.Project, out returns, out remarks);
                    XSharpXMLDocMember.GetMemberParameters(symbol1, member.File?.Project, pNames, pDescriptions);
                }
                else if (member is XSourceTypeSymbol symbol)
                {
                    summary = XSharpXMLDocMember.GetTypeSummary(symbol, member.File?.Project, out returns, out remarks);
                }
                if (!string.IsNullOrEmpty(summary))
                {
                    description.Add(Item(NEWLINE, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                    description.Add(Item(SUMMARY, VSOBDESCRIPTIONSECTION.OBDS_NAME));
                    description.Add(Item(summary, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                }
                if (pNames.Count > 0)
                {
                    description.Add(Item(NEWLINE, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                    description.Add(Item(PARAMETERS, VSOBDESCRIPTIONSECTION.OBDS_NAME));
                    for( int i =0; i < pNames.Count; i++)
                    {
                        description.Add(Item(NEWLINE + pNames[i], VSOBDESCRIPTIONSECTION.OBDS_PARAM));
                        description.Add(Item(" : ", VSOBDESCRIPTIONSECTION.OBDS_MISC));
                        description.Add(Item(pDescriptions[i], VSOBDESCRIPTIONSECTION.OBDS_MISC));
                    }
                    
                }
                if (!string.IsNullOrEmpty(returns))
                {
                    description.Add(Item(NEWLINE, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                    description.Add(Item(RETURNS, VSOBDESCRIPTIONSECTION.OBDS_NAME));
                    description.Add(Item(returns, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                }
                if (!string.IsNullOrEmpty(remarks))
                {
                    description.Add(Item(NEWLINE, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                    description.Add(Item(REMARKS, VSOBDESCRIPTIONSECTION.OBDS_NAME));
                    description.Add(Item(remarks, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                }
            }
        }

        public override string UniqueName
        {
            get
            {
                if (string.IsNullOrEmpty(fileMoniker))
                {
                    ThreadHelper.JoinableTaskFactory.Run(async ( )=>
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                        if ((this.filesId.Count > 0) && (this.filesId[0] != VSConstants.VSITEMID_NIL))
                        {
                            if (ownerHierarchy != null)
                            {
                                ErrorHandler.ThrowOnFailure(ownerHierarchy.GetCanonicalName(this.filesId[0], out fileMoniker));
                            }
                        }
                    });
                }
                string result = "";
                if (fileMoniker != null)
                    result = string.Format(CultureInfo.InvariantCulture, "{0}/{1}", fileMoniker, Name);
                return result;
            }
        }

        /// <summary>
        /// Search for a class, whose Fully Qualified Name is known
        /// </summary>
        /// <param name="fqName">The Fully Qualified Name class to search for</param>
        /// <returns></returns>
        public LibraryNode SearchClass(string fqName)
        {
            //
            var result = children.Find(node => MatchesName(node, fqName));
            if (result == null)
            {
                foreach (XSharpLibraryNode child in children)
                {
                    result = child.SearchClass(fqName);
                    if (result != null)
                        break;
                }
            }
            return result;
        }
        bool MatchesName(LibraryNode node, string name)
        {
            return String.Compare(node.Name, name, true) == 0 &&
                (node.NodeType & LibraryNodeType.Classes) != LibraryNodeType.None;
        }
    }

}

