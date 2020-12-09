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
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Shell;
using ErrorHandler = Microsoft.VisualStudio.ErrorHandler;
using VSConstants = Microsoft.VisualStudio.VSConstants;
using XSharpModel;
using System.Collections.Generic;
using System.Diagnostics;
namespace XSharp.Project
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
        //private TextSpan sourceSpan;
        private string fileMoniker;
        //private XEntityDefinition member;
        // Description Infos...
        private List<Tuple<string, VSOBDESCRIPTIONSECTION>> description;
        private SourcePosition editorInfo;
        private String nodeText;
        // ClassName & Namespace
        private string nameSp = "";
        private string className = "";

        internal XSharpLibraryNode(string namePrefix, LibraryNodeType nType)
            : base(namePrefix)
        {
            //
            this.filesId = new List<uint>();
            //
            this.ownerHierarchy = null;
            this.Depends(0);
            //this.member = null;
            this.NodeType = nType;
            if (this.NodeType == LibraryNodeType.Namespaces)
            {
                buildImageData(Kind.Namespace, Modifiers.Public);
            }
            //
            description = new List<Tuple<string, VSOBDESCRIPTIONSECTION>>();
            editorInfo = null;
        }


        internal XSharpLibraryNode(XEntityDefinition scope, string namePrefix, IVsHierarchy hierarchy, uint itemId)
            : base(String.IsNullOrEmpty(scope.FullName) ? namePrefix : scope.FullName)
        {
            if (scope.Kind == Kind.Namespace)
            {
                this.NodeType = LibraryNodeType.Namespaces;
            }
            else if (scope.Kind.IsType())
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
                    FileName = scope.File.FullPath,
                    Line = scope.Range.StartLine + 1,
                    Column = scope.Range.StartColumn,
                };
            }
            //
            this.buildImageData(scope.Kind, scope.Modifiers);
            this.initText(scope);
            this.initDescription(scope);
        }

        private void buildImageData(Kind elementType, Modifiers accessType)
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
                //this.member.OpenEditor();
                var file = XSolution.FindFile(editorInfo.FileName);
                var project = file.Project;
                var node = project.ProjectNode;
                node.OpenElement(file.FullPath, editorInfo.Line, editorInfo.Column);
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
                    descText = nameSp;
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

        private void initText(XEntityDefinition member)
        {
            string descText = this.Name;
            //
            if (member != null)
            {
                if (member.Parent is XTypeDefinition)
                {
                    nameSp = ((XTypeDefinition)member.Parent).Namespace;
                    className = ((XTypeDefinition)member.Parent).Name;
                }
                //
                descText = member.Name;
                if (member is XMemberDefinition)
                {
                    var tm = member as XMemberDefinition;
                    if (tm.Kind == Kind.Constructor)
                    {
                        descText = "Constructor";
                    }
                    else
                    {
                        descText = member.Name;
                    }
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
                else if (member is XTypeDefinition)
                {
                    var tm = member as XTypeDefinition;
                    if ((tm.Kind == Kind.Namespace) && (String.IsNullOrEmpty(descText)))
                    {
                        descText = "Default NameSpace";
                    }
                }

            }
            nodeText = descText;
        }

        public string NodeText
        {
            get
            {
                return nodeText != null ? nodeText : "";
            }
        }

        protected override void buildDescription(_VSOBJDESCOPTIONS flags, IVsObjectBrowserDescription3 obDescription)
        {
            obDescription.ClearDescriptionText();
            foreach (var element in description)
            {
                obDescription.AddDescriptionText3(element.Item1, element.Item2, null);
            }
        }

        private void initDescription(XEntityDefinition member) //, _VSOBJDESCOPTIONS flags, IVsObjectBrowserDescription3 description)
        {
            description = new List<Tuple<string, VSOBDESCRIPTIONSECTION>>();
            //
            string descText = this.Name;
            // 
            string namesp = "";
            string className = "";
            if (member != null)
            {
                if (member.Parent != null)
                {
                    if (member.Parent is IXType)
                    {
                        namesp = ((IXType)member.Parent).Namespace;
                        className = ((IXType)member.Parent).Name;
                    }
                }
                //
                string modifier = "";
                string access = "";
                if ((member is XTypeDefinition) && (member.Kind != Kind.Namespace))
                {
                    modifier = member.Modifiers.ToDisplayString() ;
                    access = member.Visibility.ToDisplayString() ;
                }
                else if ((member is XMemberDefinition) && ((member.Kind != Kind.Function) && (member.Kind != Kind.Procedure)))
                {
                    modifier = member.Modifiers.ToDisplayString();
                    access = member.Visibility.ToDisplayString() ;
                }
                //
                if (!String.IsNullOrEmpty(modifier))
                {
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(modifier + " ", VSOBDESCRIPTIONSECTION.OBDS_ATTRIBUTE));
                }
                //
                if (!String.IsNullOrEmpty(access))
                {
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(access + " ", VSOBDESCRIPTIONSECTION.OBDS_ATTRIBUTE));
                    //description.AddDescriptionText3(access, VSOBDESCRIPTIONSECTION.OBDS_MISC, null);
                }
                // 
                if (member.Kind != Kind.Field)
                {
                    VSOBDESCRIPTIONSECTION descName = VSOBDESCRIPTIONSECTION.OBDS_MISC;
                    descText = XSettings.FormatKeyword(member.Kind.DisplayName()) + " ";
                    if (member.Kind == Kind.Constructor)
                    {
                        descName = VSOBDESCRIPTIONSECTION.OBDS_NAME;
                    }
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(descText, descName));
                    //description.AddDescriptionText3(descText, VSOBDESCRIPTIONSECTION.OBDS_MISC, null);
                }
                if (member.Kind != Kind.Constructor)
                {
                    descText = member.Name;
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(descText, VSOBDESCRIPTIONSECTION.OBDS_NAME));
                }
                //description.AddDescriptionText3(descText, VSOBDESCRIPTIONSECTION.OBDS_NAME, null);
                // Parameters ?
                if (member.Kind.HasParameters())
                {
                    descText = "(";
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(descText, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                    //description.AddDescriptionText3(descText, VSOBDESCRIPTIONSECTION.OBDS_MISC, null);
                    XMemberDefinition realmember;
                    XTypeDefinition type = member as XTypeDefinition;
                    if (member.Kind == Kind.Delegate && type?.XMembers.Count > 0)
                        realmember = type.XMembers[0] ;
                    else
                        realmember = member as XMemberDefinition;

                    if (realmember != null && realmember.HasParameters)
                    {
                        //
                        int paramNum = 1;
                        foreach (XVariable param in realmember.Parameters)
                        {
                            descText = param.Name;
                            description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(descText, VSOBDESCRIPTIONSECTION.OBDS_PARAM));
                            descText = param.ParamTypeDesc;
                            description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(descText, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                            //description.AddDescriptionText3(descText, VSOBDESCRIPTIONSECTION.OBDS_PARAM, null);
                            descText = param.TypeName;
                            //
                            IVsNavInfo navInfo = buildNavInfo(member.File, param.TypeName);
                            //
                            description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(descText, VSOBDESCRIPTIONSECTION.OBDS_TYPE));
                            //description.AddDescriptionText3(descText, VSOBDESCRIPTIONSECTION.OBDS_TYPE, navInfo);
                            // Need a comma ?
                            if (paramNum < realmember.ParameterCount)
                            {
                                paramNum++;
                                descText = ",";
                                description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(descText, VSOBDESCRIPTIONSECTION.OBDS_COMMA));
                                //description.AddDescriptionText3(descText, VSOBDESCRIPTIONSECTION.OBDS_COMMA, null);
                            }
                        }
                    }
                    descText = ")";
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(descText, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                    //description.AddDescriptionText3(descText, VSOBDESCRIPTIONSECTION.OBDS_MISC, null);
                }
                if (member.Kind.HasReturnType())
                {
                    descText = XLiterals.AsKeyWord;
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(descText, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                    //description.AddDescriptionText3(descText, VSOBDESCRIPTIONSECTION.OBDS_MISC, null);
                    descText = member.TypeName;
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(descText, VSOBDESCRIPTIONSECTION.OBDS_TYPE));
                    //description.AddDescriptionText3(descText, VSOBDESCRIPTIONSECTION.OBDS_TYPE, null);
                }

                //
                //if (!((member.Kind == Kind.Function) || (member.Kind == Kind.Procedure) || (member.Kind == Kind.VODLL)) &&
                //    ((member.Parent is XTypeDefinition) && (member.Parent.Kind == Kind.Class)))
                //{
                //    descText = " CLASS ";
                //    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(descText, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                //    //description.AddDescriptionText3(descText, VSOBDESCRIPTIONSECTION.OBDS_MISC, null);
                //    descText = className;
                //    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(descText, VSOBDESCRIPTIONSECTION.OBDS_TYPE));
                //    //description.AddDescriptionText3(descText, VSOBDESCRIPTIONSECTION.OBDS_TYPE, null);
                //}
                //
                description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(null, VSOBDESCRIPTIONSECTION.OBDS_ENDDECL));
                //description.AddDescriptionText3(null, VSOBDESCRIPTIONSECTION.OBDS_ENDDECL, null);
            }
            //
            if (member.File?.Project != null)
            {
                string summary=null, returns=null, remarks=null;
                List<string> pNames = new List<string>();
                List<string> pDescriptions = new List<string>();
                if (member is XMemberDefinition)
                {
                    summary = XSharpXMLDocMember.GetMemberSummary((XMemberDefinition)member, member.File?.Project, out returns, out remarks);
                    XSharpXMLDocMember.GetMemberParameters((XMemberDefinition)member, member.File?.Project, pNames, pDescriptions);
                }
                else if ( member is XTypeDefinition)
                {
                    summary = XSharpXMLDocMember.GetTypeSummary((XTypeDefinition)member, member.File?.Project, out returns, out remarks);
                }
                if (!String.IsNullOrEmpty(summary))
                {
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>("\n", VSOBDESCRIPTIONSECTION.OBDS_MISC));
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>("\nSummary:\n", VSOBDESCRIPTIONSECTION.OBDS_NAME));
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(summary, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                }
                if (pNames.Count > 0)
                {
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>("\n", VSOBDESCRIPTIONSECTION.OBDS_MISC));
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>("\nParameters:", VSOBDESCRIPTIONSECTION.OBDS_NAME));
                    for( int i =0; i < pNames.Count; i++)
                    {
                        description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>("\n"+pNames[i], VSOBDESCRIPTIONSECTION.OBDS_PARAM));
                        description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(" : ", VSOBDESCRIPTIONSECTION.OBDS_MISC));
                        description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(pDescriptions[i], VSOBDESCRIPTIONSECTION.OBDS_MISC));
                    }
                    
                }
                if (!String.IsNullOrEmpty(returns))
                {
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>("\n", VSOBDESCRIPTIONSECTION.OBDS_MISC));
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>("\nReturn:\n", VSOBDESCRIPTIONSECTION.OBDS_NAME));
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(returns, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                }
                if (!String.IsNullOrEmpty(remarks))
                {
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>("\n", VSOBDESCRIPTIONSECTION.OBDS_MISC));
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>("\nRemarks:\n", VSOBDESCRIPTIONSECTION.OBDS_NAME));
                    description.Add(new Tuple<string, VSOBDESCRIPTIONSECTION>(remarks, VSOBDESCRIPTIONSECTION.OBDS_MISC));
                }
            }
        }

        private IVsNavInfo buildNavInfo(XFile file, string typeName)
        {
            IVsNavInfo navInfo = null;
            CompletionType completion = new CompletionType(typeName, file, "");
            //
            //
            return navInfo;
        }

        public override string UniqueName
        {
            get
            {
                if (string.IsNullOrEmpty(fileMoniker))
                {
                    if ((this.filesId.Count > 0) && (this.filesId[0] != VSConstants.VSITEMID_NIL))
                    {
                        if (ownerHierarchy != null)
                        {
                            ErrorHandler.ThrowOnFailure(ownerHierarchy.GetCanonicalName(this.filesId[0], out fileMoniker));
                        }
                    }
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
            LibraryNode result = null;
            //
            result = children.Find(
                            delegate (LibraryNode nd)
                            {

                                return ((String.Compare(nd.Name, fqName, true) == 0) && ((nd.NodeType & LibraryNode.LibraryNodeType.Classes) != LibraryNode.LibraryNodeType.None));
                            }
                                    );
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
    }


    public class XSharpNavInfo : IVsNavInfo
    {
        public int EnumCanonicalNodes(out IVsEnumNavInfoNodes ppEnum)
        {
            throw new NotImplementedException();
        }

        public int EnumPresentationNodes(uint dwFlags, out IVsEnumNavInfoNodes ppEnum)
        {
            throw new NotImplementedException();
        }

        public int GetLibGuid(out Guid pGuid)
        {
            throw new NotImplementedException();
        }

        public int GetSymbolType(out uint pdwType)
        {
            throw new NotImplementedException();
        }
    }

    public class XSharpNavInfoNode : IVsNavInfoNode
    {
        public string Name { get; }
        public _LIB_LISTTYPE ListType { get; }

        public XSharpNavInfoNode(string name, uint listType)
        {
            Name = name;
            ListType = (_LIB_LISTTYPE)listType;
        }

        public XSharpNavInfoNode(string name, _LIB_LISTTYPE listType)
        {
            Name = name;
            ListType = listType;
        }

        int IVsNavInfoNode.get_Name(out string pbstrName)
        {
            pbstrName = Name;
            return VSConstants.S_OK;
        }

        int IVsNavInfoNode.get_Type(out uint pllt)
        {
            pllt = (uint)ListType;
            return VSConstants.S_OK;
        }
    }
}
