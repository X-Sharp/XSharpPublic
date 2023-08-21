//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.CodeDom;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;

using EnvDTE;
using System.Text;
using Microsoft.VisualStudio.Project;
using System.Diagnostics;

namespace XSharp.Project.FileCodeModel
{
    [ComVisible(true)]
    [SuppressMessage("Microsoft.Interoperability", "CA1409:ComVisibleTypesShouldBeCreatable")]
    [SuppressMessage("Microsoft.Interoperability", "CA1405:ComVisibleTypeBaseTypesShouldBeComVisible")]
    public class CodeDomCodeFunction : CodeDomCodeElement<CodeMemberMethod>, CodeFunction, NativeMethods.IMethodXML
    {
        private CodeElement parent;
        [SuppressMessage("Microsoft.Usage", "CA1801:ReviewUnusedParameters", MessageId = "kind")]
        [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "0#dte")]
        public CodeDomCodeFunction(DTE dte, CodeElement parent, string name, vsCMFunction kind, object type, vsCMAccess access)
            : base(dte, name)
        {
            this.parent = parent;

            this.CodeObject = new CodeMemberMethod();
            this.CodeObject.Name = name;
            this.CodeObject.ReturnType = new CodeTypeReference(ObjectToClassName(type));
            this.CodeObject.Attributes = VSAccessToMemberAccess(access);
            this._kind = kind;
        }

        public CodeDomCodeFunction(CodeElement parent, CodeMemberMethod method, vsCMFunction kind)
            : base((null == parent) ? null : parent.DTE, (null == method) ? null : method.Name)
        {
            this.parent = parent;
            CodeObject = method;
            this._kind = kind;
        }

        #region CodeFunction Members

        public vsCMAccess Access
        {
            get { return MemberAccessToVSAccess(CodeObject.Attributes); }
            [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#")]
            set
            {
                CodeObject.Attributes = VSAccessToMemberAccess(value);
                CommitChanges();
            }
        }

        public CodeAttribute AddAttribute(string Name, string Value, object Position)
        {
            CodeAttribute ca = AddCustomAttribute(CodeObject.CustomAttributes, Name, Value, Position);

            CommitChanges();

            return ca;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="Name">Required. The name of the parameter.</param>
        /// <param name="Type">Required. A vsCMTypeRef constant indicating the data type that the function returns. This can be a CodeTypeRef object, a vsCMTypeRef constant, or a fully qualified type name.</param>
        /// <param name="Position">Optional. Default = 0. The code element after which to add the new element. If the value is a CodeElement, then the new element is added immediately after it.
        /// 
        /// If the value is a Long, then AddParameter indicates the element after which to add the new element.
        /// 
        /// Because collections begin their count at 1, passing 0 indicates that the new element should be placed at the beginning of the collection. A value of -1 means the element should be placed at the end. 
        /// </param>
        /// <returns>A CodeParameter object. </returns>
        public CodeParameter AddParameter(string Name, object Type, object Position)
        {
            CodeParameter res = AddParameter(CodeObject.Parameters, Name, Type, Position);

            CommitChanges();
            return res;
        }

        public bool CanOverride
        {
            get
            {
                return (CodeObject.Attributes & MemberAttributes.Final) != 0;
            }
            [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#")]
            set
            {
                if (value) CodeObject.Attributes |= MemberAttributes.Final;
                else CodeObject.Attributes &= ~MemberAttributes.Final;

                CommitChanges();
            }
        }

        public string Comment
        {
            get
            {
                return GetComment(CodeObject.Comments, false);
            }
            [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#")]
            set
            {
                ReplaceComment(CodeObject.Comments, value, false);

                CommitChanges();
            }
        }

        public string DocComment
        {
            get
            {
                return GetComment(CodeObject.Comments, true);
            }
            [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#")]
            set
            {
                ReplaceComment(CodeObject.Comments, value, true);

                CommitChanges();
            }
        }

        public bool IsShared
        {
            get
            {
                return (CodeObject.Attributes & MemberAttributes.Static) != 0;
            }
            [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#")]
            set
            {
                if (value) CodeObject.Attributes |= MemberAttributes.Static;
                else CodeObject.Attributes &= ~MemberAttributes.Static;

                CommitChanges();
            }
        }

        public bool MustImplement
        {
            get
            {
                return (CodeObject.Attributes & MemberAttributes.Abstract) != 0;
            }
            [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#")]
            set
            {
                if (value) CodeObject.Attributes |= MemberAttributes.Abstract;
                else CodeObject.Attributes &= ~MemberAttributes.Abstract;

                CommitChanges();
            }
        }

        public CodeElements Parameters
        {
            get
            {
                return GetParameters(CodeObject.Parameters);
            }
        }

        public object Parent
        {
            get { return parent; }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="Element">Required. A CodeElement object or the name of one in the collection.</param>
        public void RemoveParameter(object Element)
        {
            RemoveParameter(CodeObject.Parameters, Element);

            CommitChanges();
        }

        /// <summary>
        /// Return type
        /// </summary>
        public CodeTypeRef Type
        {
            get
            {
                return CodeDomCodeTypeRef.FromCodeTypeReference(this.DTE, CodeObject.ReturnType);
            }
            [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#")]
            set
            {
                CodeObject.ReturnType = CodeDomCodeTypeRef.ToCodeTypeReference(value);
            }
        }

        public CodeElements Attributes
        {
            get { return GetCustomAttributes(CodeObject.CustomAttributes); }
        }

        private vsCMFunction _kind;
        public vsCMFunction FunctionKind
        {
            get { return _kind; }
        }

        public bool IsOverloaded
        {
            get { throw new NotImplementedException(); }
        }

        public CodeElements Overloads
        {
            get { throw new NotImplementedException(); }
        }

        public string get_Prototype(int Flags)
        {
            CodeMemberMethod cmm = CodeObject as CodeMemberMethod;

            if (cmm == null)
                return cmm.Name;

            vsCMPrototype flags = (vsCMPrototype)Flags;

            StringBuilder sb = new StringBuilder(Name);

            if ((flags & vsCMPrototype.vsCMPrototypeParamTypes) != 0)
            {
                sb.Append(" (");

                bool first = true;

                foreach (CodeParameterDeclarationExpression cpde in cmm.Parameters)
                {
                    first = false;
                    sb.Append(cpde.Type.BaseType);
                    sb.Append(", ");
                }

                if (!first)
                {
                    sb.Length = sb.Length - 2;
                }

                sb.Append(")");
            }

            if ((flags & vsCMPrototype.vsCMPrototypeType) != 0)
            {
                sb.Append(" : ");
                sb.Append(cmm.ReturnType.BaseType);
            }

            return sb.ToString();
        }

        #endregion

        public override object ParentElement
        {
            get { return parent; }
        }

        public override CodeElements Children
        {
            get { return null; }
        }

        public override CodeElements Collection
        {
            get { return parent.Children; }
        }

        public override string FullName
        {
            get { return CodeObject.Name; }
        }

        public override vsCMElement Kind
        {
            get
            {
                return vsCMElement.vsCMElementFunction;
            }
        }

        public override ProjectItem ProjectItem
        {
            get { return parent.ProjectItem; }
        }

        #region IMethodXML Members

        void NativeMethods.IMethodXML.GetXML(ref string xml)
        {
            xml = string.Empty;
        }

        int NativeMethods.IMethodXML.SetXML(string xml)
        {
            return NativeMethods.S_FALSE;
        }

        int NativeMethods.IMethodXML.GetBodyPoint(out object bodyPoint)
        {
            bodyPoint = null;
            return NativeMethods.S_FALSE;
        }

        #endregion

        private string GetSignature(string container, CodeMemberMethod method)
        {
            var sb = new StringBuilder();
            sb.Append(container);
            sb.Append(method.Name);
            sb.Append('|');
            foreach (CodeParameterDeclarationExpression p in method.Parameters)
            {
                AddParameterToSb(sb, p);
            }
            return sb.ToString();

        }
        private void AddParameterToSb(StringBuilder sb, CodeParameterDeclarationExpression p)
        {
            sb.Append(p.Name);
            sb.Append('|');
            sb.Append(p.Direction.ToString());
            sb.Append('|');
            sb.Append(p.Type.BaseType);
            sb.Append('|');
        }

        private string GetSignature()
        {
            StringBuilder sb = new StringBuilder();
            if (this.ParentElement is CodeDomCodeClass myClass)
            {
                sb.Append(myClass.FullName);
                sb.Append('|');
                sb.Append(this.Name);
                sb.Append('|');
                foreach (CodeParameterDeclarationExpression p in this.CodeObject.Parameters)
                {
                    AddParameterToSb(sb, p);
                }
            }
            //
            return sb.ToString();
        }

        protected override void UpdateStartPoint()
        {
            object curParent = ParentElement;
            XSharpFileCodeModel fcm = null;
            while (!(curParent is EnvDTE.FileCodeModel))
            {
                curParent = ((ICodeDomElement)curParent).ParentElement;
                fcm = curParent as XSharpFileCodeModel;
                if (fcm != null)
                {
                    break;
                }

                if (curParent == null)
                {
                    Debug.Assert(false, "Not ICodeDomElement or CodeDomFileCodeModel in parent hierarchy");
                    break;
                }
            }
            //
            if (fcm != null)
            {
                if (fcm.CompileUnit != null)
                {
                    StringBuilder sb = new StringBuilder();
                    foreach (System.CodeDom.CodeNamespace ns in fcm.CompileUnit.Namespaces)
                    {
                        if (!String.IsNullOrEmpty(ns.Name))
                        {

                            foreach (CodeTypeDeclaration nsType in ns.Types)
                            {
                                sb.Append(ns.Name);
                                sb.Append(".");
                                sb.Append(nsType.Name);
                                sb.Append('|');
                                string container = sb.ToString();
                                sb.Clear();
                                // Ok, we got the class, now search the Method
                                foreach (CodeTypeMember member in nsType.Members)
                                {
                                    if (member is CodeMemberMethod method)
                                    {
                                        var sig = GetSignature(container, method);
                                        // Is it what we are looking for ???
                                        if (sig == this.GetSignature())
                                        {
                                            this.CodeObject.UserData[pointkey] = method.UserData[pointkey];
                                            return;
                                        }
                                        sb.Clear();
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
