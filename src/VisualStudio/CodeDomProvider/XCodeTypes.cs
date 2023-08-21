//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.CodeDom;
using System.Diagnostics;
using XSharpModel;

namespace XSharp.CodeDom
{
    public interface IXCodeObject
    {
    }
   
    /// <summary>
    /// Enhanced Type reference with System.Type property, since CodeTypeReference does not hold on to the type
    /// </summary>
    [DebuggerDisplay("{BaseType,nq}")]
    internal class XCodeTypeReference : CodeTypeReference, IXCodeObject
    {
        internal XCodeTypeReference(string typeName) : base(typeName)
        {
        }
        internal XCodeTypeReference(IXTypeSymbol type) : base(type.FullName)
        {
        }
    }

    [DebuggerDisplay("{Name,nq}")]
    internal class XCodeTypeReferenceExpression : CodeTypeReferenceExpression, IXCodeObject
    { 
        internal string Name { get; private set; }
        internal XCodeTypeReferenceExpression(System.Type type) : base(type)
        {
            Name = type.FullName;
        }
        internal XCodeTypeReferenceExpression(string type) : base(type)
        {
            Name = type;
            if (Name.StartsWith("global::"))
            {
                var typeName= Name.Substring(8);
                typeName = typeName.Replace(".Properties.Resources",".Resources");
                this.Type.BaseType = typeName;
                this.Type.Options = CodeTypeReferenceOptions.GlobalReference;
            }
        }
        internal XCodeTypeReferenceExpression(CodeTypeReference type) : base(type)
        {
            Name = type.BaseType;
        }
    }

    [DebuggerDisplay("{FileName,nq}")]
    public class XCodeCompileUnit : CodeCompileUnit, IXCodeObject
    {
        public string FileName { get; set; } = "";
        public string Source { get; set; } = "";
        public bool MustWrite { get; set; } = false;
        public CodeTypeMemberCollection Members { get; set; } = new CodeTypeMemberCollection();
        public bool GenerateHeader { get; set; }
        public XCodeCompileUnit() : base()
        {

        }
        public XCodeCompileUnit(CodeCompileUnit source) : base()
        {
            if (source != null)
            {
                Namespaces.AddRange(source.Namespaces);
                EndDirectives.AddRange(source.EndDirectives);
                StartDirectives.AddRange(source.StartDirectives);
                AssemblyCustomAttributes.AddRange(source.AssemblyCustomAttributes);
                foreach (string name in source.ReferencedAssemblies)
                {
                    ReferencedAssemblies.Add(name);
                }
                source.CopyUserData(this);
            }
        }

    }

    [DebuggerDisplay("MergedUnit: {FileName,nq}")]
    public class XMergedCodeCompileUnit : XCodeCompileUnit, IXCodeObject
    {
        public XCodeCompileUnit FormUnit { get; set; }
        public XCodeCompileUnit DesignerUnit { get; set; }
        public CodeNamespace FormNamespace { get; set; }
        public CodeNamespace DesignerNamespace { get; set; }
        public XMergedCodeCompileUnit() : base()
        {

        }
        public XMergedCodeCompileUnit(XCodeCompileUnit source) : base(source)
        {
            source.CopyUserData(this);
            this.FileName = source.FileName;
            this.FormUnit = source;
        }
    }


    [DebuggerDisplay("{Name,nq}")]
    public class XCodeMemberField : CodeMemberField, IXCodeObject
    {
        public XCodeMemberField() : base()
        {

        }
    }
    [DebuggerDisplay("{Name,nq}")]
    public class XCodeNamespace : CodeNamespace, IXCodeObject
    {
        public XCodeNamespace(string name) : base(name)
        {
            
        }
    }

    [DebuggerDisplay("{Name,nq}")]
    public class XCodeTypeDelegate : CodeTypeDelegate, IXCodeObject
    {
        public XCodeTypeDelegate(string name) : base(name)
        {

        }
    }
    [DebuggerDisplay("{Name,nq}")]
    public class XCodeTypeDeclaration : CodeTypeDeclaration, IXCodeObject
    {
        public XCodeTypeDeclaration(string name) : base(name)
        {

        }
    }
    [DebuggerDisplay("{Name,nq}")]
    public class XCodeMemberMethod : CodeMemberMethod, IXCodeObject
    {

        public XCodeMemberMethod() : base()
        {

        }
    }
    
    [DebuggerDisplay("{Name,nq}")]
    public class XCodeMemberEvent : CodeMemberEvent, IXCodeObject
    {
        public XCodeMemberEvent() : base()
        {

        }
    }
    [DebuggerDisplay("{Name,nq}")]
    public class XCodeTypeConstructor : CodeTypeConstructor, IXCodeObject
    {
        public XCodeTypeConstructor() : base()
        {

        }
    }
    [DebuggerDisplay("{Name,nq}")]
    public class XCodeConstructor : CodeConstructor, IXCodeObject
    {
        public XCodeConstructor() : base()
        {

        }
    }
    [DebuggerDisplay("{Name,nq}")]
    public class XCodeNamespaceImport : CodeNamespaceImport, IXCodeObject
    {

        public XCodeNamespaceImport(string name) : base(name)
        {

        }
    }
    [DebuggerDisplay("{FieldName,nq}")]
    internal class XCodeFieldReferenceExpression : CodeFieldReferenceExpression, IXCodeObject
    {
        internal XCodeFieldReferenceExpression(CodeExpression lhs, string name) : base(lhs, name)
        {

        }
    }

    internal class XCodeSnippetTypeMember : CodeSnippetTypeMember, IXCodeObject
    {
   
        internal XCodeSnippetTypeMember(string text) : base(text)
        {

        }
    }

    [DebuggerDisplay("{PropertyName,nq}")]
    internal class XCodePropertyReferenceExpression : CodePropertyReferenceExpression, IXCodeObject
    {
        internal XCodePropertyReferenceExpression(CodeExpression lhs, string name) : base(lhs, name)
        {

        }
    }

    [DebuggerDisplay("{MethodName,nq}")]
    internal class XCodeMethodReferenceExpression : CodeMethodReferenceExpression, IXCodeObject
    {
        internal XCodeMethodReferenceExpression(CodeExpression lhs, string name) : base(lhs, name)
        {
        }
    }

    [DebuggerDisplay("{VariableName,nq}")]
    internal class XCodeVariableReferenceExpression : CodeVariableReferenceExpression, IXCodeObject
    {
        internal XCodeVariableReferenceExpression(string name) : base(name)
        {
        }
    }

    [DebuggerDisplay("{EventName,nq}")]
    internal class XCodeEventReferenceExpression : CodeEventReferenceExpression, IXCodeObject
    {
        internal XCodeEventReferenceExpression(CodeExpression lhs, string name) : base(lhs, name)
        {
        }
    }
    [DebuggerDisplay("SUPER")]
    internal class XCodeBaseReferenceExpression : CodeBaseReferenceExpression, IXCodeObject
    {
        internal XCodeBaseReferenceExpression() : base()
        {
        }
    }

    [DebuggerDisplay("SELF")]
    internal class XCodeThisReferenceExpression : CodeThisReferenceExpression, IXCodeObject
    {
        internal XCodeThisReferenceExpression() : base()
        {
        }
    }
}
