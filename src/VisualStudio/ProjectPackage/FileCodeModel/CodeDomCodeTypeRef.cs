//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using EnvDTE;

namespace XSharp.Project.FileCodeModel
{
	[ComVisible(true)]
	[SuppressMessage("Microsoft.Interoperability", "CA1409:ComVisibleTypesShouldBeCreatable")]
	[SuppressMessage("Microsoft.Interoperability", "CA1405:ComVisibleTypeBaseTypesShouldBeComVisible")]
	public class CodeDomCodeTypeRef : CodeDomCodeElement<CodeTypeReference>, CodeTypeRef
	{
		[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "0#dte")]
		public CodeDomCodeTypeRef(DTE dte, string name)
			: base(dte, name)
		{
			CodeObject = new CodeTypeReference(name);
			CodeObject.UserData[CodeKey] = this;
		}

		[SuppressMessage("Microsoft.Interoperability", "CA1407:AvoidStaticMembersInComVisibleTypes")]
		public static CodeTypeReference ToCodeTypeReference(CodeTypeRef typeRef)
		{
			if (null == typeRef)
			{
				throw new ArgumentNullException("typeRef");
			}
			CodeDomCodeTypeRef cdTypeRef = typeRef as CodeDomCodeTypeRef;
			if (cdTypeRef != null) return cdTypeRef.CodeObject;

			CodeTypeReference ctr = new CodeTypeReference();
			ctr.BaseType = typeRef.AsFullName;
			if (typeRef.Rank != 0)
			{
				ctr.ArrayRank = typeRef.Rank;
				ctr.ArrayElementType = ToCodeTypeReference(typeRef.ElementType);
			}
			ctr.UserData[CodeKey] = cdTypeRef;
			return ctr;
		}

		[SuppressMessage("Microsoft.Interoperability", "CA1407:AvoidStaticMembersInComVisibleTypes")]
		[SuppressMessage("Microsoft.Design", "CA1011:ConsiderPassingBaseTypesAsParameters")]
		public static CodeDomCodeTypeRef FromCodeTypeReference(DTE dte, CodeTypeReference typeRef)
		{
			if (null == typeRef)
			{
				throw new ArgumentNullException("typeRef");
			}

			var saved = typeRef.UserData[CodeKey];

			return saved == null ? new CodeDomCodeTypeRef(dte, typeRef.BaseType) : (CodeDomCodeTypeRef)saved;
		}

		#region CodeTypeRef Members

		public string AsFullName
		{
			get { return CodeObject.BaseType; }
		}

		public string AsString
		{
			get { return CodeObject.BaseType; }
		}

		public CodeType CodeType
		{
			get
			{
				throw new NotImplementedException();
			}
			[SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#")]
			set
			{
				throw new NotImplementedException();
			}
		}

		public CodeTypeRef CreateArrayType(int Rank)
		{
			throw new NotImplementedException();
		}

		public CodeTypeRef ElementType
		{
			get
			{
				throw new NotImplementedException();
			}
			[SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#")]
			set
			{
				throw new NotImplementedException();
			}
		}

		public object Parent
		{
			get { throw new NotImplementedException(); }
		}

		public int Rank
		{
			get
			{
				return CodeObject.ArrayRank;
			}
			[SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#")]
			set
			{
				CodeObject.ArrayRank = value;

				CommitChanges();
			}
		}

		public vsCMTypeRef TypeKind
		{
            get
            {

                switch (CodeObject.BaseType)
                {
                    case "System.String":
                        return vsCMTypeRef.vsCMTypeRefString;
                    case "System.Void":
                        return vsCMTypeRef.vsCMTypeRefVoid;
                    case "System.Boolean":
                        return vsCMTypeRef.vsCMTypeRefBool;
                    case "System.Int16":
                    case "System.UInt16":
                        return vsCMTypeRef.vsCMTypeRefShort;
                    case "System.Int32":
                    case "System.UInt32":
                        return vsCMTypeRef.vsCMTypeRefInt;
                    case "System.Int64":
                    case "System.UInt64":
                        return vsCMTypeRef.vsCMTypeRefLong;
                    case "System.Decimal":
                        return vsCMTypeRef.vsCMTypeRefDecimal;
                    case "System.Char":
                        return vsCMTypeRef.vsCMTypeRefChar;
                    case "System.Byte":
                        return vsCMTypeRef.vsCMTypeRefByte;
                    case "System.Object":
                        return vsCMTypeRef.vsCMTypeRefObject;
                    case "System.Double":
                        return vsCMTypeRef.vsCMTypeRefDouble;
                    case "System.Single":
                        return vsCMTypeRef.vsCMTypeRefFloat;
                }
                return vsCMTypeRef.vsCMTypeRefOther;
            }
		}

		#endregion

		public override object ParentElement
		{
			get { return null; }
		}

		public override CodeElements Children
		{
			get { return null; }
		}

		public override CodeElements Collection
		{
			get { return null; }
		}

		public override string FullName
		{
			get { return CodeObject.BaseType; }
		}

		public override vsCMInfoLocation InfoLocation
		{
			get { return vsCMInfoLocation.vsCMInfoLocationNone; }
		}

		public override vsCMElement Kind
		{
			get { return vsCMElement.vsCMElementOther; }
		}

		public override ProjectItem ProjectItem
		{
			get { return null; }
		}
	}
}
