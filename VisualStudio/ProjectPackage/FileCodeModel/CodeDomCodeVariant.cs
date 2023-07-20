//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.CodeDom;
using EnvDTE;
using System.Diagnostics.CodeAnalysis;

namespace XSharp.Project.FileCodeModel
{
    class CodeDomCodeVariant : CodeDomCodeClass
	{
		[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "0#dte")]
		public CodeDomCodeVariant(DTE dte, CodeElement parent, string name, object bases, object interfaces, vsCMAccess access)
			: base(dte, parent, name, bases, interfaces, access)
		{ }

		[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "0#dte")]
		public CodeDomCodeVariant(DTE dte, CodeElement parent, CodeTypeDeclaration declaration)
			: base(dte, parent, declaration)
		{ }
	}
}
