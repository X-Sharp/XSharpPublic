

USING System.Collections.Generic
USING System.Diagnostics
USING System
USING System.Linq
USING LanguageService.CodeAnalysis.XSharp

BEGIN NAMESPACE XSharpModel
   
	[DebuggerDisplay("{FullName,nq}")];
	CLASS XAssembly
      
      PROPERTY TypeList             AS IDictionary<STRING, XTypeRef> AUTO
      PROPERTY ExtensionMethods     AS IList<XTypeRefMember> AUTO
      PROPERTY ImplicitNamespaces   AS IList<String> AUTO
      PROPERTY Namespaces           AS IList<String> AUTO
      PROPERTY FunctionsClass       AS STRING AUTO
      PROPERTY FullName             AS STRING AUTO
      PROPERTY IsXSharp             AS LOGIC AUTO
      CONSTRUCTOR (cFileName as STRING)
         FullName          := cFileName
         TypeList          := Dictionary<STRING, XTypeRef>{}
         ImplicitNamespaces:= List<STRING>{}
         Namespaces        := List<STRING>{}
         FunctionsClass    := ""
         ExtensionMethods  := List<XTypeRefMember>{}
         
   END CLASS
END NAMESPACE   