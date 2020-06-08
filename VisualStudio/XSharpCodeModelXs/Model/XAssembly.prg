

USING System.Collections.Generic
USING System.Diagnostics
USING System
USING System.Linq
USING LanguageService.CodeAnalysis.XSharp

BEGIN NAMESPACE XSharpModel
   
	[DebuggerDisplay("{FullName,nq}")];
	CLASS XAssembly
      
      PROPERTY TypeList             AS IDictionary<STRING, XTypeReference> AUTO
      PROPERTY TypeCatalog          AS IDictionary<Char, List<String> > AUTO
      PROPERTY ExtensionMethods     AS IList<XMemberReference> AUTO
      PROPERTY ImplicitNamespaces   AS IList<String> AUTO
      PROPERTY Namespaces           AS IList<String> AUTO
      PROPERTY ReferencedAssemblies AS IList<String> AUTO
      PROPERTY CustomAttributes     AS IList<String> AUTO
      PROPERTY GlobalClassName      AS STRING AUTO
      PROPERTY FullName             AS STRING AUTO
      PROPERTY RuntimeVersion       AS STRING AUTO
      PROPERTY FileName             AS STRING AUTO
      PROPERTY IsXSharp             AS LOGIC AUTO
      PROPERTY Loaded               AS LOGIC AUTO
      CONSTRUCTOR (cFileName as STRING)
         FileName             := cFileName
         TypeList             := Dictionary<STRING, XTypeReference>{StringComparer.OrdinalIgnoreCase}
         TypeCatalog          := Dictionary<Char, List<String> > {}
         ImplicitNamespaces   := List<STRING>{}
         Namespaces           := List<STRING>{}
         ReferencedAssemblies := List<STRING>{}
         CustomAttributes     := List<STRING>{}
         GlobalClassName      := ""
         ExtensionMethods     := List<XMemberReference>{}
 
   METHOD Read() AS LOGIC
      var reader := AssemblyReader{FileName}
      reader:Read(SELF)
      RETURN SELF:Loaded

   END CLASS
END NAMESPACE   