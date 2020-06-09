

USING System.Collections.Generic
USING System.Diagnostics
USING System
USING System.Linq
USING LanguageService.CodeAnalysis.XSharp

BEGIN NAMESPACE XSharpModel
   
	[DebuggerDisplay("{FullName,nq}")];
	CLASS XAssembly
      
      PROPERTY TypeList             AS XSortedDictionary<STRING, XTypeReference> AUTO
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
         TypeList             := XSortedDictionary<STRING, XTypeReference>{TypeNameComparer{}}
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

    PRIVATE CLASS TypeNameComparer IMPLEMENTS IComparer<STRING>
      METHOD Compare(x as String, y as String) AS LONG
         if x == NULL .or. y == NULL
            return 0
         endif
         return String.Compare(x, 0, y, 0, y:Length, TRUE)
   END CLASS

   END CLASS
END NAMESPACE   