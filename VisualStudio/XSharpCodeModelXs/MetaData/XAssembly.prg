//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System
USING System.IO
USING System.Linq
USING System.Diagnostics

BEGIN NAMESPACE XSharpModel
   [DebuggerDisplay("{DisplayName,nq}")];
   CLASS XAssembly
      // Fields
      PRIVATE _projects             AS List<XProject>
      PRIVATE _wasRead              AS LOGIC

      PROPERTY Id                   AS INT64 AUTO GET INTERNAL SET
      PROPERTY Types                AS XDictionary<STRING, XPETypeSymbol> AUTO
      PROPERTY GlobalMembers        AS XDictionary<STRING, XPEMemberSymbol> AUTO
      PROPERTY ExtensionMethods     AS IList<XPEMemberSymbol> AUTO
      PROPERTY ExtensionDict        AS XDictionary<STRING, IList<IXMemberSymbol> > AUTO
      PROPERTY ImplicitNamespaces   AS IList<STRING> AUTO
      // Namespaces property can be deleted once we do the name bases lookup in the database
      INTERNAL PROPERTY Namespaces   AS IList<STRING> AUTO
      PROPERTY ReferencedAssemblies AS IList<STRING> AUTO
      PROPERTY GlobalClassName      AS STRING AUTO
      PROPERTY FullName             AS STRING AUTO
      PROPERTY FileName             AS STRING AUTO
      PROPERTY IsXSharp             AS LOGIC AUTO
      PROPERTY Loaded               AS LOGIC AUTO
      PROPERTY LastChanged          AS DateTime AUTO GET INTERNAL SET
      PROPERTY Size                 AS INT64 AUTO GET INTERNAL SET
      PROPERTY DuplicateTypes       AS List<XPETypeSymbol> AUTO
      PROPERTY HasDuplicateTypes    AS LOGIC GET DuplicateTypes != NULL .AND. DuplicateTypes:Count > 0
      PROPERTY Version              AS STRING AUTO

      STATIC CONSTRUCTOR
         RETURN


      // Methods
      CONSTRUCTOR()
         SUPER()
         SELF:_projects := List<XProject>{}


      CONSTRUCTOR(cFileName AS STRING, dModified AS System.DateTime)
         SELF()
         Id                   := -1
         FileName             := cFileName
         LastChanged          := dModified
         _wasRead             := FALSE
         Loaded               := FALSE
         Size                 := 0
         SELF:Initialize()
         SELF:UpdateAssembly()

     METHOD Initialize() AS VOID
         Types                := XDictionary<STRING, XPETypeSymbol>{StringComparer.OrdinalIgnoreCase}
         GlobalMembers        := XDictionary<STRING, XPEMemberSymbol>{}
         ImplicitNamespaces   := List<STRING>{}
         ReferencedAssemblies := List<STRING>{}
         DuplicateTypes       := NULL
         ExtensionMethods     := List<XPEMemberSymbol>{}
         ExtensionDict        := NULL
         GlobalClassName      := ""
         ExtensionDict        := NULL
         _wasRead             := FALSE

      METHOD Read() AS LOGIC
         VAR reader := AssemblyReader{FileName}
         SELF:Initialize()
         reader:Read(SELF)
         XDatabase.Read(SELF)
         VAR fi := FileInfo{FileName}
         IF SELF:Size != fi:Length .OR. SELF:LastChanged != fi:LastWriteTime
            // update assembly info and type names in the database
            XDatabase.Update(SELF)
         ENDIF
         SELF:_wasRead := TRUE
         RETURN SELF:Loaded



      METHOD AddProject(project AS XProject) AS VOID
         IF ! SELF:_projects:Contains(project)
            SELF:_projects:Add(project)
         ENDIF

      METHOD GetType(name AS STRING) AS XPETypeSymbol
         IF SELF:IsModifiedOnDisk .OR. ! SELF:_wasRead
            SELF:Read()
         ENDIF
         IF SELF:Types:ContainsKey(name)
            VAR found := Types[name]
            IF String.Equals(name, found:FullName)
               RETURN found
            ENDIF
            IF String.Equals(name, found:TypeDef:FullName)
               RETURN found
            ENDIF

            IF SELF:HasDuplicateTypes
               FOREACH VAR type IN DuplicateTypes
                  IF String.Equals(type:FullName, name)
                     RETURN type
                  ENDIF
                  IF String.Equals(type:TypeDef:FullName, name)
                     RETURN type
                  ENDIF
               NEXT
            ENDIF
            // no exact match in the duplicates. Return the type with not exact match
            RETURN found
         ELSE // Type not found
            // Todo: Use the Intellisense database instead of the Namespaces and Types collection.
            // when we do so we have to separate the ns out of the name
            // and look for all types that match in name.
            // then later we can match the typenames that we found with the ns that is part of the parameter
            // the resulting type comes out of the collection based on the info we read from the database
            // so any methods that we already cached are loaded.
            // here we look in the database
            // After we changed this then we can get rid of the Namespaces collection in the XAssembly class as well.
            FOREACH VAR ns IN SELF:Namespaces
               VAR fullName := ns+"."+name
               IF SELF:Types:ContainsKey(fullName)
                  VAR found := Types[fullName]
                  IF String.Equals(name, found:FullName)
                     RETURN found
                  ENDIF
                  IF String.Equals(name, found:TypeDef:FullName)
                     RETURN found
                  ENDIF
                  IF SELF:HasDuplicateTypes
                     FOREACH VAR type IN DuplicateTypes
                        IF String.Equals(type:FullName, fullName)
                           RETURN type
                        ENDIF
                        IF String.Equals(type:TypeDef:FullName, name)
                           RETURN type
                        ENDIF
                     NEXT
                  ENDIF
                  // no exact match in the duplicates. Return the type with not exact match
                  RETURN found
               ENDIF
            NEXT
         ENDIF
         RETURN NULL




      INTERNAL METHOD LoadAssembly()  AS VOID
         IF SELF:Exists
            SELF:Read()
         ENDIF

       METHOD RemoveProject(project AS XProject) AS VOID
         IF SELF:_projects:Contains(project)
            SELF:_projects:Remove(project)
         ENDIF

      METHOD Refresh() AS VOID
         IF SELF:Exists
            IF SELF:IsModifiedOnDisk
               SELF:_wasRead := FALSE
               //WriteOutputMessage("AssemblyInfo.Refresh() Assembly was changed: "+SELF:FileName )
               SELF:Loaded := FALSE
               SELF:UpdateAssembly()
            ENDIF
         ENDIF

      PRIVATE METHOD GetPropertySafe(type AS System.Type, obj AS OBJECT, PropName AS STRING) AS STRING
         FOREACH VAR prop IN type:GetProperties()
            IF String.Compare(prop:Name, PropName, StringComparison.OrdinalIgnoreCase) == 0
               RETURN prop:GetValue(obj, NULL):ToString()
            ENDIF
         NEXT
         RETURN ""

      INTERNAL METHOD UpdateAssembly() AS VOID
         IF SELF:Loaded
            RETURN
         ENDIF
         IF ! SELF:Exists
            //WriteOutputMessage("****** AssemblyInfo.UpdateAssembly: assembly "+SELF:FileName +" does not exist")
            RETURN
         ENDIF
         TRY
            //WriteOutputMessage("-->AssemblyInfo.UpdateAssembly load types from assembly "+SELF:FileName )
            SELF:Read()
         CATCH e as Exception
            WriteOutputMessage(" *** Exception")
            WriteOutputMessage(e:ToString())
         FINALLY
            //WriteOutputMessage("<-- AssemblyInfo.UpdateAssembly load types from assembly "+SELF:FileName )
         END TRY
      // Properties
      PROPERTY DisplayName AS STRING
         GET
            IF String.IsNullOrEmpty(SELF:FileName)
               RETURN "(Empty)"
            ENDIF
            RETURN Path.GetFileName(SELF:FileName)
         END GET
      END PROPERTY

      INTERNAL STATIC METHOD _SafeExists(cFileName AS STRING) AS LOGIC
         LOCAL lExists := FALSE AS LOGIC
         TRY
            IF !String.IsNullOrEmpty(cFileName)
               IF File.Exists(cFileName)
                  lExists := TRUE
               ENDIF
            ENDIF
         CATCH
            lExists := FALSE
         END TRY
         RETURN lExists

      PROPERTY Exists               AS LOGIC GET _SafeExists(SELF:FileName)
      PROPERTY HasProjects          AS LOGIC GET SELF:_projects:Count > 0
      PROPERTY IsModifiedOnDisk     AS LOGIC GET SELF:LastWriteTimeOnDisk != SELF:LastChanged
      PROPERTY LastWriteTimeOnDisk  AS DateTime GET IIF(SELF:Exists, File.GetLastWriteTime(SELF:FileName), DateTime.MinValue)
      PROPERTY RuntimeVersion       AS STRING   AUTO GET INTERNAL SET
      PROPERTY HasExtensions        AS LOGIC GET SELF:ExtensionMethods:Count > 0


      STATIC METHOD WriteOutputMessage(message AS STRING) AS VOID
         XSolution.WriteOutputMessage("XModel.XAssembly " +message )

      METHOD FindExtensionMethodsForType(typeName AS STRING) AS IList<IXMemberSymbol>
         VAR result := List<IXMemberSymbol>{}
         IF SELF:HasExtensions
            IF ExtensionDict == NULL
               SELF:BuildExtensionDict()
            ENDIF
            IF ExtensionDict:ContainsKey(typeName)
               result:AddRange(ExtensionDict[typeName])
            ENDIF
         ENDIF

         RETURN result

      METHOD BuildExtensionDict() AS VOID
         ExtensionDict := XDictionary<STRING, IList<IXMemberSymbol>> {StringComparer.OrdinalIgnoreCase}
         FOREACH ext AS IXMemberSymbol IN SELF:ExtensionMethods
            IF ext:Parameters:Count > 0
               VAR par  := ext:Parameters:First()
               VAR type := par:TypeName
               IF !ExtensionDict:ContainsKey(type)
                  ExtensionDict:Add(type, List<IXMemberSymbol>{})
               ENDIF
               ExtensionDict[type]:Add(ext)
            ENDIF
         NEXT
         RETURN


   END CLASS

END NAMESPACE

