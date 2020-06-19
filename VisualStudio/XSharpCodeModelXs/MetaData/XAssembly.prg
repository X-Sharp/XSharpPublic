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
      
      PROPERTY Id                   AS INT64 AUTO GET INTERNAL SET
      PROPERTY Types                AS Dictionary<STRING, XTypeReference> AUTO
      PROPERTY ExtensionMethods     AS IList<XMemberReference> AUTO
      PROPERTY ExtensionDict        AS Dictionary<STRING, IList<IXMember> > AUTO
      PROPERTY ImplicitNamespaces   AS IList<STRING> AUTO
      PROPERTY Namespaces           AS IList<STRING> AUTO
      PROPERTY ReferencedAssemblies AS IList<STRING> AUTO
      PROPERTY CustomAttributes     AS IList<STRING> AUTO
      PROPERTY GlobalClassName      AS STRING AUTO
      PROPERTY FullName             AS STRING AUTO
      PROPERTY FileName             AS STRING AUTO
      PROPERTY IsXSharp             AS LOGIC AUTO
      PROPERTY Loaded               AS LOGIC AUTO
      PROPERTY LastChanged          AS DateTime AUTO GET INTERNAL SET
      PROPERTY Size                 AS INT64 AUTO GET INTERNAL SET      
      
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
         Size                 := 0
         SELF:Initialize()
         SELF:UpdateAssembly()

     METHOD Initialize() AS VOID
         Types                := Dictionary<STRING, XTypeReference>{StringComparer.OrdinalIgnoreCase}
         ImplicitNamespaces   := List<STRING>{}
         Namespaces           := List<STRING>{}
         ReferencedAssemblies := List<STRING>{}
         CustomAttributes     := List<STRING>{}
         ExtensionMethods     := List<XMemberReference>{}
         ExtensionDict        := NULL
         GlobalClassName      := ""
         ExtensionDict        := NULL

      METHOD Read() AS LOGIC
         VAR reader := AssemblyReader{FileName}
         SELF:Initialize()
         reader:Read(SELF)
         XDatabase:Read(SELF)
         VAR fi := FileInfo{FileName}
         IF SELF:Size != fi:Length .OR. SELF:LastChanged != fi:LastWriteTime
            XDatabase.Update(SELF)
         ENDIF
      
         RETURN SELF:Loaded

   
         
      METHOD AddProject(project AS XProject) AS VOID
         IF ! SELF:_projects:Contains(project)
            SELF:_projects:Add(project)
         ENDIF
         
      METHOD GetType(name AS STRING) AS XTypeReference
         IF SELF:IsModifiedOnDisk .OR. SELF:Types:Count == 0
            SELF:Read()
         ENDIF
         IF SELF:Types:ContainsKey(name)
            RETURN Types[name]
         ENDIF
         IF !name:Contains(".")
            FOREACH VAR ns IN SELF:Namespaces
               IF SELF:Types:ContainsKey(ns+"."+name)
                  RETURN Types[ns+"."+name]
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
               //WriteOutputMessage("AssemblyInfo.Refresh() Assembly was changed: "+SELF:FileName )
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

      METHOD FindExtensionMethodsForType(typeName AS STRING) AS IList<IXMember>
         VAR result := List<IXMember>{}
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
         ExtensionDict := Dictionary<STRING, IList<IXMember>> {StringComparer.OrdinalIgnoreCase}
         FOREACH ext AS IXMember IN SELF:ExtensionMethods
            IF ext:Parameters:Count > 0
               VAR par  := ext:Parameters:First()
               VAR type := par:TypeName
               IF !ExtensionDict:ContainsKey(type)
                  ExtensionDict:Add(type, List<IXMember>{})
               ENDIF
               ExtensionDict[type]:Add(ext)
            ENDIF
         NEXT
         RETURN
         

   END CLASS
   
END NAMESPACE

