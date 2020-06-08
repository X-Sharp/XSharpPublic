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
   CLASS AssemblyInfo
      // Fields
      PRIVATE _assembly             AS XAssembly
      PRIVATE _modified             AS System.DateTime
      PRIVATE _projects             AS List<XProject>
      
      
      PUBLIC STATIC PROPERTY DisableAssemblyReferences AS LOGIC AUTO
      PUBLIC STATIC PROPERTY DisableForeignProjectReferences AS LOGIC AUTO
      PUBLIC STATIC PROPERTY DisableXSharpProjectReferences AS LOGIC AUTO
      STATIC CONSTRUCTOR
         DisableXSharpProjectReferences := FALSE
         DisableAssemblyReferences := FALSE
         DisableForeignProjectReferences := FALSE
         RETURN
         
         
      // Methods
      CONSTRUCTOR()
         SUPER()
         SELF:_projects := List<XProject>{}
         SELF:_assembly := XAssembly{""}
         
         
      CONSTRUCTOR(_cFileName AS STRING, _dModified AS System.DateTime)
         SELF()
         SELF:FileName := _cFileName
         SELF:Modified := _dModified
         SELF:UpdateAssembly()
         
         
      METHOD AddProject(project AS XProject) AS VOID
         IF ! SELF:_projects:Contains(project)
            SELF:_projects:Add(project)
         ENDIF
         
      METHOD GetType(name AS STRING, searched := NULL AS List<String>) AS XTypeReference
         if (searched == NULL)
            searched := List<String>{} {SELF:FullName}
         ELSEIF searched:Contains(SELF:FullName)
            RETURN NULL
         ELSE
            searched:Add(SELF:FullName)
         ENDIF
         IF SELF:IsModifiedOnDisk
            SELF:LoadAssembly()
         ENDIF
         IF SELF:_assembly != NULL .AND. SELF:Types:Count == 0
            SELF:UpdateAssembly()
         ENDIF
         IF SELF:Types:ContainsKey(name)
            RETURN Types[name]
         ENDIF
         // look in referenced assemblies. This also used to resolve base types for types
         FOREACH var refasm in _assembly:ReferencedAssemblies
            var asm := SystemTypeController.FindAssembly(refasm)
            if asm != NULL
               var type := asm:GetType(name,searched)
               if type != NULL
                  return type
               endif
            endif
         NEXT
         RETURN NULL

            
        
      INTERNAL METHOD LoadTypesAndNamespaces() AS VOID
         SELF:_assembly := XAssembly{SELF:FileName}
         SELF:_assembly:Read()
         
         
      INTERNAL METHOD LoadAssembly()  AS VOID
         WriteOutputMessage("--> LoadAssembly : "+SELF:FileName)
//         IF String.IsNullOrEmpty(SELF:FileName) .AND. SELF:_reference != NULL
//            SELF:FileName := SELF:_reference:Path
//         ENDIF
         IF SELF:Exists
            SELF:LoadTypesAndNamespaces()
            
            SELF:Modified   := SELF:LastWriteTime
            //				ENDIF
         ENDIF
         WriteOutputMessage("<-- LoadAssembly : "+SELF:FileName)
         
       METHOD RemoveProject(project AS XProject) AS VOID
         //
         IF SELF:_projects:Contains(project)
            SELF:_projects:Remove(project)
         ENDIF
         
      METHOD Refresh() AS VOID
         IF SELF:Exists
            VAR currentDT := SELF:LastWriteTime
            IF currentDT != SELF:Modified
               WriteOutputMessage("AssemblyInfo.Refresh() Assembly was changed: "+SELF:FileName )
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
         IF SELF:_assembly:Loaded
            RETURN
         ENDIF
         IF ! SELF:Exists
            WriteOutputMessage("****** AssemblyInfo.UpdateAssembly: assembly "+SELF:FileName +" does not exist")
            RETURN
         ENDIF
         TRY
            WriteOutputMessage("-->AssemblyInfo.UpdateAssembly load types from assembly "+SELF:FileName )
            SELF:LoadTypesAndNamespaces()
         CATCH e as Exception
            WriteOutputMessage(" *** Exception")
            WriteOutputMessage(e:ToString())
         FINALLY
            WriteOutputMessage("<-- AssemblyInfo.UpdateAssembly load types from assembly "+SELF:FileName )
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
      PROPERTY FileName             AS STRING AUTO
      PROPERTY FullName             AS STRING GET SELF:_assembly:FullName
      PROPERTY GlobalClassName      AS STRING GET SELF:_assembly:GlobalClassName
      PROPERTY HasProjects          AS LOGIC GET SELF:_projects:Count > 0
      PROPERTY ImplicitNamespaces   AS IList<STRING>    GET SELF:_assembly:ImplicitNamespaces
      PROPERTY IsModifiedOnDisk     AS LOGIC GET SELF:LastWriteTime != SELF:Modified
      PROPERTY LastWriteTime        AS DateTime GET IIF(SELF:Exists, File.GetLastWriteTime(SELF:FileName), DateTime.MinValue)
      PROPERTY Modified             AS DateTime GET IIF(SELF:Exists, SELF:_modified, DateTime.MinValue) SET SELF:_modified := VALUE
      PROPERTY Namespaces           AS IList<STRING> GET SELF:_assembly:Namespaces
      PROPERTY ReferencedAssemblies AS IList<STRING> GET SELF:_assembly:ReferencedAssemblies
      PROPERTY RuntimeVersion       AS STRING
         GET
            SELF:UpdateAssembly()
            RETURN SELF:_assembly:RuntimeVersion
         END GET
      END PROPERTY
      PROPERTY Types              AS IDictionary<STRING, XTypeReference> GET _assembly:TypeList
      PROPERTY TypeCatalog        AS IDictionary<CHAR, List<STRING> > GET _assembly:TypeCatalog
      
      PROPERTY HasExtensions      AS LOGIC GET SELF:_assembly:ExtensionMethods:Count > 0
      PROPERTY Extensions         AS IList<XMemberReference> GET _assembly:ExtensionMethods
      
      
      STATIC METHOD WriteOutputMessage(message AS STRING) AS VOID
         XSolution.WriteOutputMessage("XModel.AssemblyInfo " +message )
         
      // Nested Types
      INTERNAL CLASS NameSpaceContainer
         // Fields
         INTERNAL _NameSpace := "" AS STRING
         INTERNAL _Types AS SortedList<STRING, AssemblyInfo.TypeTypes>
         
         CONSTRUCTOR(_cNameSpace AS STRING);SUPER()
            //
            SELF:_NameSpace := _cNameSpace
            SELF:_Types := SortedList<STRING, AssemblyInfo.TypeTypes>{}
            
         METHOD AddType(typeName AS STRING, type AS AssemblyInfo.TypeTypes) AS VOID
            //
            IF ! SELF:_Types:ContainsKey(typeName)
               //
               SELF:_Types:Add(typeName, type)
            ENDIF
            
         METHOD Clear() AS VOID
            //
            SELF:_Types:Clear()
      END CLASS
      
      INTERNAL ENUM TypeTypes AS LONG
         MEMBER @@All:=0xff
         MEMBER @@Class:=1
         MEMBER @@Delegate:=8
         MEMBER @@Interface:=4
         MEMBER @@None:=0
         MEMBER @@Structure:=2
      END ENUM
      
      
   END CLASS
   
END NAMESPACE

