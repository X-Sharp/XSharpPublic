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
      PRIVATE _aExtensions          AS IList<XTypeRefMember>
      PRIVATE _assembly             AS XAssembly
      PRIVATE _aTypes               AS IDictionary<STRING, XTypeRef>
      PRIVATE _fullName             AS STRING
      PRIVATE _globalClassName      := "" AS STRING
      PRIVATE _hasExtensions        AS LOGIC
      PRIVATE _implicitNamespaces   AS IList<STRING>
      PRIVATE _modified             AS System.DateTime
      PRIVATE _nameSpaces           AS System.Collections.Hashtable
      PRIVATE _nameSpaceTexts       AS IList<STRING>
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
         SELF:_fullName := ""
         SELF:_assembly := NULL
         SELF:_projects := List<XProject>{}
         SELF:_clearInfo()
         
         
      CONSTRUCTOR(_cFileName AS STRING, _dModified AS System.DateTime)
         SELF()
         SELF:FileName := _cFileName
         SELF:Modified := _dModified
         SELF:UpdateAssembly()
         
      PRIVATE METHOD _clearInfo() AS VOID
         SELF:_aTypes               := Dictionary<STRING, XTypeRef>{System.StringComparer.OrdinalIgnoreCase}
         SELF:_aExtensions          := List<XTypeRefMember>{}
         SELF:_nameSpaces           := System.Collections.Hashtable{System.StringComparer.OrdinalIgnoreCase}
         SELF:_nameSpaceTexts       := List<STRING>{}
         SELF:_implicitNamespaces   := List<STRING>{}
         SELF:_hasExtensions        := FALSE
         
      METHOD AddProject(project AS XProject) AS VOID
         IF ! SELF:_projects:Contains(project)
            SELF:_projects:Add(project)
         ENDIF
         
      METHOD GetType(name AS STRING) AS XTypeRef
         IF SELF:IsModifiedOnDisk
            SELF:LoadAssembly()
         ENDIF
         IF SELF:_assembly != NULL .AND. SELF:_aTypes:Count == 0
            SELF:UpdateAssembly()
         ENDIF
         IF SELF:_aTypes:ContainsKey(name)
            RETURN SELF:Types:Item[name]
         ENDIF
         RETURN NULL
//         
//      PRIVATE METHOD GetTypeTypesFromType(oType AS System.Type) AS AssemblyInfo.TypeTypes
//         IF oType:IsValueType
//            RETURN AssemblyInfo.TypeTypes.Structure
//         ENDIF
//         IF oType:IsInterface
//            RETURN AssemblyInfo.TypeTypes.Interface
//         ENDIF
//         IF TYPEOF(System.Delegate):IsAssignableFrom(oType)
//            RETURN AssemblyInfo.TypeTypes.Delegate
//         ENDIF
//         RETURN AssemblyInfo.TypeTypes.Class
//         
//      PRIVATE STATIC METHOD HasExtensionAttribute(memberInfo AS MemberInfo) AS LOGIC
//         TRY
//            VAR customAttributes := memberInfo:GetCustomAttributes(FALSE)
//            FOREACH VAR custattr IN customAttributes
//               IF custattr:ToString() == "System.Runtime.CompilerServices.ExtensionAttribute"
//                  RETURN TRUE
//               ENDIF
//            NEXT
//         CATCH
//            // Failed to retrieve custom attributes
//         END TRY
//         RETURN FALSE
         
      INTERNAL METHOD LoadTypesAndNamespaces() AS VOID
         LOCAL reader as AssemblyReader
         SELF:_clearInfo()
         reader := AssemblyReader{SELF:FileName}
         var result := reader:Read()
         IF (result != NULL)
            SELF:_assembly             := result
            SELF:_aTypes               := _assembly:TypeList
            SELF:_globalClassName      := _assembly:FunctionsClass
            SELF:_nameSpaceTexts       := _assembly:Namespaces
            SELF:_implicitNamespaces   := _assembly:ImplicitNamespaces
            SELF:_aExtensions          := _assembly:ExtensionMethods
            SELF:_hasExtensions        := _assembly:ExtensionMethods:Count > 0
            WriteOutputMessage("   ...Types             found: "+_aTypes:Count:ToString() )
            WriteOutputMessage("   ...Namespaces        found: "+_nameSpaceTexts:Count:ToString() )
            WriteOutputMessage("   ...Extension methods found: "+_aExtensions:Count:ToString() )
            WriteOutputMessage("   ...Implicit namespa. found: "+_implicitNamespaces:Count:ToString())
            WriteOutputMessage("   ...Globals Classname found: "+_globalClassName )
         ELSE
            SELF:_assembly := XAssembly{SELF:FileName}
         ENDIF
         
         
         
      INTERNAL METHOD LoadAssembly()  AS VOID
         WriteOutputMessage("--> LoadAssembly : "+SELF:FileName)
//         IF String.IsNullOrEmpty(SELF:FileName) .AND. SELF:_reference != NULL
//            SELF:FileName := SELF:_reference:Path
//         ENDIF
         IF SELF:Exists
            SELF:LoadTypesAndNamespaces()
            
            SELF:_fullName  := SELF:_assembly:FullName
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
         
      PROPERTY Exists             AS LOGIC GET _SafeExists(SELF:FileName)
      PROPERTY FileName           AS STRING AUTO
      PROPERTY FullName           AS STRING GET SELF:_fullName
      PROPERTY GlobalClassName    AS STRING GET SELF:_globalClassName
      PROPERTY HasProjects        AS LOGIC GET SELF:_projects:Count > 0
      PROPERTY ImplicitNamespaces AS IList<STRING> GET SELF:_implicitNamespaces
      PROPERTY IsModifiedOnDisk   AS LOGIC GET SELF:LastWriteTime != SELF:Modified
      PROPERTY LastWriteTime      AS DateTime GET IIF(SELF:Exists, File.GetLastWriteTime(SELF:FileName), DateTime.MinValue)
      PROPERTY Modified           AS DateTime GET IIF(SELF:Exists, SELF:_modified, DateTime.MinValue) SET SELF:_modified := VALUE
      PROPERTY Namespaces         AS IList<STRING> GET SELF:_nameSpaceTexts
      PROPERTY RuntimeVersion     AS STRING
         GET
            SELF:UpdateAssembly()
//            IF SELF:_assembly != NULL
//               RETURN SELF:_assembly:ImageRuntimeVersion
//            ENDIF
            RETURN ""
         END GET
      END PROPERTY
      PROPERTY Types              AS IDictionary<STRING, XTypeRef> GET SELF:_aTypes
      
      PROPERTY HasExtensions      AS LOGIC GET SELF:_hasExtensions
      PROPERTY Extensions         AS IList<XTypeRefMember> GET SELF:_aExtensions:ToArray()
      
      
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

