USING System
USING System.Linq
USING System.Collections.Generic
USING System.IO
USING Mono.Cecil


BEGIN NAMESPACE XSharpModel
CLASS AssemblyReader
   PROTECT reader             AS AssemblyDefinition
   PROTECT _fileName          AS STRING
   PROTECT _types             AS IDictionary<STRING, XTypeRef>
   PROTECT _namespaces        AS IList<STRING>
   PROTECT _attributes        AS IList<String>
   protect _extensionTypes    as IList<TypeDefinition>
   protect _extensionMethods  as IList<MethodDefinition>
   
   PROPERTY Types       as IDictionary<STRING, XTypeRef> GET _types
   PROPERTY Namespaces  as IList<STRING> GET _namespaces
   PROPERTY Attributes  AS IList<String> GET _attributes
   PROPERTY Loaded      AS LOGIC   GET reader != NULL 
   CONSTRUCTOR (cFileName AS STRING)
      SELF:_fileName          := cFileName
      SELF:_types             := Dictionary<STRING, XTypeRef>{StringComparer.OrdinalIgnoreCase}
      SELF:_namespaces        := List<STRING>{}
      SELF:_attributes        := List<String>{}
      SELF:_extensionMethods  := List<MethodDefinition>{}
      SELF:_extensionTypes    := List<TypeDefinition>{}
      TRY
         IF File.Exists(cFileName)
            var resolver := AssemblyResolver{}
            resolver:AddSearchDirectory(System.IO.Path.GetDirectoryName(cFileName))
            var @@params := ReaderParameters{}{AssemblyResolver := resolver}
            reader       := AssemblyDefinition.ReadAssembly(cFileName, @@params)
         ELSE
            SELF:reader  := NULL
         ENDIF
      CATCH
         SELF:reader := NULL
      END TRY
   DESTRUCTOR
      if reader != NULL
         reader:Dispose()
      endif
      
      
   METHOD Read AS XAssembly
      IF SELF:reader != NULL
         var result := XAssembly{_fileName}
         FOREACH VAR module in reader:Modules
            FOREACH var type in module:Types
               VAR vis := _AND(type:Attributes, TypeAttributes.VisibilityMask )
               IF vis == TypeAttributes.Public .OR. vis == TypeAttributes.NestedPublic 
                  VAR name := type:FullName
                  var ns := type:Namespace
                  IF  ns:Length > 0
                     IF ! _namespaces:Contains(ns)
                        _namespaces:Add(ns)
                     ENDIF
                  ENDIF
                  var typeref := XTypeRef{type, result}
                  SELF:_types:Add(name, typeref)
                  
                  if SELF:HasExtensionMethods(type)
                     SELF:LoadExtensionMethods(type)
                  endif
               ENDIF
            NEXT   
         NEXT
         FOREACH var att in reader:CustomAttributes
            var type := att:AttributeType 
    			SWITCH type:ToString():ToLower()
				CASE "vulcan.internal.vulcanclasslibraryattribute"
            CASE "xsharp.internal.classlibraryattribute"
               if att:ConstructorArguments:Count >= 2
                  var arg1 := att:ConstructorArguments[0]:Value:ToString()
                  var arg2 := att:ConstructorArguments[1]:Value:ToString()
					   result:FunctionsClass := arg1
					   IF ! String.IsNullOrEmpty(arg2)
						   result:ImplicitNamespaces:Add(arg2)
                  ENDIF
                  result:IsXSharp := TRUE
               ENDIF
				CASE "vulcan.vulcanimplicitnamespaceattribute"
				CASE "xsharp.implicitnamespaceattribute"
					if att:ConstructorArguments:Count >= 1
                  var ns := att:ConstructorArguments[0]:Value:ToString()
						result:ImplicitNamespaces:Add(ns)
                  result:IsXSharp := TRUE
					ENDIF
   			END SWITCH
         NEXT
         result:TypeList         := SELF:_types
         result:Namespaces       := SELF:_namespaces
         foreach var md in _extensionMethods
            result:ExtensionMethods:Add( XTypeRefMember{md, result })
         next
         RETURN result 
      ENDIF
      RETURN NULL   
      
   PRIVATE METHOD HasExtensionMethods(typedef as TypeDefinition) AS LOGIC
      IF typedef:HasCustomAttributes
         foreach var custatt in typedef:CustomAttributes
            if custatt:AttributeType:FullName == "System.Runtime.CompilerServices.ExtensionAttribute"
               self:_extensionTypes:Add(typedef)
               return true
            ENDIF
         NEXT
      ENDIF
      RETURN FALSE
   
   PRIVATE METHOD LoadExtensionMethods(typedef as TypeDefinition) AS LOGIC
      FOREACH var md in typedef:Methods:Where( { m=> m:HasCustomAttributes} )
         foreach var custatt in md:CustomAttributes
            if custatt:AttributeType:FullName == "System.Runtime.CompilerServices.ExtensionAttribute"
               SELF:_extensionMethods:Add(md)
               EXIT
            ENDIF
         NEXT
      NEXT
      RETURN FALSE
      
      
   PRIVATE CLASS AssemblyResolver IMPLEMENTS IAssemblyResolver, IDisposable
      PRIVATE INITONLY _directories := HashSet<string>{StringComparer.OrdinalIgnoreCase} AS HashSet<string>
      
      PUBLIC METHOD AddSearchDirectory(directory AS string ) AS void
         SELF:_directories:Add(directory)
         
         
      PUBLIC METHOD Resolve(name AS AssemblyNameReference ) AS AssemblyDefinition
         RETURN SELF:Resolve(name, ReaderParameters{})
         
         
      PUBLIC METHOD Resolve(name AS AssemblyNameReference , parameters AS ReaderParameters ) AS AssemblyDefinition
         LOCAL assembly AS AssemblyDefinition
         //
         assembly := SELF:SearchDirectory(name, SELF:_directories, parameters)
         IF (assembly != null)
            RETURN assembly
         ENDIF
         THROW AssemblyResolutionException{name}
         
         
      PUBLIC METHOD Dispose() AS void
      
      
      PRIVATE METHOD SearchDirectory(name AS AssemblyNameReference , directories AS IEnumerable<string> , parameters AS ReaderParameters ) AS AssemblyDefinition
         LOCAL extensions  AS string[]
         LOCAL file        AS string
         //
         extensions := IIF((!name:IsWindowsRuntime) , <string> {".exe",".dll"} , <string>{".winmd",".dll"})
         FOREACH directory AS string IN directories 
            FOREACH extension AS string IN extensions 
               file := Path.Combine(directory, name:Name + extension)
               IF File.Exists(file)
                  TRY
                     RETURN SELF:GetAssembly(file, parameters)
                     
                  CATCH AS BadImageFormatException
                     RETURN NULL
                  END TRY
               ENDIF
            NEXT
         NEXT
         RETURN null
         
         
      PRIVATE METHOD GetAssembly(file AS string , parameters AS ReaderParameters ) AS AssemblyDefinition
         IF (parameters:AssemblyResolver == null)
            parameters:AssemblyResolver := SELF
         ENDIF
         RETURN ModuleDefinition.ReadModule(file , parameters):Assembly
         
         
   END CLASS
   
END CLASS
END NAMESPACE