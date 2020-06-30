USING System
USING System.Linq
USING System.Collections.Generic
USING System.IO
USING Mono.Cecil

CLASS MetadataLoader
   PROTECT reader AS AssemblyDefinition
   PROTECT _fileName as STRING
   PROTECT _typeNames  AS List<STRING>
   PROTECT _namespaces AS List<STRING>
   PROTECT _attributes AS List<String>
   protect _extensionTypes   as List<TypeDefinition>
   protect _extensionMethods as List<string>
   
   PROPERTY TypeNames  as List<STRING> GET _typeNames
   PROPERTY Namespaces as List<STRING> GET _namespaces
   PROPERTY Attributes AS List<String> GET _attributes
   PROPERTY Loaded     AS LOGIC   GET reader != NULL 
   CONSTRUCTOR (cFileName AS STRING)
      SELF:_fileName          := cFileName
      SELF:_typeNames         := List<STRING>{}
      SELF:_namespaces        := List<STRING>{}
      SELF:_attributes        := List<String>{}
      SELF:_extensionMethods  := List<String>{}
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
   METHOD LoadTypesAndNamespaces AS VOID
      IF SELF:reader != NULL
        FOREACH VAR module in reader:MOdules
            FOREACH var type in module:Types
            VAR vis := _AND(type:Attributes, TypeAttributes.VisibilityMask )
            IF vis == TypeAttributes.Public .OR. vis == TypeAttributes.NestedPublic 
               VAR name := type:FullName
               var ns := type:Namespace
               _typeNames:Add(name)
               IF ns:Length > 0
                  IF ! _namespaces:Contains(ns)
                     _namespaces:Add(ns)
                  ENDIF
               ENDIF
               if SELF:HasExtensionMethods(type)
                  SELF:LoadExtensionMethods(type)
               endif
            ENDIF
         NEXT   
         NEXT
      FOREACH var att in reader:CustomAttributes
         var type := att:AttributeType 
         var ns := type:Namespace:ToLower()
         if ns:StartsWith("xsharp") .or. ns:StartsWith("vulcan")
            self:_attributes:Add(type:FullName)
//            foreach var arg in att:ConstructorArguments
//               ? arg:Type, arg:Value
//            next
         endif
      NEXT
      ENDIF
      
//      foreach var name in _attributes
//         ? name
//      next
      
      RETURN 
      
      PRIVATE METHOD HasExtensionMethods(typedef as TypeDefinition) AS LOGIC
         IF typedef:HasCustomAttributes
            foreach var custatt in typedef:CustomAttributes
               if custatt:AttributeType:FullName == "System.Runtime.CompilerServices.ExtensionAttribute"
                  self:_ExtensionTypes:Add(typedef)
                  return true
               ENDIF
            NEXT
         ENDIF
         RETURN FALSE
         
      PRIVATE METHOD LoadExtensionMethods(typedef as TypeDefinition) AS LOGIC
         FOREACH var md in typedef:Methods:Where( { m=> m:HasCustomAttributes} )
             
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
		LOCAL extensions AS string[]
		LOCAL @@array AS string[]
		LOCAL @@file AS string
		//
		extensions := IIF((!name:IsWindowsRuntime) , <string>;
			{".exe",;
			".dll";
			} , <string>;
			{".winmd",;
			".dll";
			})
		FOREACH directory AS string IN directories 
			@@array := extensions
			FOREACH extension AS string IN @@array 
				@@file := Path.Combine(directory, name:Name + extension)
				IF (@@File.Exists(@@file))
					TRY
						RETURN SELF:GetAssembly(@@file, parameters)

               CATCH AS BadImageFormatException
                  RETURN NULL
					END TRY
				ENDIF
			NEXT
		NEXT
		RETURN null


	PRIVATE METHOD GetAssembly(@@file AS string , parameters AS ReaderParameters ) AS AssemblyDefinition
		IF (parameters:AssemblyResolver == null)
			parameters:AssemblyResolver := SELF
		ENDIF
		RETURN ModuleDefinition.ReadModule(@@file, parameters):Assembly


END CLASS

   END CLASS
   