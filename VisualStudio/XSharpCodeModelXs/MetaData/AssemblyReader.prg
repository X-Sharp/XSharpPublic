USING System
USING System.Linq
USING System.Collections.Generic
USING System.IO
USING Mono.Cecil


BEGIN NAMESPACE XSharpModel
INTERNAL CLASS AssemblyReader
   PROTECT reader             AS AssemblyDefinition
   PROTECT _extensionMethods  AS List<MethodDefinition>
   
   CONSTRUCTOR (cFileName AS STRING)
      TRY
         IF File.Exists(cFileName)
            var resolver := AssemblyResolver{}
            resolver:AddSearchDirectory(System.IO.Path.GetDirectoryName(cFileName))
            VAR rdrparams := ReaderParameters{}{AssemblyResolver := resolver, InMemory := TRUE}
            reader       := AssemblyDefinition.ReadAssembly(cFileName, rdrparams)
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
      
   METHOD Read(assembly as XAssembly) AS VOID
      IF SELF:reader != NULL
         _extensionMethods  := List<MethodDefinition>{}
         
         assembly:FullName := reader:FullName
         IF reader:MainModule:HasAssemblyReferences
            FOREACH var refasm in reader:MainModule:AssemblyReferences
               assembly:ReferencedAssemblies:Add(refasm:FullName)
            NEXT
         ENDIF
         assembly:Types:Clear()
         assembly:RuntimeVersion := reader:MainModule:RuntimeVersion
         FOREACH VAR module in reader:Modules
            FOREACH var type in module:Types
               SELF:AddType(type,assembly)
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
					   assembly:GlobalClassName := arg1
					   IF ! String.IsNullOrEmpty(arg2)
						   assembly:ImplicitNamespaces:Add(arg2)
                  ENDIF
                  assembly:IsXSharp := TRUE
               ENDIF
				CASE "vulcan.vulcanimplicitnamespaceattribute"
				CASE "xsharp.implicitnamespaceattribute"
					if att:ConstructorArguments:Count >= 1
                  var ns := att:ConstructorArguments[0]:Value:ToString()
						assembly:ImplicitNamespaces:Add(ns)
                  assembly:IsXSharp := TRUE
					ENDIF
   			END SWITCH
         NEXT
         foreach var md in _extensionMethods
            assembly:ExtensionMethods:Add( XMethodReference{md, assembly })
         next
         assembly:Loaded                 := TRUE
         RETURN 
      ENDIF
      RETURN    
      
   PRIVATE METHOD AddType(type as TypeDefinition, assembly as XAssembly) AS VOID
      VAR vis := _AND(type:Attributes, TypeAttributes.VisibilityMask )
      IF vis == TypeAttributes.Public .OR. vis == TypeAttributes.NestedPublic 
         VAR name := type:FullName
         VAR typeref := XTypeReference{type, assembly}
         VAR ns := typeref:Namespace
         IF  ns:Length > 0
            IF ! assembly:Namespaces:Contains(ns)
               assembly:Namespaces:Add(ns)
            ENDIF
         ENDIF
         assembly:Types:Add(name, typeref)      // FullName
         IF SELF:HasExtensionMethods(type)
            SELF:LoadExtensionMethods(type)
         endif
         if type:HasNestedTypes
               FOREACH var child in type:NestedTypes
                  SELF:AddType(child,assembly)
               NEXT
         ENDIF  
     ENDIF
     RETURN
   PRIVATE METHOD HasExtensionMethods(typedef as TypeDefinition) AS LOGIC
      IF typedef:HasCustomAttributes
         foreach var custatt in typedef:CustomAttributes
            if custatt:AttributeType:FullName == "System.Runtime.CompilerServices.ExtensionAttribute"
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
         IF assembly != null
            RETURN assembly
         ENDIF
         THROW AssemblyResolutionException{name}
         
      PUBLIC METHOD Dispose() AS void
         // Does nothing but is required 
         RETURN
      
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
         IF name:Name:ToLower():Contains("mscorlib")
            VAR asm  := typeof(System.String):Assembly
            VAR loc  := asm:Location
            RETURN SELF:GetAssembly(loc, parameters)
         ENDIF
         RETURN NULL
         
      PRIVATE METHOD GetAssembly(file AS string , parameters AS ReaderParameters ) AS AssemblyDefinition
         IF parameters:AssemblyResolver == null
            parameters:AssemblyResolver := SELF
         ENDIF
         RETURN ModuleDefinition.ReadModule(file , parameters):Assembly
         
         
   END CLASS
   
END CLASS
END NAMESPACE