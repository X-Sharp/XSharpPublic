USING System
USING System.Linq
USING System.Collections.Generic
USING System.IO
USING Mono.Cecil


BEGIN NAMESPACE XSharpModel
INTERNAL CLASS AssemblyReader
   PROTECT reader             AS AssemblyDefinition
   PROTECT _extensionMethods  AS List<MethodDefinition>
   PROTECT _nameSpaces        AS List<STRING>

   CONSTRUCTOR (cFileName AS STRING)
      TRY
         _nameSpaces := List<STRING>{}
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
         assembly:Version  := reader:MainModule:Assembly:Name:Version:ToString()
         IF reader:MainModule:HasAssemblyReferences
            FOREACH var refasm in reader:MainModule:AssemblyReferences
               assembly:ReferencedAssemblies:Add(refasm:FullName)
            NEXT
         ENDIF
         assembly:Types:Clear()
         assembly:RuntimeVersion := reader:MainModule:RuntimeVersion
         FOREACH VAR module in reader:Modules
            FOREACH var type in module:Types
               SELF:AddType(type,assembly,null)
            NEXT
         NEXT
         FOREACH VAR att IN reader:CustomAttributes
            VAR type := att:AttributeType
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
			    IF att:ConstructorArguments:Count >= 1
                    VAR ns := att:ConstructorArguments[0]:Value:ToString()
				    assembly:ImplicitNamespaces:Add(ns)
                    assembly:IsXSharp := TRUE
			    ENDIF
   			END SWITCH
         NEXT
         foreach var md in _extensionMethods
            assembly:ExtensionMethods:Add( XPEMethodSymbol{md, assembly })
         next
         assembly:Namespaces             := _nameSpaces
         assembly:Loaded                 := TRUE
         if ! String.IsNullOrEmpty(assembly:GlobalClassName)
             var globaltype := assembly:Types[assembly:GlobalClassName]
             var members := globaltype:XMembers:Where ({ m => m:IsPublic })
             foreach var mem in members
                 assembly:GlobalMembers:TryAdd(mem:Name, mem)
             next
         elseif assembly:Types:ContainsKey("Functions")
             var globaltype := assembly:Types["Functions"]
             var members := globaltype:XMembers:Where ({ m => m:IsPublic })
             foreach var mem in members
                 assembly:GlobalMembers:TryAdd(mem:Name, mem)
             next
         endif
         RETURN
      ENDIF
      RETURN

   PRIVATE METHOD AddType(type as TypeDefinition, assembly as XAssembly, parentType as XPETypeSymbol) AS VOID
      VAR vis := _AND(type:Attributes, TypeAttributes.VisibilityMask )
      IF vis == TypeAttributes.Public .OR. vis == TypeAttributes.NestedPublic
         VAR name := type:FullName
         // we represent nested types by changing the parent to be a namespace
         IF name:Contains("/")
                name := name:Replace("/",".")
         ENDIF
         VAR typeref := XPETypeSymbol{type, assembly}
         IF parentType != NULL
             parentType:AddChild(typeref)
         ENDIF
         VAR ns := typeref:Namespace
         IF  ns:Length > 0
            IF ! _nameSpaces:Contains(ns)
               _nameSpaces:Add(ns)
            ENDIF
         ENDIF
         IF ! assembly:Types:ContainsKey(name)
            assembly:Types:Add(name, typeref)      // FullName
         ELSE
            IF assembly:DuplicateTypes == NULL
               assembly:DuplicateTypes := List<XPETypeSymbol>{}
            ENDIF
            XSolution.WriteOutputMessage("**************************")
            XSolution.WriteOutputMessage("Duplicate type name found: "+typeref:FullName)
            XSolution.WriteOutputMessage("**************************")
            assembly:DuplicateTypes:Add(typeref)
         ENDIF
         IF SELF:HasExtensionMethods(type)
            SELF:LoadExtensionMethods(type)
         endif
         if type:HasNestedTypes
               FOREACH var child in type:NestedTypes
                  SELF:AddType(child,assembly, typeref)
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
         FOREACH directory AS STRING IN directories
            FOREACH extension AS STRING IN extensions
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
