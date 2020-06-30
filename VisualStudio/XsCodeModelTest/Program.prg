USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.Reflection
USING System.IO

//FUNCTION Start() AS VOID
//   LOCAL oImport AS MetadataImport
//   LOCAL aFiles AS STRING[]
//   aFiles := System.IO.Directory.GetFiles("c:\XSharp\DevRt\Binaries\Debug","XSharp*.dll")
//   FOREACH VAR cFile IN aFiles
//      ? cFile
//      TRY
//         oImport := MetadataImport{cFile}
//         FOREACH type AS MetadataType IN oImport:DefinedTypes
//            VAR vis := _AND(type:Attributes, TypeAttributes.VisibilityMask )
//            IF vis == TypeAttributes.Public .OR. vis == TypeAttributes.NestedPublic 
//               ? type:Name, type:ParentName, type:Attributes
//            ENDIF
//         NEXT
//      CATCH  AS Exception
//         //? e:Message
//      END TRY
//   NEXT
//   Console.ReadLine()
//   RETURN
//   
//      


FUNCTION Start AS VOID
   LOCAL aFiles AS STRING[]
   aFiles := System.IO.Directory.GetFiles("c:\XSharp\DevRt\Binaries\Debug","*.dll")
   FOREACH VAR cFile IN aFiles
      try
      var loader := MetadataLoader{cFile}
      loader:LoadTypesAndNamespaces()
      if loader:Loaded
      ? cFile, loader:TypeNames:Count, loader:Namespaces:Count, loader:Attributes:Count
      endif
      catch
      end try
   NEXT
   Console.ReadLine()
   RETURN
   
   