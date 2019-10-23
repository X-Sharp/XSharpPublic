//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// Code to load assemblies such as the Macro compiler and RDD system
USING System.Reflection
/// <exclude />
STATIC CLASS XSharp.AssemblyHelper

    /// <exclude />
    STATIC METHOD LoadFrom(cName AS STRING, cFileName AS STRING) AS Assembly
        TRY
            VAR oAsm  := Assembly.LoadFrom(cFileName)
            RETURN oAsm
        CATCH  AS Exception
            THROW Error{EG_OPEN, "", "Could not load Assembly '"+cName+ "' from the file "+cFileName} {Filename := cFileName,FuncSym := "AssemblyLoader"}
        END TRY
        
        /// <exclude />
    STATIC METHOD Load(cName AS STRING) AS Assembly
        // first locate the assembly that has the macro compiler in the list of loaded assemblies
        FOREACH oAsm AS Assembly IN AppDomain.CurrentDomain:GetAssemblies()
            IF String.Compare(oAsm:GetName():Name,cName, StringComparison.OrdinalIgnoreCase) == 0
                RETURN oAsm
            ENDIF
        NEXT
        VAR oCore := typeof(Error):Assembly
        // locate the dll in the GAC 
        VAR oName       := oCore:GetName()
        VAR cFullName   := oName:FullName:Replace("XSharp.Core", cName)
        TRY
            VAR oAsm := Assembly.Load(cFullName)
            RETURN oAsm
        CATCH AS Exception
            NOP
        END TRY
        // locate in the same folder as X# Core
        VAR cFolder   := System.IO.Path.GetDirectoryName(oCore:Location)+System.IO.Path.DirectorySeparatorChar:ToString()
        VAR cFileName := cFolder  + cName
        VAR cExt      := System.IO.Path.GetExtension(oCore:Location)
        IF ! System.IO.File.Exists(cFileName)
            IF ! cFileName:ToLower():EndsWith(".dll")
               cFilename := cFileName+ cExt
            ELSE
               cFilename := System.IO.Path.ChangeExtension(cFileName, cExt)
            ENDIF
        ENDIF
        IF System.IO.File.Exists(cFileName)
            RETURN AssemblyHelper.LoadFrom(cName, cFileName)
        ENDIF
        // locate from the X# paths
        
        cFileName := System.IO.Path.GetFileName(cFileName)
        IF File(cFileName)
            RETURN AssemblyHelper.LoadFrom(cName, FPathName())
        ENDIF
        // There could be a case difference (on Mono). Lets look in the X# Core folder
        cFileName   := System.IO.Path.GetFileName(cFileName)
        VAR aFiles  := System.IO.Directory.GetFiles(cFolder, "*.*")
        FOREACH VAR cFile IN aFiles
            IF cFile:EndsWith(cFileName, StringComparison.OrdinalIgnoreCase)
                RETURN AssemblyHelper.LoadFrom(cName, cFile)
            ENDIF
        NEXT
            
        THROW Error{EG_OPEN, "", "Could not find "+cName} {Filename := cFileName,FuncSym := "AssemblyLoader"}
END CLASS
