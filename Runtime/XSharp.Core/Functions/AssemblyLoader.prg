//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// Code to load assemblies such as the Macro compiler and RDD system
USING System.Reflection
STATIC CLASS XSharp.AssemblyHelper
    STATIC METHOD Load(cName AS STRING) AS Assembly
        LOCAL cLowerName := cName:ToLower() AS STRING
        // first locate the assembly that has the macro compiler in the list of loaded assemblies
        FOREACH oAsm AS Assembly IN AppDomain.CurrentDomain:GetAssemblies()
            IF oAsm:GetName():Name:ToLower() == cLowerName
                RETURN oAsm
            ENDIF
        NEXT
        VAR oCore := typeof(Error):Assembly
        // locate the dll in the GAC 
        VAR oName := oCore:GetName()
        VAR cFullName := oName:FullName:Replace("XSharp.Core", cName)
        TRY
            VAR oAsm := Assembly.Load(cFullName)
            RETURN oAsm
        CATCH AS Exception
            NOP
        END TRY
        // locate in the same folder as X# Core
        VAR cFileName := oCore:Location:ToLower()
        cFileName := cFileName:Replace("xsharp.core", cName)
        IF System.IO.File.Exists(cFileName)
            TRY
                VAR oAsm  := Assembly.LoadFrom(cFileName)
                RETURN oAsm
            CATCH  AS Exception
                THROW Error{EG_CORRUPTION, "", "Could not load "+cName+ " from the file "+cFileName}
            END TRY
        ENDIF
        // locate from the X# paths
        cFileName := System.IO.Path.GetFileName(cFileName)
        IF File(cFileName)
            TRY
                cFilename := FPathName()
                VAR oAsm  := Assembly.LoadFrom(cFileName)
                RETURN oAsm
            CATCH  AS Exception
                THROW Error{EG_CORRUPTION, "", "Could not load "+cName+ " from the file "+cFileName}
            END TRY
        ENDIF
        THROW Error{EG_CORRUPTION, "", "Could not load "+cName}
END CLASS
