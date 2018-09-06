//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// Code to load assemblies such as the Macro compiler and RDD system
using System.Reflection
STATIC CLASS XSharp.AssemblyHelper
    STATIC METHOD Load(cName as string) as Assembly
        local cLowerName := cName:ToLower() as STRING
        // first locate the assembly that has the macro compiler in the list of loaded assemblies
        FOREACH oAsm AS Assembly IN AppDomain.CurrentDomain:GetAssemblies()
            IF oAsm:GetName():Name:ToLower() == cLowerName
                return oAsm
            ENDIF
        NEXT
        var oCore := typeof(Error):Assembly
        // locate the dll in the GAC 
        var oName := oCore:GetName()
        var cFullName := oName:FullName:Replace("XSharp.Core", cName)
        TRY
            var oAsm := Assembly.Load(cFullName)
            RETURN oAsm
        CATCH AS Exception
            NOP
        END TRY
        // locate in the same folder as X# Core
        var cFileName := oCore:Location:ToLower()
        cFileName := cFileName:Replace("xsharp.core", cName)
        IF System.IO.File.Exists(cFileName)
            TRY
                VAR oAsm  := Assembly.LoadFrom(cFileName)
                return oAsm
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
                return oAsm
            CATCH  AS Exception
                THROW Error{EG_CORRUPTION, "", "Could not load "+cName+ " from the file "+cFileName}
            END TRY
        ENDIF
        THROW Error{EG_CORRUPTION, "", "Could not load "+cName}
END CLASS
