//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD

	CLASS RddError INHERIT Error
    CONSTRUCTOR()
         RETURN
    CONSTRUCTOR(msg AS string)
      SUPER(msg)
         RETURN
          

STATIC METHOD PostArgumentError( funcName AS STRING, subcode AS DWORD, argName AS STRING, argNum AS DWORD, args AS OBJECT[] ) AS VOID
    LOCAL e AS Error
    e := Error.ArgumentError(funcName, argname,argNum, args)
    e:SubSystem := "DBCMD"
    e:Severity := ES_ERROR
    e:GenCode := EG_ARG
    e:SubCode := subcode
    THROW e
STATIC METHOD PostNoTableError( funcName AS STRING ) AS RddError
    LOCAL e := RddError{} AS RddError
    e:SubSystem := "DBCMD"
    e:Severity := 2
    e:GenCode := EG_NOTABLE
    e:SubCode := EDB_NOTABLE
    e:FuncSym := funcName
    THROW e
STATIC METHOD  PostError( funcName AS STRING, gencode AS DWORD, subcode AS DWORD ) AS VOID
    LOCAL e AS RddError
    e := RddError{}
    e:SubSystem := "DBCMD"
    e:Severity := ES_ERROR
    e:GenCode := gencode
    e:SubCode := subcode
    e:FuncSym := funcName
    THROW e
	END CLASS
END NAMESPACE // XSharp.Rdd

// Generate NOTABLE Error    
    

    

    
    
