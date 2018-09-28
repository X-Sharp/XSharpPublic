//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
USING System.Collections.Generic
USING System.Reflection
BEGIN NAMESPACE XSharp.RDD
    STATIC CLASS RDDHelpers
    
        STATIC METHOD CWA(cFunction AS STRING, lThrow := TRUE AS LOGIC) AS IRDD 
            LOCAL oResult AS IRDD
            RuntimeState.LastRddError := NULL
            oResult := RuntimeState.Workareas:CurrentWorkArea
            IF oResult != NULL_OBJECT
                RETURN oResult
            ENDIF
            IF lThrow
                RddError.PostNoTableError(cFunction)
            ENDIF
            RETURN NULL
            
        STATIC METHOD CWANum(cFunction AS STRING)  AS DWORD
            VAR oWA := RuntimeState.Workareas:CurrentWorkArea
            IF oWA != NULL
                RETURN oWA:Area
            ENDIF
            RddError.PostNoTableError(cFunction)
            RETURN 0
            END CLASS
            
    CLASS RegisteredRDD
        PROPERTY AssemblyName   AS STRING AUTO 
        PROPERTY Assembly       AS Assembly AUTO 
        PROPERTY RddName        AS STRING AUTO
        PROPERTY RddType        AS System.Type AUTO
        PROPERTY TypeName       AS STRING AUTO
        STATIC PRIVATE rDDs     AS Dictionary<STRING, RegisteredRDD>
        
        CONSTRUCTOR(cRDDName AS STRING, oType AS System.Type)
            SELF:RddName        := cRDDName
            SELF:RddType        := oType
            SELF:TypeName       := oType:Name
            SELF:Assembly       := oType:Assembly
            SELF:AssemblyName   := SELF:Assembly:GetName():Name
            RETURN
        CONSTRUCTOR (cAssemblyName AS STRING, cRddName AS STRING, cTypeName AS STRING)
            SELF:AssemblyName := cAssemblyName
            SELF:RddName      := cRddName
            SELF:TypeName     := cTypeName
            RETURN    
            
        STATIC CONSTRUCTOR()
            rDDs    := Dictionary<STRING, RegisteredRDD>{StringComparer.OrdinalIgnoreCase}
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "CAVODBF", "XSharp.RDD.DBF"})          // Just DBF
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "DBF",     "XSharp.RDD.DBF"})          // Just DBF
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "DBFDBT",  "XSharp.RDD.DBFDBT"})       // DBF + DBT
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "DBFNTX",  "XSharp.RDD.DBFNTX"})       // DBF + DBT + NTX
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "DBFFPT",  "XSharp.RDD.DBFFPT"})       // DBF + FPT
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "DBFCDX",  "XSharp.RDD.DBFCDX"})       // DBF + FPT + CDX
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "DBFMEMO", "XSharp.RDD.DBFMEMO"})      // DBF + NTX + DBV
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "DBFBLOB", "XSharp.RDD.DBFBLOB"})      // DBV only
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "DBFSMT",  "XSharp.RDD.DBFSMT"})       // DBF + SMT
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "DBFNSX",  "XSharp.RDD.DBFNSX"})       // DBF + SMT + NSX
            
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "ADSADT",    "XSharp.ADS.ADSADT"})       // ADSADT
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "AXDBFCDX",  "XSharp.ADS.AXDBFCDX"})       // ADS DBFCDX
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "AXDBFNTX",  "XSharp.ADS.AXDBFNTX"})       // ADS DBFNTX
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "AXDBFVFP",  "XSharp.ADS.AXDBFVFP"})       // ADS AXDBFVFP
            
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "Advantage.AXSQLCDX",  "XSharp.ADS.AXSQLCDX"})       // ADS DBFCDX
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "Advantage.AXSQLNTX",  "XSharp.ADS.AXSQLNTX"})       // ADS DBFNTX
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "Advantage.AXSQLVFP",  "XSharp.ADS.AXSQLVFP"})       // ADS AXDBFVFP
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "Advantage.ADSADT",    "XSharp.ADS.ADSADT"})         // ADSADT
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "Advantage.AXDBFCDX",  "XSharp.ADS.AXDBFCDX"})       // ADS DBFCDX
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "Advantage.AXDBFNTX",  "XSharp.ADS.AXDBFNTX"})       // ADS DBFNTX
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "Advantage.AXDBFVFP",  "XSharp.ADS.AXDBFVFP"})       // ADS AXDBFVFP
            
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "Advantage.AXSQLCDX",  "XSharp.ADS.AXSQLCDX"})       // ADS DBFCDX
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "Advantage.AXSQLNTX",  "XSharp.ADS.AXSQLNTX"})       // ADS DBFNTX
            RegisteredRDD.Add( RegisteredRDD{"XSharp.RDD", "Advantage.AXSQLVFP",  "XSharp.ADS.AXSQLVFP"})       // ADS AXDBFVFP
            
            RETURN
            
        STATIC METHOD Find(cRddName AS STRING) AS RegisteredRDD
            IF RDDs:ContainsKey(cRddName)
                RETURN (RegisteredRDD) RDDs:Item[cRddName]
            ENDIF
            RETURN NULL
            
        STATIC METHOD Add(oRDD AS RegisteredRDD) AS LOGIC
            LOCAL cRddname AS STRING
            cRddName := oRDD:RddName
            IF RDDs:ContainsKey(cRddName)
                RETURN FALSE
            ENDIF
            RDDs:Add(cRddName, oRDD)
            RETURN TRUE
            
        METHOD Load() AS VOID
            IF SELF:RddType == NULL 
                IF SELF:Assembly == NULL
                    SELF:Assembly := AssemblyHelper.Load(SELF:AssemblyName)
                ENDIF
                IF (SELF:Assembly != NULL)
                    SELF:RddType := SELF:Assembly:GetType(SELF:TypeName)
                ENDIF
            ENDIF
            
            END CLASS
            
END NAMESPACE




