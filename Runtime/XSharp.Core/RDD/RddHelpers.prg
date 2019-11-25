//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
USING System.Collections.Generic
USING System.Reflection
#define XSHARPRDD "XSharp.Rdd"  // Make sure this is the same as the file name for XSharp.Rdd (includin the case)
BEGIN NAMESPACE XSharp.RDD
            
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
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "CAVODBF", "XSharp.RDD.DBF"})          // Just DBF
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBF",     "XSharp.RDD.DBF"})          // Just DBF
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFDBT",  "XSharp.RDD.DBFDBT"})       // DBF + DBT
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFNTX",  "XSharp.RDD.DBFNTX"})       // DBF + DBT + NTX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFFPT",  "XSharp.RDD.DBFFPT"})       // DBF + FPT
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFCDX",  "XSharp.RDD.DBFCDX"})       // DBF + FPT + CDX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFVFP",  "XSharp.RDD.DBFVFP"})       // DBF + FPT + CDX + VFP fields
            //RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFMEMO", "XSharp.RDD.DBFMEMO"})      // DBF + NTX + DBV
            //RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFBLOB", "XSharp.RDD.DBFBLOB"})      // DBV only
            //RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFSMT",  "XSharp.RDD.DBFSMT"})       // DBF + SMT
            //RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFNSX",  "XSharp.RDD.DBFNSX"})       // DBF + SMT + NSX
            
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "ADSADT",    "XSharp.ADS.ADSADT"})       // ADSADT
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "AXDBFCDX",  "XSharp.ADS.AXDBFCDX"})       // ADS DBFCDX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "AXDBFNTX",  "XSharp.ADS.AXDBFNTX"})       // ADS DBFNTX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "AXDBFVFP",  "XSharp.ADS.AXDBFVFP"})       // ADS AXDBFVFP
            
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "AXSQLCDX",  "XSharp.ADS.AXSQLCDX"})       // SQL CDX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "AXSQLNTX",  "XSharp.ADS.AXSQLNTX"})       // SQL NTX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "AXSQLVFP",  "XSharp.ADS.AXSQLVFP"})       // SQL VFP
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "AXSQLADT",  "XSharp.ADS.AXSQLADT"})       // SQL ADT

            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "Advantage.ADSADT",    "XSharp.ADS.ADSADT"})         // ADSADT
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "Advantage.AXDBFCDX",  "XSharp.ADS.AXDBFCDX"})       // ADS DBFCDX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "Advantage.AXDBFNTX",  "XSharp.ADS.AXDBFNTX"})       // ADS DBFNTX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "Advantage.AXDBFVFP",  "XSharp.ADS.AXDBFVFP"})       // ADS AXDBFVFP

            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "Advantage.AXSQLCDX",  "XSharp.ADS.AXSQLCDX"})       // ADS DBFCDX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "Advantage.AXSQLNTX",  "XSharp.ADS.AXSQLNTX"})       // ADS DBFNTX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "Advantage.AXSQLVFP",  "XSharp.ADS.AXSQLVFP"})       // ADS AXDBFVFP
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "Advantage.AXSQLADT",  "XSharp.ADS.AXSQLADT"})       // ADS AXDBFVFP
            
            
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




