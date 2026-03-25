//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp
USING System.Collections.Concurrent
USING System.Reflection
#define XSHARPRDD "XSharp.Rdd"  // Make sure this is the same as the file name for XSharp.Rdd (includin the case)
BEGIN NAMESPACE XSharp.RDD
    /// <include file="XSharp.Core.Docs.xml" path="doc/RegisteredRDD/*" />
    CLASS RegisteredRDD
        /// <include file="XSharp.Core.Docs.xml" path="doc/RegisteredRDD.AssemblyName/*" />
        PROPERTY AssemblyName   AS STRING AUTO
        /// <include file="XSharp.Core.Docs.xml" path="doc/RegisteredRDD.Assembly/*" />
        PROPERTY Assembly       AS Assembly AUTO
        /// <include file="XSharp.Core.Docs.xml" path="doc/RegisteredRDD.RddName/*" />
        PROPERTY RddName        AS STRING AUTO
        /// <include file="XSharp.Core.Docs.xml" path="doc/RegisteredRDD.RddType/*" />
        PROPERTY RddType        AS System.Type AUTO
        /// <include file="XSharp.Core.Docs.xml" path="doc/RegisteredRDD.TypeName/*" />
        PROPERTY TypeName       AS STRING AUTO
        STATIC PRIVATE RDDs     AS ConcurrentDictionary<STRING, RegisteredRDD>

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
            RDDs := ConcurrentDictionary<STRING, RegisteredRDD>{StringComparer.OrdinalIgnoreCase}
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "CAVODBF", "XSharp.RDD.DBF"})          // Just DBF
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBF",     "XSharp.RDD.DBF"})          // Just DBF
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFDBT",  "XSharp.RDD.DBFDBT"})       // DBF + DBT
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFNTX",  "XSharp.RDD.DBFNTX"})       // DBF + DBT + NTX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFFPT",  "XSharp.RDD.DBFFPT"})       // DBF + FPT
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFCDX",  "XSharp.RDD.DBFCDX"})       // DBF + FPT + CDX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFVFP",  "XSharp.RDD.DBFVFP"})       // DBF + FPT + CDX + VFP fields
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFVFPSQL","XSharp.RDD.DBFVFPSQL"})       // DBF + FPT + CDX + VFP fields
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DELIM",   "XSharp.RDD.DELIM"})         // DELIM
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "SDF",     "XSharp.RDD.SDF"})           // SDF
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "TSV",     "XSharp.RDD.TSV"})           // TSV = tab separated
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "CSV",     "XSharp.RDD.CSV"})           // CSV = semi colon separated

            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFMEMO", "XSharp.RDD.DBFMEMO"})      // DBF + NTX + DBV
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFMEMOCDX", "XSharp.RDD.DBFMEMOCDX"}) // DBF + CDX + DBV
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFBLOB", "XSharp.RDD.DBFBLOB"})      // DBV only
            //RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFSMT",  "XSharp.RDD.DBFSMT"})       // DBF + SMT
            //RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "DBFNSX",  "XSharp.RDD.DBFNSX"})       // DBF + SMT + NSX

            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "ADSADT",    "Advantage.ADSADT"})       // ADSADT
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "AXDBFCDX",  "Advantage.AXDBFCDX"})       // ADS DBFCDX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "AXDBFNTX",  "Advantage.AXDBFNTX"})       // ADS DBFNTX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "AXDBFVFP",  "Advantage.AXDBFVFP"})       // ADS AXDBFVFP

            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "AXSQLCDX",  "Advantage.AXSQLCDX"})       // SQL CDX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "AXSQLNTX",  "Advantage.AXSQLNTX"})       // SQL NTX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "AXSQLVFP",  "Advantage.AXSQLVFP"})       // SQL VFP
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "AXSQLADT",  "Advantage.AXSQLADT"})       // SQL ADT

            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "Advantage.ADSADT",    "Advantage.ADSADT"})         // ADSADT
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "Advantage.AXDBFCDX",  "Advantage.AXDBFCDX"})       // ADS DBFCDX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "Advantage.AXDBFNTX",  "Advantage.AXDBFNTX"})       // ADS DBFNTX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "Advantage.AXDBFVFP",  "Advantage.AXDBFVFP"})       // ADS AXDBFVFP

            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "Advantage.AXSQLCDX",  "Advantage.AXSQLCDX"})       // ADS DBFCDX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "Advantage.AXSQLNTX",  "Advantage.AXSQLNTX"})       // ADS DBFNTX
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "Advantage.AXSQLVFP",  "Advantage.AXSQLVFP"})       // ADS AXDBFVFP
            RegisteredRDD.Add( RegisteredRDD{XSHARPRDD, "Advantage.AXSQLADT",  "Advantage.AXSQLADT"})       // ADS AXDBFVFP


            RETURN

        /// <include file="XSharp.Core.Docs.xml" path="doc/RegisteredRDD.Find/*" />
        STATIC METHOD Find(cRddName AS STRING) AS RegisteredRDD
            IF RDDs:TryGetValue(cRddName, OUT oRdd AS RegisteredRDD)
                RETURN oRdd
            ENDIF
            RETURN NULL

        /// <include file="XSharp.Core.Docs.xml" path="doc/RegisteredRDD.Add/*" />
        STATIC METHOD Add(oRdd AS RegisteredRDD) AS LOGIC
            LOCAL cRddName AS STRING
            cRddName := oRdd:RddName
            IF RDDs:TryAdd(cRddName, oRdd)
                RETURN TRUE
            ENDIF
            RETURN FALSE

        /// <include file="XSharp.Core.Docs.xml" path="doc/RegisteredRDD.Load/*" />
        METHOD Load() AS VOID
            IF SELF:RddType == NULL
                IF SELF:Assembly == NULL
                    SELF:Assembly := AssemblyHelper.Load(SELF:AssemblyName)
                ENDIF
                IF SELF:Assembly != NULL
                    SELF:RddType := SELF:Assembly:GetType(SELF:TypeName)
                ENDIF
            ENDIF

    END CLASS

END NAMESPACE




