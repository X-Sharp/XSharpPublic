//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Text
USING XSharp.RDD.Support

/// <include file="XSharp.CoreDocs.xml" path="doc/BaseMemo/*" />
CLASS XSharp.RDD.BaseMemo IMPLEMENTS IMemo
    PROTECTED _oArea AS Workarea
    VIRTUAL PROPERTY FullPath as STRING
        GET
            THROW NotImplementedException{}
        END GET
    END PROPERTY



    /// <include file="XSharp.CoreDocs.xml" path="doc/BaseMemo.ctor/*" />
    CONSTRUCTOR(oArea AS Workarea)
        _oArea := oArea

    VIRTUAL PROPERTY LastWrittenBlockNumber AS LONG AUTO GET SET

    // Read & Write
    /// <include file="XSharp.CoreDocs.xml" path="doc/BaseMemo.GetValue/*" />
    VIRTUAL METHOD GetValue(nFldPos AS INT) AS OBJECT
        THROW NotImplementedException{}

        /// <include file="XSharp.CoreDocs.xml" path="doc/BaseMemo.GetValueFile/*" />
    VIRTUAL METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
        THROW NotImplementedException{}

        /// <include file="XSharp.CoreDocs.xml" path="doc/BaseMemo.GetValueLength/*" />
    VIRTUAL METHOD GetValueLength(nFldPos AS INT) AS INT
        THROW NotImplementedException{}

        /// <include file="XSharp.CoreDocs.xml" path="doc/BaseMemo.Flush/*" />
    VIRTUAL METHOD Flush() 			AS LOGIC
        THROW NotImplementedException{}

        /// <include file="XSharp.CoreDocs.xml" path="doc/BaseMemo.PutValue/*" />
    VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
        THROW NotImplementedException{}

        /// <include file="XSharp.CoreDocs.xml" path="doc/BaseMemo.PutValueFile/*" />
    VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
        THROW NotImplementedException{}

        // Memo File Access
        /// <include file="XSharp.CoreDocs.xml" path="doc/BaseMemo.CloseMemFile/*" />
    VIRTUAL METHOD CloseMemFile( ) AS LOGIC
        THROW NotImplementedException{}

        /// <include file="XSharp.CoreDocs.xml" path="doc/BaseMemo.CreateMemFile/*" />
    VIRTUAL METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
        THROW NotImplementedException{}

        /// <include file="XSharp.CoreDocs.xml" path="doc/BaseMemo.OpenMemFile/*" />
    VIRTUAL METHOD OpenMemFile(info AS DbOpenInfo ) AS LOGIC
        THROW NotImplementedException{}

    /// <include file="XSharp.CoreDocs.xml" path="doc/BaseMemo.Zap/*" />
    VIRTUAL METHOD Zap() AS LOGIC
        THROW NotImplementedException{}

END CLASS
