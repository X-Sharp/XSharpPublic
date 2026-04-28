//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp.RDD.Support
/// <include file="XSharp.Core.Docs.xml" path="doc/IMemo/*" />
INTERFACE XSharp.RDD.IMemo

#region Properties
    PROPERTY FullPath as STRING GET
#endregion
#region Read/Write
    /// <include file="XSharp.Core.Docs.xml" path="doc/IMemo.GetValue/*" />
    METHOD GetValue(nFldPos AS INT) AS OBJECT
    /// <include file="XSharp.Core.Docs.xml" path="doc/IMemo.GetValueFile/*" />
    METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
    /// <include file="XSharp.Core.Docs.xml" path="doc/IMemo.GetValueLength/*" />
    METHOD GetValueLength(nFldPos AS INT) AS INT
    /// <include file="XSharp.Core.Docs.xml" path="doc/IMemo.PutValue/*" />
    METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
    /// <include file="XSharp.Core.Docs.xml" path="doc/IMemo.PutValueFile/*" />
    METHOD PutValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
    /// <include file="XSharp.Core.Docs.xml" path="doc/IMemo.Flush/*" />
    METHOD Flush() 			AS LOGIC
    /// <include file="XSharp.Core.Docs.xml" path="doc/IMemo.Zap/*" />
    METHOD Zap() AS LOGIC
#endregion
#region Memo File Access
    /// <include file="XSharp.Core.Docs.xml" path="doc/IMemo.CloseMemFile/*" />
    METHOD CloseMemFile() 	AS LOGIC
    /// <include file="XSharp.Core.Docs.xml" path="doc/IMemo.CreateMemFile/*" />
    METHOD CreateMemFile(info AS DbOpenInfo) 	AS LOGIC
    /// <include file="XSharp.Core.Docs.xml" path="doc/IMemo.OpenMemFile/*" />
    METHOD OpenMemFile(info AS DbOpenInfo) 	AS LOGIC
#endregion

END INTERFACE

/// <include file="XSharp.Core.Docs.xml" path="doc/IRawData/*" />
INTERFACE XSharp.RDD.IRawData
    /// <include file="XSharp.Core.Docs.xml" path="doc/IRawData.ReturnRawData/*" />
    PROPERTY ReturnRawData AS LOGIC GET SET
END INTERFACE

/// <include file="XSharp.Core.Docs.xml" path="doc/IBlobData/*" />
INTERFACE XSharp.RDD.IBlobData
    /// <include file="XSharp.Core.Docs.xml" path="doc/IBlobData.Pointer/*" />
    PROPERTY Pointer   as INT GET SET
    /// <include file="XSharp.Core.Docs.xml" path="doc/IBlobData.Start/*" />
    PROPERTY Start     AS INT GET SET
    /// <include file="XSharp.Core.Docs.xml" path="doc/IBlobData.Length/*" />
    PROPERTY Length    AS INT GET SET
    /// <include file="XSharp.Core.Docs.xml" path="doc/IBlobData.Data/*" />
    PROPERTY Data      AS OBJECT GET SET
END INTERFACE

