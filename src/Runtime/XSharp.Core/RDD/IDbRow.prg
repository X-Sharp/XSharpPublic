//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharp.RDD.Support
/// <include file="XSharp.Core.Docs.xml" path="doc/IDbRow/*" />
INTERFACE XSharp.RDD.IDbRow
    PROPERTY RecNo as DWORD GET SET
END INTERFACE


INTERFACE XSharp.IDbConnectionClient
    PROPERTY DbConnection as System.Data.Common.DbConnection GET
END INTERFACE
