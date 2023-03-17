//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections
USING System.Collections.Generic
USING System.Diagnostics
USING System.Globalization
USING System.IO
USING System.Reflection
USING System.Text
USING System.Threading
USING XSharp.RDD.Enums
USING XSharp.RDD.Support

BEGIN NAMESPACE XSharp.RDD.CDX

    INTERNAL CLASS CdxLockException INHERIT Exception
    END CLASS

    INTERNAL PARTIAL CLASS CdxTag
        INTERNAL PROPERTY IsLocked AS LOGIC GET _bag:IsLocked

        INTERNAL PROPERTY LockNeedsRefresh AS LOGIC GET _bag:LockNeedsRefresh

        INTERNAL METHOD NeedsLock() AS VOID
            _bag:NeedsLock()

        INTERNAL METHOD NeedsNoLock() AS VOID
            _bag:NeedsNoLock()

        // Methods for NTX Locking
        INTERNAL METHOD Slock AS LOGIC
            var result := SELF:_bag:SLock()
            SELF:Header:UpdateWhenNeeded()
            RETURN result

        INTERNAL METHOD Xlock AS LOGIC
            var result := SELF:_bag:XLock()
            SELF:Header:UpdateWhenNeeded()
            RETURN result

        INTERNAL METHOD UnLock AS LOGIC
            SELF:_bag:UnLock()
            RETURN TRUE

    END CLASS
END NAMESPACE
