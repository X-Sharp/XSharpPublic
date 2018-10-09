//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp
    INTERFACE IIndexedProperties
        PROPERTY SELF[index AS INT   ] AS USUAL GET SET
        PROPERTY SELF[name  AS STRING] AS USUAL GET SET
    END INTERFACE

END NAMESPACE    
