//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp
    /// <summary> This interface can be used to access any object with an indexer.</summary>
    INTERFACE IIndexedProperties
        PROPERTY SELF[index AS INT   ] AS USUAL GET SET
        PROPERTY SELF[name  AS STRING] AS USUAL GET SET
    END INTERFACE

    /// <summary> This interface is used to index a collection using the VO Array syntax</summary>
    INTERFACE IIndexer
        PUBLIC PROPERTY SELF[index PARAMS INT[]] AS USUAL GET SET
    END INTERFACE

    /// <summary> This interface is used to index a collection using a numeric and a string index.</summary>
    INTERFACE INamedIndexer
        PUBLIC PROPERTY SELF[index AS INT, name as STRING] AS USUAL GET SET
    END INTERFACE


END NAMESPACE    
