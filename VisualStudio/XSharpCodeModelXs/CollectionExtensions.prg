//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Generic
using System.Linq
USING System
BEGIN NAMESPACE XSharpModel

    STATIC ;
    CLASS CollectionExtensions
        // Methods
        STATIC METHOD Find( SELF collection AS IEnumerable<XTypeMember>, pred AS System.Func<XTypeMember, Logic>) AS XTypeMember
            //
            RETURN collection:Where(pred):FirstOrDefault()


    END CLASS

END NAMESPACE // XSharpModel

