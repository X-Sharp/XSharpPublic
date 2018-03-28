//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
BEGIN NAMESPACE XSharpModel

    STATIC CLASS CompletionTypeExtensions
        STATIC METHOD IsEmpty( SELF cType AS CompletionType) AS Logic
            RETURN cType == null .OR. ! cType:IsInitialized


    END CLASS

END NAMESPACE 

