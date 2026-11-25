//
// Copyright (c) XSharp B.V. All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic

BEGIN NAMESPACE XSharp.Data.Query

/// <summary>
/// Candidate rows that are safe to use as prefilters for a query.
/// The full WHERE expression must still be evaluated for candidate row combinations.
/// </summary>
PUBLIC CLASS QueryOptimizationPlan
    PUBLIC PROPERTY CandidateBitmaps AS Dictionary<STRING, RecordBitmap> AUTO

    PUBLIC CONSTRUCTOR()
        SELF:CandidateBitmaps := Dictionary<STRING, RecordBitmap>{StringComparer.OrdinalIgnoreCase}
        RETURN
END CLASS

END NAMESPACE
