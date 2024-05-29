// AEventsDefinition.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.VFP.UI

    /// <summary>
    /// The AEventsDefinition class.
    /// </summary>
    PUBLIC CLASS AEventsDefinition
        PUBLIC EventSource AS USUAL
        PUBLIC EventName   AS STRING
        PUBLIC EventType   AS USUAL

        CONSTRUCTOR( s AS USUAL, n AS STRING, t := 0 AS USUAL )
            EventSource := s
            EventName := n
            EventType := t

    END CLASS

END NAMESPACE // XSharp.VFP.UI



