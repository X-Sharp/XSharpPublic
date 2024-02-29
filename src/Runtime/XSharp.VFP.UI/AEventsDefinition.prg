// AEventsDefinition.prg
// Created by    : fabri
// Creation Date : 11/20/2023 11:44:07 AM
// Created for   :
// WorkStation   : FABXPS


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



