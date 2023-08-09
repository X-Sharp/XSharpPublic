// EventObject.prg
// Created by    : robert
// Creation Date : 8/9/2023 9:01:29 AM
// Created for   :
// WorkStation   : NYX


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD.SqlRDD

	/// <summary>
    /// The EventObject class.
    /// </summary>
	CLASS SqlDbEventObject INHERIT SqlDbHandleObject

        CONSTRUCTOR(cName as STRING)
            SUPER(cName)
         RETURN

    PUBLIC EVENT CallBack AS SqlRDDEventHandler
    PROTECTED METHOD RaiseEvent(oObject AS SqlDbEventObject, nEvent AS SqlRDDEventReason, cTable as string, oValue AS Object) AS Object
        VAR oArgs := SqlRddEventArgs{ nEvent, cTable, oValue}
        IF @@CallBack != NULL
            @@CallBack ( oObject, oArgs )
        ENDIF
        RETURN oArgs:Value
    PROTECTED METHOD RaiseStringEvent(oObject AS SqlDbEventObject, nEvent AS SqlRDDEventReason, cTable as string, oValue AS STRING) as String
        var result := RaiseEvent(oObject, nEvent, cTable, oValue)
        if result IS String var strValue
            RETURN strValue
        ENDIF
        return oValue
    PROTECTED METHOD RaiseIntEvent(oObject AS SqlDbEventObject, nEvent AS SqlRDDEventReason, cTable as string, oValue AS INT) as INT
        var result := RaiseEvent(oObject, nEvent, cTable, oValue)
        if result IS Int var intValue
            RETURN intValue
        ENDIF
        return oValue
    PROTECTED METHOD RaiseListEvent(oObject AS SqlDbEventObject, nEvent AS SqlRDDEventReason, cTable as string, oValue AS IList<String>) as IList<String>
        var result := RaiseEvent(oObject, nEvent, cTable, oValue)
        if result IS IList<String> VAR listValue
            RETURN listValue
        ENDIF
        return oValue
    PROTECTED METHOD RaiseLogicEvent(oObject AS SqlDbEventObject, nEvent AS SqlRDDEventReason, cTable as string, oValue AS LOGIC) as LOGIC
        var result := RaiseEvent(oObject, nEvent, cTable, oValue)
        if result IS Logic var logValue
            RETURN logValue
        ENDIF
        return oValue

	END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD.Classes
