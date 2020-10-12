USING System.Collections.Generic
/// <summary>Enum that matches the various FoxPro cursor properties, used in CursorGetProp() and CursorSetProp()</summary>
/// <seealso "O:XSharp.VFP.Functions.CursorGetProp" />
/// <seealso "O:XSharp.VFP.Functions.CursorSetProp" />

ENUM XSharp.RDD.CursorProperty 
   MEMBER ADOBookmark               
   MEMBER ADOCodePage              
   MEMBER ADORecordset             
   MEMBER AllowSimultaneousFetch   
   MEMBER AutoIncError             
   MEMBER BatchUpdateCount         
   MEMBER Buffering                
   MEMBER CompareMemo              
   MEMBER ConnectHandle            
   MEMBER ConnectName              
   MEMBER Database                 
   MEMBER FetchAsNeeded            
   MEMBER FetchIsComplete          
   MEMBER FetchMemo                
   MEMBER FetchSize                
   MEMBER KeyFieldList             
   MEMBER MapBinary                
   MEMBER MapVarchar               
   MEMBER MaxRecords               
   MEMBER ParameterList            
   MEMBER Prepared                 
   MEMBER RecordsFetched           
   MEMBER Refresh                  
   MEMBER SendUpdates              
   MEMBER SourceName               
   MEMBER SourceType               
   MEMBER SQL                      
   MEMBER Tables                    
   MEMBER UpdatableFieldList        
   MEMBER UpdateNameList            
   MEMBER UpdateType                
   MEMBER UseMemoSize               
   MEMBER WhereType                 
END ENUM

INTERNAL GLOBAL cursorProperties AS Dictionary<STRING, LONG>

FUNCTION GetCursorProperty(propertyName as STRING) AS LONG
    IF cursorProperties == NULL
        cursorProperties := Dictionary<STRING, LONG>{StringComparer.OrdinalIgnoreCase}
        var values := System.Enum.GetValues(typeof(XSharp.RDD.CursorProperty))
        FOREACH var enumvalue in values
            var name := System.Enum.GetName(typeof(XSharp.RDD.CursorProperty), enumvalue)
            cursorProperties:Add(name, (LONG) enumvalue)
        NEXT
    ENDIF
    IF cursorProperties:ContainsKey(propertyName)
        return cursorProperties[propertyName]
    ENDIF
    RETURN -1
