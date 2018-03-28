
using System
using System.Collections.Generic
BEGIN NAMESPACE XSharpModel

    STATIC CLASS DictionaryExtensions
        // Methods
        STATIC METHOD AddUnique<TKey, TValue>( SELF dict AS Dictionary<TKey, TValue>, key AS TKey, value AS TValue) AS TValue where TValue is new()
            //
            IF ((dict != null) .AND. (key != null))
                //
                IF (! dict:ContainsKey(key))
                    //
                    dict:Add(key, value)
                    RETURN value
                ENDIF
                RETURN dict:Item[key]
            ENDIF
            RETURN TValue{}


    END CLASS

END NAMESPACE 

