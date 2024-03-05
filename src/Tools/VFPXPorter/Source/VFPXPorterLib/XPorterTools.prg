// XPorterTools.prg
// Created by    : fabri
// Creation Date : 9/24/2023 6:11:26 PM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING Newtonsoft.Json
USING System.IO
USING System.Xml.Serialization
USING VFPXPorterLib


/// <summary>
/// The XPorter Tools  .
/// </summary>
FUNCTION DeserializeJSON( jsonFile AS STRING ) AS Dictionary<STRING, Dictionary<STRING,STRING>>
    LOCAL jsonData AS Dictionary<STRING, Dictionary<STRING,STRING>>
    LOCAL orgData AS Dictionary<STRING, Dictionary<STRING,STRING>>
    LOCAL innerData AS Dictionary<STRING,STRING>
    //
    orgData := JsonConvert.DeserializeObject<Dictionary<STRING, Dictionary<STRING,STRING>>>( jsonFile )
    // Create a Dictionary, with string keys and a case-insensitive comparer
    jsonData := Dictionary<STRING, Dictionary<STRING,STRING>>{ StringComparer.InvariantCultureIgnoreCase }
    //
    FOREACH topElement AS KeyValuePair<STRING, Dictionary<STRING,STRING>> IN orgData
        // Create a Dictionary, with string keys and a case-insensitive comparer
        innerData := Dictionary<STRING,STRING>{StringComparer.InvariantCultureIgnoreCase}
        FOREACH subElement AS KeyValuePair<STRING,STRING> IN topElement:Value
            //
            innerData:Add( subElement:Key, subElement:Value )
        NEXT
        jsonData:Add( topElement:Key, innerData )
    NEXT

    RETURN jsonData

FUNCTION DeserializeJSONSimpleArray( jsonFile AS STRING ) AS Dictionary<STRING,STRING[]>
    LOCAL jsonData AS Dictionary<STRING,STRING[]>
    LOCAL orgData AS Dictionary<STRING,STRING[]>
    //
    orgData := JsonConvert.DeserializeObject<Dictionary<STRING,STRING[]>>( jsonFile )
    jsonData := Dictionary<STRING,STRING[]>{StringComparer.InvariantCultureIgnoreCase}
    //
    FOREACH subElement AS KeyValuePair<STRING,STRING[]> IN orgData
        //
        jsonData:Add( subElement:Key, subElement:Value )
    NEXT

    RETURN jsonData

FUNCTION DeserializeJSONArray( jsonFile AS STRING ) AS Dictionary<STRING, Dictionary<STRING,STRING[]>>
    LOCAL jsonData AS Dictionary<STRING, Dictionary<STRING,STRING[]>>
    LOCAL orgData AS Dictionary<STRING, Dictionary<STRING,STRING[]>>
    LOCAL innerData AS Dictionary<STRING,STRING[]>
    //
    orgData := JsonConvert.DeserializeObject<Dictionary<STRING, Dictionary<STRING,STRING[]>>>( jsonFile )
    jsonData := Dictionary<STRING, Dictionary<STRING,STRING[]>>{StringComparer.InvariantCultureIgnoreCase}
    //
    FOREACH topElement AS KeyValuePair<STRING, Dictionary<STRING,STRING[]>> IN orgData
        //
        innerData := Dictionary<STRING,STRING[]>{StringComparer.InvariantCultureIgnoreCase}
        FOREACH subElement AS KeyValuePair<STRING,STRING[]> IN topElement:Value
            //
            innerData:Add( subElement:Key, subElement:Value )
        NEXT
        jsonData:Add( topElement:Key, innerData )
    NEXT

    RETURN jsonData


/// <summary>
/// Clone a List of SCX/VCX Items
/// </summary>
FUNCTION CloneItemList( itemList AS List<BaseItem> ) AS List<BaseItem>
    LOCAL newList AS List<BaseItem>
    newList := List<BaseItem>{}
    FOREACH VAR item IN itemList
        newList:Add( SCXVCXItem{ item } )
    NEXT
    RETURN newList

FUNCTION CloneItemList( itemList AS List<SCXVCXItem> ) AS List<BaseItem>
    LOCAL newList AS List<BaseItem>
    newList := List<BaseItem>{}
    FOREACH VAR item IN itemList
        newList:Add( SCXVCXItem{ item } )
    NEXT
    RETURN newList

FUNCTION CloneItemList( itemList AS List<MNXItem> ) AS List<MNXItem>
    LOCAL newList AS List<MNXItem>
    newList := List<MNXItem>{}
    FOREACH VAR item IN itemList
        newList:Add( MNXItem{ item } )
    NEXT
    RETURN newList

FUNCTION CloneItemList( itemList AS List<FRXItem> ) AS List<FRXItem>
    LOCAL newList AS List<FRXItem>
    newList := List<FRXItem>{}
    FOREACH VAR item IN itemList
        newList:Add( FRXItem{ item } )
    NEXT
    RETURN newList


