// NtxItem.prg
// Created by    : fabri
// Creation Date : 6/24/2018 8:54:48 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD

	/// <summary>
    /// The NtxItem class.
    /// </summary>
	CLASS NtxItem
        PROTECTED _Page AS NtxPage
        PROTECTED _Offset AS WORD
 
    CONSTRUCTOR( page AS NtxPage, offset AS WORD)
         RETURN

	END CLASS
END NAMESPACE // global::XSharp.RDD.Types.DbfNtx