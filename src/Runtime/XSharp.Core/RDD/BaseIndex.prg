//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp.RDD.Support

/// <include file="XSharp.Core.Docs.xml" path="doc/BaseIndex/*" />
CLASS XSharp.RDD.BaseIndex IMPLEMENTS IOrder
	PRIVATE _oArea AS Workarea


 /// <include file="XSharp.Core.Docs.xml" path="doc/BaseIndex.ctor/*" />
	CONSTRUCTOR(oArea AS Workarea)
		_oArea := oArea

	/// <inheritdoc />
	VIRTUAL METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
		IF info = NULL
            info := DbOrderCondInfo{}
        ENDIF
        info:Compile(_oArea)
        info:Validate()
        _oArea:OrderCondInfo := info
        RETURN TRUE

	/// <inheritdoc />
	VIRTUAL METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC
		THROW NotImplementedException{}

	/// <inheritdoc />
	VIRTUAL METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}

	/// <inheritdoc />
	VIRTUAL METHOD OrderInfo(nOrdinal AS DWORD, info AS DbOrderInfo) AS OBJECT
		SWITCH nOrdinal
        CASE DBOI_NUMBER
        CASE DBOI_KEYNO
        // CASE DBOI_POSITION alias for KEYNO
        // CASE DBOI_KEYGOTO  alias for KEYNO
		CASE DBOI_RECNO
		CASE DBOI_ORDERCOUNT
		CASE DBOI_OPTLEVEL
		CASE DBOI_KEYSIZE
		CASE DBOI_KEYCOUNT
		CASE DBOI_KEYTYPE
		CASE DBOI_KEYSINCLUDED
        CASE DBOI_KEYNORAW
       	//CASE DBOI_KEYGOTORAW 	alias for KEYNORAW
		CASE DBOI_KEYCOUNTRAW
		CASE DBOI_KEYDEC
		CASE DBOI_LOCKOFFSET
        CASE DBOI_BAGCOUNT
            info:Result := NULL
		CASE DBOI_CONDITION
        CASE DBOI_EXPRESSION
		CASE DBOI_NAME
		CASE DBOI_BAGNAME
		//CASE DBOI_INDEXNAME  alias for BAGNAME
		CASE DBOI_BAGEXT
		CASE DBOI_DEFBAGEXT
		//CASE DBOI_INDEXEXT  alias for BAGEXT
		CASE DBOI_FULLPATH
		CASE DBOI_COLLATION
            info:Result := ""
        CASE DBOI_FILEHANDLE
            info:Result := IntPtr.Zero
        CASE DBOI_FILESTREAM
            info:Result := NULL
		CASE DBOI_ISCOND
		CASE DBOI_ISDESC
        CASE DBOI_UNIQUE
		CASE DBOI_CUSTOM
		CASE DBOI_HPLOCKING
            info:Result := FALSE
		CASE DBOI_KEYADD
		CASE DBOI_KEYDELETE
		CASE DBOI_KEYVAL
		CASE DBOI_SCOPETOP
		CASE DBOI_SCOPEBOTTOM
		CASE DBOI_SCOPETOPCLEAR
		CASE DBOI_SCOPEBOTTOMCLEAR
		CASE DBOI_SETCODEBLOCK
		CASE DBOI_SKIPUNIQUE
            info:Result := NULL
        END SWITCH
        RETURN info:Result

	/// <inheritdoc />
	VIRTUAL METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}

	/// <inheritdoc />
	VIRTUAL METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}

	/// <inheritdoc />
	VIRTUAL METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
		THROW NotImplementedException{}

	/// <inheritdoc />
	VIRTUAL METHOD OrderListRebuild( ) AS LOGIC
		THROW NotImplementedException{}

	/// <inheritdoc />
	VIRTUAL METHOD Seek(info AS DbSeekInfo) AS LOGIC
		THROW NotImplementedException{}

	/// <inheritdoc />
	VIRTUAL PROPERTY Found AS LOGIC	GET _oArea:Found SET _oArea:Found := value

	/// <inheritdoc />
	VIRTUAL METHOD Flush() 							AS LOGIC
		THROW NotImplementedException{}

END CLASS
