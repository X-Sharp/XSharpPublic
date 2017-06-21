#using System.Text
#using System.Collections


//GLOBAL genenc := Encoding.ASCII AS Encoding
DEFINE ascenc := Encoding.Default AS Encoding
DEFINE Chr09 := e"\t" AS STRING

CLASS xARRAY
	PROTECT aItems := ArrayList{} AS ArrayList
CONSTRUCTOR()
CONSTRUCTOR(u1 AS OBJECT)
	SELF:aItems:Add(u1)
CONSTRUCTOR(u1 AS OBJECT,u2 AS OBJECT)
	SELF:aItems:Add(u1)
	SELF:aItems:Add(u2)
CONSTRUCTOR(u1 AS OBJECT,u2 AS OBJECT,u3 AS OBJECT)
	SELF:aItems:Add(u1)
	SELF:aItems:Add(u2)
	SELF:aItems:Add(u3)
CONSTRUCTOR(u1 AS OBJECT,u2 AS OBJECT,u3 AS OBJECT,u4 AS OBJECT)
	SELF:aItems:Add(u1)
	SELF:aItems:Add(u2)
	SELF:aItems:Add(u3)
	SELF:aItems:Add(u4)

PROPERTY SELF[nIndex AS DWORD] AS OBJECT ;
GET SELF:aItems[(INT)nIndex - 1] ;
SET SELF:aItems[(INT)nIndex - 1] := VALUE
PROPERTY SELF[nIndex1 AS DWORD,nIndex2 AS DWORD] AS OBJECT ;
GET ((xARRAY)(SELF:aItems[(INT)nIndex1 - 1]))[nIndex2] ;
SET ((xARRAY)(SELF:aItems[(INT)nIndex1 - 1]))[nIndex2] := VALUE
METHOD ASize(nSize AS DWORD) AS xARRAY
	IF nSize < SELF:aItems:Count
		SELF:aItems:RemoveRange((INT)nSize, SELF:aItems:Count - nSize)
	ELSEIF nSize > SELF:aItems:Count
		DO WHILE nSize > SELF:aItems:Count
			SELF:aItems:Add(xNIL)
		END DO
	END IF
RETURN SELF
METHOD AAdd(oItem AS OBJECT) AS xARRAY
	SELF:aItems:Add(oItem)
RETURN SELF

METHOD ATail() AS OBJECT
	IF SELF:aItems:Count == 0
		RETURN NULL
	END IF
RETURN SELF:aItems[SELF:aItems:Count - 1]


METHOD ATrueIns(nPos AS DWORD,u AS OBJECT) AS xARRAY
	SELF:aItems:Insert((INT)nPos - 1 , u)
RETURN SELF
METHOD ATrueIns(nPos AS DWORD) AS xARRAY
	SELF:aItems:Insert((INT)nPos - 1 , xNIL)
RETURN SELF
METHOD ATrueDel(nPos AS DWORD) AS xARRAY
	SELF:aItems:RemoveAt((INT)nPos - 1)
RETURN SELF

METHOD ALen() AS DWORD
RETURN (DWORD)SELF:aItems:Count

METHOD Clear() AS VOID
	SELF:aItems:Clear()
RETURN

END CLASS


DEFINE xNIL := NULL AS OBJECT

FUNCTION ASize(a AS xARRAY, nSize AS DWORD) AS xARRAY
RETURN a:ASize(nSize)
FUNCTION AAdd(a AS xARRAY, oItem AS OBJECT) AS xARRAY
RETURN a:AAdd(oItem)
FUNCTION ALen(a AS xARRAY) AS DWORD
RETURN a:ALen()
FUNCTION AKill(a AS xARRAY, nPos AS DWORD) AS xARRAY
RETURN a:ATrueDel(nPos)
FUNCTION ATrueIns(a AS xARRAY,nPos AS DWORD) AS xARRAY
RETURN a:ATrueIns(nPos)
FUNCTION ATail(a AS xARRAY) AS OBJECT
RETURN a:ATail()
FUNCTION ArrayCreate(len AS DWORD) AS xARRAY
	LOCAL a AS xARRAY
	a := xARRAY{}
	FOR LOCAL n := 1 AS DWORD UPTO len
		AAdd(a , xNIL)
	NEXT
RETURN a
FUNCTION Empty(o AS OBJECT) AS LOGIC
	IF o == NULL
		RETURN TRUE
	END IF
	DO CASE
	CASE o:GetType() == TypeOf(INT)
		RETURN (INT)o == 0
	CASE o:GetType() == TypeOf(DWORD)
		RETURN (DWORD)o == 0
	CASE o:GetType() == TypeOf(STRING)
		RETURN Empty((STRING)o)
	OTHERWISE
		THROW Exception{"not handled"}
	END CASE
//RETURN FALSE


FUNCTION Left(c AS STRING , dwLen AS INT) AS STRING
RETURN Left(c , (DWORD)dwLen)
FUNCTION Left(c AS STRING , dwLen AS DWORD) AS STRING
RETURN iif(dwLen >= c:Length , c , c:Substring(0 , (INT)dwLen ))

FUNCTION Right(c AS STRING , dwLen AS INT) AS STRING
RETURN Right(c , (DWORD)dwLen)
FUNCTION Right(c AS STRING , dwLen AS DWORD) AS STRING
RETURN IIF( dwLen >= c:Length , c , c:Substring(c:Length - (INT)dwLen , (INT)dwLen))

FUNCTION SLen(c AS STRING) AS DWORD
RETURN (DWORD)c:Length

FUNCTION SubStr2(c AS STRING, wLen AS DWORD) AS STRING
RETURN c:Substring((INT)wLen - 1)

FUNCTION SubStr3(c AS STRING , iStart AS DWORD , wLen AS DWORD) AS STRING
RETURN SubStr(c , (INT)iStart , (INT)wLen)
FUNCTION SubStr(c AS STRING , iStart AS DWORD , wLen AS DWORD) AS STRING
RETURN SubStr(c , (INT)iStart , (INT)wLen)
FUNCTION SubStr(c AS STRING , iStart AS INT , wLen AS INT) AS STRING
	IF iStart > c:Length
		RETURN ""
	ENDIF
	IF wLen < 0
		wLen := c:Length
	ENDIF
RETURN c:Substring(iStart - 1 , Math.Min(c:Length - iStart + 1 , wLen))

FUNCTION At(cSearch AS STRING , c AS STRING) AS DWORD
RETURN (DWORD)c:IndexOf(cSearch) + 1
FUNCTION RAt(cSearch AS STRING , c AS STRING) AS DWORD
RETURN (DWORD)c:LastIndexOf(cSearch) + 1

FUNCTION AScanExact(a AS STRING[] , cSearch AS STRING) AS INT
RETURN System.Array.IndexOf(a, cSearch) + 1

FUNCTION Upper(s AS STRING) AS STRING
RETURN s:ToUpper()
FUNCTION Lower(s AS STRING) AS STRING
RETURN s:ToLower()

FUNCTION InStr(cSearch AS STRING , c AS STRING) AS LOGIC
	IF cSearch == NULL .or. c == NULL
		RETURN FALSE
	ENDIF
RETURN c:Contains(cSearch)



FUNCTION Empty(c AS STRING) AS LOGIC
	LOCAL nChar AS INT
	LOCAL n AS INT
	IF String.IsNullOrEmpty(c)
		RETURN TRUE
	ENDIF
	FOR n := 0 UPTO c:Length - 1
		nChar := (INT)c[n] 
		IF nChar != 9 .and. nChar != 10 .and. nChar != 13 .and. nChar != 32
			RETURN FALSE
		ENDIF
	NEXT
RETURN TRUE

FUNCTION StrTran(c AS STRING , cSearch AS STRING , cReplace AS STRING) AS STRING
RETURN c:Replace(cSearch , cReplace)


FUNCTION AllTrim(c AS STRING) AS STRING
RETURN c:Trim()

FUNCTION At2(search AS STRING, c AS STRING) AS DWORD
RETURN (DWORD)c:IndexOf(search) + 1

FUNCTION Max(n AS INT,m AS INT) AS INT
RETURN Math.Max(n,m)
FUNCTION File(c AS STRING) AS LOGIC
RETURN System.IO.File.Exists(c)

FUNCTION AScan(a AS xARRAY , cKeyword AS STRING , nKeySize AS DWORD) AS DWORD
//	|cKey| cKeyword == Left( cKey, nKeySize )
	FOR LOCAL n := 1 AS DWORD UPTO ALen(a)
		LOCAL cKey AS STRING
		cKey := (STRING)a[n]
		IF cKeyword == Left( cKey, nKeySize )
			RETURN n
		END IF
	NEXT
RETURN 0

