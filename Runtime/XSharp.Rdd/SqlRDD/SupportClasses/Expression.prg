//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

BEGIN NAMESPACE XSharp.RDD.SqlRDD

/// <summary>
/// The Expression class.
/// </summary>
CLASS SqlDbExpression INHERIT SqlDbObject
    #ifdef DEBUG
        STATIC PUBLIC SqlTranslator as TranslateFunctionDelegate
    #endif
    PROPERTY Owner      AS OBJECT AUTO
    PROPERTY VoKey		AS STRING AUTO
    PROPERTY Tokens		AS IList<SqlDbToken> AUTO
    PROPERTY Funcs		AS IList<SqlDbToken> AUTO
    PROPERTY HasFunctions	AS LOGIC AUTO
    PROPERTY CurToken 	AS INT AUTO
    PROPERTY KeyLen		AS INT GET SELF:VoKey:Length
    PROPERTY ColumnList AS List<String> AUTO
    PROPERTY Translated AS LOGIC AUTO
    PROPERTY Segments	AS IList<SqlDbSegment> AUTO
    PROPERTY Widths		AS IList<LONG> AUTO

    PROPERTY OrderListString as String => List2String(SELF:OrderList)
    PROPERTY ColumnListString as String => List2String(SELF:ColumnList)


    CONSTRUCTOR(oOwner as Object, cIndexExpr AS STRING)
        SUPER(cIndexExpr)
        SELF:Owner := oOwner
        SELF:VoKey := SELF:TranslateField(cIndexExpr)
        // Parse the expression and create a column list (for index on)
        // and an OrderList (for Orderby clauses)
        // and more
        SELF:CurToken := 1
        SELF:Tokens  	:= List<SqlDbToken>{}
        SELF:Funcs	  	:= List<SqlDbToken>{}
        SELF:ColumnList := List<String>{}
        SELF:Segments   := SELF:GetSegments( SELF:VoKey)
        SELF:ParseVoKey()
        #ifdef DEBUG
        //oCb				:= oOwner:oWorkArea:oConnInfo:oCallBack
        //SELF:TranslateFunctions(oCb)
        SELF:TranslateFunctions(SqlTranslator)
        #endif
        IF SELF:Segments:Count > 1
            FOREACH var oSeg in SELF:Segments
                VAR oExp 			:= SqlDbExpression{oOwner, oSeg:Key}
                //oExp:CalculateColumnWidths()
                oSeg:HasFunctions   := oExp:HasFunctions
                oSeg:SQLKey			:= oExp:SQLKey
                oSeg:OrderList 	    := oExp:OrderList
                oSeg:ColumnList	    := oExp:ColumnList
                oSeg:Widths		    := oExp:Widths
            NEXT
        ELSE
            VAR oSeg := SELF:Segments:First()
            oSeg:HasFunctions   := SELF:HasFunctions
            oSeg:SQLKey			:= SELF:SQLKey
            oSeg:OrderList 	    := SELF:OrderList
            oSeg:ColumnList	    := SELF:ColumnList
            oSeg:Widths		    := SELF:Widths
        ENDIF
        RETURN
    PROTECT METHOD TranslateField(cIndexExpr AS STRING) AS STRING
        LOCAL cField as STRING
        cField := "_FIELD->"
        DO WHILE TRUE
            var index := cIndexExpr:IndexOf(cField, StringComparison.OrdinalIgnoreCase)
            if index == -1
                EXIT
            ENDIF
            cIndexExpr :=cIndexExpr:Substring(0, index)+ cIndexExpr:Substring(index)
        ENDDO
        RETURN cIndexExpr


    PROTECT METHOD AddToken(symType AS TokenType, sb AS StringBuilder) AS VOID
        AddToken(symType, sb:ToString())
    PROTECT METHOD AddToken(symType AS TokenType, cName as STRING) AS VOID
        LOCAL oFunc		AS SqlDbToken
        LOCAL oToken 	AS SqlDbToken
        LOCAL i			AS DWORD
        oToken := SqlDbToken{symType, cName}
        IF symType = TokenType.Token
            oToken:Name     := cName
            oToken:SQLName 	:= cName
            IF cName:ToUpper() == ".OR." .or. cName:ToUpper() == ".AND."
                oToken:Type     := TokenType.Logic
                oToken:SQLName 	:= cName:Replace(".","")
            ELSEIF IsValidToken(cName)
                oToken:Type := TokenType.Token
                SELF:ColumnList:Add(cName)
            ELSE
                oToken:Type := TokenType.Literal
            ENDIF
        ELSEIF symType = TokenType.BoFunc
            oToken:Name 	:= cName
            oToken:SQLName := cName
        ENDIF
        SELF:Tokens:Add(oToken)
        IF symType == TokenType.BoFunc
            oFunc := oToken
            SELF:Funcs:Add(oFunc)
            oFunc:PCount := 1
            SELF:HasFunctions := TRUE

        ELSEIF symType == TokenType.Delimiter
            IF SELF:Funcs:Count > 0
                oFunc := SELF:Funcs:Last()
                oFunc:PCount 	+= 1
                oToken:Name 	:= ""
                oToken:SQLName := ""
            ENDIF
        ELSEIF symType == TokenType.EoFunc
            IF SELF:Funcs:Count > 0
                oFunc	:= SELF:Funcs:Last()
                FOR i := 1 TO oFunc:PCount
                    IF i > 1
                        oFunc:Name += ","
                    ENDIF
                    oFunc:Name += "%"+i:ToString()+"%"
                NEXT
                oFunc:Name += ")"
                oFunc:SQLName := oFunc:Name
                SELF:Funcs:RemoveAt(SELF:Funcs:Count-1)
                oToken:SQLName := ""
            ENDIF
        ENDIF
        RETURN
    METHOD CalculateColumnWidths AS VOID
        /*
        LOCAL dw 	AS DWORD
        LOCAL cCol	AS STRING
        LOCAL nWidth AS DWORD
        LOCAL nPos	AS DWORD
        TRACE ENTER
        SELF:aWidths := ArrayNew(ALen(SELF:aColumns))
        FOR dw := 1 TO ALen(SELF:aColumns)
        cCol   := aColumns[dw]
        nPos	:= FieldPos(cCol)
        IF nPos > 0
        nWidth := SELF:Owner:oWorkArea:aColumnWidths[nPos]
        ELSE
        nWidth := 0
        ENDIF
        aWidths[dw] := nWidth
        TRACE cCol, nWidth
        NEXT
        RETURN
        */


    PROTECT METHOD GetExpression(nLevel AS DWORD) AS STRING
        LOCAL cKey	 AS STRING
        cKey := ""
        IF ! SELF:Translated
            RETURN ""
        ENDIF
        DO WHILE SELF:NextToken() <> NULL_OBJECT
            VAR oToken := SELF:GetToken()
            var done := FALSE
            SWITCH oToken:Type
            CASE TokenType.EoExpr
                cKey += oToken:SQLName
                done := TRUE
            CASE TokenType.EoFunc
                // doe niets
            CASE TokenType.Delimiter
                cKey += oToken:SQLName
            CASE TokenType.Operator
                cKey += " "+oToken:SQLName +" "
            CASE TokenType.Logic
                DO CASE
                CASE oToken:SQLName = "!"
                    cKey += " Not "
                OTHERWISE
                    cKey += " "+oToken:SQLName +" "
                ENDCASE
            CASE TokenType.BoFunc
                oToken:Args := List<Object>{}
                FOR VAR i := 1 TO oToken:PCount
                    oToken:Args:Add(SELF:GetExpression(++nLevel))
                NEXT
                cKey += oToken:Func2String()

            OTHERWISE
                cKey += oToken:SQLName
            END SWITCH
            if done
                Exit
            ENDIF
        ENDDO
        RETURN cKey

    PROTECT METHOD GetToken AS SqlDbToken
        LOCAL oToken  := NextToken() AS SqlDbToken
        IF oToken != NULL
            ++SELF:CurToken
        ENDIF
        RETURN oToken

    PROTECT METHOD NextToken AS SqlDbToken
        LOCAL oToken := NULL	AS SqlDbToken
        IF SELF:CurToken <= SELF:Tokens:Count
            oToken := SELF:Tokens[SELF:CurToken]
        ENDIF
        RETURN oToken

    METHOD GetSegments(cKey AS STRING) AS IList<SqlDbSegment>

        LOCAL aSeg	AS List<SqlDbSegment>
        LOCAL cSeg	AS STRING
        LOCAL nPos	AS DWORD
        LOCAL nLev	AS INT
        aSeg := List<SqlDbSegment>{}
        nPos := AtC("DESCEND(" , cKey)
        DO WHILE nPos > 0
            cSeg := GetSegKey(Left(cKey, nPos-1))
            IF ! String.IsNullOrEmpty(cSeg)
                aSeg:Add(SqlDbSegment{SELF, cSeg, FALSE})
            ENDIF
            cKey := SubStr2(cKey, nPos)
            nLev := 1
            nPos := 9
            DO WHILE nPos <= cKey:Length .AND. nLev > 0
                VAR cChar := cKey[(int) nPos-1]
                IF  cChar == c'('
                    ++nLev
                ELSEIF cChar == c')'
                    IF nLev == 1
                        // take part upto close parenthesis of DESCEND(..)
                        cSeg := Left(cKey, nPos-1)
                        // Remove 'DESCEND('
                        cSeg := SubStr2(cSeg, 9)
                        aSeg:Add( SqlDbSegment{SELF, cSeg, TRUE})
                        cKey := SubStr2(cKey, nPos+1)
                        EXIT
                    ELSE
                        --nLev
                    ENDIF
                ENDIF
                ++nPos
            ENDDO
            nPos := AtC("DESCEND(" , cKey)
        ENDDO
        cKey := GetSegKey(cKey)
        IF ! String.IsNullOrEmpty(cKey)
            aSeg:Add(SqlDbSegment{SELF, cKey,FALSE})
        ENDIF
        RETURN aSeg


    PRIVATE METHOD List2String(list as List<String>) as STring
        var sb := StringBuilder{}
        var first := TRUE
        foreach var column in list
            if first
                first := FALSE
            else
                sb:Append(", ")
            endif
            sb:Append(column)
        next
        return sb:ToString()
    ACCESS OrderList AS List<String>
        LOCAL aList AS List<String>
        IF SELF:Segments:Count > 1
            aList := List<String>{}
            FOREACH var oSeg in SELF:Segments
                FOREACH VAR col in oSeg:OrderList
                    IF oSeg:Descending
                        aList:Add(col+" DESC")
                    ELSE
                        aList:Add(col+" ASC")
                    ENDIF
                NEXT
            NEXT
        ELSE
            var oSeg := SELF:Segments:First()
            IF SELF:HasFunctions
                aList := List<String>{} {SELF:SQLKey}
            ELSE
                aList := SELF:ColumnList
            ENDIF
            IF oSeg:Descending
                FOR VAR i := 1 to aList:Count
                    aList[i] += " DESC"
                NEXT
            ENDIF
        ENDIF
        RETURN aList

    ACCESS SQLKey as STRING
        SELF:CurToken := 1
        local cSqlKey as STRING
        cSqlKey 	:= SELF:GetExpression(1):Trim()
        IF cSqlKey:StartsWith("(") .and. cSqlKey:EndsWith(")")
            cSqlKey := cSqlKey:Substring(1, cSqlKey:Length-2)
        ENDIF
        RETURN cSqlKey
    METHOD AddTokenWhenNotEmpty(token as StringBuilder, symType as TokenType) AS VOID
        IF token:Length >0
            SELF:AddToken(symType, token)
        ENDIF
    PROTECT METHOD ParseVoKey() AS VOID
        VAR token    := StringBuilder{SELF:KeyLen}
        VAR symType  := TokenType.None
        VAR aPars	  	:= List<TokenType>{}
        LOCAL cEnd	:= '\0'	AS CHAR
        SELF:AddToken(TokenType.BoExpr,"")
        VAR lInString := FALSE
        FOREACH VAR cChar in SELF:VoKey
            VAR lClear 	:= FALSE
            DO CASE
            CASE lInString
                token:Append(cChar)
                IF cChar == cEnd
                    SELF:AddToken(symType, token)
                    lClear 	  := TRUE
                    lInString := FALSE
                ENDIF
            CASE cChar = c' '
                SELF:AddTokenWhenNotEmpty(token, symType)
                SELF:AddToken(TokenType.Blank,"")
                lClear := TRUE
            CASE cChar = c'!'
                SELF:AddTokenWhenNotEmpty(token, symType)
                SELF:AddToken(TokenType.Logic,cChar:ToString())
                lClear := TRUE
            CASE cChar == c'\'' .or. cChar == c'\"'
                // Start of string
                lInString 	 := TRUE
                token:Clear()
                token:Append(cChar)
                symType := TokenType.String
                cEnd := cChar
            CASE cChar == c'['
                // Start of string
                lInString 	:= TRUE
                token:Clear()
                token:Append(cChar)
                cEnd	 	:= c']'

            CASE IsOperator(cChar)
                IF token:Length > 0
                    IF symType == TokenType.Operator
                        token:Append(cChar)
                        SELF:AddToken(symType, token)
                        lClear := TRUE
                    ELSE
                        SELF:AddToken(symType, token)
                        token:Clear()
                        token:Append(cChar)
                        symType	    := TokenType.Operator
                    ENDIF
                ENDIF
                token:Clear()
                token:Append(cChar)
                symType    := TokenType.Operator
            CASE IsDelimiter(cChar)
                SELF:AddTokenWhenNotEmpty(token, symType)
                SELF:AddToken(TokenType.EoExpr,"")
                SELF:AddToken(TokenType.Delimiter, cChar:ToString())
                SELF:AddToken(TokenType.BoExpr,"")
                lClear := TRUE
            CASE cChar == c'('
                IF token:Length > 0 .AND. symType == TokenType.Token
                    token:Append(cChar)
                    SELF:AddToken(TokenType.BoFunc, token)
                    SELF:AddToken(TokenType.BoExpr, "")
                    aPars:Add(TokenType.BoFunc)
                    lClear := TRUE
                ELSE
                    SELF:AddTokenWhenNotEmpty(token, symType)
                    SELF:AddToken(TokenType.BoExpr, cChar:ToString())
                    aPars:Add(TokenType.BoExpr)
                ENDIF
            CASE cChar == c')'
                SELF:AddTokenWhenNotEmpty(token, symType)
                IF aPars:Count > 0 .and. aPars:Last() = TokenType.BoExpr
                    SELF:AddToken(TokenType.EoExpr,cChar:ToString())
                ELSE
                    SELF:AddToken(TokenType.EoExpr,"")
                    SELF:AddToken(TokenType.EoFunc,cChar:ToString())
                ENDIF
                aPars:RemoveAt(aPars:Count-1)
                lClear := TRUE

            OTHERWISE
                IF symType != TokenType.Token .AND. symType != TokenType.None
                    SELF:AddToken(symType, token)
                    token:Clear()
                ENDIF
                token:Append(cChar)
                symType := TokenType.Token
            ENDCASE
            IF lClear
                token:Clear()
                symType    := TokenType.None
            ENDIF
        NEXT
        SELF:AddTokenWhenNotEmpty(token, symType)
        SELF:AddToken(TokenType.EoExpr,"")
        RETURN
    ACCESS SegmentCount AS LONG
        RETURN SELF:Segments:Count
    METHOD TranslateFunctions(oDel AS TranslateFunctionDelegate) AS VOID
        FOREACH var token in SELF:Tokens
            SWITCH token:Type
            CASE TokenType.BoFunc
                IF token:Name:StartsWith("DESCEND",StringComparison.OrdinalIgnoreCase)
                    token:SQLName := "(%1%)"
                    token:Descend := TRUE
                ELSE
                    token:SQLName := oDel(token:Name:ToUpper())
                ENDIF
            CASE TokenType.Operator
                IF token:Name:Trim() = "+"
                    token:SQLName := oDel("+")
                ENDIF
            END SWITCH
        NEXT
        SELF:Translated := TRUE
        RETURN
    STATIC METHOD IsDelimiter(cChar as Char) AS LOGIC
        RETURN cChar == c',' .or. cChar == c';'

    STATIC METHOD IsOperator(cChar as Char) AS LOGIC
        SWITCH cChar
        CASE c'*' ; CASE c'/'
        CASE c'+' ; CASE c'-'
        CASE c'>' ; CASE c'<'
        CASE c'='
            RETURN TRUE
        END SWITCH
        RETURN FALSE
    STATIC METHOD IsValidToken(cToken AS STRING) AS LOGIC
        var first := TRUE
        FOREACH VAR c in cToken
            VAR lOk := FALSE
            IF c == c'_'
                lOk := TRUE
            ELSEIF first
                lOk := Char.IsLetter(c)
            ELSE
                lOk := Char.IsLetterOrDigit(c)
            ENDIF
            IF ! lOk
                RETURN FALSE
            ENDIF
            first := FALSE
        NEXT
        RETURN TRUE
    STATIC METHOD GetSegKey(cK AS STRING) AS STRING
        cK := cK:Trim()
        IF cK.StartsWith("+")
            cK := cK:Substring(1)
        ENDIF
        IF cK:EndsWith("+")
            cK := cK:Substring(0, cK:Length-1)
        ENDIF
        RETURN cK

END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD


DELEGATE TranslateFunctionDelegate(cName as STRING) AS STRING
