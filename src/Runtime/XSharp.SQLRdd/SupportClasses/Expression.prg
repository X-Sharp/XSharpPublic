//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Collections.Generic
using System.Linq
using System.Text

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The Expression class.
/// </summary>
class SqlDbExpression inherit SqlDbObject
    property Owner      as SqlDbOrder auto
    property XsKey		as string auto
    property Tokens		as IList<SqlDbToken> auto
    property Funcs		as IList<SqlDbToken> auto
    property HasFunctions	as logic auto
    property CurToken 	as int auto
    property KeyLen		as int get self:XsKey:Length
    property ColumnList as List<string> auto
    property Translated as logic auto
    property Segments	as IList<SqlDbSegment> auto
    property Widths		as IList<long> auto
    private _provider  as SqlDbProvider

    property OrderListString as string => SELF:List2String(self:OrderList)
    property ColumnListString as string => SELF:List2String(self:ColumnList)


    constructor(oOwner as SqlDbOrder, cIndexExpr as string)
        super(cIndexExpr)
        self:Owner := oOwner
        self:XsKey := self:TranslateField(cIndexExpr)
        // Parse the expression and create a column list (for index on)
        // and an OrderList (for Orderby clauses)
        // and more
        self:CurToken := 1
        self:Tokens  	:= List<SqlDbToken>{}
        self:Funcs	  	:= List<SqlDbToken>{}
        self:ColumnList := List<string>{}
        self:Segments   := self:GetSegments( self:XsKey)
        _provider := oOwner:Provider
        self:ParseXsKey()
        self:TranslateFunctions()
        if self:Segments:Count > 1
            foreach var oSeg in self:Segments
                var oExp 			:= SqlDbExpression{oOwner, oSeg:Key}
                //oExp:CalculateColumnWidths()
                oSeg:HasFunctions   := oExp:HasFunctions
                oSeg:SQLKey			:= oExp:SQLKey
                oSeg:OrderList 	    := oExp:OrderList
                oSeg:ColumnList	    := oExp:ColumnList
                oSeg:Widths		    := oExp:Widths
            next
        else
            var oSeg := self:Segments:First()
            oSeg:HasFunctions   := self:HasFunctions
            oSeg:SQLKey			:= self:SQLKey
            oSeg:OrderList 	    := self:OrderList
            oSeg:ColumnList	    := self:ColumnList
            oSeg:Widths		    := self:Widths
        endif
        return
    protect method TranslateField(cIndexExpr as string) as string
        local cField as string
        cField := "_FIELD->"
        do while true
            var index := cIndexExpr:IndexOf(cField, StringComparison.OrdinalIgnoreCase)
            if index == -1
                exit
            endif
            cIndexExpr :=cIndexExpr:Substring(0, index)+ cIndexExpr:Substring(index)
        enddo
        return cIndexExpr


    protect method AddToken(symType as TokenType, sb as StringBuilder) as void
        AddToken(symType, sb:ToString())
    protect method AddToken(symType as TokenType, cName as string) as void
        local oFunc		as SqlDbToken
        local oToken 	as SqlDbToken
        local i			as dword
        oToken := SqlDbToken{symType, cName}
        if symType = TokenType.Token
            oToken:Name     := cName
            oToken:SQLName 	:= cName
            if cName:ToUpper() == ".OR." .or. cName:ToUpper() == ".AND."
                oToken:Type     := TokenType.Logic
                oToken:SQLName 	:= cName:Replace(".","")
            elseif IsValidToken(cName)
                oToken:Type := TokenType.Token
                self:ColumnList:Add( _provider:QuoteIdentifier(cName))
            else
                oToken:Type := TokenType.Literal
            endif
        elseif symType = TokenType.BoFunc
            oToken:Name 	:= cName
            oToken:SQLName := cName
        endif
        self:Tokens:Add(oToken)
        if symType == TokenType.BoFunc
            oFunc := oToken
            self:Funcs:Add(oFunc)
            oFunc:PCount := 1
            self:HasFunctions := true

        elseif symType == TokenType.Delimiter
            if self:Funcs:Count > 0
                oFunc := self:Funcs:Last()
                oFunc:PCount 	+= 1
                oToken:Name 	:= ""
                oToken:SQLName := ""
            endif
        elseif symType == TokenType.EoFunc
            if self:Funcs:Count > 0
                oFunc	:= self:Funcs:Last()
                for i := 1 to oFunc:PCount
                    if i > 1
                        oFunc:Name += ","
                    endif
                    oFunc:Name += "%"+i:ToString()+"%"
                next
                oFunc:Name += ")"
                oFunc:SQLName := oFunc:Name
                self:Funcs:RemoveAt(self:Funcs:Count-1)
                oToken:SQLName := ""
            endif
        endif
        return
    method CalculateColumnWidths as void
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


    protect method GetExpression(nLevel as dword) as string
        local cKey	 as string
        cKey := ""
        if ! self:Translated
            return ""
        endif
        do while self:NextToken() <> null_object
            var oToken := self:GetToken()
            var done := false
            switch oToken:Type
            case TokenType.EoExpr
                cKey += oToken:SQLName
                done := true
            case TokenType.EoFunc
                // doe niets
            case TokenType.Delimiter
                cKey += oToken:SQLName
            case TokenType.Operator
                cKey += " "+oToken:SQLName +" "
            case TokenType.Logic
                do case
                case oToken:SQLName = "!"
                    cKey += " Not "
                otherwise
                    cKey += " "+oToken:SQLName +" "
                endcase
            case TokenType.BoFunc
                oToken:Args := List<object>{}
                for var i := 1 to oToken:PCount
                    oToken:Args:Add(self:GetExpression(++nLevel))
                next
                cKey += oToken:Func2String()

            otherwise
                cKey += oToken:SQLName
            end switch
            if done
                exit
            endif
        enddo
        return cKey

    protect method GetToken as SqlDbToken
        local oToken  := NextToken() as SqlDbToken
        if oToken != null
            ++self:CurToken
        endif
        return oToken

    protect method NextToken as SqlDbToken
        local oToken := null	as SqlDbToken
        if self:CurToken < self:Tokens:Count
            oToken := self:Tokens[self:CurToken]
        endif
        return oToken

    method GetSegments(cKey as string) as IList<SqlDbSegment>

        local aSeg	as List<SqlDbSegment>
        local cSeg	as string
        local nPos	as dword
        local nLev	as int
        aSeg := List<SqlDbSegment>{}
        nPos := AtC("DESCEND(" , cKey)
        do while nPos > 0
            cSeg := GetSegKey(Left(cKey, nPos-1))
            if ! String.IsNullOrEmpty(cSeg)
                aSeg:Add(SqlDbSegment{self, cSeg, false})
            endif
            cKey := SubStr2(cKey, nPos)
            nLev := 1
            nPos := 9
            do while nPos <= cKey:Length .and. nLev > 0
                var cChar := cKey[(int) nPos-1]
                if  cChar == c'('
                    ++nLev
                elseif cChar == c')'
                    if nLev == 1
                        // take part upto close parenthesis of DESCEND(..)
                        cSeg := Left(cKey, nPos-1)
                        // Remove 'DESCEND('
                        cSeg := SubStr2(cSeg, 9)
                        aSeg:Add( SqlDbSegment{self, cSeg, true})
                        cKey := SubStr2(cKey, nPos+1)
                        exit
                    else
                        --nLev
                    endif
                endif
                ++nPos
            enddo
            nPos := AtC("DESCEND(" , cKey)
        enddo
        cKey := GetSegKey(cKey)
        if ! String.IsNullOrEmpty(cKey)
            aSeg:Add(SqlDbSegment{self, cKey,false})
        endif
        return aSeg


    private method List2String(list as List<string>) as string
        var sb := StringBuilder{}
        var first := true
        foreach var column in list
            if first
                first := false
            else
                sb:Append(", ")
            endif
            sb:Append(column)
        next
        return sb:ToString()
    access OrderList as List<string>
        local aList as List<string>
        if self:Segments:Count > 1
            aList := List<string>{}
            foreach var oSeg in self:Segments
                foreach var col in oSeg:OrderList
                    if oSeg:Descending
                        aList:Add(col+" DESC")
                    else
                        aList:Add(col+" ASC")
                    endif
                next
            next
        else
            var oSeg := self:Segments:First()
            if self:HasFunctions
                aList := List<string>{} {self:SQLKey}
            else
                aList := self:ColumnList
            endif
            if oSeg:Descending
                for var i := 1 to aList:Count
                    aList[i] += " DESC"
                next
            endif
        endif
        return aList

    access SQLKey as string
        self:CurToken := 1
        local cSqlKey as string
        cSqlKey 	:= self:GetExpression(1):Trim()
        if cSqlKey:StartsWith("(") .and. cSqlKey:EndsWith(")")
            cSqlKey := cSqlKey:Substring(1, cSqlKey:Length-2)
        endif
        return cSqlKey
    method AddTokenWhenNotEmpty(token as StringBuilder, symType as TokenType) as void
        if token:Length >0
            self:AddToken(symType, token)
        endif
    protect method ParseXsKey() as void
        var token    := StringBuilder{self:KeyLen}
        var symType  := TokenType.None
        var aPars	  	:= List<TokenType>{}
        local cEnd	:= '\0'	as char
        self:AddToken(TokenType.BoExpr,"")
        var lInString := false
        foreach var cChar in self:XsKey
            var lClear 	:= false
            do case
            case lInString
                token:Append(cChar)
                if cChar == cEnd
                    self:AddToken(symType, token)
                    lClear 	  := true
                    lInString := false
                endif
            case cChar = c' '
                self:AddTokenWhenNotEmpty(token, symType)
                self:AddToken(TokenType.Blank,"")
                lClear := true
            case cChar = c'!'
                self:AddTokenWhenNotEmpty(token, symType)
                self:AddToken(TokenType.Logic,cChar:ToString())
                lClear := true
            case cChar == c'\'' .or. cChar == c'\"'
                // Start of string
                self:AddTokenWhenNotEmpty(token, symType)
                lInString 	 := true
                token:Clear()
                token:Append(cChar)
                symType := TokenType.String
                cEnd := cChar
            case cChar == c'['
                // Start of string
                lInString 	:= true
                token:Clear()
                token:Append(cChar)
                cEnd	 	:= c']'

            case IsOperator(cChar)
                if token:Length > 0
                    if symType == TokenType.Operator
                        token:Append(cChar)
                        self:AddToken(symType, token)
                        lClear := true
                    else
                        self:AddToken(symType, token)
                        token:Clear()
                        token:Append(cChar)
                        symType	    := TokenType.Operator
                    endif
                endif
                token:Clear()
                token:Append(cChar)
                symType    := TokenType.Operator
            case IsDelimiter(cChar)
                self:AddTokenWhenNotEmpty(token, symType)
                self:AddToken(TokenType.EoExpr,"")
                self:AddToken(TokenType.Delimiter, cChar:ToString())
                self:AddToken(TokenType.BoExpr,"")
                lClear := true
            case cChar == c'('
                if token:Length > 0 .and. symType == TokenType.Token
                    token:Append(cChar)
                    self:AddToken(TokenType.BoFunc, token)
                    self:AddToken(TokenType.BoExpr, "")
                    aPars:Add(TokenType.BoFunc)
                    lClear := true
                else
                    self:AddTokenWhenNotEmpty(token, symType)
                    self:AddToken(TokenType.BoExpr, cChar:ToString())
                    aPars:Add(TokenType.BoExpr)
                endif
            case cChar == c')'
                self:AddTokenWhenNotEmpty(token, symType)
                if aPars:Count > 0 .and. aPars:Last() = TokenType.BoExpr
                    self:AddToken(TokenType.EoExpr,cChar:ToString())
                else
                    self:AddToken(TokenType.EoExpr,"")
                    self:AddToken(TokenType.EoFunc,cChar:ToString())
                endif
                aPars:RemoveAt(aPars:Count-1)
                lClear := true

            otherwise
                if symType != TokenType.Token .and. symType != TokenType.None
                    self:AddToken(symType, token)
                    token:Clear()
                endif
                token:Append(cChar)
                symType := TokenType.Token
            endcase
            if lClear
                token:Clear()
                symType    := TokenType.None
            endif
        next
        self:AddTokenWhenNotEmpty(token, symType)
        self:AddToken(TokenType.EoExpr,"")
        return
    access SegmentCount as long
        return self:Segments:Count
    method TranslateFunctions() as void
        foreach var token in self:Tokens
            switch token:Type
            case TokenType.BoFunc
                if token:Name:StartsWith("DESCEND",StringComparison.OrdinalIgnoreCase)
                    token:SQLName := "(%1%)"
                    token:Descend := true
                else
                    token:SQLName := _provider:GetFunction(token:Name:ToUpper())
                endif
            case TokenType.Operator
                if token:Name:Trim() = "+"
                    token:SQLName := _provider:GetFunction("+")
                endif
            case TokenType.Token
                token:SQLName := _provider.QuoteIdentifier(token:Name)
            end switch
        next
        self:Translated := true
        return
    static method IsDelimiter(cChar as char) as logic
        return cChar == c',' .or. cChar == c';'

    static method IsOperator(cChar as char) as logic
        switch cChar
        case c'*' ; case c'/'
        case c'+' ; case c'-'
        case c'>' ; case c'<'
        case c'='
            return true
        end switch
        return false
    static method IsValidToken(cToken as string) as logic
        var first := true
        foreach var c in cToken
            var lOk := false
            if c == c'_'
                lOk := true
            elseif first
                lOk := Char.IsLetter(c)
            else
                lOk := Char.IsLetterOrDigit(c)
            endif
            if ! lOk
                return false
            endif
            first := false
        next
        return true
    static method GetSegKey(cK as string) as string
        cK := cK:Trim()
        if cK.StartsWith("+")
            cK := cK:Substring(1)
        endif
        if cK:EndsWith("+")
            cK := cK:Substring(0, cK:Length-1)
        endif
        return cK

end class
end namespace // XSharp.RDD.SqlRDD


delegate TranslateFunctionDelegate(cName as string) as string
