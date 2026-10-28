//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Collections.Generic
using System.Linq
using System.Text
using XSharp.RDD.SqlRDD.Providers
begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The Expression class.
/// </summary>
internal class SqlDbExpression inherit SqlDbObject
    property Owner      as SqlDbOrder auto
    property XsKey		as string auto
    property Tokens		as IList<SqlDbToken> auto
    property Funcs		as IList<SqlDbToken> auto
    property HasFunctions	as logic auto
    property CurToken 	as int auto
    property KeyLength  as int get self:XsKey:Length
    property ColumnList as List<string> auto
    property Translated as logic auto
    property Segments	as IList<SqlDbSegment> auto
    /// <summary>
    /// The parts of a key that is a plain concatenation of columns and DTOS(column), e.g.
    /// REF_NR+DTOS(DATUM)+BEL_NR, in key order. NULL for any other expression.
    /// </summary>
    property KeyTerms   as List<SqlDbKeyTerm> auto
    private _provider   as ISqlDbProvider

    property OrderListStr as string => SELF:List2String(self:OrderList)
    property ColumnListStr as string => SELF:List2String(self:ColumnList)


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
        if self:Segments:Count == 1
            self:KeyTerms := self:GetKeyTerms(self:XsKey)
        endif
        if self:Segments:Count > 1
            foreach var oSeg in self:Segments
                var oExp 			:= SqlDbExpression{oOwner, oSeg:Key}
                oSeg:HasFunctions   := oExp:HasFunctions
                oSeg:SQLKey			:= oExp:SQLKey
                oSeg:OrderList 	    := oExp:OrderList
                oSeg:ColumnList	    := oExp:ColumnList
            next
        else
            var oSeg := self:Segments:First()
            oSeg:HasFunctions   := self:HasFunctions
            oSeg:SQLKey			:= self:SQLKey
            oSeg:OrderList 	    := self:OrderList
            oSeg:ColumnList	    := self:ColumnList
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
        SELF:AddToken(symType, sb:ToString())
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

    protect method GetExpression(nLevel as dword) as string
        local cKey	 as string
        // Parenthesized groups opened at this level: their closing EoExpr belongs to them and
        // must not end the expression, or everything after e.g. "!(A=='') .AND. B" is dropped.
        local nParens := 0 as int
        cKey := ""
        if ! self:Translated
            return ""
        endif
        do while self:NextToken() <> null_object
            var oToken := self:GetToken()
            var done := false
            switch oToken:Type
            case TokenType.BoExpr
                cKey += oToken:SQLName
                if oToken:SQLName == "("
                    nParens++
                endif
            case TokenType.EoExpr
                cKey += oToken:SQLName
                if nParens > 0 .and. oToken:SQLName == ")"
                    nParens--
                else
                    done := true
                endif
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
        local oToken  := SELF:NextToken() as SqlDbToken
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
        local cDesc as string
        local cAsc  as string

        if self:Owner:Descending
            cDesc   := ""
            cAsc    := " DESC"
        else
            cDesc   := " DESC"
            cAsc    := ""
        endif
        if self:Segments:Count > 1
            aList := List<string>{}
            foreach var oSeg in self:Segments
                foreach var col in oSeg:OrderList
                    if oSeg:Descending
                        aList:Add(col+cDesc)
                    else
                        aList:Add(col+cAsc)
                    endif
                next
            next
        else
            var oSeg := self:Segments:First()
            if !self:HasFunctions
                aList := self:ColumnList:ToList()
            elseif self:KeyTerms != null
                // REF_NR+DTOS(DATUM): DTOS() keeps the order of the date and all other parts are
                // fixed-width columns, so sorting by the raw columns gives the same order as sorting
                // by the concatenated text - and the server can read it from an index on
                // (REF_NR, DATUM) instead of computing and sorting the text for every row.
                // ART_NR+LISTE+STR(MENGE,5,0) the same way, with the STR() part as it is: the order
                // of the text, and the leading columns still come from an index.
                aList := List<string>{}
                foreach var oTerm in self:KeyTerms
                    aList:Add(oTerm:OrderExpr)
                next
            else
                aList := List<string>{} {self:SQLKey}
            endif
            for var i := 0 to aList:Count-1
                if oSeg:Descending
                    aList[i] += cDesc
                else
                    aList[i] += cAsc
                endif
            next
        endif
        return aList

    access SQLKey as string
        self:CurToken := 1
        local cSqlKey as string
        cSqlKey 	:= self:GetExpression(1):Trim()
        if cSqlKey:StartsWith("(") .and. cSqlKey:EndsWith(")") .and. IsEnclosed(cSqlKey)
            cSqlKey := cSqlKey:Substring(1, cSqlKey:Length-2)
        endif
        return cSqlKey

    /// <summary>TRUE when the opening parenthesis at the start closes only at the very end,
    /// so "(A AND B)" is enclosed but "(A) AND (B)" is not.</summary>
    static method IsEnclosed(cExpr as string) as logic
        local nDepth := 0 as int
        local cQuote := '\0' as char
        for var i := 0 to cExpr:Length-1
            var c := cExpr[i]
            if cQuote != '\0'
                if c == cQuote
                    cQuote := '\0'
                endif
            elseif c == c'\'' .or. c == c'"'
                cQuote := c
            elseif c == c'('
                nDepth++
            elseif c == c')'
                nDepth--
                if nDepth == 0 .and. i < cExpr:Length-1
                    return false
                endif
            endif
        next
        return nDepth == 0
    method AddTokenWhenNotEmpty(token as StringBuilder, symType as TokenType) as void
        if token:Length >0
            self:AddToken(symType, token)
        endif
    protect method ParseXsKey() as void
        var token    := StringBuilder{self:KeyLength}
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
    /// <summary>TRUE when oField is a character column of the table that cLiteral covers completely.</summary>
    private method _CoversField(oField as SqlDbToken, cLiteral as string) as logic
        if oField == null .or. oField:Type != TokenType.Token .or. SELF:Owner == null .or. SELF:Owner:RDD == null
            return false
        endif
        var nLen := SELF:Owner:RDD:_CharColumnLength(oField:Name)
        return nLen > 0 .and. cLiteral:Length >= nLen
    end method

    method TranslateFunctions() as void
        local lastToken := null as SqlDbToken
        local prevToken := null as SqlDbToken
        foreach var token in self:Tokens
            switch token:Type
            case TokenType.BoFunc
                if token:Name:StartsWith("DESCEND",StringComparison.OrdinalIgnoreCase)
                    token:SQLName := "(%1%)"
                    token:Descend := true
                elseif token:Name:StartsWith("RECNO(",StringComparison.OrdinalIgnoreCase) .and. !String.IsNullOrEmpty(self:Owner:RDD:RecnoColumnName)
                    // No SQL function for this: RecNo() is the table's recno column.
                    token:SQLName := _provider:QuoteIdentifier(self:Owner:RDD:RecnoColumnName)
                else
                    token:SQLName := _provider:GetFunction(token:Name:ToUpper())
                endif
            case TokenType.Operator
                if token:Name:Trim() = "+"
                    token:SQLName := _provider:GetFunction("+")
                elseif token:Name:Trim() == "=="
                    token:SQLName := "="
                else
                    token:SQLName := token:Name
                endif
            case TokenType.Token
                token:SQLName := _provider.QuoteIdentifier(token:Name)
            case TokenType.String
                if lastToken != null .and. lastToken:Name:Trim() == "=" .and. ;
                    !SELF:_CoversField(prevToken, token:Name:Substring(1, token:Name:Length-2))
                    // A single equals compares like a prefix in xBase (SET EXACT OFF): field = 'A'
                    // is TRUE for 'AB'. The literal (without its quotes) is matched literally.
                    lastToken:SQLName := "like"
                    token:SQLName := _provider:LikePattern(token:Name:Substring(1, token:Name:Length-2))
                else
                    // A literal as long as the field compares the whole field: the prefix compare
                    // is an equality. LIKE is a range for the server: with TYP = 'L' (TYP C(1)) as
                    // FOR condition of ART_NR+REF_NR, it sorted all 477,757 rows of type L for the
                    // first page instead of reading them in the order of the index.
                    token:SQLName := token:Name
                endif
            end switch
            prevToken := lastToken
            lastToken := token
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

    /// <summary>
    /// Split a key into its parts when it only concatenates columns, DTOS(column) and
    /// STR(column, n[, d]), else NULL.
    /// </summary>
    private method GetKeyTerms(cKey as string) as List<SqlDbKeyTerm>
        var aTerms := List<SqlDbKeyTerm>{}
        var cDtos  := _provider:GetFunction("DTOS(%1%)")
        foreach var cPart in cKey:Split(<char>{c'+'})
            var cTerm := cPart:Trim()
            if IsValidToken(cTerm) .and. cTerm:Length > 0
                var cCol := _provider:QuoteIdentifier(cTerm)
                aTerms:Add(SqlDbKeyTerm{cCol, cCol, false})
            elseif cTerm:StartsWith("DTOS(", StringComparison.OrdinalIgnoreCase) .and. cTerm:EndsWith(")")
                var cName := cTerm:Substring(5, cTerm:Length - 6):Trim()
                // a provider without a DTOS() translation cannot compare the date part at all
                if !IsValidToken(cName) .or. cName:Length == 0 .or. String.Equals(cDtos, "DTOS(%1%)", StringComparison.OrdinalIgnoreCase)
                    return null
                endif
                var cCol := _provider:QuoteIdentifier(cName)
                aTerms:Add(SqlDbKeyTerm{cCol, cDtos:Replace("%1%", cCol), true})
            elseif cTerm:StartsWith("STR(", StringComparison.OrdinalIgnoreCase) .and. cTerm:EndsWith(")")
                // STR(MENGE,5,0): a part of fixed width n. Unlike DTOS() its text does not sort like
                // the number (negative values), so the order keeps the expression for this part.
                var aArgs := cTerm:Substring(4, cTerm:Length - 5):Split(<char>{c','})
                var cName := aArgs[0]:Trim()
                local nLen := 10 as long
                local nDec := 0 as long
                if aArgs:Length > 3 .or. !IsValidToken(cName) .or. cName:Length == 0 ;
                    .or. (aArgs:Length > 1 .and. !Int32.TryParse(aArgs[1]:Trim(), out nLen)) ;
                    .or. (aArgs:Length > 2 .and. !Int32.TryParse(aArgs[2]:Trim(), out nDec)) .or. nLen <= 0 .or. nDec < 0
                    return null
                endif
                var cCol := _provider:QuoteIdentifier(cName)
                var cStr := _provider:GetFunction("STR(%1%,%2%,%3%)"):Replace("%1%", cCol):Replace("%2%", nLen:ToString()):Replace("%3%", nDec:ToString())
                aTerms:Add(SqlDbKeyTerm{cCol, cStr, false, nLen})
            else
                return null
            endif
        next
        return iif(aTerms:Count > 0, aTerms, null)

end class

/// <summary>One part of a concatenated key, see SqlDbExpression.KeyTerms.</summary>
internal class SqlDbKeyTerm
    /// <summary>Quoted column name</summary>
    property Column  as string auto get private set
    /// <summary>SQL for the part as it appears in the key text (the column, or DTOS of it)</summary>
    property SqlExpr as string auto get private set
    /// <summary>TRUE for DTOS(column): always 8 characters (yyyymmdd)</summary>
    property IsDate  as logic auto get private set
    /// <summary>Width of a STR(column, n) part, 0 for the other parts</summary>
    property Width   as long auto get private set
    /// <summary>What the part is sorted by: the column, except for STR() (see GetKeyTerms())</summary>
    property OrderExpr as string get iif(SELF:Width > 0, SELF:SqlExpr, SELF:Column)
    constructor(cColumn as string, cSqlExpr as string, lDate as logic, nWidth := 0 as long)
        self:Column  := cColumn
        self:SqlExpr := cSqlExpr
        self:IsDate  := lDate
        self:Width   := nWidth
        return
end class
end namespace // XSharp.RDD.SqlRDD



