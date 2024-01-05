// SQLParser.prg
// Created by    : robert
// Creation Date : 11/9/2023 11:50:41 AM
// Created for   :
// WorkStation   : NYX


USING System
USING System.Linq
USING System.Collections.Generic
USING System.Text
using XSharp.RDD.Enums

BEGIN NAMESPACE XSharp.Parsers
/// <summary>
/// The SQLParser class.
/// </summary>
CLASS SQLParser
    PRIVATE _list as XTokenList
    PRIVATE _Error as STRING
    PROPERTY Error as STRING GET _Error

    METHOD SetError(message as STRING) AS VOID
        SetError(message, Lt1)
    METHOD SetError(message as STRING, token as XToken) AS VOID
        _Error := message+", found '" + token:Text+"' at position "+SELF:Lt1:Start:ToString()
        RETURN

#region Properties and Methods that are delegated to the XSharpTokenList type
    PRIVATE PROPERTY La1 AS XTokenType => _list:La1
    PRIVATE PROPERTY La2 AS XTokenType => _list:La2
    PRIVATE PROPERTY La3 AS XTokenType => _list:La3
    PRIVATE PROPERTY Lt1 AS XToken => _list:Lt1
    PRIVATE PROPERTY Lt2 AS XToken => _list:Lt2
    PRIVATE PROPERTY Lt3 AS XToken => _list:Lt3
    PRIVATE PROPERTY LastToken AS XToken => _list:LastReadToken
    PRIVATE METHOD La(nToken AS LONG) AS LONG => _list:La(nToken)
    PRIVATE METHOD Lt(nToken AS LONG) AS XToken => _list:Lt(nToken)
    PRIVATE METHOD Eoi() AS LOGIC => _list:Eoi()
    PRIVATE METHOD Eos() AS LOGIC => _list:Eos()
    PRIVATE METHOD Consume() AS VOID =>_list:Consume()
    PRIVATE METHOD ConsumeAndGet() AS XToken => _list:ConsumeAndGet()
    PRIVATE METHOD ConsumeAndGetAny(nTypes PARAMS XTokenType[]) AS XToken => _list:ConsumeAndGetAny(nTypes)
    PRIVATE METHOD ConsumeAndGetText() AS STRING => _list:ConsumeAndGetText()
    PRIVATE METHOD Expect(nType AS XTokenType) AS LOGIC => _list:Expect(nType)
    PRIVATE METHOD Expect(cText AS STRING) AS LOGIC => _list:Expect(cText)
    PRIVATE METHOD ExpectAny(nTypes PARAMS XTokenType[]) AS LOGIC => _list:ExpectAny(nTypes)
    PRIVATE METHOD ExpectAndGet(nType AS XTokenType, t OUT XToken) AS LOGIC => _list:ExpectAndGet(nType, OUT t)
    PRIVATE METHOD Matches(nType AS XTokenType) AS LOGIC => _list:La1 == nType
    PRIVATE METHOD Matches(cText AS STRING) AS LOGIC => _list:Matches(cText)
    PRIVATE METHOD Matches(nTypes PARAMS XTokenType[]) AS LOGIC => _list:Matches(nTypes)
    PRIVATE METHOD PushBack() AS VOID => _list:PushBack()
    PRIVATE METHOD ReadLine() AS VOID => _list:ReadLine()
    PRIVATE METHOD ReadUntilEos() AS VOID
        DO WHILE _list:La1 != XTokenType.EOS .and. !_list:Eoi()
            _list:Consume()
        ENDDO
    PRIVATE METHOD ExpectOnThisLine(nType as XTokenType) AS LOGIC
        return _list:ExpectOnThisLine(nType)

#endregion

    CONSTRUCTOR(tokens as XTokenList)
        _list := tokens
        RETURN
    PRIVATE METHOD ParseExpression(endTokens PARAMS XTokenType[]) AS STRING
        RETURN SELF:TokensAsString(SELF:ParseExpressionAsTokens(endTokens))

    PRIVATE METHOD ParseExpressionAsTokens(endTokens as XTokenType[]) AS IList<XToken>
    VAR tokens := List<XToken>{}
    var nestLevel := 0
    DO WHILE ! SELF:Eoi() .and. ! SELF:Eos()
        IF SELF:Matches(XTokenType.LPAREN)
            nestLevel += 1
        ELSEIF SELF:Matches(XTokenType.RPAREN)
            nestLevel -= 1
        ENDIF
        IF SELF:Matches(endTokens) .and. nestLevel == 0
            EXIT
        ENDIF
        tokens:Add(SELF:ConsumeAndGet())
    ENDDO
    RETURN tokens
    PRIVATE METHOD ParseExpressionAsTokens() AS IList<XToken>
        // parse until SELF:Eos() or tokens such as AS, IS,
        LOCAL nested := 0 AS LONG
        LOCAL done  := FALSE AS LOGIC
        VAR tokens := List<XToken>{}

        DO WHILE ! SELF:Eos() .AND. ! done
            SWITCH SELF:La1
            CASE XTokenType.LT
            CASE XTokenType.LPAREN
            CASE XTokenType.LBRKT
            CASE XTokenType.LCURLY
                nested++
            CASE XTokenType.GT
            CASE XTokenType.RPAREN
            CASE XTokenType.RBRKT
            CASE XTokenType.RCURLY
                nested--

            CASE XTokenType.AS
            CASE XTokenType.IS
            CASE XTokenType.COMMA
                // The comma is used for a comma separated list of expression.
                // however inside a method call or constructor call we should have nested > 0
                IF (nested == 0)
                    done := TRUE
                ENDIF

            OTHERWISE
                // other keywords, operators etc.
                //
                NOP
            END SWITCH
            IF !done
                tokens:Add(SELF:ConsumeAndGet())
            ENDIF
        ENDDO
        RETURN tokens

    PRIVATE METHOD ParseExpression() AS STRING
        RETURN SELF:TokensAsString(SELF:ParseExpressionAsTokens())

    PRIVATE METHOD TokensAsString(tokens AS IList<XToken>, lAddTrivia := TRUE AS LOGIC) AS STRING
        LOCAL sb AS StringBuilder
        IF (tokens == NULL .or. tokens:Count == 0)
            RETURN ""
        ENDIF
        sb := StringBuilder{}

        FOREACH t AS XToken IN tokens
            IF t:HasTrivia .AND. lAddTrivia
                sb:Append(t:Leadingws)
            ENDIF
            sb:Append(t:Text)
        NEXT
        RETURN sb:ToString():Trim()


    METHOD ParseConstraints(token as XToken, table as FoxCreateTableContext) as logic

        return true

    #region Worker Methods
    METHOD ParseAlterTable() AS FoxAlterTableContext
        /*
        3 MOdes:
        1 Add/Alter Column
        ALTER TABLE TableName1 ADD | ALTER [COLUMN] FieldName1
              FieldType [( nFieldWidth [, nPrecision])] [NULL | NOT NULL] [CHECK lExpression1 [ERROR cMessageText1]]
           [AUTOINC [NEXTVALUE NextValue [STEP StepValue]]] [DEFAULT eExpression1]
           [PRIMARY KEY | UNIQUE [COLLATE cCollateSequence]]
           [REFERENCES TableName2 [TAG TagName1]] [NOCPTRANS] [NOVALIDATE]

            2. Alternative modiffication of column
        ALTER TABLE TableName1 ALTER [COLUMN] FieldName2 [NULL | NOT NULL] [SET DEFAULT eExpression2]
           [SET CHECK lExpression2 [ERROR cMessageText2]] [ DROP DEFAULT ] [ DROP CHECK ] [ NOVALIDATE ]

            3. Drop Column or set Table properties
            ALTER TABLE TableName1 [DROP [COLUMN] FieldName3]
               [SET CHECK lExpression3 [ERRORcMessageText3]] [DROP CHECK]
               [ADD PRIMARY KEY eExpression3 [FOR lExpression4] TAG TagName2
               [COLLATE cCollateSequence]] [DROP PRIMARY KEY]
               [ADD UNIQUE eExpression4 [[FOR lExpression5] TAG TagName3
                  [COLLATE cCollateSequence]]] [DROP UNIQUE TAG TagName4]
               [ADD FOREIGN KEY [eExpression5] [FOR lExpression6] TAG TagName4
                  REFERENCES TableName4 [TAG TagName4][COLLATE cCollateSequence]
                  REFERENCES TableName2 [TAG TagName5]]
               [DROP FOREIGN KEY TAG TagName6 [SAVE]]
               [RENAME COLUMN FieldName4 TO FieldName5] [NOVALIDATE]

        */
        IF ! SELF:Expect(XTokenType.ALTER)
            SetError("Expected ALTER", SELF:Lt1 )
            return null
        ENDIF
        IF !SELF:Expect(XTokenType.TABLE)
            SetError("Expected TABLE", SELF:Lt1 )
            return null
        endif
        var table := FoxAlterTableContext{}
        if !SELF:ExpectAndGet(XTokenType.ID, out var id)
            SetError("Expected ID", SELF:Lt1 )
            RETURN null
        ENDIF
        table:Name := id:Text
        IF SELF:Expect("ADD")
            table:Mode := FoxAlterMode.AddColumn
            SELF:Expect("COLUMN")
            var cols := List<FoxColumnContext>{}
            IF SELF:ParseColumn(cols, TRUE)
                table:ColumnInfo := cols:First()
            else
                table := NULL
            ENDIF
        elseif SELF:Expect("ALTER")
            table:Mode := FoxAlterMode.AlterColumn
            SELF:Expect("COLUMN")
            var cols := List<FoxColumnContext>{}
            IF SELF:ParseColumn(cols, TRUE)
                table:ColumnInfo := cols:First()
            else
                table := NULL
            ENDIF
        elseif SELF:Expect("DROP")
            table:Mode := FoxAlterMode.DropColumn
            SELF:Expect("COLUMN")
            table:ColumnInfo := FoxColumnContext{}
            IF SELF:ExpectAndGet(XTokenType.ID, out id)
                table:ColumnInfo:Name := id:Text
            else
                table := NULL
            ENDIF
        else
            // This may be a modification of the table properties
            table:Mode := FoxAlterMode.AlterTable
            VAR tokens := List<XToken>{}
            DO WHILE ! SELF:Eos()
                tokens:Add(SELF:ConsumeAndGet())
            ENDDO
            table:TableRules := SELF:TokensAsString(tokens, TRUE)
        ENDIF

        RETURN table

    METHOD ParseCreateTable(table out FoxCreateTableContext) AS LOGIC
        return ParseCreateCursorTable(out table, TRUE)

    METHOD ParseCreateCursor(table out FoxCreateTableContext) AS LOGIC
        return ParseCreateCursorTable(out table, FALSE)

    PRIVATE METHOD ParseCreateCursorTable(table out FoxCreateTableContext, lTable as LOGIC) AS LOGIC
        /*
        CREATE CURSOR alias_name
        ...[CODEPAGE=nCodePage]
           (fname1 cFieldType [(nFieldWidth [, nPrecision])] [NULL | NOT NULL]
           [CHECK lExpression [ERROR cMessageText]]
           [AUTOINC [NEXTVALUE NextValue [STEP StepValue]]]
           [DEFAULT eExpression] [UNIQUE [COLLATE cCollateSequence]]
           [NOCPTRANS] [, fname2 ...])
           | FROM ARRAY ArrayName

        CREATE TABLE | DBF TableName1 [NAME LongTableName] [FREE]
            [CODEPAGE = nCodePage]
            ( FieldName1 FieldType [( nFieldWidth [, nPrecision] )] [NULL | NOT NULL]
            [CHECK lExpression1 [ERROR cMessageText1]]
            [AUTOINC [NEXTVALUE NextValue [STEP StepValue]]] [DEFAULT eExpression1]
            [PRIMARY KEY | UNIQUE [COLLATE cCollateSequence]]
            [REFERENCES TableName2 [TAG TagName1]] [NOCPTRANS]
            [, FieldName2 ... ]
            [, PRIMARY KEY eExpression2 TAG TagName2 |, UNIQUE eExpression3 TAG TagName3
            [COLLATE cCollateSequence]]
            [, FOREIGN KEY eExpression4 TAG TagName4 [NODUP]
            [COLLATE cCollateSequence]
            REFERENCES TableName3 [TAG TagName5]] [, CHECK lExpression2 [ERROR cMessageText2]] )
            | FROM ARRAY ArrayName

        */
        local id   as XToken
        local nCP  as XToken

        table := FoxCreateTableContext{}
        table:IsCursor := !lTable
        IF ! SELF:Expect(XTokenType.CREATE)
            return FALSE
        ENDIF
        IF lTable .and. !SELF:Expect(XTokenType.TABLE)
            return FALSE
        endif
        IF !lTable .and. !SELF:Expect(XTokenType.CURSOR)
            return FALSE
        endif
        if !SELF:ExpectAndGet(XTokenType.ID, out id)
            RETURN FALSE
        ENDIF
        if lTable .and. SELF:Expect("NAME")
            table:LongName := SELF:ConsumeAndGet():Text
        ENDIF
        IF lTable .and. SELF:Expect("FREE")
            table:Free := TRUE
        ENDIF
        table:Name := id:Text
        IF SELF:Expect(XTokenType.CODEPAGE)
            nCP := SELF:ConsumeAndGet()
            IF Int32.TryParse( nCP:Text, out var cp)
                table:CodePage := cp
            endif
        ENDIF
        IF SELF:Expect(XTokenType.LPAREN)
            // read fields
            var done := FALSE
            DO WHILE ! Matches(XTokenType.RPAREN) .and. ! SELF:Eoi() .and. ! done
                if lTable
                    var Lt := SELF:Lt1
                    switch La1
                    case XTokenType.FOREIGN
                    case XTokenType.PRIMARY
                    case XTokenType.REFERENCES
                        IF ! SELF:ParseConstraints(Lt, table)
                            done := true
                        endif
                    case XTokenType.CHECK
                        Consume()
                        table:RuleExpression := SELF:ParseExpression()
                        IF SELF:Expect(XTokenType.ERROR)
                            table:RuleText := SELF:ParseExpression()
                        endif

                    case XTokenType.ID
                        IF ! SELF:ParseColumn(table:Columns,  lTable)
                            RETURN FALSE
                        ENDIF
                        IF SELF:Expect(XTokenType.COMMA)
                            // Another column
                            LOOP
                        endif
                    case XTokenType.RPAREN
                        done := true
                    case XTokenType.COMMA
                        Consume()
                    otherwise
                        NOP
                    end switch
                else
                    IF ! SELF:ParseColumn( table:Columns, lTable)
                        RETURN FALSE
                    ENDIF
                    IF SELF:Expect(XTokenType.COMMA)
                        // Another column
                        LOOP
                    endif
                endif
            ENDDO
            SELF:Expect(XTokenType.RPAREN)
        ENDIF
        IF SELF:Expect(XTokenType.FROM)
            IF SELF:Expect("ARRAY")
                table:ArrayName := SELF:ConsumeAndGet():Text
            ENDIF
        ENDIF
        foreach var column in table:Columns
            column:Table := table
        next

        RETURN TRUE

    METHOD ParseColumn(oColumns as List<FoxColumnContext>, lTable as LOGIC) AS LOGIC
        local name      as XToken
        local oType      as XToken
        local endTokens as XTokenType[]
        local lOk := FALSE as logic
        endTokens := <XTokenType>{XTokenType.CHECK,XTokenType.DEFAULT,XTokenType.AUTOINC,XTokenType.PRIMARY,XTokenType.UNIQUE,XTokenType.ERROR,XTokenType.COMMA}
        var sqlField := FoxColumnContext{}
        oColumns:Add(sqlField)
        IF !SELF:ExpectAndGet(XTokenType.ID, out name)
            SetError("Expected Column Name", SELF:Lt1 )
            RETURN FALSE
        ENDIF
        sqlField:Name    := name:Text
        sqlField:Caption := name:Text
        IF !SELF:ExpectAndGet(XTokenType.ID, out oType)
            SetError("Expected Column Type", SELF:Lt1 )
            RETURN FALSE
        ENDIF
        IF SELF:Expect(XTokenType.LPAREN)
            var len := SELF:ConsumeAndGet()
            if Int32.TryParse(len:Text, out var nLen)
                sqlField:Length := nLen
            else
                SetError("Expected Length" ,len)
                RETURN FALSE
            endif
            IF SELF:Expect(XTokenType.COMMA)
                var dec := SELF:ConsumeAndGet()
                if Int32.TryParse(dec:Text, out var nDec)
                    sqlField:Decimals := nDec
                else
                    SetError("Expected Decimals", dec)
                    RETURN FALSE
                endif
            ENDIF
            IF ! SELF:Expect(XTokenType.RPAREN)
                SetError("Expected ')'")
                return FALSE
            ENDIF
        ENDIF
        LOCAL done := FALSE AS LOGIC
        lOk := TRUE
        DO WHILE !done .and. ! SELF:Eoi() .and. SELF:La1 != XTokenType.RPAREN .and. SELF:La1 != XTokenType.COMMA
            SWITCH SELF:La1
            CASE XTokenType.PRIMARY WHEN SELF:La2 == XTokenType.KEY
                // This is only supported when part of a DBC
                SELF:Consume()
                SELF:Consume()
                sqlField:PrimaryKey := TRUE
            CASE XTokenType.NOT WHEN SELF:La2 == XTokenType.NULL
                SELF:Consume()
                SELF:Consume()
                sqlField:Flags &= _NOT(DBFFieldFlags.Nullable)
            CASE XTokenType.NULL
                SELF:Consume()
                sqlField:Flags |= DBFFieldFlags.Nullable
            CASE XTokenType.CHECK
                SELF:Consume()
                // Only when in DBC
                sqlField:RuleExpression := SELF:ParseExpression(endTokens)
                IF SELF:Expect(XTokenType.ERROR)
                    sqlField:RuleText := SELF:ParseExpression(endTokens)
                endif
                LOOP
            CASE XTokenType.AUTOINC
                SELF:Consume()
                sqlField:Flags |= DBFFieldFlags.AutoIncrement
                sqlField:NextValue := 1
                sqlField:StepValue := 1
                IF SELF:Expect("NEXTVALUE")
                    var expr := SELF:ParseExpression(endTokens)
                    IF Int32.TryParse(expr, out var nextval)
                        sqlField:NextValue := nextval
                    ELSE
                        SetError("Expected NextValue")
                        RETURN FALSE
                    endif
                    IF SELF:Expect("STEP")
                        var step:= SELF:ParseExpression(endTokens)
                        IF Int32.TryParse(step, out var stepval)
                            sqlField:StepValue := stepval
                        ELSE
                            SetError("Expected StepValue")
                            RETURN FALSE
                        endif
                    ENDIF
                ENDIF
            CASE XTokenType.DEFAULT
                SELF:Consume()
                sqlField:DefaultValue := SELF:ParseExpression(endTokens)
            CASE XTokenType.UNIQUE
                // This should create a unique index on the column
                SELF:Consume()
                sqlField:IsUnique := TRUE
            CASE XTokenType.REFERENCES WHEN lTable
                SELF:Consume()
                if SELF:ExpectAndGet(XTokenType.ID, out var foreignTable)
                    sqlField:Foreign := foreignTable:Text
                    IF SELF:Expect("TAG")
                        if SELF:ExpectAndGet(XTokenType.ID, out var foreignTag)
                            sqlField:ForeignTag := foreignTag:Text
                        else
                            SetError("Expected TagName")
                            return false
                        endif
                    ENDIF
                else
                    SetError("Expected Foreign TableName")
                    return false
                endif
            CASE XTokenType.NOCPTRANS
                SELF:Expect(XTokenType.NOCPTRANS)
                sqlField:Flags |= DBFFieldFlags.Binary
            OTHERWISE
                SetError("Unexpected token ", SELF:Lt1)
                lOk := FALSE
                done := TRUE
            END SWITCH
        ENDDO
        if lOk
            sqlField:FieldType := XSharp.RDD.Support.RddFieldInfo.FindType(oType:Text)
            sqlField:Validate()
        endif
        return lOk

    METHOD ParseDeleteStatement(stmt out FoxDeleteContext) AS LOGIC
        /*
        DELETE [Target] FROM [FORCE] Table_List [[, Table_List ...] | [JOIN [ Table_List]]]
           [WHERE FilterCondition1 [AND | OR FilterCondition2 ...]]
        */
        stmt := FoxDeleteContext{}
        IF ! SELF:Expect(XTokenType.DELETE)
            return FALSE
        ENDIF
        IF SELF:Matches(XTokenType.ID)
            stmt:TableName := SELF:ParseTableName()
        ENDIF
        IF SELF:Expect(XTokenType.FROM)
            IF SELF:Expect("FORCE")
                stmt:IsForce := TRUE
            ENDIF
            DO WHILE SELF:Matches(XTokenType.ID)
                var sName := SELF:ParseTableName()
                stmt:TableList:Add(sName)
                SELF:Expect(XTokenType.COMMA)
            ENDDO
        ELSEIF SELF:Matches(XTokenType.JOIN)
            DO WHILE SELF:Matches(XTokenType.ID)
                var sName := SELF:ParseTableName()
                stmt:JoinList:Add(sName)
                SELF:Expect(XTokenType.COMMA)
            ENDDO
        ELSE
            SetError("Expected FROM or JOIN")
            RETURN FALSE
        ENDIF
        IF SELF:Expect(XTokenType.WHERE)
            var sb := StringBuilder{}
            do while ! SELF:Eos() .and. ! SELF:Eoi()
                var token := SELF:ConsumeAndGet()
                sb:Append(token:Leadingws)
                sb:Append(token:Text)
            enddo
            stmt:WhereClause := sb:ToString()
        ELSEIF SELF:Eos() .or. SELF:Eoi()
            return true
        ELSE
            SELF:SetError("Expected Where clause or EOS")
            return false
        ENDIF
        RETURN true


    METHOD ParseTableName() AS STRING
        local sTable as STRING
        sTable := SELF:ConsumeAndGetAny(XTokenType.ID):Text
        IF SELF:La1 == XTokenType.EXCLAMATIONMARK .and. SELF:La2 == XTokenType.ID
            SELF:Consume()
            sTable += "!"+self:ConsumeAndGetText()
        ENDIF
        RETURN sTable

    METHOD ParseUpdateStatement(stmt out FoxUpdateContext) AS LOGIC
        /*
            UPDATE Target
               SET Column_Name1 = eExpression1 [, Column_Name2 = eExpression2 ...]
               [FROM [FORCE] Table_List_Item [[, ...] | [JOIN [ Table_List_Item]]]
                WHERE FilterCondition1 [AND | OR FilterCondition2 ...]

            Table_List_Item can have the following syntaxes:
            [DatabaseName!]Table [[AS] Local_Alias]
            DatabaseName! specifies the name of a database containing the table if the table
            is in a noncurrent database. If the table is in a noncurrent database, you must include
            the name of database. Use an exclamation point (!) as a delimiter immediately following
            the database name and preceding the table name.
            Table specifies the name of the table or cursor you want to update data from.
            If no table is open, Visual FoxPro displays the Open dialog box so you can specify the file location.
            After the table opens, it remains open when the query is complete.
            Local_Alias specifies a temporary name for the table specified in Table.
            If you specify a local alias, you must use the local alias instead of the table name in the UPDATE statement. The alias can represent a table or a cursor.

            JOIN provides the capability for specifying one or more secondary tables.
            There is no hard coded limit on the number of tables, aliases, or JOIN clauses per UPDATE statement.
            (Subquery) AS Subquery_Alias

            A subquery specifies a SELECT statement within another SELECT statement. For more information about subqueries in SELECT statements, see the FROM clause in SELECT - SQL Command.



        */
            stmt := FoxUpdateContext{}
            IF ! SELF:Expect(XTokenType.UPDATE)
                SetError("Expected UPDATE")
                return FALSE
            ENDIF
            IF SELF:Matches(XTokenType.ID)
                stmt:TableName := SELF:ParseTableName()
            ENDIF
            IF ! SELF:Expect(XTokenType.SET)
                SetError("Expected SET")
                return FALSE
            ENDIF
            IF ! SELF:Matches(XTokenType.ID)
                SetError("Expected ID")
                RETURN FALSE
            ENDIF
            do while SELF:Matches(XTokenType.ID)
                var column := SELF:ConsumeAndGetText()
                IF ! SELF:Expect(XTokenType.EQ)
                    SetError("Expected '='")
                    RETURN FALSE
                ENDIF
                var expr := SELF:ParseExpression(XTokenType.FROM, XTokenType.JOIN, XTokenType.WHERE)
                stmt:ColumnList:Add(column)
                stmt:ValueList:Add(expr)
                IF ! SELF:Expect(XTokenType.COMMA)
                    // End of list
                    EXIT
                ENDIF
            enddo
            IF SELF:Expect(XTokenType.FROM)
                IF SELF:Expect("FORCE")
                    stmt:IsForce := TRUE
                ENDIF
                DO WHILE SELF:Matches(XTokenType.ID)
                    var tableName := SELF:ParseTableName()
                    stmt:TableList:Add(tableName)
                    IF ! SELF:Expect(XTokenType.COMMA)
                        EXIT
                    ENDIF
                ENDDO
            ELSEIF SELF:Expect(XTokenType.JOIN)
                IF SELF:Matches(XTokenType.ID)
                    DO WHILE SELF:Matches(XTokenType.ID)
                        var tableName := SELF:ParseTableName()
                        IF SELF:Expect(XTokenType.AS) .and. SELF:Matches(XTokenType.ID)
                            tableName += "->"+SELF:ConsumeAndGetText()
                        ENDIF
                        stmt:JoinList:Add(tableName)
                        IF ! SELF:Expect(XTokenType.COMMA)
                            EXIT
                        ENDIF
                    ENDDO
                ELSEIF SELF:Matches(XTokenType.SELECT)
                    SetError("SUBSELECT NOT supported yet")
                    RETURN FALSE
                ENDIF
            ENDIF
            IF !SELF:Expect(XTokenType.WHERE) .and. ! SELF:Eoi() .and. ! Self:Eos()
                SetError("Expected WHERE")
                RETURN FALSE
            ENDIF
            stmt:WhereClause := SELF:ParseExpression()
            return true


#endregion
END CLASS
END NAMESPACE
