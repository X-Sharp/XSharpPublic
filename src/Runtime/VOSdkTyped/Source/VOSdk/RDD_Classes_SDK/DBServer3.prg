//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#pragma options ("enforceself", on)
#pragma warnings(165, off)

partial class DbServer


    /// <include file="Rdd.xml" path="doc/DbServer.GetArray/*" />
    method GetArray( nMaxRows:= 100 as long, uField1 := 1 as usual, uSearchValue := nil as usual)  as array
        local uValue as usual
        local cbKey as usual
        local aResult := { } as array
        local wRows := 32767 as long
        local oError as usual
        local dwCurrentWorkArea := 0 as dword
        local oHLTemp as HyperLabel
        local wPos as dword




        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if ! self:Notify( NOTIFYINTENTTOMOVE )
                break DbError{ self, #GetArray, 999, VO_Sprintf( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) }
            endif


            if nMaxRows < wRows
                wRows := nMaxRows
            endif


            if IsNil( uField1 )
                wPos := 1
            elseif IsSymbol( uField1 )
                wPos := FieldPosSym( uField1 )
            elseif IsString( uField1 )
                wPos := FieldPos( uField1 )
            else
                wPos := uField1
            endif


            if lSelectionActive
                uValue := uSelectionValue
                cbKey := cbSelectionIndexingExpression
                if ! VoDbSeek( uValue, false )
                    self:__SetStatusHL( #GetArray, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOSEEK ) )
                    oHLTemp := oHLStatus
                    aResult := null_array
                else
                    self:__DbServerEval( { | | AAdd( aResult, __DBSFieldGet( wPos ) ) },  ;
                        nil,  ;
                        { || Eval( cbKey ) = uValue },  ;
                        wRows,  ;
                        nil,  ;
                        true,  ;
                        DBCCON,  ;
                        DBCCREADONLY )
                    siSelectionStatus := DBSELECTIONEOF
                    if ! VoDbGoBottom( )
                        break ErrorBuild( _VoDbErrInfoPtr( ) )
                    endif
                    if ! VoDbSkip( 1 )
                        break ErrorBuild( _VoDbErrInfoPtr( ) )
                    endif
                endif


            elseif ! IsNil( uSearchValue )
                if ! VoDbSeek( uSearchValue, false )
                    self:__SetStatusHL( #GetArray, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOSEEK ) )
                    oHLTemp := oHLStatus
                    aResult := null_array
                else
                    cbKey := &( "{ ||"+__DBSDbOrderInfo( DBOI_EXPRESSION, "", 0 ) + " }" )
                    self:__DbServerEval( { || AAdd( aResult, __DBSFieldGet( wPos ) ) },  ;
                        nil,  ;
                        nil,  ;
                        wRows,  ;
                        nil,  ;
                        true,  ;
                        DBCCON,  ;
                        DBCCREADONLY )
                endif


            else
                self:__DbServerEval( { || AAdd( aResult, __DBSFieldGet( wPos ) ) },  ;
                    nil,  ;
                    nil,  ;
                    wRows,  ;
                    nil,  ;
                    true,  ;
                    DBCCON,  ;
                    DBCCREADONLY )
            endif


            self:__ProcessConcurrency( true )


            __DBSSetSelect( dwCurrentWorkArea )


        recover using oError
            self:__ProcessConcurrency(false)
            __DBSSetSelect( dwCurrentWorkArea )
            self:Error( oError, #GetArray )
            oErrorInfo := oError
            oHLTemp := oHLStatus
            aResult := null_array
        end sequence




        self:__Notify( NOTIFYRECORDCHANGE )


        if ! IsNil( oHLTemp )
            lErrorFlag := true
            oHLStatus := oHLTemp
            if ! IsNil( oError )
                oErrorInfo := oError
            else
                oErrorInfo := null_object
            endif
        endif


        return aResult


    /// <include file="Rdd.xml" path="doc/DbServer.GetLocate/*" />
    method GetLocate ( ) as usual


        local dwCurrentWorkArea := 0 as dword
        local oError as usual
        local uInfo as usual


        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if ! VoDbInfo( DBI_GETSCOPE, ref uInfo )
                break ErrorBuild( _VoDbErrInfoPtr( ) )
            endif
            __DBSSetSelect( dwCurrentWorkArea )
        recover using oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            self:Error( oErrorInfo, #Info )
        end sequence


        return uInfo


    /// <include file="Rdd.xml" path="doc/DbServer.GetLookupTable/*" />
    method GetLookupTable( nMaxRows , uField1 , uField2 , uSearchValue )  as array clipper
        local uValue as usual
        local cbKey as usual
        local aResult := { } as array
        local wRows := 32767 as long
        local oError as usual
        local dwCurrentWorkArea := 0 as dword
        local oHLTemp as HyperLabel

        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if ! self:Notify( NOTIFYINTENTTOMOVE )
                break DbError{ self, #GetLookupTable, 999, VO_Sprintf( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) }
            endif

            if IsNil( nMaxRows )
                wRows := 100
            elseif nMaxRows < wRows
                wRows := nMaxRows
            endif

            if IsNil( uField1 )
                uField1 := 1
            elseif IsSymbol( uField1 )
                uField1 := FieldPosSym( uField1 )
            elseif IsString( uField1 )
                uField1 := FieldPos( uField1 )
            endif


            if IsNil( uField2 )
                uField2 := 2
            elseif IsSymbol( uField2 )
                uField2 := FieldPosSym( uField2 )
            elseif IsString( uField2 )
                uField2 := FieldPos( uField2 )
            endif


            if lSelectionActive
                uValue := uSelectionValue
                cbKey := cbSelectionIndexingExpression
                if ! VoDbSeek( uValue, false )
                    self:__SetStatusHL( #GetLookupTable, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOSEEK ) )
                    oHLTemp := oHLStatus
                    aResult := null_array
                else
                    self:__DbServerEval( { || AAdd( aResult, { __DBSFieldGet( uField1 ), __DBSFieldGet( uField2 ) } ) },  ;
                        nil,  ;
                        { || Eval( cbKey ) == uValue },  ;
                        wRows,  ;
                        nil,  ;
                        true,  ;
                        DBCCON,  ;
                        DBCCREADONLY )
                    siSelectionStatus := DBSELECTIONEOF
                    if ! VoDbGoBottom( )
                        break ErrorBuild( _VoDbErrInfoPtr( ) )
                    endif
                    if ! VoDbSkip( 1 )
                        break ErrorBuild( _VoDbErrInfoPtr( ) )
                    endif
                endif


            elseif ! IsNil( uSearchValue )
                if ! VoDbSeek( uSearchValue, false )
                    self:__SetStatusHL ( #GetLookupTable, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOSEEK ) )
                    oHLTemp := oHLStatus
                    aResult := null_array
                else
                    self:__DbServerEval( { || AAdd( aResult, { __DBSFieldGet( uField1 ), __DBSFieldGet( uField2 ) } ) },  ;
                        nil,  ;
                        nil,  ;
                        wRows,  ;
                        nil,  ;
                        true,  ;
                        DBCCON,  ;
                        DBCCREADONLY )
                endif


            else
                self:__DbServerEval( { || AAdd( aResult, { __DBSFieldGet( uField1 ), __DBSFieldGet( uField2 ) } ) },  ;
                    nil,  ;
                    nil,  ;
                    wRows,  ;
                    nil,  ;
                    true,  ;
                    DBCCON,  ;
                    DBCCREADONLY )
            endif


            self:__ProcessConcurrency( true )
            __DBSSetSelect( dwCurrentWorkArea )


        recover using oError
            oErrorInfo := oError
            self:__ProcessConcurrency( false )
            __DBSSetSelect( dwCurrentWorkArea )
            self:Error( oError, #GetLookupTable )
            oErrorInfo := oError
            oHLTemp := oHLStatus
            aResult := null_array
        end sequence




        self:__Notify( NOTIFYRECORDCHANGE )


        if ! IsNil( oHLTemp )
            lErrorFlag := true
            oHLStatus := oHLTemp
            if ! IsNil( oError )
                oErrorInfo := oError
            else
                oErrorInfo := null_object
            endif
        endif




        return aResult


    /// <include file="Rdd.xml" path="doc/DbServer.GoBottom/*" />
    method GoBottom( )   as logic
        local dwCurrentWorkArea := 0 as dword
        local uValue as usual
        local cbKey as usual
        local lRetCode := false as logic
        local oError as usual
        local oHLTemp as HyperLabel
        local nTries as dword


        lErrorFlag := false
        nTries := self:nRetries


        begin sequence
            VoDbSelect( self:wWorkArea, out dwCurrentWorkArea )
            if self:Notify( NOTIFYINTENTTOMOVE )
                if lSelectionActive
                    if siSelectionStatus == DBSELECTIONEMPTY
                        lRetCode := true
                    elseif siSelectionStatus == DBSELECTIONEOF
                        lRetCode := self:Skip( -1 )
                        siSelectionStatus := DBSELECTIONNULL
                    else
                        uValue := uSelectionValue
                        cbKey := cbSelectionIndexingExpression
                        __DBSSeek( uSelectionValue, false, false , nTries )
                        if Eval( cbKey ) = uValue .or. VoDbFound( )
                            lRetCode := self:__DbServerEval( { || },  ;
                                nil,  ;
                                { || Eval( cbKey ) = uValue },  ;
                                nil,  nil,  true , false, false)
                            lRetCode := __DBSSkip( -1, nTries )
                            siSelectionStatus := DBSELECTIONNULL
                        else
                            siSelectionStatus := DBSELECTIONEMPTY
                            self:__SetStatusHL( #GoBottom, EG_BOUND, __CavoStr( __CAVOSTR_DBFCLASS_SELECTIVEVALUE ) )
                            oHLTemp := oHLStatus
                            lRetCode := false
                        endif
                    endif
                else
                    lRetCode := __DBSGoBottom( nTries )
                endif


                self:Notify( NOTIFYGOBOTTOM )
                if ! lRetCode .and. ! IsNil( oHLTemp )
                    lErrorFlag := true
                    oHLStatus := oHLTemp
                endif
                if lRetCode
                    lRetCode := self:__ProcessConcurrency( true )
                endif
            else
                lRetCode := false
                self:__SetStatusHL( #GoBottom, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
                    __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
            endif


            __DBSSetSelect( dwCurrentWorkArea )


        recover using oError
            oHLStatus := self:__GenerateStatusHL( oError )
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            lRetCode := false
        end sequence




        return lRetCode


    /// <include file="Rdd.xml" path="doc/DbServer.GoTo/*" />
    method GoTo( nRecordNumber as long) as logic
        local nCurrentRecord as dword
        local lRetCode := false as logic
        local dwCurrentWorkArea := 0 as dword
        local oError as usual
        local nTries as dword




        lErrorFlag := false


        nTries := self:nRetries


        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if self:Notify( NOTIFYINTENTTOMOVE )
                if lSelectionActive


                    if siSelectionStatus == DBSELECTIONEMPTY
                        lRetCode := true
                    else
                        nCurrentRecord := VoDbRecno( )
                        lRetCode := __DBSGoTo( nRecordNumber, nTries )




                        if Eval( cbSelectionIndexingExpression ) = uSelectionValue
                            siSelectionStatus := DBSELECTIONNULL


                        else


                            if ! siSelectionStatus == DBSELECTIONEOF
                                __DBSGoBottom( nTries )
                                __DBSSkip( 1, nTries )
                            else
                                __DBSGoTo( (int) nCurrentRecord, nTries )
                            endif
                        endif
                    endif
                else
                    lRetCode := __DBSGoTo( nRecordNumber, nTries )
                endif
                if lRetCode
                    lRetCode := self:__ProcessConcurrency( true )
                endif
                self:Notify( NOTIFYRECORDCHANGE )


            else
                lRetCode := false
                self:__SetStatusHL( #GoTo, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
                    __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
            endif


            __DBSSetSelect( dwCurrentWorkArea )


        recover using oError
            oHLStatus := self:__GenerateStatusHL( oError )
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            lRetCode := false
        end sequence




        return lRetCode


    /// <include file="Rdd.xml" path="doc/DbServer.GoTop/*" />
    method GoTop( ) as logic
        local lRetCode := false as logic
        local oError as usual
        local dwCurrentWorkArea := 0 as dword
        local nTries as dword


        lErrorFlag := false
        nTries     := self:nRetries


        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if self:Notify( NOTIFYINTENTTOMOVE )
                if lSelectionActive
                    if siSelectionStatus == DBSELECTIONEMPTY
                        lRetCode := true
                    else
                        lRetCode := __DBSSeek( uSelectionValue, false, false, nTries )
                        if lRetCode
                            siSelectionStatus := DBSELECTIONNULL
                        else
                            siSelectionStatus := DBSELECTIONEMPTY
                        endif
                    endif
                else
                    lRetCode := __DBSGoTop( nTries )
                endif


                if lRetCode
                    lRetCode := self:__ProcessConcurrency( true )
                endif
                self:Notify( NOTIFYGOTOP )
            else
                lRetCode := false
                self:__SetStatusHL( #GoTop, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
                    __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
            endif
            __DBSSetSelect( dwCurrentWorkArea )


        recover using oError
            oHLStatus := self:__GenerateStatusHL( oError )
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            lRetCode := false
        end sequence




        return lRetCode


    /// <include file="Rdd.xml" path="doc/DbServer.IndexKey/*" />
    method IndexKey( uOrder as usual) as usual


        local dwCurrentWorkArea := 0 as dword
        local oError as usual
        local uOrdVal as usual




        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if ! VoDbOrderInfo( DBOI_EXPRESSION, "", uOrder, ref uOrdVal )
                break ErrorBuild( _VoDbErrInfoPtr( ) )
            endif
            __DBSSetSelect( dwCurrentWorkArea )
        recover using oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            self:Error( oErrorInfo, #OrderInfo )
        end sequence




        return uOrdVal


    /// <include file="Rdd.xml" path="doc/DbServer.IndexOrd/*" />
    method IndexOrd( ) as usual


        local dwCurrentWorkArea := 0 as dword
        local oError as usual
        local uOrdVal as usual


        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if ! VoDbOrderInfo( DBOI_NUMBER, "", nil, ref uOrdVal )
                break ErrorBuild( _VoDbErrInfoPtr( ) )
            endif
            __DBSSetSelect( dwCurrentWorkArea )
        recover using oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            self:Error( oErrorInfo, #OrderInfo )
        end sequence




        return uOrdVal


    /// <include file="Rdd.xml" path="doc/DbServer.Info/*" />
    method Info( kInfoType as long, uInfo := nil as usual) as usual


        local dwCurrentWorkArea := 0 as dword
        local oError as usual




        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if ! VoDbInfo( (dword) kInfoType, ref uInfo)
                break ErrorBuild( _VoDbErrInfoPtr( ) )
            endif
            __DBSSetSelect( dwCurrentWorkArea )
        recover using oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            self:Error( oErrorInfo, #Info )
        end sequence


        return uInfo


    /// <include file="Rdd.xml" path="doc/DbServer.Join/*" />
    method Join( oDBSource, oFSTarget, aFieldList, cbForBlock ) as logic clipper
        local cSource as string
        local cTarget as string
        local aFieldNames as array
        local w as dword
        local lRetCode := false as logic
        local oError as usual
        local dwCurrentWorkArea := 0 as dword
        local wLen as dword
        local lRestore	as logic


        lRestore	:= DbSetRestoreWorkarea(true)


        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if self:Notify( NOTIFYINTENTTOMOVE )
                if IsObject(oDBSource) .and. __Usual.ToObject(oDBSource) is DbServer var oDb
                    cSource := oDb:Alias
                else
                    cSource := AsString( oDBSource )
                endif


                if IsObject(oFSTarget) .and. __Usual.ToObject(oFSTarget) is FileSpec var oFs
                    cTarget := oFs:FullPath
                else
                    cTarget := AsString( oFSTarget )
                endif


                aFieldNames := ArrayNew( ALen( aFieldList ) )
                wLen := ALen( aFieldList )
                for w := 1 upto wLen
                    aFieldNames[w] := AsString( aFieldList[w] )
                next
                if IsNil( cbForBlock )
                    cbForBlock := cbStoredForBlock
                    if IsNil( cbStoredForBlock )
                        cbStoredForBlock := { || true }
                    endif
                endif
                lRetCode := __DBSDBJOIN( cSource, cTarget, aFieldNames, cbForBlock, self:cRDDName )
                lRetCode := self:__ProcessConcurrency( true )
                siSelectionStatus := DBSELECTIONNULL
                self:Notify( NOTIFYRECORDCHANGE )


            else
                lRetCode := false
                self:__SetStatusHL ( #Join, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
                    __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
            endif
            __DBSSetSelect( dwCurrentWorkArea )


        recover using oError
            oHLStatus := self:__GenerateStatusHL( oError )
            oErrorInfo := oError
            self:__ProcessConcurrency( false )
            __DBSSetSelect( dwCurrentWorkArea )
            lRetCode := false
        end sequence


        DbSetRestoreWorkarea(lRestore)
        return lRetCode


    /// <include file="Rdd.xml" path="doc/DbServer.Locate/*" />
    method Locate( cbForBlock, cbWhileBlock, uScope )  as logic clipper
        local uValue as usual
        local cbKey as usual
        local nNextCount as longint
        local lRestOfFile as logic
        local lRetCode := false as logic
        local oError as usual
        local dwCurrentWorkArea := 0 as dword
        local oHLTemp as HyperLabel
        local lRestore	as logic


        lRestore	:= DbSetRestoreWorkarea(true)


        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if self:Notify( NOTIFYINTENTTOMOVE )
                if ! IsNil( cbForBlock ) .or. ! IsNil( cbWhileBlock ) .or. ! IsNil( uScope )
                    if Empty( cbForBlock )
                        cbForBlock := { || true }
                    elseif IsString( cbForBlock )
                        cbForBlock := &( "{ || " + cbForBlock + " } " )
                    endif


                    if Empty( cbWhileBlock )
                        cbWhileBlock := { || true }
                    else
                        lRestOfFile := true
                        if IsString( cbWhileBlock )
                            cbWhileBlock := &( "{ || " + cbWhileBlock + " }" )
                        endif
                    endif


                    if ! IsNil( uScope )
                        if IsNumeric( uScope )
                            nNextCount := uScope
                        else
                            lRestOfFile := uScope
                        endif
                    endif
                    if ! VoDbLocate( cbForBlock,  ;
                            cbWhileBlock,  ;
                            nNextCount,  ;
                            nil,  ;
                            lRestOfFile )
                        break ErrorBuild( _VoDbErrInfoPtr( ) )
                    endif
                    lRetCode := VoDbFound( )


                elseif lActiveScope
                    lRestOfFile := lStoredRestOfFile
                    if IsNil( cbStoredWhileBlock )
                        cbWhileBlock := { || true }
                    else
                        cbWhileBlock := cbStoredWhileBlock
                        lRestOfFile := true
                    endif


                    if ! VoDbLocate( cbStoredForBlock,  ;
                            cbWhileBlock,  ;
                            nStoredNextCount,  ;
                            nil,  ;
                            lRestOfFile  )
                        break ErrorBuild( _VoDbErrInfoPtr( ) )
                    endif
                    lRetCode := VoDbFound( )


                elseif lSelectionActive
                    uValue := uSelectionValue
                    cbKey := cbSelectionIndexingExpression
                    if ! VoDbLocate( { || Eval( cbKey ) = uValue },  ;
                            { || true },  ;
                            0,  ;
                            nil,  ;
                            true  )
                        break ErrorBuild( _VoDbErrInfoPtr( ) )
                    endif
                    lRetCode := VoDbFound( )
                    if lRetCode
                        siSelectionStatus := DBSELECTIONFOUND
                    else
                        siSelectionStatus := DBSELECTIONEOF
                        if ! VoDbGoBottom( )
                            break ErrorBuild( _VoDbErrInfoPtr( ) )
                        endif
                        if ! VoDbSkip( 1 )
                            break ErrorBuild( _VoDbErrInfoPtr( ) )
                        endif
                    endif


                else
                    if ! VoDbLocate( { || true },  ;
                            { || true },  ;
                            0,  ;
                            nil,  ;
                            false  )
                        break ErrorBuild( _VoDbErrInfoPtr( ) )
                    endif
                    lRetCode := VoDbFound( )
                endif
                self:__ProcessConcurrency( true )


            else
                lRetCode := false
                self:__SetStatusHL( #Locate, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
                    __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
                oHLTemp := oHLStatus
            endif
            __DBSSetSelect( dwCurrentWorkArea )


        recover using oError
            oHLStatus := self:__GenerateStatusHL( oError )
            oHLTemp := oHLStatus
            oErrorInfo := oError
            self:__ProcessConcurrency( false )
            __DBSSetSelect( dwCurrentWorkArea )
            lRetCode := false
        end sequence




        self:__Notify( NOTIFYRECORDCHANGE )


        if ! lRetCode .and. ! IsNil( oHLTemp )
            lErrorFlag := true
            oHLStatus := oHLTemp
            if ! IsNil( oError )
                oErrorInfo := oError
            else
                oErrorInfo := null_object
            endif
        endif


        DbSetRestoreWorkarea(lRestore)
        return lRetCode


    /// <include file="Rdd.xml" path="doc/DbServer.LockCurrentRecord/*" />
    method LockCurrentRecord( ) as logic strict
        return self:RLock( -1 )


    /// <include file="Rdd.xml" path="doc/DbServer.LockSelection/*" />
    method LockSelection( )  as logic
        local uCurrentRecord as usual
        local uValue as usual
        local cbKey as usual
        local lRetCode := false as logic
        local dwCurrentWorkArea := 0 as dword
        local oError as usual


        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if lSelectionActive
                if self:Notify( NOTIFYINTENTTOMOVE )
                    uCurrentRecord := VoDbRecno( )
                    uValue := uSelectionValue
                    cbKey := cbSelectionIndexingExpression
                    if VoDbSeek( uSelectionValue, false )
                        lRetCode := self:__DbServerEval( { || VoDbRlock( VoDbRecno( ) ) },  ;
                            nil,  ;
                            { || Eval( cbKey ) = uValue },  ;
                            nil,  nil,  true , false, false)
                        if ! lRetCode .or. ! VoDbGoto( uCurrentRecord )
                            break ErrorBuild( _VoDbErrInfoPtr( ) )
                        endif
                    else
                        self:__SetStatusHL( #LockSelection, EG_BOUND,  ;
                            __CavoStr( __CAVOSTR_DBFCLASS_SELECTIVENOTFOUND ) )
                        lRetCode := false
                    endif
                else
                    lRetCode := false
                    self:__SetStatusHL( #LockSelection,  ;
                        __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
                        __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
                endif
            else
                lRetCode := VoDbFlock( )
                if ! lRetCode
                    break ErrorBuild( _VoDbErrInfoPtr( ) )
                endif
            endif
            self:__OptimisticFlushNoLock( )
            __DBSSetSelect( dwCurrentWorkArea )


        recover using oError
            oHLStatus := self:__GenerateStatusHL( oError )
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            lRetCode := false
        end sequence




        return lRetCode


    /// <include file="Rdd.xml" path="doc/DbServer.NoIVarGet/*" />
    method NoIVarGet( symFieldName as usual) as usual


        local dwCurrentWorkArea := 0 as dword
        local uRetVal := nil as usual
        local oError as usual
        local wPos as dword




        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            wPos:= FieldPosSym( symFieldName )
            if wPos > 0  .and. nEffectiveCCMode == ccOptimistic .and. lCCOptimisticRecChg .and. ;
                    aCurrentBuffer[BUFFER_IS_CHANGED, wPos] .and. ! aOriginalBuffer[BUFFER_IS_BLOB, wPos]


                uRetVal := aCurrentBuffer[BUFFER_VALUE, wPos]
            else
                if ! VoDbFieldGet( wPos, ref uRetVal )
                    break ErrorBuild( _VoDbErrInfoPtr( ) )
                endif
            endif
            __DBSSetSelect( dwCurrentWorkArea )
        recover using oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            self:Error( oErrorInfo, #symFieldName )
        end sequence




        return uRetVal


    /// <include file="Rdd.xml" path="doc/DbServer.NoIVarPut/*" />
    method NoIVarPut( symFieldName as usual, uValue as usual ) as usual
        local uRetVal := nil as usual
        local uError as usual
        local dwCurrentWorkArea := 0 as dword
        local wPos as dword
        local uIsRlock as usual


        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if nEffectiveCCMode == ccOptimistic .and. VoDbRecno() <= VoDbLastRec()
                if ! VoDbRecordInfo( DBRI_LOCKED, 0, ref uIsRlock )
                    break ErrorBuild( _VoDbErrInfoPtr( ) )
                endif


                if uIsRlock
                    uRetVal := FieldPutSym( symFieldName, uValue )
                else
                    wPos := FieldPosSym( symFieldName )
                    //type checking for optimistic locking
                    if ! __CheckFieldType(ref uValue, aStruct[wPos], ref uError)
                        ASize(uError, 3)
                        break DbError{self, #NoIVarPut, uError[1], VO_Sprintf(uError[2], "Field " + aStruct[wPos,DBS_NAME], uError[3]), uValue, "uValue"}
                    endif
                    aCurrentBuffer[BUFFER_VALUE, wPos]   := uRetVal := uValue
                    aCurrentBuffer[BUFFER_IS_CHANGED, wPos] := true
                    lCCOptimisticRecChg := true
                endif
            else
                uRetVal := FieldPutSym( symFieldName, uValue )
            endif
            self:Notify( Notify.FieldChange, symFieldName )
            __DBSSetSelect( dwCurrentWorkArea )


        recover using uError
            oErrorInfo := uError
            __DBSSetSelect( dwCurrentWorkArea )
            self:Error( oErrorInfo, #symFieldName )
        end sequence




        return uRetVal


    /// <include file="Rdd.xml" path="doc/DbServer.Notify/*" />
    method Notify( kNotification as long, uDescription := nil as usual) as usual


        local dwCurrentWorkArea := 0 as dword
        local uVOVal, uVoVal2 as usual
        local uRetValue as usual
        //	STATIC lNITMStart := FALSE AS LOGIC




        uRetValue := true
        do case
        case kNotification <= NOTIFYCOMPLETION
            if siSuspendNotification == 0 .and. nClients > 0
                VoDbSelect( wWorkArea, out dwCurrentWorkArea )
                foreach oClient as usual in aClients
                    Send(oClient,#Notify, kNotification, uDescription )
                next
                VoDbSetSelect( longint(dwCurrentWorkArea ) )
            endif


        case kNotification == NOTIFYINTENTTOMOVE
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            uRetValue := true
            if siSuspendNotification == 0
                if nClients > 0
                    foreach oClient as usual in aClients
                        uRetValue := Send(oClient,#Notify, kNotification, uDescription )
                        if IsLogic(uRetValue) .and. ! uRetValue
                            exit
                        endif
                    next
                endif


                if uRetValue .and. lRelationsActive
                    foreach oChild as usual in aRelationChildren
                        uRetValue := Send(oChild, #Notify, kNotification , uDescription)
                        if IsLogic(uRetValue) .and. ! uRetValue
                            exit
                        endif
                    next  // nChild
                endif
            else
                foreach oChild as usual in aRelationChildren
                    Send(oChild, #__NotifyBufferFlush)
                next  // nChild
            endif


            if uRetValue
                VoDbSetSelect( longint( wWorkArea ) )
                self:__OptimisticFlush()
            endif
            VoDbSetSelect( longint(dwCurrentWorkArea ) )


        case kNotification <= NOTIFYFILECHANGE
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )


            self:__InitRecordBuf()


            if siSuspendNotification == 0
                if lRelationsActive
                    foreach oChild as usual in aRelationChildren
                        Send(oChild, #Notify, NOTIFYRELATIONCHANGE)
                    next
                endif
                if nClients > 0
                    foreach oClient as usual in aClients
                        Send(oClient, #Notify, kNotification, uDescription )
                    next
                endif
            endif
            VoDbSetSelect( longint(dwCurrentWorkArea ))


        case kNotification == NOTIFYRELATIONCHANGE
            if siSuspendNotification == 0
                if lSelectionActive
                    if uDescription == nil .or. uDescription == DBSELECTIONNULL
                        VoDbSelect( wSelectionWorkArea, out dwCurrentWorkArea )
                        uSelectionValue := Eval( cbSelectionParentExpression )
                        VoDbSetSelect( longint( wWorkArea ) )
                        if VoDbEof( ) .or. ! ( Eval( cbSelectionIndexingExpression ) = uSelectionValue )
                            siSelectionStatus := DBSELECTIONEMPTY
                        else
                            siSelectionStatus := DBSELECTIONNULL
                        endif
                    else
                        VoDbSelect( wWorkArea, out dwCurrentWorkArea )
                        siSelectionStatus := DBSELECTIONEMPTY
                    endif


                elseif lCDXSelectionActive
                    VoDbSelect( wSelectionWorkArea, out dwCurrentWorkArea )
                    uVOVal := uVoVal2 := Eval( cbSelectionParentExpression )
                    VoDbSetSelect( longint(wWorkArea ) )
                    VoDbOrderInfo( DBOI_SCOPETOP	 , "", nil, ref uVOVal )
                    VoDbOrderInfo( DBOI_SCOPEBOTTOM, "", nil, ref uVoVal2 )
                    if ! VoDbGoTop()
                        VoDbSetSelect( longint(dwCurrentWorkArea ))
                        break ErrorBuild(_VoDbErrInfoPtr())
                    endif
                else
                    VoDbSelect( wWorkArea, out dwCurrentWorkArea )
                endif


                self:__InitRecordBuf()


                if nClients > 0
                    foreach oClient as usual in aClients
                        Send(oClient, #Notify, NOTIFYFILECHANGE)
                    next
                endif
                if lRelationsActive
                    foreach oChild as usual in aRelationChildren
                        Send(oChild, #Notify, NOTIFYRELATIONCHANGE, siSelectionStatus)
                    next
                endif


                VoDbSetSelect( longint(dwCurrentWorkArea ) )
            endif


        case kNotification == NOTIFYCLEARRELATION
            lSelectionActive 	:= false
            oDBSelectionParent := null_object
            wSelectionWorkArea := 0
            cbSelectionParentExpression := nil
            cbSelectionIndexingExpression := nil
            if lCDXSelectionActive
                lCDXSelectionActive := false
                VoDbSelect( wWorkArea, out dwCurrentWorkArea )
                uVOVal := nil
                VoDbOrderInfo( DBOI_SCOPETOPCLEAR, "", nil, ref uVOVal )
                uVOVal := nil
                VoDbOrderInfo( DBOI_SCOPEBOTTOMCLEAR, "", nil, ref uVOVal )
                VoDbSetSelect( longint(dwCurrentWorkArea ) )
            endif


        otherwise
            self:__InitRecordBuf()
            if siSuspendNotification == 0
                VoDbSelect( wWorkArea, out dwCurrentWorkArea )
                if nClients > 0
                    foreach oClient as usual in aClients
                        Send(oClient, #Notify, kNotification )
                    next
                endif
                if lRelationsActive
                    foreach oChild as usual in aRelationChildren
                        Send(oChild, #Notify, kNotification )
                    next
                endif
                VoDbSetSelect( longint(dwCurrentWorkArea ))
            endif
        endcase


        return uRetValue


    /// <include file="Rdd.xml" path="doc/DbServer.OrderDescend/*" />
    method OrderDescend( uOrder as usual, oFSIndex as FileSpec, lNew := nil as usual) as long
        return self:OrderDescend(uOrder, oFSIndex:FullPath, lNew)


    /// <include file="Rdd.xml" path="doc/DbServer.OrderDescend/*" />
    method OrderDescend( uOrder as usual, cIndex := "" as string, lNew := nil as usual) as long
        local dwCurrentWorkArea := 0 as dword
        local oError as usual


        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            VoDbOrderInfo( DBOI_ISDESC, cIndex, uOrder, ref lNew )
            __DBSSetSelect( dwCurrentWorkArea )
        recover using oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            self:Error( oErrorInfo, #OrderDescend )
            lNew := nil
        end sequence




        return lNew


    /// <include file="Rdd.xml" path="doc/DbServer.OrderInfo/*" />
    method OrderInfo( kOrderInfoType, oFSIndex, uOrder, uOrdVal ) as usual clipper


        local dwCurrentWorkArea := 0 as dword
        local oError as usual
        local cTarget as string
        local lKeyVal as logic


        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if IsObject(oFSIndex) .and. __Usual.ToObject(oFSIndex) is FileSpec var oFS
                cTarget := oFS:FullPath
            else
                if IsString( oFSIndex )
                    cTarget := oFSIndex
                endif
            endif


            if IsString(uOrder)
                if Len(uOrder) == 0
                    uOrder := nil
                endif
            endif


            if kOrderInfoType == DBOI_KEYVAL
                lKeyVal := .t.
                kOrderInfoType := DBOI_EXPRESSION
            endif


            if ! VoDbOrderInfo(kOrderInfoType, cTarget, uOrder, ref uOrdVal)
                break ErrorBuild(_VoDbErrInfoPtr())
            endif


            if lKeyVal
                if IsString(uOrdVal)
                    if Len(uOrdVal) == 0
                        uOrdVal := nil
                    else
                        uOrdVal := &(uOrdVal)
                    endif
                endif
            endif


            __DBSSetSelect( dwCurrentWorkArea )


        recover using oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            self:Error( oErrorInfo, #OrderInfo )
        end sequence




        return uOrdVal




    /// <include file="Rdd.xml" path="doc/DbServer.OrderIsUnique/*" />
    method OrderIsUnique( uOrder as usual, oFSIndex as FileSpec) as long
        return self:OrderIsUnique(uOrder, oFSIndex:FullPath)


    /// <include file="Rdd.xml" path="doc/DbServer.OrderIsUnique/*" />
    method OrderIsUnique( uOrder as usual, cTarget := "" as string) as long
        local dwCurrentWorkArea := 0 as dword
        local lRetVal as usual
        local oError as usual


        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if ! VoDbOrderInfo( DBOI_UNIQUE, cTarget, uOrder, ref lRetVal )
                break ErrorBuild(_VoDbErrInfoPtr())
            endif
            __DBSSetSelect( dwCurrentWorkArea )


        recover using oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            self:Error( oErrorInfo, #OrderIsUnique )
        end sequence




        return lRetVal




    /// <include file="Rdd.xml" path="doc/DbServer.OrderKeyAdd/*" />
    method OrderKeyAdd( uOrder as usual, oFSIndex as FileSpec, uKeyValue  := nil as usual) as long
        return self:OrderKeyAdd(uOrder, oFSIndex:FullPath,uKeyValue)


    /// <include file="Rdd.xml" path="doc/DbServer.OrderKeyAdd/*" />
    method OrderKeyAdd( uOrder as usual, cIndex := "" as string, uKeyValue := nil  as usual) as long




        local dwCurrentWorkArea := 0 as dword
        local oError as usual


        VoDbSelect( wWorkArea, out dwCurrentWorkArea )


        lErrorFlag := false
        begin sequence
            if ! VoDbOrderInfo( DBOI_KEYADD, cIndex, uOrder, ref uKeyValue )
                break ErrorBuild(_VoDbErrInfoPtr())
            endif


        recover using oError
            oHLStatus := self:__GenerateStatusHL( oError )
            oErrorInfo := oError
            uKeyValue := false
        end sequence


        __DBSSetSelect( dwCurrentWorkArea )




        return uKeyValue




    /// <include file="Rdd.xml" path="doc/DbServer.OrderKeyCount/*" />
    method OrderKeyCount( uOrder as usual, oFSIndex as FileSpec) as long
        return self:OrderKeyCount(uOrder, oFSIndex:FullPath)


    /// <include file="Rdd.xml" path="doc/DbServer.OrderKeyCount/*" />
    method OrderKeyCount( uOrder as usual, cIndex := "" as string) as long
        local dwCurrentWorkArea := 0 as dword
        local uRetVal := nil as usual
        local oError as usual




        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if ! VoDbOrderInfo( DBOI_KEYCOUNT, cIndex, uOrder, ref uRetVal )
                break ErrorBuild(_VoDbErrInfoPtr())
            endif
            __DBSSetSelect( dwCurrentWorkArea )


        recover using oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            self:Error( oErrorInfo, #OrderKeyCount )
            uRetVal := nil
        end sequence




        return uRetVal


    /// <include file="Rdd.xml" path="doc/DbServer.OrderKeyDel/*" />
    method OrderKeyDel( uOrder as usual, oFSIndex as FileSpec) as long
        return self:OrderKeyDel(uOrder, oFSIndex:FullPath)


    /// <include file="Rdd.xml" path="doc/DbServer.OrderKeyDel/*" />
    method OrderKeyDel( uOrder as usual, cIndex := "" as string) as long
        local dwCurrentWorkArea := 0 as dword
        local lRetCode as usual
        local oError as usual


        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if ! VoDbOrderInfo( DBOI_KEYDELETE, cIndex, uOrder, ref lRetCode )
                break ErrorBuild(_VoDbErrInfoPtr())
            endif


        recover using oError
            oHLStatus := self:__GenerateStatusHL( oError )
            oErrorInfo := oError
            lRetCode := false
        end sequence


        __DBSSetSelect( dwCurrentWorkArea )




        return lRetCode


    /// <include file="Rdd.xml" path="doc/DbServer.OrderKeyGoTo/*" />
    method OrderKeyGoTo( nKeyNo as long) as logic
        local lRetCode := false as logic
        local oError as usual
        local dwCurrentWorkArea := 0 as dword




        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if self:Notify( NOTIFYINTENTTOMOVE )
                if IsNil( nKeyNo )
                    nKeyNo := 1
                endif
                if IsNumeric(nKeyNo)
                    if ! VoDbGoTop()
                        break ErrorBuild(_VoDbErrInfoPtr())
                    endif
                    if ! VoDbSkip(nKeyNo-1L)
                        break ErrorBuild(_VoDbErrInfoPtr())
                    endif
                    lRetCode := true
                endif
                if lRetCode
                    lRetCode := self:__ProcessConcurrency( true )
                endif
                self:Notify( NOTIFYRECORDCHANGE )
            else
                lRetCode := false
                self:__SetStatusHL( #OrderKeyGoTo,  ;
                    __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
                    __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
            endif
            __DBSSetSelect( dwCurrentWorkArea )


        recover using oError
            __DBSSetSelect( dwCurrentWorkArea )
            oHLStatus := self:__GenerateStatusHL( oError )
            oErrorInfo := oError
            lRetCode := false
        end sequence




        return lRetCode




    /// <include file="Rdd.xml" path="doc/DbServer.OrderKeyNo/*" />
    method OrderKeyNo( uOrder as usual, oFSIndex as FileSpec) as long
        return self:OrderKeyNo(uOrder, oFSIndex:FullPath)


    /// <include file="Rdd.xml" path="doc/DbServer.OrderKeyNo/*" />
    method OrderKeyNo( uOrder as usual, cIndex := "" as string) as long
        //
        local dwCurrentWorkArea := 0 as dword
        local uRetVal := nil as usual
        local oError as usual

        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if ! VoDbOrderInfo( DBOI_POSITION, cIndex, uOrder, ref uRetVal )
                break ErrorBuild(_VoDbErrInfoPtr())
            endif
            __DBSSetSelect( dwCurrentWorkArea )


        recover using oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            self:Error( oErrorInfo, #OrderKeyNo )
            uRetVal := 0
        end sequence




        return uRetVal


    /// <include file="Rdd.xml" path="doc/DbServer.OrderScope/*" />
    method OrderScope( nScope := TOPSCOPE as long, uValue := nil as usual) as usual


        local dwCurrentWorkArea := 0 as dword
        local oError as usual
        local n as dword


        lErrorFlag := false
        begin sequence


            if nScope == TOPSCOPE
                n := DBOI_SCOPETOP
                if IsNil( uValue )
                    n := DBOI_SCOPETOPCLEAR
                endif
            else
                n := DBOI_SCOPEBOTTOM
                if IsNil( uValue )
                    n := DBOI_SCOPEBOTTOMCLEAR
                endif
            endif


            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            self:__OptimisticFlush()
            if ! VoDbOrderInfo( n, "", nil, ref uValue )
                break ErrorBuild(_VoDbErrInfoPtr())
            endif
            if ! __DBSGoTop(self:nRetries)
                break ErrorBuild(_VoDbErrInfoPtr())
            endif
            __DBSSetSelect( dwCurrentWorkArea )
            self:Notify( NOTIFYFILECHANGE )


        recover using oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            self:Error( oErrorInfo, #OrderScope )
            uValue := nil
        end sequence




        return uValue


    /// <include file="Rdd.xml" path="doc/DbServer.OrderSkipUnique/*" />
    method OrderSkipUnique( nDirection as usual)  as logic
        local lRetCode := false as logic
        local oError as usual
        local dwCurrentWorkArea := 0 as dword


        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            if self:Notify( NOTIFYINTENTTOMOVE )
                lRetCode := VoDbOrderInfo( DBOI_SKIPUNIQUE, "", nil, ref nDirection )
                if lRetCode
                    lRetCode := self:__ProcessConcurrency( true )
                endif


                self:Notify( NOTIFYRECORDCHANGE )
            else
                lRetCode := false
                self:__SetStatusHL( #OrderSkipUnique,  ;
                    __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
                    __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
            endif
            __DBSSetSelect( dwCurrentWorkArea )


        recover using oError
            __DBSSetSelect( dwCurrentWorkArea )
            oErrorInfo := oError
            oHLStatus := self:__GenerateStatusHL( oError )
            oErrorInfo := oError
            lRetCode := false
        end sequence




        return lRetCode


    /// <include file="Rdd.xml" path="doc/DbServer.Pack/*" />
    method Pack( ) as logic


        local dwCurrentWorkArea := 0 as dword
        local lRetCode := false as logic
        local oError as usual


        lErrorFlag := false
        begin sequence
            VoDbSelect( wWorkArea, out dwCurrentWorkArea )
            self:__OptimisticFlush()
            if (lRetCode := VoDbPack( ))
                __DBSSetSelect( dwCurrentWorkArea )
                self:Notify( NOTIFYFILECHANGE )
            else
                break ErrorBuild( _VoDbErrInfoPtr( ) )
            endif
            wLastSelectionRec := 0


        recover using oError
            oHLStatus := self:__GenerateStatusHL( oError )
            __DBSSetSelect( dwCurrentWorkArea )
            oErrorInfo := oError
            lRetCode := false
        end sequence


        return lRetCode
end class


