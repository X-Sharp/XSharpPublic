//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#pragma options ("enforceself", on)
#pragma warnings(165, off)
/// <include file="Rdd.xml" path="doc/DbServer/*" />
[XSharp.Internal.TypesChanged];
partial class DbServer inherit DataServer
    protect lShared as logic
    protect lReadOnly as logic
    protect symAlias as symbol
    protect wWorkArea as dword
    protect cRDDName as string
    protect oFileSpec as FileSpec
    protect aOriginalBuffer as array
        //PROTECT aRLockVerifyBuffer AS ARRAY
    protect siSuspendNotification as shortint
    protect lRelationsActive as logic
    protect lCDXSelectionActive as logic
    protect aRelationChildren as array
    protect lSelectionActive as logic
    protect oDBSelectionParent as object
    protect wSelectionWorkArea as dword
    protect cbSelectionParentExpression as usual
    protect uSelectionValue as usual
    protect cbSelectionIndexingExpression as usual
    protect siSelectionStatus as shortint
    protect lActiveScope as logic
    protect cbStoredForBlock as usual
    protect cbStoredWhileBlock as usual
    protect uStoredScope as usual
    protect nStoredNextCount as longint
    protect lStoredAllRecords as usual
    protect lStoredRestOfFile as logic
    protect wLastSelectionRec as dword
    protect oErrorInfo as Error
    protect lErrorFlag as logic
    protect nEffectiveCCMode as dword
    protect aCurrentBuffer as array
    protect lCCOptimisticRecChg as logic
    protect aStruct as array
    protect aRdds as array
    protect nRetries as dword
    protect oRDD as XSharp.RDD.IRdd




    /// <exclude />
    method __AcceptSelectiveRelation( oDBParent as DbServer, wParentWorkArea as dword, cbSelection as usual) as void strict


        local cIndexExt as string
        local dwCurrentWorkArea as dword




        oDBSelectionParent := oDBParent
        wSelectionWorkArea := wParentWorkArea


        cbSelectionParentExpression := cbSelection


        VoDbSelect( wWorkArea, out dwCurrentWorkArea )


        cIndexExt := IndexExt( )


        if Instr( "MDX", cIndexExt )
            lSelectionActive := true
            siSelectionStatus := DBSELECTIONNULL
            cbSelectionIndexingExpression := DBSelectionIndex{ self, __DBSDbOrderInfo( DBOI_EXPRESSION, "", 0 ), (long) wWorkArea }
        else
            lCDXSelectionActive := true
        endif


        __DBSSetSelect( dwCurrentWorkArea  )


        return


    /// <exclude />
    method __BuildDataField( a as array ) as DataField strict
        local oRet := null as DataField


        if IsArray( a )
            oRet := DataField{ (string) a[ DBS_NAME ], FieldSpec{ (string) a[ DBS_NAME ] , a[ DBS_TYPE ],  ;
                (dword) a[ DBS_LEN ], (dword)  a[ DBS_DEC ] } }
        endif
        return oRet


    /// <exclude />
    method __ClearChildRelation( oChild as DbServer ) as void strict
        local w as dword
        local wLen as dword




        if ! ( ( w := AScan( aRelationChildren, oChild ) ) == 0 )
            if ALen( aRelationChildren ) == 1
                lRelationsActive := false
                aRelationChildren := { }
            else
                wLen := ALen( aRelationChildren )
                aRelationChildren[w] := aRelationChildren[wLen]
                ASize( aRelationChildren, wLen - 1 )
            endif
        endif


        return


    /// <exclude />
    method __ClearLocks( )  as void strict
        local uVOVal as usual


        do case
        case nEffectiveCCMode == ccOptimistic
            self:__OptimisticFlush( )


        case nEffectiveCCMode == ccStable
            if ! VoDbInfo( DBI_ISFLOCK, ref uVOVal )
                break ErrorBuild( _VoDbErrInfoPtr( ) )
            endif
            if ! uVOVal
                self:UnLock( nLastLock )
            endif


        case nEffectiveCCMode == ccRepeatable
            if ! VoDbInfo( DBI_ISFLOCK, ref uVOVal )
                break ErrorBuild( _VoDbErrInfoPtr( ) )
            endif
            if ! uVOVal
                self:UnLock( )
            endif


        case nEffectiveCCMode == ccFile
            self:UnLock( )
        endcase




        return


    /// <exclude />
    method __DbServerEval( uBlock as usual, uCobFor as usual, uCobWhile as usual, ;
            nNext as usual, nRecno as usual, lRest as logic, lCC as logic, lCCUpdate as logic ) as logic strict
        local lRetCode := true as logic
        local lLimit as logic
        local lBlock as logic
        local lFor as logic
        local lWhile as logic
        local lInternalError as logic
        local iRecno as int
        local dwCurrentWorkArea as dword
        local uRC as usual
        local oError as usual
        local nCurrRec as dword
        local uFLock as usual
        local lRestore	as logic

        // Make sure we restore the workarea
        // The codeblocks may select another workarea
        lRestore	:= DbSetRestoreWorkarea(true)

        VoDbSelect( wWorkArea, out dwCurrentWorkArea )


        lErrorFlag := false
        begin sequence


            if ! IsNil( nNext ) .and. nNext != 0
                lLimit := true
                iRecno := nNext
                if iRecno < 1
                    oHLStatus := HyperLabel{ #BadNext, __CavoStr( __CAVOSTR_DBFCLASS_BADNEXT_CAPTION ),  ;
                        __CavoStr( __CAVOSTR_DBFCLASS_BADNEXT ) }
                    lInternalError := true
                    break ErrorBuild( _VoDbErrInfoPtr( ) )
                endif
            else
                lLimit := false
                if IsNil( uCobWhile ) .and. ! lRest .and. ! self:GoTop( )
                    oHLStatus := HyperLabel{ #NoGoTop, __CavoStr( __CAVOSTR_DBFCLASS_NOGOTOP_CAPTION ),  ;
                        __CavoStr( __CAVOSTR_DBFCLASS_NOGOTOP ) }
                    lInternalError := true
                    break ErrorBuild( _VoDbErrInfoPtr( ) )
                endif
            endif

            lBlock 	:= ! IsNil( uBlock )
            lFor 		:= ! IsNil( uCobFor )
            lWhile 	:= ! IsNil( uCobWhile )

            if ! VoDbInfo( DBI_ISFLOCK, ref uFLock )
                break ErrorBuild( _VoDbErrInfoPtr( ) )
            endif
            do while ! VoDbEof( )
                if lWhile .and. ! Eval( uCobWhile )
                    exit
                endif
                if ( ! lFor .or. Eval( uCobFor ) ) .and. lBlock
                    if lCC
                        if nEffectiveCCMode = ccOptimistic
                            if lCCUpdate .and. ! uFLock
                                nCurrRec := VoDbRecno( )
                                VoDbRlock( nCurrRec )
                            endif
                        else
                            if nEffectiveCCMode = ccStable .and. lCCUpdate .and. ! uFLock
                                if nLastLock != 0
                                    VoDbUnlock( nLastLock )
                                endif
                                nLastLock := (int) VoDbRecno( )
                                if ! VoDbEof( )
                                    lRetCode := VoDbRlock( nLastLock )
                                else
                                    nLastLock := 0
                                endif
                            elseif nEffectiveCCMode = ccRepeatable
                                if ! VoDbEof( )
                                    lRetCode := VoDbRlock( VoDbRecno( ) )
                                endif
                            endif
                        endif
                    endif


                    uRC := Eval( uBlock )
                    if lCC .and. nEffectiveCCMode = ccOptimistic .and. lCCUpdate .and. ! uFLock
                        VoDbUnlock( nCurrRec )
                    endif


                    if IsLogic( uRC )
                        lRetCode := lRetCode .and. uRC
                        if ! lRetCode
                            exit
                        endif
                    endif
                endif


                if lLimit .and. ( --iRecno = 0 )
                    exit
                endif


                if ! VoDbSkip( 1 )
                    break ErrorBuild( _VoDbErrInfoPtr( ) )
                endif
            enddo
        recover using oError
            oErrorInfo := oError
            if ! lInternalError
                __DBSSetSelect(dwCurrentWorkArea  )
                self:Error( oErrorInfo, #__DbServerEval )
            endif
            lRetCode := false
        end sequence
        __DBSSetSelect( dwCurrentWorkArea )
        DbSetRestoreWorkarea(lRestore)
        return lRetCode


    /// <exclude />
    method __GenerateStatusHL( oError as  Error) as HyperLabel strict
        local oRet as HyperLabel
        local cDesc as string
        if oError == null_object
            oError := Error{}
        endif


        lErrorFlag := true


        cDesc := Symbol2String( ClassName(self) ) + ": "
        if Len( oError:Description ) > 0
            cDesc += oError:Description
        else
            cDesc += ErrString( oError:Gencode )
        endif
        if  SLen(oError:SubCodeText) > 0
            cDesc += " ("+oError:SubCodeText+")"
        endif


        if oError:OSCode != 0
            cDesc += ":" + DosErrString( oError:OSCode )
        endif


        oRet := HyperLabel{ oError:FuncSym, AsString( oError:Gencode ), cDesc }




        return oRet


    /// <exclude />
    method __InitRecordBuf( ) as void strict
        local i as dword
        local x as usual


        for i := 1 to self:wFieldCount
            if VoDbFieldGet( i, ref x )
                //Mark as BLOB when fieldtype = 'M' and not string
                aOriginalBuffer[BUFFER_VALUE, i] 			:= x
                if ! IsString(x) .and. self:aStruct[i,DBS_TYPE] == "M"
                    aOriginalBuffer[BUFFER_IS_BLOB, i] 		:= true
                else
                    aOriginalBuffer[BUFFER_IS_BLOB, i] 	:= false
                endif
            else
                // If VoDbFieldGet() fails this may be a BLOB field > 64 Kb
                aOriginalBuffer[BUFFER_VALUE, i] 			:= nil
                aOriginalBuffer[BUFFER_IS_BLOB, i] 		:= true
            endif
            aCurrentBuffer[BUFFER_IS_CHANGED, i] := false
        next  // i




        return




    /// <exclude />
    method __Notify( kNotification, uDescription )  as usual clipper
        local uRetValue as usual
        local oError as usual


        begin sequence
            uRetValue := self:Notify( kNotification, uDescription )
        recover using oError
            oErrorInfo := oError
            oHLStatus := self:__GenerateStatusHL( oError )
        end sequence


        return uRetValue


    /// <exclude />
    method __NotifyBufferFlush( ) as void strict
        local dwCurrentWorkArea  as dword




        ASend( aRelationChildren, #__NotifyBufferFlush )
        VoDbSelect( wWorkArea, out dwCurrentWorkArea )
        self:__OptimisticFlush( )
        __DBSSetSelect( dwCurrentWorkArea  )




        return


    /// <exclude />
    method __OptimisticFlush() as void strict
        local w as dword
        local uFLock as usual
        local uValue as usual
        local nCurRec as dword
        local uIsRLock as usual
        local nOrgBuffLen as dword
        local cFieldType	as string




        if nEffectiveCCMode == ccOptimistic .and. lCCOptimisticRecChg
            nCurRec := VoDbRecno( )


            if ! VoDbRecordInfo( DBRI_LOCKED, 0, ref uIsRLock )
                break ErrorBuild( _VoDbErrInfoPtr( ) )
            endif


            if ! uIsRLock
                if self:__RLockVerify( )
                    if ! VoDbInfo( DBI_ISFLOCK, ref uFLock )
                        break ErrorBuild( _VoDbErrInfoPtr( ) )
                    endif
                    for w := 1 upto wFieldCount
                        cFieldType := self:aStruct[w, DBS_TYPE]
                        if aCurrentBuffer[BUFFER_IS_CHANGED, w] .and. ! aOriginalBuffer[BUFFER_IS_BLOB, w]
                            uValue := aCurrentBuffer[BUFFER_VALUE, w]
                            if ! VoDbFieldPut( w, uValue )
                                break ErrorBuild( _VoDbErrInfoPtr( ) )
                            endif
                            if ! uFLock
                                // Memo Fields must NOT be padded !
                                if cFieldType != "M" .and. IsString( aOriginalBuffer[BUFFER_VALUE, w] )
                                    nOrgBuffLen := SLen( aOriginalBuffer[BUFFER_VALUE, w] )
                                    aOriginalBuffer[BUFFER_VALUE, w] := PadR( uValue, nOrgBuffLen )
                                else
                                    aOriginalBuffer[BUFFER_VALUE, w] := uValue
                                endif
                            endif
                            aCurrentBuffer[BUFFER_VALUE, w]   := nil
                            aCurrentBuffer[BUFFER_IS_CHANGED, w] := false
                        endif
                    next


                    lCCOptimisticRecChg := false


                    if ! uFLock
                        VoDbUnlock( nCurRec )
                    endif
                else
                    if oErrorInfo = null_object
                        break DbError{ self, #Optimistic_Buffer_flush, EG_LOCK,  ;
                            __CavoStr( __CAVOSTR_DBFCLASS_RECORDCHANGED ) }
                    else
                        break oErrorInfo
                    endif
                endif
            endif
        endif




        return


    /// <exclude />
    method __OptimisticFlushNoLock( ) as void strict
        local w as dword
        local uValue as usual
        if nEffectiveCCMode == ccOptimistic .and. lCCOptimisticRecChg
            for w := 1 upto wFieldCount
                if aCurrentBuffer[BUFFER_IS_CHANGED, w] .and. ! aOriginalBuffer[BUFFER_IS_BLOB, w]
                    uValue := aCurrentBuffer[BUFFER_VALUE, w]
                    if ! VoDbFieldPut( w, uValue )
                        break ErrorBuild( _VoDbErrInfoPtr( ) )
                    endif
                    aCurrentBuffer[BUFFER_VALUE, w]		:= nil
                    aCurrentBuffer[BUFFER_IS_CHANGED, w]	:= false
                endif
            next
            lCCOptimisticRecChg := false
        endif




        return


    /// <exclude />
    method __ProcessConcurrency( lBreak as logic) as logic strict
        local uVOVal as usual
        local lError as logic
        local lRetCode as logic

        lError := false
        if self:nEffectiveCCMode = ccStable
            if VoDbInfo( DBI_ISFLOCK, ref uVOVal )
                if ! uVOVal
                    if nLastLock != 0
                        VoDbUnlock( self:nLastLock )
                    endif
                    nLastLock := (int) VoDbRecno( )
                    if ! VoDbEof( )
                        lRetCode := VoDbRlock( self:nLastLock )
                    else
                        self:nLastLock := 0
                    endif
                else
                    self:nLastLock := 0
                endif
            else
                lError := true
            endif

        elseif self:nEffectiveCCMode = ccRepeatable
            if ! VoDbEof( )
                if VoDbInfo( DBI_ISFLOCK, ref uVOVal )
                    if ! uVOVal
                        lRetCode := VoDbRlock( VoDbRecno( ) )
                    endif
                else
                    lError := true
                endif
            endif
        else
            lRetCode := true
        endif

        if lBreak .and. (lError .or. ! lRetCode) .and. CanBreak()
            break ErrorBuild( _VoDbErrInfoPtr( ) )
        endif
        return lRetCode


    /// <exclude />
    method __RLockVerify( ) as logic strict
        local w as dword
        local siCurrentRec as dword
        local uWasLocked as usual
        local uVOVal as usual
        local lRetCode as logic
        local nDiff	as float
        local uValue	as usual
        local aRLockVerifyBuffer as array




        lRetCode := true
        siCurrentRec := VoDbRecno( )
        if ! VoDbInfo( DBI_ISFLOCK, ref uVOVal )
            break ErrorBuild( _VoDbErrInfoPtr( ) )
        endif
        if ! VoDbRecordInfo( DBRI_LOCKED, 0, ref uWasLocked )
            break ErrorBuild( _VoDbErrInfoPtr( ) )
        endif
        uWasLocked := uWasLocked .or. uVOVal
        if ! uWasLocked
            lRetCode := VoDbRlock( siCurrentRec )
        endif
        if ! lRetCode
            break ErrorBuild( _VoDbErrInfoPtr( ) )
        else
            // Store our 'current record'
            aRLockVerifyBuffer := ArrayNew( wFieldCount )
            for w := 1 upto wFieldCount
                aRLockVerifyBuffer[w] := __DBSFieldGet(w)
            next
            // Get the current record buffer from disk and compare the fields
            // in the current buffer with our values.
            VoDbBuffRefresh( )
            for w := 1 upto wFieldCount
                uValue := __DBSFieldGet( w )
                //
                if aOriginalBuffer[BUFFER_IS_BLOB, w]
                    // Field was a blob. Compare field types
                    if UsualType(aOriginalBuffer[BUFFER_VALUE, w]) != UsualType(uValue)
                        lRetCode := false
                    endif
                elseif ! ( aOriginalBuffer[BUFFER_VALUE, w] == uValue )
                    // Field has changed.
                    // Test for float fields with non-relevant differences
                    if IsFloat(uValue)
                        nDiff := 10 ^ -(aStruct[w, DBS_DEC])
                        if Abs(aOriginalBuffer[BUFFER_VALUE, w] - uValue) > nDiff
                            lRetCode := false
                        endif
                    else
                        lRetCode := false
                    endif
                    //ELSE
                    // The field has not changed
                endif
                if ! lRetCode
                    oHLStatus := HyperLabel{ #RECORDCHANGED,  ;
                        __CavoStr( __CAVOSTR_DBFCLASS_RECORDCHANGED_CAPTION ),  ;
                        __CavoStr( __CAVOSTR_DBFCLASS_RECORDCHANGED ), nil }
                    oErrorInfo := null_object
                    lErrorFlag := true
                    exit
                endif
            next
            // restore the 'current record' in the buffer
            for w := 1 upto wFieldCount
                if ! VoDbFieldPut( w, aRLockVerifyBuffer[w] )
                    break ErrorBuild( _VoDbErrInfoPtr( ) )
                endif
            next


            if ! lRetCode .and. ! uWasLocked
                VoDbUnlock( siCurrentRec )
            endif
        endif

        return lRetCode


    /// <exclude />
    method __SetAlias( cName as string, aField as array, nField as dword) as void strict
        local cAlias as string
        local oFSpec as FieldSpec

        cAlias := "_" + cName
        oFSpec := FieldSpec{ cAlias, aField[DBS_TYPE], aField[DBS_LEN], aField[DBS_DEC] }
        self:aDataFields[nField] := DataField{ cName, oFSpec }

        return


    /// <exclude />
    method __SetStatusHL( uFuncSym as usual, uGenCode as usual, uMessage as usual ) as void strict


        lErrorFlag := true
        oErrorInfo := null_object
        if ! IsString( uGenCode )
            uGenCode := AsString( uGenCode )
        endif
        oHLStatus := HyperLabel{ uFuncSym, uGenCode, uMessage, nil }

        return


    /// <exclude />
    method __SetupLocks( )  as void strict
        local w as dword
        local uFlock as usual

        nLastLock := 0
        do case
        case nEffectiveCCMode == ccNone
            //nothing to do
            nop

        case nEffectiveCCMode == ccOptimistic
            for w := 1 upto wFieldCount
                aCurrentBuffer[BUFFER_VALUE, w]   := nil
                aCurrentBuffer[BUFFER_IS_CHANGED, w] := false
            next
            lCCOptimisticRecChg := false
            self:__InitRecordBuf()


        case nEffectiveCCMode == ccStable .or. nEffectiveCCMode == ccRepeatable
            if ! VoDbInfo( DBI_ISFLOCK, ref uFlock )
                break ErrorBuild( _VoDbErrInfoPtr( ) )
            endif
            if ! uFlock
                nLastLock := (long) self:RecNo
                if ! self:RLock( nLastLock )
                    nLastLock := 0
                    oHLStatus := self:Status
                endif
            endif

            case nEffectiveCCMode == ccFile
            if ! self:FLock( )
                oHLStatus := self:Status
            endif

        otherwise
            oErrorInfo := DbError{ self, #ConcurrencyControl, EG_ARG,  ;
                __CavoStr( __CAVOSTR_DBFCLASS_BADCONCURRENCYASSIGN ), nCCMode, "nCCMode" }
            self:Error( oErrorInfo, #ConcurrencyControl )
        endcase

        return
 

    /// <include file="Rdd.xml" path="doc/DbServer.ctor/*" />
    constructor( cFile as usual, lShareMode := null as object, lReadOnlyMode := null as object, xDriver:= "" as string, aRDD := null_array as array)
        if cFile is FileSpec var oFs
            self(oFs, lShareMode, lReadOnlyMode , xDriver, aRDD )
        elseif cFile is string var strFile
            self(FileSpec{strFile}, lShareMode, lReadOnlyMode , xDriver, aRDD )
        else
            break DbError{ self, #Init, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADFILENAME ),  cFile, "cFile" }
        endif

    /// <include file="Rdd.xml" path="doc/DbServer.ctor/*" />
    constructor( oFS := null as FileSpec, lShareMode := null as object, lReadOnlyMode := null as object, xDriver:= "" as string, aRDD := null_array as array)
        local dwCurrentWorkArea := 0 as dword
        local cFileName as string
        local w as dword
        local n as dword
        local oError as usual
        local cTemp as string
        local rddList as _RddList
        local uTemp as usual
        local aField as array
        local uProps as usual
        local wProps as dword
        local lRetCode as logic

        super( )

        aRelationChildren := { }

        lErrorFlag := false
        begin sequence
            siSuspendNotification := 0
            dwCurrentWorkArea := VoDbGetSelect( )
            if oFS == null
                break DbError{ self, #Init, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFILENAME ),  ;
                    nil, "oFs" }
            endif

            if Empty( oFS:Extension )
                oFS:Extension := ".DBF"
            endif
            oFileSpec := oFS


            cFileName := oFileSpec:FileName


            symAlias := self:ConstructUniqueAlias( cFileName )


            if lShareMode  == null
                self:lShared := ! SetExclusive( )
            elseif lShareMode is logic var lSh
                self:lShared := lSh
            else
                break DbError{ self, #Init, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADSHAREMODE ),  lShareMode, "lShareMode" }
            endif


            if lReadOnlyMode == null
                lReadOnly := false
            elseif lReadOnlyMode is logic
                lReadOnly := (logic) lReadOnlyMode
            else
                break DbError{ self, #Init, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADREADONLYMODE ),  ;
                    lReadOnlyMode, "lReadOnlyMode" }
            endif


            cTemp := Symbol2String( ClassName( self ) )
            oHyperLabel := HyperLabel{ cFileName, cFileName,  ;
                cTemp + ": " + cFileName + " " +  ;
                VO_Sprintf( __CAVOSTR_DBFCLASS_ALIAS, Symbol2String( symAlias ) ),  ;
                cTemp + "_" + cFileName }


            oHLStatus := nil


            self:wWorkArea := VoDbSetSelect( -1 )


            if ! ( IsArray( xDriver ) .or. IsString( xDriver ) )
                xDriver := RddName( )
            endif


            self:aRdds := __RDDList( xDriver, aRDD )
            rddList := __AllocRddList( aRdds )


            lRetCode := VoDbUseArea( false, rddList, oFileSpec:FullPath, Symbol2String( symAlias ),  ;
                lShared, lReadOnly )


            if ! lRetCode
                break ErrorBuild( _VoDbErrInfoPtr( ) )
            endif


            self:cRDDName := RddName( )
            self:oRDD     := (XSharp.RDD.IRdd) DbInfo(DBI_RDD_OBJECT)


            self:nCCMode := self:nEffectiveCCMode := DbGetDefaultLockMode()
            if lReadOnly .or. ! lShared
                self:nEffectiveCCMode := ccNone
            endif


            wFieldCount := FCount( )


            aStruct := ArrayCreate( wFieldCount )


            for w := 1 upto wFieldCount
                uProps := nil
                if ! VoDbFieldInfo( DBS_PROPERTIES, w, ref uProps )
                    break ErrorBuild( _VoDbErrInfoPtr( ) )
                endif
                wProps := uProps
                aField := ArrayCreate( wProps )
                for n := 1 upto wProps
                    VoDbFieldInfo( n, w, ref uTemp )
                    aField[n] := uTemp
                next
                aStruct[w] := aField
            next


            aDataFields := ArrayNew( wFieldCount )


            aOriginalBuffer := ArrayNew( 2, wFieldCount )
            aCurrentBuffer  := ArrayNew( 2, wFieldCount )
            self:__InitRecordBuf( )




            __DBSSetSelect(dwCurrentWorkArea)


        recover using oError
            oErrorInfo := oError
            if Used( )
                VoDbCloseArea( )
            endif
            wWorkArea := 0
            __DBSSetSelect(dwCurrentWorkArea)
            oHLStatus := HyperLabel{ #NoTable, __CavoStr( __CAVOSTR_DBFCLASS_NOTABLE_CAPTION ),  ;
                __CavoStr( __CAVOSTR_DBFCLASS_NOTABLE ) }
            self:Error( oErrorInfo, #Init )
        end sequence


        self:nRetries := LockTries( )


        return




end class


