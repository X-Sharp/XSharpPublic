///////////////////////////////////////////////////////////////////////////
// DBCmds.vh
//
// Copyright (c) 2007 Grafx Database Systems, Inc.  All rights reserved.
//
// Vulcan.NET VO-compatible database-related command definitions.

#command APPEND BLANK ;
   => DBAppend( .T. )

#command APPEND        ;
   [FROM <(src)>]      ;
   [FIELDS <list,...>] ;
   [FOR <fo>]          ;
   [WHILE <wh>]        ;
   [NEXT <nx>]         ;
   [RECORD <rec>]      ;
   [<rs:REST>]         ;
   [VIA <rdd>]         ;
   [ALL]               ;
   => DBApp( <(src)>, { <(list)> }, <{fo}>, <{wh}>, <nx>,  <rec>, <.rs.>, <rdd> )

#command APPEND               ;
   [FROM <(src)>]             ;
   [DELIMITED [WITH <*dlm*>]] ;
   [FIELDS <list,...>]        ;
   [FOR <fo>]                 ;
   [WHILE <wh>]               ;
   [NEXT <nx>]                ;
   [RECORD <rec>]             ;
   [<rs:REST>]                ;
   [ALL]                      ;
   => DBAppDelim( <(src)>, <(dlm)>, { <(list)> }, <{fo}>, <{wh}>, <nx>, <rec>, <.rs.> )

#command APPEND               ;
   [FROM <(src)>]             ;
   [SDF]                      ;
   [FIELDS <list,...>]        ;
   [FOR <fo>]                 ;
   [WHILE <wh>]               ;
   [NEXT <nx>]                ;
   [RECORD <rec>]             ;
   [<rs:REST>]                ;
   [ALL]                      ;
   => DBAppSDF( <(src)>, { <(list)> }, <{fo}>, <{wh}>, <nx>, <rec>, <.rs.> )

#command AVERAGE <fld1> [,<fldn>] [TO <var1> [,<varn>]] ;
   [FOR <fo>]     ;
   [WHILE <wh>]   ;
   [NEXT <nx>]    ;
   [RECORD <rec>] ;
   [<rs:REST>]    ;
   [ALL]          ;
   => Vulcan.Runtime.State.DBAvgCount := <var1> [:= <varn>] := 0                                                                               ;
   ; DBEval( {|| <var1> := <var1> + <fld1> [,<varn> := <varn> + <fldn>], Vulcan.Runtime.State.DBAvgCount++ }, <{fo}>, <{wh}>, <nx>, <rec>, <.rs.> ) ;
   ; <var1> /= Vulcan.Runtime.State.DBAvgCount [; <varn> /= Vulcan.Runtime.State.DBAvgCount ]

#command CLOSE ;
   => DBCloseArea()

#command CLOSE ALL ;
   => DBCloseAll() ; Select(1)

#command CLOSE DATABASES ;
   => DBCloseAll()

#command INDEX ON <key> TO <(file)> [<u:UNIQUE>]                        ;
   => OrdCondSet() ; DBCreateIndex( <(file)>, <(key)>, , IIF( <.u.>, .T., NIL ) )

#command INDEX ON <key> ;
   [TAG <order>]  ;
   [TO <(file)>]  ;
   [FOR <fo>]     ;
   [<all:ALL>]    ;
   [WHILE <wh>]   ;
   [NEXT <nx>]    ;
   [RECORD <rec>] ;
   [<rs:REST>]    ;
   [EVAL <ev>]    ;
   [EVERY <evr>]  ;
   [<u:UNIQUE>]   ;
   [<asd:ASCENDING>]      ;
   [<dsd:DESCENDING>]     ;
   [<lCur:USECURRENT>]    ;
   [<lAdd:ADDITIVE>]      ;
   [<lCus:CUSTOM>]        ;
   [<lNop:NOOPTIMIZE>]    ;
   => OrdCondSet( <"fo">, <{fo}>, [<.all.>], <{wh}>, <{ev}>, <evr>, RECNO(), <nx>, <rec>, [<.rs.>], [<.dsd.>], <.lAdd.>, [<.lCur.>], [<.lCus.>], [<.lNop.>] ) ;
   ; OrdCreate( <(file)>, <(order)>, <(key)>, NIL, IIF( <.u.>, .T., NIL ) )

#command CLOSE INDEXES ;
   => DBClearIndex()

#command CLOSE <alias> ;
   => <alias>->( DBCloseArea() )

#command COMMIT ;
   => DBCommit()

#command COMMIT ALL ;
   => DBCommitAll()

#command CONTINUE ;
   => DBContinue()

#command COPY              ;
   [TO <(dest)>]           ;
   [FIELDS <list,...>]     ;
   [FOR <fo>]              ;
   [WHILE <wh>]            ;
   [NEXT <nx>]             ;
   [RECORD <rec>]          ;
   [<rs:REST>]             ;
   [VIA <rdd>]             ;
   [ALL]                   ;
   [INHERIT <hdnlist,...>] ;
   => DBCopy( <(dest)>, { <(list)> }, <{fo}>, <{wh}>, <nx>, <rec>, <.rs.>, <rdd>, { <(hdnlist)> } )

#command COPY               ;
   [TO <(dest)>]            ;
   [DELIMITED [WITH <dlm>]] ;
   [FIELDS <flds,...>]      ;
   [FOR <fo>]               ;
   [WHILE <wh>]             ;
   [NEXT <nx>]              ;
   [RECORD <rec>]           ;
   [<rs:REST>]              ;
   [ALL]                    ;
   => DBCopyDelim( <(dest)>, <dlm>, { <(flds)> }, <{fo}>, <{wh}>, <nx>, <rec>, <.rs.> )

#command COPY          ;
   [TO <(dest)>]       ;
   [SDF]               ;
   [FIELDS <flds,...>] ;
   [FOR <fo>]          ;
   [WHILE <wh>]        ;
   [NEXT <nx>]         ;
   [RECORD <rec>]      ;
   [<rs:REST>]         ;
   [ALL]               ;
   => DBCopySDF( <(dest)>, { <(flds)> }, <{fo}>, <{wh}>, <nx>, <rec>, <.rs.> )

#command COPY          ;
   [STRUCTURE]         ;
   [TO <(file)>]       ;
   [FIELDS <flds,...>] ;
   => DBCopyStruct( <(file)>, { <(flds)> } )

#command COPY     ;
   [STRUCTURE]    ;
   [EXTENDED]     ;
   [TO <(file)>]  ;
   => DBCopyXStruct( <(file)> )

#command COUNT    ;
   [TO <var>]     ;
   [FOR <fo>]     ;
   [WHILE <wh>]   ;
   [NEXT <nx>]    ;
   [RECORD <rec>] ;
   [<rs:REST>]    ;
   [ALL]   ;
   => <var> := 0 ; DBEval( {|| <var>++ }, <{fo}>, <{wh}>, <nx>, <rec>, <.rs.> )
       
#command CREATE      ;
    <(file1)>        ;
    [FROM <(file2)>] ;
    [VIA <rdd>]      ;
    [ALIAS <a>]      ;
    [<nw:NEW>]       ;
    => _DBCreate( <(file1)>, <(file2)>, <rdd>, <.nw.>, <(a)> )

#command DELETE TAG          ;
   <cOrd1> [IN <(file1)>]    ;
   [,<cOrdN> [IN <(fileN)>]] ;
   => OrdDestroy( <(cOrd1)> , <(file1)> ) ; [ORDDESTROY( <(cOrdN)> ,<(fileN)> )]

#command DELETE ;
   => DBDelete()

#command DELETE   ;
   [FOR <fo>]     ;
   [WHILE <wh>]   ;
   [NEXT <nx>]    ;
   [RECORD <rec>] ;
   [<rs:REST>]    ;
   [ALL]          ;
   => DBEval( {|| DBDelete()}, <{fo}>, <{wh}>, <nx>, <rec>, <.rs.> )

#command FIND <*txt*> ;
   => DBSeek( <(txt)> )
   
#command FIND := <xpr> ;
   => ( find := <xpr> )
   
#command GO TOP ;
   => DBGoTop()
   
#command GOTO TOP ;
   => DBGoTop()
   
#command GOTO BOTTOM ;
   => DBGoBottom()
   
#command GO BOTTOM ;
   => DBGoBottom()
   
#command GOTO <n> ;
   => DBGoto(<n>)
   
#command GO <n> ;
   => DBGoto(<n>)

#command JOIN            ;
   [WITH <alias>]        ;
   [TO <(file)>]         ;
   [FIELDS <fields,...>] ;
   [FOR <fo>]            ;
   => DBJoin( <(alias)>, <(file)>,  {<(fields)>}, <{fo}> )

#command LOCATE   ;
   [FOR <fo>]     ;
   [WHILE <wh>]   ;
   [NEXT <nx>]    ;
   [RECORD <rec>] ;
   [<rs:REST>]    ;
   [ALL]          ;
   => DBLocate( <{fo}>, <{wh}>, <nx>, <rec>, <.rs.> )

#command PACK ;
   => DBPack()

#command RECALL ;
   => DBRecall()

#command RECALL   ;
   [FOR <fo>]     ;
   [WHILE <wh>]   ;
   [NEXT <nx>]    ;
   [RECORD <rec>] ;
   [<rs:REST>]    ;
   [ALL]  ;
   => DBEval( {|| DBRecall()}, <{fo}>, <{wh}>, <nx>, <rec>, <.rs.> )
    
#command REINDEX ;
   => OrdListRebuild()

#command REINDEX  ;
   [EVAL <ev>]    ;
   [EVERY <evr>]  ;
   => OrdCondSet( ,,,, <{ev}>, <evr>,,,,,,, ) ; ORDLISTREBUILD()

#command REPLACE       ;
   <f1> WITH <v1>      ;
   [, <fN> WITH <vN> ] ;
   => _FIELD-><f1> := <v1> [; _FIELD-><fN> := <vN> ]

#command REPLACE                        ;
   [<f1> WITH <v1> [, <fN> WITH <vN>] ] ;
   [FOR <fo>]                           ;
   [WHILE <wh>]                         ;
   [NEXT <nx>]                          ;
   [RECORD <rec>]                       ;
   [<rs:REST>]                          ;
   [ALL]                                ;
   => DBEval( {|| _FIELD-><f1> := <v1> [, _FIELD-><fN> := <vN>] }, <{fo}>, <{wh}>, <nx>, <rec>, <.rs.> )

#command SEEK          ;
   <xValue>           ;
   [<lSoft:SOFTSEEK>] ;
   [<lLast:LAST>]     ;
   => DBSeek( <xValue>, IIF( <.lSoft.>, .T., NIL ), IIF( <.lLast.>, .T., NIL ) )

//#command SELECT( <whatever> ) ;
//   => _SELECT( <whatever> )
   
#command SELECT <whatever> ;
   => DBSelectArea( <"whatever"> )

#command SET DRIVER TO <drv> ;
   => DBSetDriver( <"drv"> )

#command SET FILTER TO ;
   => DBClearFilter()
   
#command SET FILTER TO <xpr> ;
   => DBSetFilter( <{xpr}>, <"xpr"> )

#command SET INDEX TO [<(index1)> [,<(indexn)>]] ;
   [<adt:ADDITIVE>]           ;
    => IF ! <.adt.> ; OrdListClear() ; ENDIF [; OrdListAdd( <(index1)> )] [; OrdListAdd( <(indexn)> )]

#command SET ORDER TO <xOrder> [IN <(file)>]        ;
   => OrdSetFocus( <xOrder> [,<(file)>] )

#command SET ORDER TO TAG <cOrder> ;
   [IN <(file)>]                   ;
   => OrdSetFocus( <cOrder> [,<(file)>] )

#command SET ORDER TO ;
   => OrdSetFocus(0)

#command SET RELATION TO ;
   => DBClearRelation()

#command SET RELATION            ;
   [<adt:ADDITIVE>]              ;
   [TO <key1> INTO <alias1>]     ;
   [, [TO <keyn> INTO <aliasn>]] ;
   => IF ! <.adt.> ; DBClearRelation() ; ENDIF ; DBSetRelation( <(alias1)>, <{key1}>, <"key1"> ) [; DBSetRelation( <(aliasn)>, <{keyn}>, <"keyn"> )]

#command SET RELATION            ;
   [<adt:ADDITIVE>]              ;
   [TO <key1> INTO <alias1>]     ;
   [, [TO <keyn> INTO <aliasn>]] ;
   SCOPED                        ;
   => IF ! <.adt.> ; DBClearRelation() ; ENDIF ; OrdSetRelation( <(alias1)>, <{key1}>, <"key1"> ) [; OrdSetRelation( <(aliasn)>, <{keyn}>, <"keyn"> )]

#command SET SCOPE TO ;
   => OrdScope( 0, NIL ) ; OrdScope( 1, NIL )

#command SET SCOPE TO <xValue> ;
   => OrdScope( 0, <xValue> ) ; OrdScope( 1, <xValue> )

#command SET SCOPE TO <xVal1>, <xVal2> ;
   => OrdScope( 0, <xVal1> ) ; OrdScope( 1, <xVal2> )

#command SET SCOPETOP TO ;
   => OrdScope( 0, NIL )

#command SET SCOPETOP TO <xValue> ;
   => OrdScope( 0, <xValue> )

#command SET SCOPEBOTTOM TO ;
   => OrdScope( 1, NIL )

#command SET SCOPEBOTTOM TO <xValue> ;
   => OrdScope( 1, <xValue> )

#command SET DESCENDING ON ;
   => OrdDescend( , , .T. )

#command SET DESCENDING OFF ;
   => OrdDescend( ,, .F. )

#command SET MEMOBLOCK TO <nSize> ;
   => RDDInfo( _SET_MEMOBLOCKSIZE, <nSize> )

#command SKIP ;
   => DBSkip( 1 )
   
#command SKIP ALIAS <a> ;
   => <a> -> ( dbSkip( 1 ) )
   
#command SKIP <n> ;
   => DBSkip( <n> )
   
#command SKIP <n> ALIAS <a> ;
   => <a> -> ( dbSkip( <n> ) )

#command SORT      ;
   [TO <(dest)>]   ;
   [ON <flds,...>] ;
   [FOR <fo>]      ;
   [WHILE <wh>]    ;
   [NEXT <nx>]     ;
   [RECORD <rec>]  ;
   [<rs:REST>]     ;
   [ALL]           ;
   => DBSort( <(dest)>, { <(flds)> }, <{fo}>, <{wh}>, <nx>, <rec>, <.rs.> )

#command SUM                        ;
   [<x1> [,<xn>]] TO [<v1> [,<vn>]] ;
   [FOR <fo>]                       ;
   [WHILE <wh>]                     ;
   [NEXT <nx>]                      ;
   [RECORD <rec>]                   ;
   [<rs:REST>]                      ;
   [ALL]  ;
   => <v1> := [<vn> := ] 0 ; DBEval( {|| <v1> += <x1> [, <vn> += <xn>] }, <{fo}>, <{wh}>, <nx>, <rec>, <.rs.> )

#command TOTAL         ;
   [TO <(dest)>]       ;
   [ON <key>]          ;
   [FIELDS <flds,...>] ;
   [FOR <fo>]          ;
   [WHILE <wh>]        ;
   [NEXT <nx>]         ;
   [RECORD <rec>]      ;
   [<rs:REST>]         ;
   [ALL]               ;
   => DBTotal( <(dest)>, <{key}>, { <(flds)> }, <{fo}>, <{wh}>, <nx>, <rec>, <.rs.> )

#command UNLOCK ;
   => DBUnlock()
   
#command UNLOCK ALL ;
   => DBUnlockAll()

#command UPDATE             ;
   [FROM <alias>]           ;
   [ON <key>]               ;
   [REPLACE <f1> WITH <v1>] ;
   [<rand:RANDOM>]          ;
   => DBUpdate( <(alias)>, <{key}>, <.rand.>, {|| _FIELD-><f1> := <v1> } )

#command USE ;
   => DBCloseArea()

#command USE <(db)>                 ;
   [VIA <rdd>]                      ;
   [ALIAS <a>]                      ;
   [<sh:SHARED>]                    ;
   [<ex:EXCLUSIVE>]                 ;
   [<ro:READONLY>]                  ;
   [<nw:NEW>]                       ;
   [FIELDS <flds>]                  ;
   [INDEX <(index1)> [,<(indexn)>]] ;
   [INHERIT <hdnlist,...>]          ;
   => DBUseArea( <.nw.>, <rdd>, <(db)>, <(a)>, IIF(<.sh.> .or. <.ex.>, !<.ex.>, NIL), <.ro.>, <flds>,NIL, { <(hdnlist)> } ) [; DBSETINDEX(<(index1)>)] [; DBSETINDEX(<(indexn)>)]

#command ZAP ;
   => DBZap()

// end
