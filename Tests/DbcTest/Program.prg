#command INDEX ON <key>         										;
     TAG <order>                                						;
     [BINARY <bin>]                  								    ;
     [COLLATE <coll>]                								    ;
     [OF <file>]                										;
     [FOR <fo>]                 										;
     [<u:UNIQUE>]               										;
     [<cmp:COMPACT>]          										    ;
     [<cand:CANDIDATE>]          										;
     [<asd:ASCENDING>]          										;
     [<dsd:DESCENDING>]         										;
     [<lAdd:ADDITIVE>]          										;
     [<lNop:NOOPTIMIZE>]        										;
    => OrdCondSet( <"fo">, , , , , , 			                        ;
       , , ,,[<.dsd.>],[<.lAdd.>], , , [<.lNop.>] )      				;;  ;
       OrdCreate(<(file)>,<(order)>,<(key)>,,IF(<.u.>,.T., NIL))


** Todo Handle Collate, Binary, Candidate, Compact clauses

#command INDEX ON <key>         										;
     TO <(file)>                										;
     [BINARY <bin>]                  								    ;
     [COLLATE <coll>]                								    ;
     [FOR <fo>]                 										;
     [<u:UNIQUE>]               										;
     [<cmp:COMPACT>]          										    ;
     [<cand:CANDIDATE>]          										;
     [<asd:ASCENDING>]          										;
     [<dsd:DESCENDING>]         										;
     [<lAdd:ADDITIVE>]          										;
     [<lNop:NOOPTIMIZE>]        										;
    => OrdCondSet(<"fo">, , , , , , ,                                   ;
	    , , ,[<.dsd.>], [<.lAdd.>], , , [<.lNop.>])      				;;  ;
       dbCreateIndex( <(file)>, <"key">,, IIF( <.u.>, TRUE, NIL ))

#command SORT  TO <(file)> ON <fields,...>    ;
         [<asc:ASCENDING>]                    ;
         [<desc:DESCENDING>]                  ;
         [FOR <lfor>]                         ;
         [WHILE <lwhile>]                     ;
         [NEXT <nNext>]                       ;
         [RECORD <rec>]                       ;
         [<rest:REST>]                        ;
         [<noopt: NOOPTIMIZE>]                ;
         [__FOXLIST__ <fldlist>]              ;
		 [__FOXWILD__ <fldwild> ]             ;
         [ALL]                                ;
                                              ;
      => DbSortFox(                           ;
                   <(file)>, { <(fields)> },  ;
                   <{lfor}>, <{lwhile}>, <nNext>, <rec>, <.rest.>, <.noopt.> ;
                 , IIF(<.asc.> .OR. <.desc.>, <.desc.>, NIL),           ;
                   IIF( <.fldlist.>, <!fldlist!>, <!fldwild!>) )


FUNCTION Start() AS VOID
LOCAL cDbf AS STRING
LOCAL n AS INT
cDbf := "C:\test\index"
FErase(cDbf + ".cdx")
DbCreate(cDbf, {{"FLD","C",10,0}})
DbUseArea(TRUE,"DBFCDX",cDbf,"myalias")
FOR n := 1 UPTO 20
	DbAppend()
	FieldPut(1,AsString(20-n))
NEXT
INDEX ON FLD TAG MYORDER BINARY OF (cDbf) FOR !Empty(FLD) COMPACT
INDEX ON FLD TO "test"
DbCloseArea()
wait
