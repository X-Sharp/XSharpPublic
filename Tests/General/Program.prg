using System.IO
FUNCTION Start() AS VOID
LOCAL cKunden AS STRING
LOCAL cRechnung AS STRING

/*
VO results:
NIL
NIL
.T.
1 5409 5411 .F. .F.
2 5380 5411 .F. .F.
3 1357 5411 .F. .F.
0382
4 5412 5411 .T. .T.
5 5409 5411 .F. .F.
6 1357 5411 .F. .F.
0382
7 5412 5411 .T. .T.
8 5409 5411 .F. .F.
2409
Press any key to continue...

VO without scope

.T.
1 5379 5411 .F. .F.
2 5291 5411 .F. .F.
3 3306 5411 .F. .F.
1749
4 5412 5411 .T. .T.
5 5379 5411 .F. .F.
6 3306 5411 .F. .F.
1749
7 5412 5411 .T. .T.
8 5379 5411 .F. .F.
0443
Press any key to continue...


// VO without DbSetOrder(3)

NIL
NIL
.T.
1 5412 5411 .T. .T.
2 5412 5411 .F. .T.
3 5412 5411 .T. .T.

4 5412 5411 .T. .T.
5 5412 5411 .F. .T.
6 5412 5411 .T. .T.

7 5412 5411 .T. .T.
8 5412 5411 .F. .T.

Press any key to continue...
Press any key to continue...


*/

RddSetDefault("DBFCDX")

cKunden := "C:\DBF\Kunden"
cRechnung := "C:\DBF\Rechnung"

DbUseArea(TRUE,,cKunden,"Kunden")
DbUseArea(TRUE,,cRechnung,"Rechnung")

//Rechnung->DbSetOrder(3)


? Rechnung->DbOrderInfo(DBOI_SCOPETOP,,,"JJ        20130301")
? Rechnung->DbOrderInfo(DBOI_SCOPEBOTTOM,,,"JJ        20211231")

Kunden->DbSetOrder( 2 )

//? Rechnung-> VoDbSetRelation( "Kunden", {|| _field->KdNo }, "KDNO" )

Rechnung -> DbGoBottom()
? 1, Recno(), LastRec(), Eof(), Bof()
Rechnung -> DbSkip(-1)
? 2, Recno(), LastRec(), Eof(), Bof()
Rechnung -> DbGoTop()
? 3, Recno(), LastRec(), Eof(), Bof()
? Rechnung -> FieldGet(1)

Rechnung -> DbGoto(5412)
? 4, Recno(), LastRec(), Eof(), Bof()
Rechnung -> DbSkip(-1)
? 5, Recno(), LastRec(), Eof(), Bof()
Rechnung -> DbGoTop()
? 6, Recno(), LastRec(), Eof(), Bof()
? Rechnung -> FieldGet(1)

Rechnung -> DbGoto(5412)
? 7, Recno(), LastRec(), Eof(), Bof()
Rechnung -> DbSkip(-1)
? 8, Recno(), LastRec(), Eof(), Bof()
? Rechnung -> FieldGet(1)
wait
