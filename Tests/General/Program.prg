USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

FUNCTION Start() AS VOID
LOCAL cbExpression AS USUAL
LOCAL u1,u2 AS USUAL
LOCAL cPath AS STRING
LOCAL n AS INT
cPath := "C:\test\"

RddSetDefault("DBFCDX")

DbCreate(cPath + "parent" , {{"FLD" , "N" , 3 , 0}})
DbCreate(cPath + "child" , {{"FLD" , "N" , 3 , 0}})


DbUseArea(TRUE,"DBFCDX", cPath + "parent", "parent" , TRUE)
parent->DbAppend()
parent->FieldPut(1,3)

DbUseArea(TRUE,"DBFCDX", cPath + "child", "child" , TRUE)
child->DbCreateIndex(cPath + "child","FLD")
FOR n := 1 UPTO 5
child->DbAppend()
child->FieldPut(1,n)
NEXT

cbExpression := &( "{ || FLD }")
child->DbGoBottom()
? "evaluate in bottom, must be 5:", child->Eval( cbExpression ) // 3

? parent->VoDbSetRelation("child" , cbExpression ,"FLD")
parent->DbGoTop()

child->DbSkip(0) // this throws a runtime error in __FieldGet()

? "Recno must be 3:",child->RecNo() 
?
? "evaluate after SetRelation(), must be 3:", child->Eval( cbExpression )
? "Recno should be 3:", child->RecNo()

u1 := u2 := 3
? child->VoDbOrderInfo( DBOI_SCOPETOP	 , "", NIL, REF u1 )
? child->VoDbOrderInfo( DBOI_SCOPEBottom, "", NIL, REF u2 )
parent->DbGoTop()


child->DbGoTop()
? "recno after set scope, must be 3:", child->RecNo()

? "Skipping forward, all must be 6 , FALSE, TRUE"
child->DbSkip(+1)
? child->RecNo() , child->Bof() , child->Eof()
child->DbSkip(+1)
? child->RecNo() , child->Bof() , child->Eof()
child->DbSkip(+1)
? child->RecNo() , child->Bof() , child->Eof()

? "Skipping backward"
child->DbSkip(-1)
? child->RecNo() , child->Bof() , child->Eof() // must be 3, false, false
child->DbSkip(-1)
? child->RecNo() , child->Bof() , child->Eof() // must be 3, TRUE, false
child->DbSkip(-1)
? child->RecNo() , child->Bof() , child->Eof() // must be 3, TRUE, false
child->DbSkip(-1)
? child->RecNo() , child->Bof() , child->Eof() // must be 3, TRUE, false

DbCloseAll()
wait
