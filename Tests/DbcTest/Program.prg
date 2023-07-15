FUNCTION Start() AS VOID
LOCAL cDbf AS STRING
LOCAL n AS INT
cDbf := "C:\test\index"
FErase(cDbf + ".cdx")
DbCreate(cDbf, {{"FLD","C",10,0},{"FLD2","C",10,0},{"FLD3","C",10,0}})
DbUseArea(TRUE,"DBFCDX",cDbf,"myalias")
FOR n := 1 UPTO 20
	DbAppend()
	FieldPut(1,AsString(20-n))
NEXT
//INDEX ON FLD TAG MYORDER  OF (cDbf) FOR !Empty(FLD) COMPACT
INDEX ON FLD TO "test"
COPY TO TEST2 FIELDS FLD, FLD2
DbCloseArea()
wait
