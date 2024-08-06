FUNCTION Start as VOID
    local cDest as String
    local cDbf := "TEST" as STRING
    ? DbCreate(cDbf,{{"FLD1","C",30,0},{"FLD2","N",10,0},{"MEMO","M",10,0}})
	? DbUseArea(TRUE,,cDbf,"source")
    cDest := "test"
    COPY STRUCTURE TO (cDest) FIELDS FLD1,FLD2,MEMO

