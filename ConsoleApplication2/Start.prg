//
// Start.prg
//
function Start as void
	LOCAL cA, cB AS STRING
	LOCAL nI, nMax AS Int64
	LOCAL lOk AS LOGIC
	nMax := 10000000// 40000000
	cA := "THE QUICK BROWN FOX JUMP OVER THE LAZY DOG"
	cB := Left(cA, Slen(cA)-1)+Lower(right(cA,1))
	LOCAL aColl AS STRING[]
	aColl := <STRING>{"Windows", "Clipper"}
	FOREACH IMPLIED  s IN aColl
		SetCollation(s)
		LOCAL nSecs AS FLOAT
		nSecs := Seconds()
		FOR nI := 1 TO nMax 
			lOk := cA <= cB
		NEXT
		? SetCollation(), transform(nMax,"999,999,999"), seconds() - nSecs
	NEXT
	WAIT
	return 