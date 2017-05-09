// 511. error XS1061: 'LineObject' does not contain a definition for 'LineText' 
// please also note the first 3 error messagee do not contain file info (they have only line/col info)

FUNCTION Start() AS VOID
	LOCAL o AS Xide.LineObject
	o := Xide.LineObject{"asd"}
	? o:LineText
	? Xide.LineObject.NewID
	? Xide.LineObject.NewID
	? Xide.LineObject.NewID
RETURN

BEGIN NAMESPACE Xide

PARTIAL CLASS LineObject
	PROTECT cLineText AS STRING
	STATIC PROTECT snID AS INT
		
	ACCESS LineText AS STRING
	RETURN SELF:cLineText

	CONSTRUCTOR(cLine AS STRING)
		SELF:cLineText := cLine
		? SELF:LineText
	RETURN
	
	STATIC ACCESS NewID AS INT
		snID ++
	RETURN snID

END CLASS

END NAMESPACE

