
FUNCTION Start( ) AS VOID
	local cText as string
	local cFile := "test.txt"
	SetTextFile(cFile)                    
	// flags 0 = do not write an CRLF after the text to the file
	// PReText 15 = delete CRLF, WS and Tabs
	// NoShow means no output to console
	TEXT to cText TEXTMERGE NOSHOW FLAGS 0 PRETEXT 15
	aaa
	bbb << Time() >> 
	ccc
	ENDTEXT
	SetTextMerge(TRUE)
	\ddd 
	\\Today is: << Today() >>
	\\ fff             
	SetTextMerge(FALSE)
	SetTextFile("")
	local cMemo := MemoRead(cFile)
	? "File contents"
	? SLen(cMemo), cMemo
	? "cText contents"
	? SLen(cText), cText
RETURN
