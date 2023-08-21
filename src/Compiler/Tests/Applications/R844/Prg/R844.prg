FUNCTION Start( ) AS VOID
local cText := "" as  String
? "TEXT Xbase++, VO and Vulcan dialect"
TEXT INTO cText WRAP ";" TRIMMED
    1
    2
    3
    4
ENDTEXT
? cText


TEXT
    Line 1
Line 2
    Line 3
endtext
TEXT TO FILE cTest.txt
    aa
    bb
    cc
endtext

TEXT TO PRINTER
    aa
    bb
    cc  
ENDTEXT

RETURN



FUNCTION _TextSave(cFile as STRING) AS VOID
    ? "_TextSave", cFile
    ?


FUNCTION _TextRestore() AS VOID
    ? "_TextRestore"
    RETURN
