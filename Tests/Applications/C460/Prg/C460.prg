// 460. Compiler crash with #xcommand

// Please ignore for now the compiler errors, will update the source 
// so that it compiles after the compiler does not crash on it.

#include "C460.CH"
FUNCTION Start( ) AS VOID

// this one is ok:
@nTextRow-12,nTextCol-24 zSay "Filler             " zGet aFiller:Filler     PICTURE "@!" MESSAGE "Enter the filler" WHEN aFiller:Mode == "ADD" VALID Fillers->(zdbCheckKey(aFiller:Filler,"FillerFL"))

// compiler crash with this one:
@nTextRow-12,nTextCol+14 zGet aFiller:Active CHECKBOX "Active " RIGHT ON .t. OFF .f. MESSAGE "Is this an active filler?" VALID ValidActive(aFiller)
RETURN
