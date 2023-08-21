// 547. error XS9002: Parser: unexpected input '@'
#pragma warnings(165, off) // unassigned

#include "C547.CH"
FUNCTION Start() AS VOID
LOCAL lVar AS LOGIC
LOCAL GetList    := {}

// those are ok:
@1,2 zSay "abc" zGet lVar STEP MESSAGE "message" VALID lVar
@1,2 zSay "abc" zGet lVar WHEN lVar STEP MESSAGE "message" VALID lVar
@1,2 zSay "abc" zGet lVar WHEN lVar MESSAGE "message" VALID lVar COLOR 123

// parser error on those:
@1,2 zSay "abc" zGet lVar WHEN lVar STEP MESSAGE "message" VALID lVar COLOR 123
@1,2 zSay "abc" zGet lVar WHEN lVar STEP MESSAGE "message" COLOR 123 VALID lVar
// harbour ppo:
/*
DevPos(1,2 ) ; DevOut("abc" );SetPos(Row(),Col()+1 ) ; AAdd(GetList,_GET_(lVar,"lVar",,{||lVar},{||lVar} ):display() ) ;atail(Getlist):reader := {|x| zGEReader(x) } ;atail(Getlist):cargo := ArrayCreate(4) ; ATail(GetList):reader := {|oGet| zGeStepReader(oGet) } ; ATail(GetList):colorDisp(123);atail(getlist):cargo[2] := "message"
DevPos(1,2 ) ; DevOut("abc" );SetPos(Row(),Col()+1 ) ; AAdd(GetList,_GET_(lVar,"lVar",,{||lVar},{||lVar} ):display() ) ;atail(Getlist):reader := {|x| zGEReader(x) } ;atail(Getlist):cargo := ArrayCreate(4) ; ATail(GetList):reader := {|oGet| zGeStepReader(oGet) } ; ATail(GetList):colorDisp(123);atail(getlist):cargo[2] := "message"
*/

RETURN

FUNCTION zGeStepReader() CLIPPER
RETURN NIL

