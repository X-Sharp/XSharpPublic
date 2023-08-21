// 478. error XS9002: Parser: unexpected input 'atail'
// NOTE: deleting lines 203-208 (optional clauses) of C478.ch allows the sample to compile
#pragma warnings(165, off) // unassigned
#pragma warnings(168, off) // declared but not used
#pragma warnings(219, off) // assigned but not used
#pragma warnings(9043, off) // zGEDBCOmboNew double defined


#translate ARRAY(<x>) => ArrayCreate(<x>)
#xtranslate zGetSysId()               => zGetSet(1 )
#xtranslate zSetSysId(<x>)            => zGetSet(1 ,<x>)

#include "C478.ch"


FUNCTION Start() AS VOID
LOCAL GetList    := {}
LOCAL nTextRow , nTextCol AS INT
LOCAL aFiller := ArrayCreate(20) AS ARRAY
LOCAL o := TRUE AS OBJECT

// Those are OK:
  @1,5 zSay "a" zGet o PICTURE "@!" MESSAGE "m" WHEN TRUE VALID FALSE
  @1,5 zGet o CHECKBOX "check " RIGHT ON .t. OFF .f. MESSAGE "message" VALID TRUE
  @2,5 zSay "b" zGet o COMBO_FILGRP STRICT .t. EMPTY_ALLOWED .f.

  @2,5 zSay "c" zGet o COMBO_USER STRICT .t. EMPTY_ALLOWED .f.

// Error here:
  @2,5 zSay "c" zGet o COMBO_USER STRICT .t. EMPTY_ALLOWED .f. MESSAGE "message"

? ALen(GetList)

OriginalCode()

RETURN

#xtranslate :Mode          => \[1 \]
#xtranslate :Filler        => \[2 \]
#xtranslate :Descript      => \[3 \]
#xtranslate :Active        => \[4 \]
#xtranslate :OrgActive     => \[5 \]
#xtranslate :Manager       => \[6 \]
#xtranslate :FlOperator    => \[7 \]
#xtranslate :WhsHeight     => \[8 \]
#xtranslate :AutoLabels    => \[9 \]
#xtranslate :GrpCode       => \[10\]
#xtranslate :LeakerGrp     => \[11\]
#xtranslate :LabelGrp      => \[12\]
#xtranslate :LocMixGrp     => \[13\]
#xtranslate :LatePkGrp     => \[14\]
#xtranslate :PalletGrp     => \[15\]
#xtranslate :LevelGrp      => \[16\]
#xtranslate :BestLevel     => \[17\]
#xtranslate :AltBestLvl    => \[18\]
#xtranslate :MinScan       => \[19\]
#xtranslate :MsGrp         => \[20\]
#xtranslate :ScanCheck     => \[21\]

FUNCTION OriginalCode() AS VOID

LOCAL GetList    := {}
LOCAL nTextRow , nTextCol AS INT
LOCAL aFiller := ArrayCreate(20) AS ARRAY

  @nTextRow-12,nTextCol-24 zSay "Filler             " zGet aFiller:Filler     PICTURE "@!" MESSAGE "Enter the filler" WHEN aFiller:Mode == "ADD" VALID Fillers->(zdbCheckKey(aFiller:Filler,"FillerFL"))
  @nTextRow-12,nTextCol+14 zGet aFiller:Active CHECKBOX "Active " RIGHT ON .t. OFF .f. MESSAGE "Is this an active filler?" VALID ValidActive(aFiller)
  @nTextRow-10,nTextCol-24 zSay "Description        " zGet aFiller:Descript   MESSAGE "Enter the description" VALID zNotEmpty(aFiller:Descript)
  @nTextRow-8 ,nTextCol-24 zSay "Manager            " zGet aFiller:Manager    COMBO_USER STRICT .t. EMPTY_ALLOWED .f.
  @nTextRow-6 ,nTextCol-24 zSay "Fork Lift Operator " zGet aFiller:FlOperator MESSAGE "Enter the fork lift operator"  VALID zNotEmpty(aFiller:FlOperator)
  @nTextRow-4 ,nTextCol-24 zGet aFiller:WhsHeight  CHECKBOX "Warehouse Height   " RIGHT ON .t. OFF .f. MESSAGE "Use the warehouse stack height when packing (false = use truck stack height)?"
  @nTextRow-2 ,nTextCol-24 zGet aFiller:AutoLabels CHECKBOX "Auto Labeler       " RIGHT ON .t. OFF .f. MESSAGE "Does this filler have access to an auto-labeler?"
  @nTextRow   ,nTextCol-24 zSay "Filler Group       " zGet aFiller:GrpCode COMBO_FILGRP STRICT .t. EMPTY_ALLOWED .f.

  @nTextRow+2 ,nTextCol-24 zSay "Leaker Email       " zGet aFiller:LeakerGrp  COMBO_USER STRICT .t. EMPTY_ALLOWED .t. MESSAGE "Select the email group to send LEAKER notification emails"
  @nTextRow+3 ,nTextCol-24 zSay "Label Email        " zGet aFiller:LabelGrp   COMBO_USER STRICT .t. EMPTY_ALLOWED .t. MESSAGE "Select the email group to send INCORRECT PALLET LABEL LOCATION emails"
  @nTextRow+4 ,nTextCol-24 zSay "Location Mix Email " zGet aFiller:LocMixGrp  COMBO_USER STRICT .t. EMPTY_ALLOWED .t. MESSAGE {"Select the email group to send MIXED F/G LOCATION emails","Pallet dropped in the same row/level with a different f/g and/or lot"}
  @nTextRow+5 ,nTextCol-24 zSay "Late Pack Email    " zGet aFiller:LatePkGrp  COMBO_USER STRICT .t. EMPTY_ALLOWED .t. MESSAGE "Select the email group to send LATE PACK notification emails"
  @nTextRow+6 ,nTextCol-24 zSay "Pallet Email       " zGet aFiller:PalletGrp  COMBO_USER STRICT .t. EMPTY_ALLOWED .t. MESSAGE "Select the email group to send INVALID PRODUCTION PALLET SELECTION notification emails"

  @nTextRow+8 ,nTextCol-24 zSay "Wrong Level Email  " zGet aFiller:LevelGrp   COMBO_USER STRICT .t. EMPTY_ALLOWED .t. MESSAGE "Select the email group to send WRONG LEVEL notification emails"
  @nTextRow+9 ,nTextCol-24 zSay "Preferred Level    " zGet aFiller:BestLevel  PICTURE "9" MESSAGE "Enter the preferred level to drop pallets produced on the current filler"           WHEN ! Empty(aFiller:LevelGrp) VALID zIsPositive(aFiller:BestLevel)
  @nTextRow+10,nTextCol-24 zSay "Alternate Level    " zGet aFiller:AltBestLvl PICTURE "9" MESSAGE "Enter the ALTERNATE preferred level to drop pallets produced on the current filler" WHEN ! Empty(aFiller:LevelGrp) VALID zIsPositive(aFiller:AltBestLvl)
  @nTextRow+12,nTextCol-24 zGet aFiller:ScanCheck  CHECKBOX "Scan Check         " RIGHT ON .t. OFF .f. MESSAGE "Perform the minimum production scan time check in p/d?"
  @nTextRow+14,nTextCol-24 zSay "Minimum Scan Time  " zGet aFiller:MinScan PICTURE "999" MESSAGE {"Enter the minimum number of seconds required to record a production scan","Any scan time prior to the entered time, results in an email"} WHEN aFiller:ScanCheck VALID zNotNegative(aFiller:MinScan)
  @nTextRow+14,nTextCol    zSay "seconds"
  @nTextRow+15,nTextCol-24 zSay "Minimum Scan Email " zGet aFiller:MsGrp      COMBO_USER STRICT .t. EMPTY_ALLOWED .t. MESSAGE "Select the email group to send MINIMUM SCAN TIME NOT MET emails" WHEN aFiller:ScanCheck
? GetList

RETURN


FUNCTION zGEDBComboNew( cVar, cTheVar, bVar, bWhen, bValid, lDropOnEnter,;
                       cAlias, cTag, xTop, xBottom, bFor, bWhile,;
                       cBaseFilter, xRetFld, aBrowse_,;
                       xDispFld, cKeyTag, lStrict, aHotKeys_, cpicture,;
                       nWidth,nHeight, cColor, bPostEval, lEmptyAllowed)
/*
? cVar, cTheVar, bVar, bWhen, bValid, lDropOnEnter,;
                       cAlias, cTag, xTop, xBottom, bFor, bWhile,;
                       cBaseFilter, xRetFld, aBrowse_,;
                       xDispFld, cKeyTag, lStrict, aHotKeys_, cpicture,;
                       nWidth,nHeight, cColor, bPostEval, lEmptyAllowed
*/
? "zGEDBComboNew() params cAlias, cTag:" , cAlias, cTag
IF cAlias == NIL .or. .not. (cAlias == "SSUsers" .or. cAlias == "FilGrp")
	THROW Exception{"cAlias parameter not preprocessed correctly"}
END IF

	LOCAL oGet AS _GET_Object
	oGet := _GET_Object{}
	oGet:cargo := ArrayCreate(10)
RETURN oGet

