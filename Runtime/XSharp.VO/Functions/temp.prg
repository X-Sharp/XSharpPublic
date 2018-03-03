15) :function FieldGetSelect(uSelect as __Usual,symField as __Symbol) as __Usual
26) :function FieldPutSelect(uSelect as __Usual,symField as __Symbol,u as __Usual) as __Usual
37) :function Used() as logic
52) :function ALIAS          (nSelect)
66) :function BOF            ()
76) :function DBAPPEND       (lReleaseLocks)
101):function DBBUFFREFRESH  ()
119):function DBCLEARFILTER  ()
137):function DBCLEARINDEX   (uOrder, cOrdBag)
148):function DBCLEARRELATION()
156):function DBCLOSEALL     ()
164):function DBCLOSEAREA    ()
173):function DBCOMMIT       ()
182):function DBCOMMITALL    ()
219):function DBCREATE (   cName,      ;
291):function DBCREATEINDEX(cName, cExpr, cobExpr, lUnique)
301):function DBCREATEORDER  (uOrder, cName, cExpr, cobExpr, lUnique)
310):function DBDELETE ()
327):function DbDeleteOrder(uOrder, cOrdBag)
347):function DBDRIVER       ()
355):function DbEval(uBlock, uCobFor, uCobWhile, nNext, nRecno, lRest)
376):function DbFieldInfo(nOrdinal, nPos, xNewVal)
390):function DbFIlter()
400):function DbGetSelect()
411):function DbGoBottom()
429):function DbGoto(uRecId)
446):function DbGotop()
464):function DbInfo(nOrdinal, xNewVal)
479):function DbContinue()
497):function DbLocate(uCobFor, uCobWhile, nNext, uRecId, lRest )
531):function DbOrderInfo(nOrdinal,cBagName, uOrder, xNewVal)
568):function DbPack()
587):function DbRecordInfo(nOrdinal, uRecId, xNewVal)
596):function DbRecall()
616):function DbRLock(uRecord)
625):function DbRLockList()
650):function DbRSelect(nPos)
663):function DbRUnLock(uRecId)
682):function DbSelect(nNew)
698):function DbSelectArea(xValue)
722):function DbSetSelect(nSelect)
735):function DbSymSelect(sAlias)
749):function DBRELATION     (wPos)
770):function DbSetDriver(cDriver)
780):function DbSetFilter(cbFilter, cFilter)
803):function DbSetRelation  (xAlias, uCobKey, cKey)
842):function DBSKIP         (nRecords)
863):function DbUnLock()
881):function DbUnlockAll()
891):function DBUSEAREA (lNew,      ;   // Select the next free Area
962):function DbZap()
979):function DELETED        ()
987):function EOF            ()
994):function FIELDPUT (wPos, xValue)
1010):function FieldGet(wPos)
1026):function FLOCK          ()
1034):function Found()
1042):function Header()
1053):function LastRec()
1063):function LUpdate()
1074):function RDDCount(nType)
1085):function RDDInfo        (nOrdinal, xNewVal)
1098):function RDDList        (nType)
1135):function RDDName        ()
1152):function RddSetDefault  (cDriver)
1168):function RecSize() AS LONG
1179):function RLock()
1188):function RecCount()
1198):function RecNo()
1206):function _Select(xValue)
1252):function Select(xValue)
1267):function DBMemoExt      (cDriver)
1278):function RDDVersion     (nParm)
1293):function DBMemoField        (xField)
1326):function DoError        (nSymFunc, nTries)
1357):function __AllocRddList (aRdds as array)    as _RDDLIST     pascal
1371):function __RddList      (xDriver, aHidden)
1446):function _DbCreate(cFile1, cFile2, cDriver,lNew, cAlias)      as logic clipper
1540):function AFields(aNames, aTypes, aLens, aDecs)  as dword clipper
1607):function DbCopyStruct(cFile as string, aFields as array) as logic pascal
1613):function DbCOpyXStruct(cFile) as logic pascal
1682):function DbStruct() as array pascal
1735):static function ParamError  (dwArgNum  as dword  ,    ;
1755):static function DBCMDError  ()                              as usual pascal
1782):function IndexHPLock(lSet as __Usual) as logic
1792):function NewIndexLock(lSet as __Usual) as logic
1803):function NewLocks() as logic
