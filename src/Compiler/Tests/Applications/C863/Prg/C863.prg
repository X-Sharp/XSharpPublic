// https://github.com/X-Sharp/XSharpPublic/issues/1074
// 863. Runtime execption when calling AScan() with 4 arguments

/*
XSharp.Error
Value does not fall within the expected range.

Callstack :
Boolean XSharp.__Usual.UsualEquals(XSharp.__Usual rhs, System.String op)()
Boolean XSharp.__Usual.Equals(System.Object obj)()
static Boolean System.Object.Equals(System.Object objA, System.Object objB)()
static UInt32 XSharp.RT.ArrayHelpers.AScan(XSharp.__ArrayBase`1[System.Object] aTarget, System.Object element, System.Int32 nStart, System.Int32 nCount)()
static UInt32 XSharp.RT.Functions.AScan(XSharp.__ArrayBase`1[System.Object] aTarget, System.Object uSearch, System.Int32 nStart, System.Int32 nCount)()
static System.Void C863.Exe.Functions.Start()()   :  C:\xSharp\Dev\Tests\Applications\C863\Prg\C863.prg  :  7
*/

FUNCTION Start() AS VOID
	LOCAL a AS ARRAY
	a := {1,2,3,4,5}
    ? AScan(a, { |x| x==3 } , 3 ) // 3 OK
	? AScan(a, { |x| x==3 } , 3 , 1 ) // exception
	? AScan(a, { |x| x==3 } , 1 , 3 ) // exception
	? AScan(a, { |x| x==3 } , 1 , 1 ) // exception
	? brwerror()                        // exception
RETURN

CLASS bRecordGroupItem
	PROTECT oParent				AS OBJECT
	PROTECT oLevel					AS OBJECT
	PROTECT iLevelNo				AS INT
	PROTECT uValue					AS USUAL			// Gruppen-Wert
	PROTECT uRecNo					AS USUAL			// Gruppen-Datensatznummer
	PROTECT iOptions				AS DWORD			// BRGIO_...
	PROTECT oItemList				AS OBJECT

CONSTRUCTOR(oParent, oLevel, iLevelNo, uValue, uRecNo, iOptions) CLIPPER
	SELF:oParent := oParent
	SELF:oLevel := oLevel
	SELF:iLevelNo := iLevelNo
	SELF:uValue := uValue
	SELF:uRecNo := uRecNo
	IF IsNumeric(iOptions)
		SELF:iOptions := iOptions
	ELSE
		SELF:iOptions := 0x00000001U// BRGIO_OPEN
	ENDIF

	// Beim Garbage Collector registrieren


	RETURN

END CLASS

FUNCTION brwerror () AS DWORD
LOCAL auRecord AS ARRAY
LOCAL itemp2 AS DWORD
LOCAL iCacheRowPos AS DWORD
LOCAL orgiTemp AS bRecordGroupItem
orgiTemp := bRecordGroupItem{NIL, NIL, 1, NIL, 1, 1}
auRecord := {;
{1, {}, 17, NIL, 1, {}, NIL, NIL, NIL, NIL, 1}, ;
{1, {}, 17, NIL, 2, {}, NIL, NIL, NIL, NIL, 1}, ;
{1, {}, 17, NIL, 3, {}, NIL, NIL, NIL, NIL, 1};
}
iCacheRowPos := 1
iTemp2 := AScan(auRecord, {|x| x[7/*BRI_RECORDGROUPITEM*/] == orgiTemp}, 1, INT(iCacheRowPos) - 1)
RETURN iTemp2

