// 521. Preprocessor problem generating codeblock
/*
The codeblock in the WHILE tag below is emitted by the preprocessor as
{|| {|| 1==1} }

instead of

{|| 1==1}

Removing the STRICT clause below appears to fix the problem
*/
USING System.IO

// DATABASE COMBO BOX
#xcommand @ <row>, <col> zGET <VAR> COMBO                         ;
                        BROWSE < aBrowse_ >                       ;
                        [WHILE <WHILE> ]                          ;
                        [STRICT <lStrict> ]                       ;
      =>                                                          ;
              DoTest(<{WHILE}>) 
          
// SSUsers
#Command @<Row>,<Col> zGet <cVar> COMBO_USER [STRICT <lStrict>]           ;
                                             [WHEN <bWhen>] =>            ;
         @<Row>,<Col> zGet <cVar>                                         ;
         COMBO BROWSE   NIL            ;
         WHILE          {|| 1==1}                ;
         [STRICT        <lStrict>]

// note removing the [STRICT] clause at the end the preprocessor gives correct reults

FUNCTION Start() AS VOID
	LOCAL cChFile , cCbText AS STRING
	cCbText := "{||"
	cCbText += cCbText
	cChFile := DirectoryInfo{Environment.CurrentDirectory}:Parent:Parent:FullName + "\Applications\C521\Prg\C521.ppo"
	IF File.ReadAllText(cChFile):Contains(cCbText)
		THROW Exception{"Ppo file C521.ppo incorrectly contains a codeblock marker " + cCbText}
	END IF
	
	@2,5 zGet 123 COMBO_USER

RETURN

FUNCTION DoTest(cb) CLIPPER
	? Eval(cb)
	xAssert(Eval(cb))
	xAssert(IsLogic(Eval(cb)))
RETURN NIL

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

