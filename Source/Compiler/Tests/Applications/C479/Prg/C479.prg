// 479. Ppo file not (fully) generated
// this happens with several .ppo files

#using System.IO

#command @ <row>, <col> zSAY <xpr>;
=> Test("one argument")

#command @ <row>, <col> zGET <getvar>                                      ;
                        [PICTURE <pic>]                                 ;
                        [VALID <valid>]                                 ;
                        [WHEN <when>]                                   ;
                        [OTHER <other>]                                 ;
                                                                        ;
=> Test( <getvar>, <"getvar">, <pic>, <{valid}>, <{when}> )

// GET & SAY
#command @ <row>, <col> zSAY <sayxpr>                                    ;
                        [<sayClauses,...>]                              ;
                        zGET <getvar>                                       ;
                        [<getClauses,...>]                              ;
                                                                        ;
      => @ <row>, <col> zSAY <sayxpr> [<sayClauses>]                     ;
       ; @ <row>, <col>+1 zGET <getvar> [<getClauses>]

FUNCTION Start() AS VOID
LOCAL cChFile AS STRING
cChFile := DirectoryInfo{Environment.CurrentDirectory}:Parent:Parent:FullName + "\Applications\C479\Prg\C479.ppo"
IF .not. File.ReadAllText(cChFile):Contains(AsString( Test("one argument")) )
	// string not found in the .ppo file
	THROW Exception{"Ppo file C479.ppo was not fully generated"}
END IF

@1,2 zSAY "Filler" zGET 1 WHEN TRUE
@1,2 zSAY "Filler" zGET 1 WHEN FALSE
// output in .ppo finishes here
LOCAL nNotSeenInPpo AS INT
nNotSeenInPpo := 0
? nNotSeenInPpo
RETURN

FUNCTION Test(a,b,c,d,e,f) CLIPPER
	? a,b,c
	? "Param b:" + AsString(b) + ":"
	IF .not. b == "1" .and. .not. (b == NIL .and. a == "one argument")
		THROW Exception{"Param b contains a space"}
	END IF
	? "This text does not appear in the ppo output"
RETURN "nNotSeenInPpo"

