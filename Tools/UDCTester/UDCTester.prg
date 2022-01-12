USING System
USING System.Collections.Generic
USING System.ComponentModel

USING System.Text
USING System.Threading.Tasks
USING System.Windows.Forms
USING System.IO
USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.SyntaxTree
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser
USING XSharp.Parser
USING LanguageService.CodeAnalysis.Text

BEGIN NAMESPACE UDCTesterApp

   EXPORT PARTIAL CLASS UDCTester	;
		INHERIT System.Windows.Forms.Form	;
		IMPLEMENTS VSParser.IErrorListener
        HIDDEN _errors AS List<STRING>
      PUBLIC CONSTRUCTOR()   STRICT//UDCTester
         SELF:InitializeComponent()
         SELF:ReadSettings()
      RETURN

      PRIVATE METHOD TestButton_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
         LOCAL cFolder AS STRING
         LOCAL cFile   AS STRING
         cFolder := oSettings:OutputPath
         IF !Directory.Exists(cFolder)
            Directory.CreateDirectory(cFolder)
         ENDIF
         _errors := List<STRING>{}
         cFile := Path.Combine(cFolder,"test.xh")
         File.WriteAllText(cFile, SELF:tbUDC:Text)
         VAR source := e"#include \"" + cFile +e"\"\r\n"
         source += SELF:tbSource:Text+e"\r\n"
         File.WriteAllText(Path.Combine(cFolder,"test.prg"), source)
         VAR opt := List<STRING>{}
         IF SELF:chkStandardDefs:Checked
            opt:Add("nostddefs")
         ENDIF
         opt:Add("dialect:"+comboDialect:Text)
         opt:Add("r:XSharp.RT.DLL")
         opt:Add("r:XSharp.Core.DLL")
         File.WriteAllText(Path.Combine(cFolder,"test.opt"), String.Join(e"\r\n", opt))
         VAR options  := XSharpParseOptions.FromVsValues(opt)

         IF oSettings:WriteLexTokens
            opt:Add("lexonly")
            VAR options2  := XSharpParseOptions.FromVsValues(opt)
            XSharp.Parser.VsParser.Lex(source, "test.prg", options2, SELF, OUT VAR lextokenStream, OUT VAR _)
            _errors:Clear()
            SELF:WriteTokens((BufferedTokenStream) lextokenStream, Path.Combine(cFolder,"test.lextokens.csv"))
         ENDIF

         XSharp.Parser.VsParser.PreProcess(source, "test.prg", options, SELF, OUT VAR tokenStream, OUT VAR _)
         IF oSettings:WritePPTokens
            SELF:WriteTokens((BufferedTokenStream) tokenStream, Path.Combine(cFolder,"test.pptokens.csv"))
         ENDIF

         IF SELF:_errors:Count > 0
            VAR cMessage := ""
            FOREACH VAR msg IN _Errors
               cMessage += msg + e"\r\n"
            NEXT
            SELF:tbResult:Text := "Preprocessor errors detected: "+e"\r\n"+cMessage
         ELSE
         VAR stream := (BufferedTokenStream) tokenStream
         VAR tokens := stream:GetTokens()
         VAR result := StringBuilder{}
         VAR afterEOS := FALSE
         FOREACH token AS XSharpToken IN tokens
            IF (token:HasTrivia)
               FOREACH VAR child IN token:Trivia
                  SWITCH child:Type
                  CASE XSharpLexer.LINE_CONT
                  CASE XSharpLexer.SL_COMMENT
                  CASE XSharpLexer.ML_COMMENT
                  CASE XSharpLexer.DOC_COMMENT
                  CASE XSharpLexer.EOS
                     NOP
                  CASE XSharpLexer.WS
                     IF ! afterEOS
                        result:Append(child:Text:Replace(e"\r\n",""))
                     ENDIF
                  OTHERWISE
                     result:Append(child:Text)

                  END SWITCH
               NEXT
            ENDIF
            SWITCH token:Type
               CASE XSharpLexer.ML_COMMENT
               CASE XSharpLexer.SL_COMMENT
               CASE XSharpLexer.DOC_COMMENT
                   NOP
               OTHERWISE
                  result:Append(token:Text)

            END SWITCH
            afterEOS := token:Type == XSharpLexer.EOS
         NEXT
         SELF:tbResult:Text := result:ToString():TrimStart()
         IF oSettings:WritePPo
            File.WriteAllText(Path.Combine(cFolder,"test.ppo"), SELF:tbResult:Text)
         ENDIF
         ENDIF
      RETURN
      METHOD WriteTokens(stream AS BufferedTokenStream, cFile AS STRING) AS VOID
        LOCAL sb AS StringBuilder
        sb := StringBuilder{}
        VAR tokens := stream:GetTokens()
        sb:AppendLine("Nr,Channel, Position, Line, Column, Type, Text")
        FOREACH token AS XSharpToken IN tokens
            WriteToken(sb, token)
        NEXT
        File.WriteAllText(cFile,sb:ToString())
        RETURN
     METHOD WriteToken(sb AS StringBuilder, token AS XSharpToken) AS VOID
        token := token:Original
        IF XSharpLexer.IsComment(token:Type) .and. oSettings:HideComments
            RETURN
        ENDIF
        IF token:Type == XSharpLexer.WS .and. oSettings:HideWhitespace
            RETURN
        ENDIF
        sb:Append(token:TokenIndex:ToString()+",")
        sb:Append(token:Channel:ToString()+",")
        sb:Append(token:Position:ToString()+",")
        sb:Append(token:Line:ToString()+",")
        sb:Append(token:Column:ToString()+",")
        sb:Append(XSharpLexer.DefaultVocabulary:GetDisplayName(token:Type):ToString()+",")
        IF token:Type == XSharpLexer.WS
            sb:Append(token:Text:Replace(e" ", "<SPACE>"):Replace(e"\t","<TAB>"))
        ELSE
            sb:Append(token:Text:Replace(e"\r", ""):Replace(e"\n","<CRLF>"))
        ENDIF

        sb:AppendLine()
PRIVATE METHOD OkButton_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
   SELF:Close()
        RETURN
    METHOD ReportError(fileName AS STRING, span AS LinePositionSpan, errorCode AS STRING, message AS STRING, args AS OBJECT[]) AS VOID
     VAR msg := errorCode
     FOREACH VAR arg IN args
         msg += " "+arg:ToString()
     NEXT
     SELF:_errors:Add(msg)

  METHOD ReportWarning(fileName AS STRING, span AS LinePositionSpan, errorCode AS STRING, message AS STRING, args AS OBJECT[]) AS VOID
     VAR msg := errorCode
     FOREACH VAR arg IN args
         msg += " "+arg:ToString()
     NEXT
     SELF:_errors:Add(msg)
    PRIVATE METHOD btnSettings_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
        LOCAL ofrmSettings AS Settings
        ofrmSettings := Settings{}
        ofrmSettings:ShowDialog()
        IF oFrmSettings:DialogResult == DialogResult.OK
            SELF:ReadSettings()
        ENDIF
        RETURN
    PRIVATE METHOD ReadSettings() AS VOID
        SELF:comboDialect:Text       := oSettings:DefaultDialect
        SELF:chkStandardDefs:Checked := oSettings:DefaultNoStdDefs
   END CLASS
END NAMESPACE
