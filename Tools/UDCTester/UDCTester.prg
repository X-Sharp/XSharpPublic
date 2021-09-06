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

   PUBLIC PARTIAL CLASS UDCTester ;
        INHERIT System.Windows.Forms.Form ;
        IMPLEMENTS VSParser.IErrorListener
        PRIVATE _errors AS List<String>
      
      PUBLIC CONSTRUCTOR()   STRICT//UDCTester
         InitializeComponent()
            combodialect:Text:= "Core"
      RETURN
      PRIVATE METHOD TestButton_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
         local cFolder as STRING
         local cFile   as string
         cFolder := System.Environment.GetEnvironmentVariable("TEMP")
         cFolder := Path.Combine(cFolder, "UDCTester")
         IF !Directory.Exists(cFolder)
            Directory.CreateDirectory(cFolder)
         ENDIF
         _errors := List<String>{}
         cFile := Path.Combine(cFolder,"test.xh")
         File.WriteAllText(cFile, SELF:tbUDC:Text)
         VAR source := e"#include \"" + cFile +e"\"\r\n"
         source += SELF:tbSource:Text+e"\r\n"
         File.WriteAllText(Path.Combine(cFolder,"test.prg"), source)
         var opt := List<STRING>{}
         if SELF:chkStandardDefs:Checked
            opt:Add("nostddefs")
         endif
         opt:Add("dialect:"+comboDialect:Text)
         opt:Add("r:XSharp.RT.DLL")
         opt:Add("r:XSharp.Core.DLL")
            File.WriteAllText(Path.Combine(cFolder,"test.opt"), String.Join(e"\r\n", opt))
         VAR options  := XSharpParseOptions.FromVsValues(opt)
         XSharp.Parser.VsParser.PreProcess(source, "test.prg", options, SELF, OUT VAR tokenStream)   
         if SELF:_errors:Count > 0
            var cMessage := ""
            FOREACH var msg in _Errors
               cMessage += msg + e"\r\n"
            NEXT
            SELF:tbResult:Text := "Preprocessor errors detected: "+e"\r\n"+cMessage
         ELSE
         VAR stream := (BufferedTokenStream) tokenStream 
         VAR tokens := stream:GetTokens()
         VAR result := StringBuilder{}
         var afterEOS := FALSE
         FOREACH token AS XSharpToken IN tokens
            IF (token:HasTrivia)
               FOREACH var child in token:Trivia
                  SWITCH child:Type
                  CASE XSharpLexer.LINE_CONT 
                  CASE XSharpLexer.SL_COMMENT
                  CASE XSharpLexer.ML_COMMENT
                  CASE XSharpLexer.DOC_COMMENT
                  CASE XSharpLexer.EOS
                     NOP      
                  CASE XSharpLexer.WS
                     if ! afterEOS      
                        result:Append(child:Text:Replace(e"\r\n",""))
                     endif
                  OTHERWISE       
                     result:Append(child:Text)
                     
                  END SWITCH
               NEXT
            ENDIF
            switch token:Type
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
            File.WriteAllText(Path.Combine(cFolder,"test.ppo"), SELF:tbResult:Text)
         ENDIF
      RETURN
PRIVATE METHOD OkButton_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
   SELF:Close()
        RETURN
    METHOD ReportError(fileName AS STRING, span AS LinePositionSpan, errorCode AS STRING, message AS STRING, args AS OBJECT[]) AS VOID
     var msg := errorCode
     foreach var arg in args
         msg += " "+arg:ToString()
     next
     SELF:_errors:Add(msg)

  METHOD ReportWarning(fileName AS STRING, span AS LinePositionSpan, errorCode AS STRING, message AS STRING, args AS OBJECT[]) AS VOID
     var msg := errorCode
     foreach var arg in args
         msg += " "+arg:ToString()
     next
     SELF:_errors:Add(msg)
   END CLASS 
END NAMESPACE
