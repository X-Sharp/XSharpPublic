USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

FUNCTION Start AS VOID
	LOCAL aFiles AS STRING[]
	aFiles := System.IO.Directory.GetFiles("c:\XSharp\DevRt\Runtime\XSharp.Core\Collations","*.Strings")
	FOREACH sFile AS STRING IN aFiles
		? sFile
		Convert(sFile)
	NEXT
	console.ReadLine()
	RETURN
	
	
	
FUNCTION Convert(sFile AS STRING) AS VOID
	LOCAL aLines AS STRING[]
	LOCAL sb AS StringBuilder
	aLines := SYstem.IO.File.ReadAllLines(sFile)
	sb := StringBuilder{aLines:Length*80}
	sb:AppendLine("<root>")
	sb:AppendLine(e"<resheader name=\"resmimetype\">")
	sb:AppendLine("<value>text/microsoft-resx</value>")
	sb:AppendLine("</resheader>")
	sb:AppendLine(e"<resheader name=\"version\">")
	sb:AppendLine("<value>2.0</value>")
	sb:AppendLine("</resheader>")
	sb:AppendLine(e"<resheader name=\"reader\">")
	sb:AppendLine("<value>System.Resources.ResXResourceReader, System.Windows.Forms, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089</value>")
	sb:AppendLine("</resheader>")
	sb:AppendLine(e"<resheader name=\"writer\">")
	sb:AppendLine("<value>System.Resources.ResXResourceWriter, System.Windows.Forms, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089</value>")
	sb:AppendLine("</resheader>")
	
	FOREACH cLine AS STRING IN aLines
		VAR aElements := cLine:Split(<CHAR>{'='},StringSplitOptions.RemoveEmptyEntries)
		IF aElements:Length == 2
			aElements[1] := aElements[1]:Replace(">", "&gt;")
			aElements[1] := aElements[1]:Replace("<", "&lt;")
			aElements[2] := aElements[2]:Replace(">", "&gt;")
			aElements[2] := aElements[2]:Replace("<", "&lt;")
			sb:AppendLine(e"<data name=\""+aElements[1]:Trim()+e"\" xml:space=\"preserve\">")
			sb:AppendLine("  <value>"+aElements[2]:Trim()+"</value>")
			sb:AppendLine("</data>")
		ENDIF
	NEXT	
	sb:AppendLine("</root>")
	sFile := System.IO.Path.ChangeExtension(sFile, ".resx")
	System.IO.File.WriteAllText(sFile, sb:ToString())
