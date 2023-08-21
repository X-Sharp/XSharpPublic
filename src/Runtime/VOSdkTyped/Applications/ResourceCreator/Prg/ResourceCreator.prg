USING System.Collections.Generic
USING System.Resources
USING System.IO

FUNCTION Start() AS VOID
	LOCAL rm AS ResourceWriter
	LOCAL aFiles AS List<STRING>
	rm := ResourceWriter{"C:\Test\" + "VOSDK.resources"}
	aFiles := List<STRING>{}
	AddFolder("C:\xSharp\SDK\Source" , aFiles , rm)
	rm:AddResource("$filelist$" , aFiles:ToArray())
	rm:Close()
RETURN

FUNCTION AddFolder(cFolder AS STRING , aFiles AS List<STRING> , rm AS ResourceWriter) AS VOID
	FOREACH cSubFolder AS STRING IN Directory.GetDirectories(cFolder)
		AddFolder(cSubFolder , aFiles , rm)
	NEXT
	FOREACH cFile AS STRING IN Directory.GetFiles(cFolder)
		LOCAL cTag AS STRING
		cTag := cFile:Substring(cFile:IndexOf("\SDK\"))
		rm:AddResource(cTag , File.ReadAllBytes(cFile))
		aFiles:Add(cTag)
		? cTag
	NEXT
	
RETURN
