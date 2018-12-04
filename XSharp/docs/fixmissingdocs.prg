USING System.Reflection
FUNCTION Start AS VOID
	// read enum values from the code analysis dll with reflection
	// as well as the resource strings 
	// check to see if a file exists in the folder 	
	// c:\XSharp\Dev\XSharp\docs\Topics
	// with the name "XSnnnn.XML" where nnnn is the 4 digit error number from the enum
	// when the file does not exist then it will be generated 
	// following a template
	// Also a toc file is generated with the new error numbers and warning numbers
	// These can be included in the TOC and after that in the UI the errors can/should be sorted
	/*
	<?xml version="1.0" encoding="UTF-8"?>
	<?xml-stylesheet type="text/xsl" href="helpproject.xsl" ?>
	<topic template="Default" status="Under Construction" lasteditedby="robert" version="2" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="helpproject.xsd">
	<title>%ERRORCODE%</title>
	<keywords>
    <keyword>%ERRORCODE%</keyword>
    </keywords>
    <body>
      <header>
        <para styleclass="Heading1">%ERRORCODE%</para>
      </header>
      <para styleclass="Normal">%ERRORMSG%</para>
      <para styleclass="Normal"></para>
    </body>
    </topic>
	*/
	// The %ERRORCODE% is identical to the file name
	// The %ERRORMSG% must be read from 
	// c:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\XSharpResources.resx 
	// which has an XML file with the following layout
	/*
	<?xml version="1.0" encoding="utf-8"?>
	<root>
	  <data name="ERR_AssignMethodsMustHaveAParameter" xml:space="preserve">
       <value>ASSIGN methods must have at least one parameter</value>
	</data>
    </root>
    */     
	// The name is the name of the enum
	// The value represents the %ERRORMSG%
	// We can probably do all of this through reflection from the XSharp.CodeAnalysis.DLL
	// LanguageService.CodeAnalysis.XSharp.ErrorCode (internal type)
	//  
	
	LOCAL asm AS System.Reflection.Assembly
	LOCAL cFolder AS STRING
	asm := System.Reflection.Assembly.LoadFile("c:\XSharp\Dev\XSharp\Binaries\Debug\XSharp.CodeAnalysis.dll")
	LOCAL res AS System.Type
	res := asm:GetType("LanguageService.CodeAnalysis.XSharpResources")	
	LOCAL type AS System.Type
	type := asm:GetType("LanguageService.CodeAnalysis.XSharp.ErrorCode")

	cFolder := "c:\XSharp\Dev\XSharp\docs\"
	VAR sb := System.Text.StringBuilder{}    
	VAR sbErrors := System.Text.StringBuilder{}
	sb:AppendLine(e"<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
	sb:AppendLine(e"<?xml-stylesheet type=\"text/xsl\" href=\"helpproject.xsl\" ?>")
	sb:AppendLine(e"<topic template=\"Default\" status=\"Under Construction\" lasteditedby=\"robert\" version=\"2\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"helpproject.xsd\">")
	sb:AppendLine(e"<title>%ERRORCODE%</title>")
	sb:AppendLine(e"<keywords>")
    sb:AppendLine(e"<keyword>%KEYWORD%</keyword>")
    sb:AppendLine(e"</keywords>")
    sb:AppendLine(e"<body>")
    sb:AppendLine(e"  <header>")
    sb:AppendLine(e"    <para styleclass=\"Heading1\">%ERRORCODE%</para>")
    sb:AppendLine(e"  </header>")
    sb:AppendLine(e"  <para styleclass=\"Normal\">%ERRORMSG%</para>")
    sb:AppendLine(e"  <para styleclass=\"Normal\"></para>")
    sb:AppendLine(e"</body>")
    sb:AppendLine(e"</topic>")
	VAR sTemplate := sb:ToString()
	sbErrors:AppendLine(e"<topicref type=\"topic\" id=\"%ID%\" build=\"ALL\" modified=\"2017-01-09T16:17:58.854Z\" icon=\"0\" href=\"%ERRORCODE%\">")
	sbErrors:AppendLine(e"<caption translate=\"true\">%ERRORCODE%</caption>")
	sbErrors:AppendLine(e"</topicref>")
	VAR sbErrTemplate := sbErrors:ToString()	                              
	sbErrors:Clear()
	FOREACH VAR name IN System.Enum.GetValues(type)
		VAR errno := (INT) name                                                            
		VAR errname := type:GetEnumName(name)        
		IF errno > 0     
			VAR errcode := "XS"+((10000+errno).ToString():SubString(1))
			VAR file    := cFolder+"Topics\"+errcode+".XML"
			IF System.IO.File.Exists(File)
				LOOP
			ENDIF
			VAR prop := res:GetProperty(errname, BindingFlags.Static | BindingFlags.NonPublic)
			IF prop != NULL
				VAR errMsg := (STRING) prop:GetValue(NULL)
				sb:Clear()
				sb:Append(sTemplate)   
				LOCAL strType AS STRING					
				IF (errname:StartsWith("WRN"))
					strType := "Warning "
				ELSE
					strType := "Error "
				ENDIF
				sb:Replace("%KEYWORD%", errcode)
				sb:Replace("%ERRORCODE%", strType + errcode)
				sb:Replace("%ERRORMSG%", errMsg)
				System.IO.File.WriteAllText(file, sb:ToString())
			
				VAR template := sbErrTemplate:Replace("%ERRORCODE%", errcode) 
				template := template:Replace("%ID%", (errno+1000000):ToString())
				sbErrors:AppendLine(template)
			ENDIF	
		ENDIF		
	NEXT
	System.IO.File.WriteAllText(cFolder+"Maps\Errors.xml", sbErrors:ToString())		
	RETURN
	
	
	
	
	


