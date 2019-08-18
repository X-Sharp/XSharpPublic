using system.io  
using system.Reflection   
define output :=  "strongnames.txt"
function Start as void  
	var folder := "C:\XSharp\Devrt\Binaries\Release"  
	File.WriteAllText(output,";This file is generated and then later used by inno to read the strong names of the X# Runtime assemblies"+chr(13)+chr(10)+"[StrongNames]"+chr(13)+chr(10))
	// X# runtime DLLs
	var aFiles := Directory.GetFiles(folder,"Xsharp*.dll")
	foreach var cName in aFiles    
		if cName:Indexof("test", StringComparison.OrdinalIgnoreCase) == -1
		GetStrongName(cName)                                                        
		endif
	next  
	// VO SDK DLLs
	aFiles := Directory.GetFiles(folder,"VO*.dll")
	foreach var cName in aFiles
		GetStrongName( cName) 
	next
	// VS integration DLLs
	folder := "C:\XSharp\Devpublic\Binaries\Release"
	aFiles := Directory.GetFiles(folder,"XSharp*.dll")
	foreach var cName in aFiles
		GetStrongName( cName) 
	next
	// System DLLs needed by Vs Integration
	aFiles := Directory.GetFiles(folder,"System*.dll")
	foreach var cName in aFiles
		GetStrongName( cName) 
	next
	
function GetStrongName(file as string) as void 
	var asm := Assembly.ReflectionOnlyLoadFrom(file) 
	var name := asm:GetName()
	
	file := Path.GetFileName(file)  
	var line :=file+"="+asm:FullName+", ProcessorArchitecture="+name:ProcessorArchitecture:ToString()  
	File.AppendAllText(output, line  +	chr(13)+chr(10) )
	RETURN
