class GraphShellWindow inherit SHELLWINDOW 
	
	
	//{{%UC%}}
	//USER CODE STARTS HERE (do NOT remove this line)

method FileExit() 
	
	self:EndWindow()
   return self	

method FileNew() 
	local oChild as GraphChildWindow
	
	oChild := GraphChildWindow{self, gaSampleData1}
	oChild:show()
	
	return oChild
	

CONSTRUCTOR(oParent,uExtra)  
	
	self:PreInit(oParent,uExtra)
	
	SUPER(oParent,uExtra)
	
	self:Caption := "Business Graphics"
	self:HyperLabel := HyperLabel{#GraphShellWindow,"Business Graphics",NULL_STRING,NULL_STRING}
	self:Menu := ShellMenu{}
	self:Origin := Point{25, 244}
	self:Size := Dimension{640, 480}
	
	self:PostInit(oParent,uExtra)
	
	return self
	

method PostInit(oParent,uExtra) 
	//Put your PostInit additions here
	GraphChildWindow{self, gaSampleData1, #DrawColumn}:Show()
	GraphChildWindow{self, gaSampleData2, #DrawBar}:Show()
	GraphChildWindow{self, gaSampleData3, #DrawPie}:Show()
	
	return nil
	
	

method WindowCascade() 
	self:Arrange(ARRANGECASCADE)
   return self	

method WindowTile() 
	self:Arrange(ARRANGETILE)
   return self	


END CLASS
