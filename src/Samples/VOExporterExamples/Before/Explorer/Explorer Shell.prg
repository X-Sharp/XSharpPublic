class ExplorerShellWindow inherit ShellWindow
	export aChildWindows as array
	

method Close(oCloseEvent) 

	self:Owner:Quit()
	super:Close(oCloseEvent)
	
	return nil
	

method FileExit() 

	return self:EndWindow()
	

method FileOpen() 
	
	return self:OpenExplorer()


method HelpAboutDialog() 

	return HelpAboutDialog{self}:Show()
	

CONSTRUCTOR(oApp) 
	local oStatusBar as StatusBar
	
	SUPER(oApp)
	
	aChildWindows := {}

	// create the status bar
	oStatusBar := self:EnableStatusBar()
	oStatusBar:DisplayKeyboard()
	oStatusBar:DisplayTime()

	// initialize window components
	self:Menu := EmptyShellMenu{self}
	self:Caption := "Explorer Application Sample"
	self:Icon := IconOne{}
	self:IconSm := IconOne{}
	
	return self


method OpenExplorer() 
	local oNewExplorer as CustomerExplorer
	
	// create the new explorer window with labels and show it
	oNewExplorer := CustomerExplorer{self, true}
	oNewExplorer:Show()

	// add the new explorer to the list of children
	AAdd(aChildWindows, oNewExplorer)

	return nil


method WindowCascade() 

	return self:Arrange(ARRANGECASCADE)


method WindowIcon() 

	return self:Arrange(ARRANGEASICONS)


method WindowTile() 

	return self:Arrange(ARRANGETILE)



END CLASS
