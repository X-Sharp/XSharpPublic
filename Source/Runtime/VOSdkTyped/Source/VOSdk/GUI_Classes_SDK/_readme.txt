+ = Partial
x = Ready
- = Not implemented
  = Todo

x Accelerator.prg
+ AnimationControl.prg			Is now a Label without animation
+ Application Window.prg		Todo: Dispatcher for Help
x Application.prg				Clean up
x BaseListBox.prg  
x Bitmap.prg
x BoundingBox.prg
x Brush.prg						Colors only
x Button.prg					Test: Images on buttons with the Pizza sample
x CAPAINT.prg
x CheckBox.prg
x ChildAppWindow.prg			
x ClipBoard.prg
x Color.prg
x ComboBox.prg					Access to Edit Control, ComboBoxEx, Combobox Events
x Control.prg					ToolTips, Theme, EnsureVisible
+ Cursor.prg					Cursor from Bitmap not implemented yet
x CustomControl.prg
x DataBrowser.prg
x DataDialog.prg
  DataListView.prg				
+ DataWindow.prg				Todo: Dispatcher for Help
x DateTimePicker.prg			
x DialogWindow.prg				
  Dispatch.prg					Todo: Check if everything is implemented
x Edit.prg						
+ Event.prg						Todo: control events slider, scrollbar, syslink, richedit
x EventContext.prg				
  Explorer Window.prg
x FixedBitmap.prg				Check
x FixedIcon.prg					Check
x FixedImage.prg				Check
x FixedText.prg					 
x Font.prg
x FormattedString.prg			
x GroupBox.prg
  HelpDisplay.prg
x HotKeyEdit.prg
x Hyperlink.prg					Todo: Dispatach and ShellOpen
x Icon.prg
+ ImageList.prg					Todo: Mask, OverlayImage, DragImage
x IPAddress.prg
+ ListBox.prg					Todo: Drag + Drop, ListFiles, ListBox Events
+ ListView.prg					Todo: 
x Menu.prg						SystemMenu class not supported in DotNet.
x MLE.prg						Uses SendMessage() for some methods
  MMContainer.prg
X ModelessDialog.prg
+ MonthCalendar.prg				Todo: Some Colors
x Pair.prg						Added Implicit operators for System.Drawing classes
x Pen.prg
x Pointer.prg					
x ProgressBar.prg
x PushButton.prg
x RadioButton.prg
x RadioButtonGroup.prg
x ResFile.prg
x ResID.prg					Added Name property (STRING), ID property is now LONG
x ResStr.prg
x RichTextEdit.prg				Printing & Font
x ScrollBar.prg					// VScrollbar and HScrollbar class
x ShellWindow.prg				Todo: Dispatcher for Help
x Slider.prg					// DotNet Trackbar class
x Spinner.prg					// Does not exist in the framework. Look at NumericUpdown Control, dummy subclasses from Scrollbar
+ Split Window.prg              // uptp 4 panels.
x StandardDialog.prg
x StatusBar.prg				
x SysLink.prg					// Partial
x TabControl.prg				Ampersand character in tab page caption
  Temp.prg
x TextBox.prg
+ TextControl.prg				Color, AutoComplete, BalloonTip, CueBanner
x ToolBar.prg					Without the Subtoolbars (rebars)
+ TopAppWindow.prg				Todo: Implement ResizeChild()?
+ TreeView.prg					Drag & Drop, Item Visual States, Search, SelectItem(), SortChildren()
x WCError.prg
+ WCFunc.prg					Some functions removed because not needed
x WCStructures.prg
- WindApp.prg					Not needed
+ Window.prg		
  WindowEventHandlers.prg
  XP Theme Support.prg
  _Dummy.prg
x Properties\AssemblyInfo.prg
x ResourceReader\ResourceDialog.prg
x ResourceReader\ResourceDlgItem.prg
x ResourceReader\ResourceReader.prg
x ResourceReader\Win32.prg
x VOWinForm\ChildWinForm.prg
x VOWinForm\Controls.prg
x VOWinForm\Forms.prg
x VOWinForm\Interfaces.prg
x VOWinForm\MenuToolbar.prg
x VOWinForm\VOWinFormApp.prg
x VOWinForm\WindowStyle.prg
x VOWinForm\WinFormVOWindow.prg
x VOWinForm\WinFormVOWindowHost.prg




Drawing:
+ BitmapObject.prg					// Implement the Draw methods for these classes
+ DrawObject.prg
+ EllipseObject.prg
+ FormattedTextObject.prg
+ LineObject.prg
+ PieObject.prg
+ RectangleObject.prg
+ ShapeObject.prg
+ TextObject.prg

Control Windows
+  ControlWindow.prg		
+  EditWindow.prg			Font


Drag & Drop
  Drag and Drop for Controls and Windows
  DragDropClient.prg
  DragDropServer.prg


IPC
+ IPCClient.prg
+ IPCEvent.prg
+ IPCServer.prg
x IPCTopic.prg


OLE
  Ole Defines.prg
x OleControl.prg
+ OleDragEvent.prg			Position Access
x OleObject.prg
x OleWindows.prg

Printing
+ Printer.prg
x PrintingDevice.prg


Not needed
x DDImp.prg						Not needed
x DocApp.prg					Not needed
x FormDialogWindow.prg			Not needed
x FormFrame.prg					Not needed
x WindowScrollBar.prg			Not accessible in .NET and also not really needed. Dummy classess



Windows Method to check / implement
- CreateSelfBitMap & Print
- Help and Help Cursors
- List Drag ?
- Tray Icons
- Animation
- Drawing
- Drag and Drop
- THeme Dialog
- PaintBackGround
- Resize (and auto alignment)
- Scroll



Events to check / implement

ComboBoxExNotify
ControlNotify
ListViewItemDrag
MouseDrag
MouseTrap()




