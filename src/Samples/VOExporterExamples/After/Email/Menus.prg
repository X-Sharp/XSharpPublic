#region DEFINES
Define IDA_ComposeEmailContextmenu := "ComposeEmailContextmenu"
Define IDM_ComposeEmailContextmenu := "ComposeEmailContextmenu"
Define IDM_ComposeEmailContextmenu_Click_Add_ID := 10223
Define IDM_ComposeEmailContextmenu_Click_Delete_ID := 10224
Define IDM_ComposeEmailContextmenu_Click_ID := 10222
Define IDM_ComposeEmailContextmenu_Click_Open_ID := 10225
Define IDM_ComposeEmailContextmenu_Click_Save_ID := 10226
Define IDM_ComposeEmailContextmenu_Click_Select_All_ID := 10227
Define IDM_EmailBrowserContextMenu_Click_Delete_ID := 27403
Define IDM_EmailBrowserContextMenu_Click_Forward_ID := 27399
Define IDM_EmailBrowserContextMenu_Click_ID := 27397
Define IDM_EmailBrowserContextMenu_Click_Properties_ID := 27406
Define IDM_EmailBrowserContextMenu_Click_Read_This_ID := 27398
Define IDM_EmailBrowserContextMenu_Click_Reply_To_ID := 27400
Define IDM_EmailBrowserContextMenu_Click_Save_As_ID := 27404
Define IDM_EmailBrowserContextMenu_Click_Toggle_Read_Status_ID := 27401
Define IDM_ReplyMenu_Reply_ID := 26765
Define IDM_ReplyMenu_Reply_Reply_to_all_ID := 26767
Define IDM_ReplyMenu_Reply_Reply_to_sender_ID := 26766
Define IDM_SaveAttachmentsContextMenu_Click_ID := 27416
Define IDM_SaveAttachmentsContextMenu_Click_Open_ID := 27417
Define IDM_SaveAttachmentsContextMenu_Click_Save_ID := 27418
Define IDM_SaveAttachmentsContextMenu_Click_Select_All_ID := 27419
#endregion

CLASS ComposeEmailContextmenu INHERIT Menu 

CONSTRUCTOR(oOwner) 

	SELF:PreInit()
	SUPER(ResourceID{IDM_ComposeEmailContextmenu, _GetInst( )})

	self:RegisterItem(IDM_ComposeEmailContextmenu_Click_ID,	;
		HyperLabel{#_Click,	;
			"Click",	;
			,	;
			,},self:Handle( ),0)
	self:RegisterItem(IDM_ComposeEmailContextmenu_Click_Add_ID,	;
		HyperLabel{#AttachButton,	;
			"&Add...",	;
			,	;
			,})
	self:RegisterItem(IDM_ComposeEmailContextmenu_Click_Delete_ID,	;
		HyperLabel{#DeleteAttachment,	;
			"&Delete...",	;
			,	;
			,})
	self:RegisterItem(IDM_ComposeEmailContextmenu_Click_Open_ID,	;
		HyperLabel{#OpenButton,	;
			"&Open...",	;
			,	;
			,})
	self:RegisterItem(IDM_ComposeEmailContextmenu_Click_Save_ID,	;
		HyperLabel{#SaveButton,	;
			"Sa&ve...",	;
			,	;
			,})
	self:RegisterItem(IDM_ComposeEmailContextmenu_Click_Select_All_ID,	;
		HyperLabel{#SelectAllButton,	;
			"&Select All...",	;
			,	;
			,})

	SELF:PostInit()
	return self
END CLASS
CLASS EmailBrowserContextMenu INHERIT Menu 

CONSTRUCTOR(oOwner) 

	SELF:PreInit()
	SUPER(ResourceID{"EmailBrowserContextMenu", _GetInst( )})

	SELF:RegisterItem(IDM_EmailBrowserContextMenu_Click_ID,	;
		HyperLabel{#_Click,	;
			"Click",	;
			,	;
			}, SELF:Handle(), 0)
	SELF:RegisterItem(IDM_EmailBrowserContextMenu_Click_Read_This_ID,	;
		HyperLabel{#MailRead,	;
			"&Read This",	;
			,	;
			})
	SELF:RegisterItem(IDM_EmailBrowserContextMenu_Click_Forward_ID,	;
		HyperLabel{#MailForward,	;
			"&Forward",	;
			,	;
			})
	SELF:RegisterItem(IDM_EmailBrowserContextMenu_Click_Reply_To_ID,	;
		HyperLabel{#MailReply,	;
			"&Reply To",	;
			,	;
			})
	SELF:RegisterItem(IDM_EmailBrowserContextMenu_Click_Toggle_Read_Status_ID,	;
		HyperLabel{#ToggleRead,	;
			"Toggle &Read Status",	;
			,	;
			})
	SELF:RegisterItem(IDM_EmailBrowserContextMenu_Click_Delete_ID,	;
		HyperLabel{#EmailDelete,	;
			"&Delete",	;
			,	;
			})
	SELF:RegisterItem(IDM_EmailBrowserContextMenu_Click_Save_As_ID,	;
		HyperLabel{#FileSaveAs,	;
			"&Save As....",	;
			,	;
			})
	SELF:RegisterItem(IDM_EmailBrowserContextMenu_Click_Properties_ID,	;
		HyperLabel{#ShowEmailProperties,	;
			"Properties",	;
			,	;
			})

	SELF:PostInit()
	return self
END CLASS
CLASS ReplyMenu INHERIT Menu 

CONSTRUCTOR(oOwner) 

	SELF:PreInit()
	SUPER(ResourceID{"ReplyMenu", _GetInst( )})

	SELF:RegisterItem(IDM_ReplyMenu_Reply_ID,	;
		HyperLabel{#_Reply,	;
			"Reply",	;
			,	;
			}, SELF:Handle(), 0)
	SELF:RegisterItem(IDM_ReplyMenu_Reply_Reply_to_sender_ID,	;
		HyperLabel{#MailReply,	;
			"&Reply to sender",	;
			,	;
			})
	SELF:RegisterItem(IDM_ReplyMenu_Reply_Reply_to_all_ID,	;
		HyperLabel{#MailReplyAll,	;
			"Reply to &all",	;
			,	;
			})

	SELF:PostInit()
	return self
END CLASS
CLASS SaveAttachmentsContextMenu INHERIT Menu 

CONSTRUCTOR(oOwner) 

	SELF:PreInit()
	SUPER(ResourceID{"SaveAttachmentsContextMenu", _GetInst( )})

	SELF:RegisterItem(IDM_SaveAttachmentsContextMenu_Click_ID,	;
		HyperLabel{#_Click,	;
			"Click",	;
			,	;
			}, SELF:Handle(), 0)
	SELF:RegisterItem(IDM_SaveAttachmentsContextMenu_Click_Open_ID,	;
		HyperLabel{#OpenButton,	;
			"&Open...",	;
			,	;
			})
	SELF:RegisterItem(IDM_SaveAttachmentsContextMenu_Click_Save_ID,	;
		HyperLabel{#SaveButton,	;
			"&Save...",	;
			,	;
			})
	SELF:RegisterItem(IDM_SaveAttachmentsContextMenu_Click_Select_All_ID,	;
		HyperLabel{#SelectAllButton,	;
			"Select &All",	;
			,	;
			})

	SELF:PostInit()
	return self

END CLASS
