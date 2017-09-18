#region DEFINES
DEFINE IDM_SaveAttachmentsContextMenu_Click_ID := 28500
DEFINE IDM_SaveAttachmentsContextMenu_Click_Open_ID := 28501
DEFINE IDM_SaveAttachmentsContextMenu_Click_Save_ID := 28502
DEFINE IDM_SaveAttachmentsContextMenu_Click_Select_All_ID := 28503
DEFINE IDM_ReplyMenu_Reply_ID := 14500
DEFINE IDM_ReplyMenu_Reply_Reply_to_sender_ID := 14501
DEFINE IDM_ReplyMenu_Reply_Reply_to_all_ID := 14502
DEFINE IDM_EmailBrowserContextMenu_Click_ID := 12000
DEFINE IDM_EmailBrowserContextMenu_Click_Read_This_ID := 12001
DEFINE IDM_EmailBrowserContextMenu_Click_Forward_ID := 12002
DEFINE IDM_EmailBrowserContextMenu_Click_Reply_To_ID := 12003
DEFINE IDM_EmailBrowserContextMenu_Click_Toggle_Read_Status_ID := 12004
DEFINE IDM_EmailBrowserContextMenu_Click_Delete_ID := 12006
DEFINE IDM_EmailBrowserContextMenu_Click_Save_As_ID := 12007
DEFINE IDM_EmailBrowserContextMenu_Click_Properties_ID := 12009
DEFINE IDM_ComposeEmailContextmenu_Click_ID := 10000
DEFINE IDM_ComposeEmailContextmenu_Click_Add_ID := 10001
DEFINE IDM_ComposeEmailContextmenu_Click_Delete_ID := 10002
DEFINE IDM_ComposeEmailContextmenu_Click_Open_ID := 10003
DEFINE IDM_ComposeEmailContextmenu_Click_Save_ID := 10004
DEFINE IDM_ComposeEmailContextmenu_Click_Select_All_ID := 10005
Define IDM_ComposeEmailContextmenu := "ComposeEmailContextmenu"
Define IDA_ComposeEmailContextmenu := "ComposeEmailContextmenu"
#endregion

CLASS EmailBrowserContextMenu INHERIT Menu

CONSTRUCTOR( oOwner )

	SELF:PreInit()

	SUPER( ResourceID { "EmailBrowserContextMenu" , _GetInst( ) } )

	SELF:RegisterItem(IDM_EmailBrowserContextMenu_Click_ID, ;
		HyperLabel{ #EmailBrowserContextMenu_Click , "Click" ,  ,  } , SELF:Handle() , 0)

	SELF:RegisterItem(IDM_EmailBrowserContextMenu_Click_Read_This_ID, ;
		HyperLabel{ #MailRead , "&Read This" ,  ,  })

	SELF:RegisterItem(IDM_EmailBrowserContextMenu_Click_Forward_ID, ;
		HyperLabel{ #MailForward , "&Forward" ,  ,  })

	SELF:RegisterItem(IDM_EmailBrowserContextMenu_Click_Reply_To_ID, ;
		HyperLabel{ #MailReply , "&Reply To" ,  ,  })

	SELF:RegisterItem(IDM_EmailBrowserContextMenu_Click_Toggle_Read_Status_ID, ;
		HyperLabel{ #ToggleRead , "Toggle &Read Status" ,  ,  })

	SELF:RegisterItem(IDM_EmailBrowserContextMenu_Click_Delete_ID, ;
		HyperLabel{ #EmailDelete , "&Delete" ,  ,  })

	SELF:RegisterItem(IDM_EmailBrowserContextMenu_Click_Save_As_ID, ;
		HyperLabel{ #FileSaveAs , "&Save As...." ,  ,  })

	SELF:RegisterItem(IDM_EmailBrowserContextMenu_Click_Properties_ID, ;
		HyperLabel{ #ShowEmailProperties , "Properties" ,  ,  })

	SELF:PostInit()

	RETURN

END CLASS
CLASS SaveAttachmentsContextMenu INHERIT Menu

CONSTRUCTOR( oOwner )

	SELF:PreInit()

	SUPER( ResourceID { "SaveAttachmentsContextMenu" , _GetInst( ) } )

	SELF:RegisterItem(IDM_SaveAttachmentsContextMenu_Click_ID, ;
		HyperLabel{ #SaveAttachmentsContextMenu_Click , "Click" ,  ,  } , SELF:Handle() , 0)

	SELF:RegisterItem(IDM_SaveAttachmentsContextMenu_Click_Open_ID, ;
		HyperLabel{ #OpenButton , "&Open..." ,  ,  })

	SELF:RegisterItem(IDM_SaveAttachmentsContextMenu_Click_Save_ID, ;
		HyperLabel{ #SaveButton , "&Save..." ,  ,  })

	SELF:RegisterItem(IDM_SaveAttachmentsContextMenu_Click_Select_All_ID, ;
		HyperLabel{ #SelectAllButton , "Select &All" ,  ,  })

	SELF:PostInit()

	RETURN

END CLASS
CLASS ComposeEmailContextmenu INHERIT Menu

CONSTRUCTOR( oOwner )

	SELF:PreInit()

	SUPER( ResourceID { "ComposeEmailContextmenu" , _GetInst( ) } )

	SELF:RegisterItem(IDM_ComposeEmailContextmenu_Click_ID, ;
		HyperLabel{ #ComposeEmailContextmenu_Click , "Click" ,  ,  } , SELF:Handle() , 0)

	SELF:RegisterItem(IDM_ComposeEmailContextmenu_Click_Add_ID, ;
		HyperLabel{ #AttachButton , "&Add..." ,  ,  })

	SELF:RegisterItem(IDM_ComposeEmailContextmenu_Click_Delete_ID, ;
		HyperLabel{ #DeleteAttachment , "&Delete..." ,  ,  })

	SELF:RegisterItem(IDM_ComposeEmailContextmenu_Click_Open_ID, ;
		HyperLabel{ #OpenButton , "&Open..." ,  ,  })

	SELF:RegisterItem(IDM_ComposeEmailContextmenu_Click_Save_ID, ;
		HyperLabel{ #SaveButton , "Sa&ve..." ,  ,  })

	SELF:RegisterItem(IDM_ComposeEmailContextmenu_Click_Select_All_ID, ;
		HyperLabel{ #SelectAllButton , "&Select All..." ,  ,  })

	SELF:PostInit()

	RETURN

END CLASS
CLASS ReplyMenu INHERIT Menu

CONSTRUCTOR( oOwner )

	SELF:PreInit()

	SUPER( ResourceID { "ReplyMenu" , _GetInst( ) } )

	SELF:RegisterItem(IDM_ReplyMenu_Reply_ID, ;
		HyperLabel{ #ReplyMenu_Reply , "Reply" ,  ,  } , SELF:Handle() , 0)

	SELF:RegisterItem(IDM_ReplyMenu_Reply_Reply_to_sender_ID, ;
		HyperLabel{ #MailReply , "&Reply to sender" ,  ,  })

	SELF:RegisterItem(IDM_ReplyMenu_Reply_Reply_to_all_ID, ;
		HyperLabel{ #MailReplyAll , "Reply to &all" ,  ,  })

	SELF:PostInit()

	RETURN

END CLASS
