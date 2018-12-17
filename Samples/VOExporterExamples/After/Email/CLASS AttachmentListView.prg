CLASS AttachmentListView INHERIT ListView

METHOD Delete(oEmail AS CEmail) AS VOID PASCAL 
	LOCAL aItems AS ARRAY
	LOCAL nItems, nN, nPosn AS DWORD
	LOCAL oItem AS ListViewItem
	LOCAL aDeletes AS ARRAY
	
	aItems := SELF:GetSelectedArray()
	nItems := ALen(aItems)
	IF nItems = 0
		RETURN	// nothing to do!
	ENDIF

	IF MessageBox(NULL_PTR, String2Psz("Remove Selected Attachment" + IF(nItems>1, "s","") + " ?"), String2Psz("Delete Warning"), MB_ICONINFORMATION+MB_YESNO) = IDYES
	   aDeletes := {}
		FOR nN := 1 UPTO nItems
			// identify the attachment by name and then scan for its possibly new location
			oItem := aItems[nN]
			AAdd(aDeletes, oItem:GetValue(#FilesAttached))
		NEXT nN
	   ASort(aDeletes)
	   FOR nN := nItems DOWNTO 1
	       nPosn := aDeletes[nN]
          oEmail:DeleteAttachment(nPosn)
      NEXT nN
	ENDIF

	RETURN


METHOD Fill(oEmail AS CEmail) AS DWORD PASCAL 
   LOCAL dwI       AS DWORD
   LOCAL dwCount   AS DWORD
   LOCAL oItem     AS ListViewItem
	LOCAL cFilename AS STRING
	LOCAL oImages   AS ImageList
	LOCAL aAList    AS ARRAY
	
	SELF:DeleteAll()

	dwCount := oEmail:AttachmentCount
	
	IF dwCount > 0
	   
	   aAList := {}
	   
	   FOR dwI := 1 UPTO dwCount
	      IF oEmail:GetAttachmentInfo(dwI, ATTACH_CONTENTID) == NULL_STRING
	         AAdd(aAList, dwI)
	      ENDIF
	   NEXT 
	   
	   dwCount := ALen(aAList)
	   
	   IF dwCount > 0
	   
   	   oImages := ImageList{dwCount, Dimension{16,16}}
   	   oItem := ListViewItem{}
   	
   	   FOR dwI := 1 UPTO dwCount
   		    oImages:Add(GetAssociatedIcon(oEmail:GetAttachmentInfo(aAList[dwI], ATTACH_FULLPATH), FALSE))
   		NEXT
   	
   	   SELF:SmallImageList := oImages
   	
   	   FOR dwI := 1 UPTO dwCount
   			cFilename := oEmail:GetAttachmentInfo(aAList[dwI], ATTACH_FILENAME) + ;
                         " ("+FileSizeString(oEmail:GetAttachmentInfo(aAList[dwI], ATTACH_FILESIZE),TRUE)+");"
   			oItem:ImageIndex := dwI
   			oItem:SetValue(dwI, #FilesAttached)
   			oItem:SetText(cFilename, #FilesAttached)
   			SELF:AddItem(oItem)
   		NEXT
   		
   		SELF:ViewAs(#IconView)
         SELF:ViewAs(#SmallIconView)
	   ENDIF   
	ENDIF         
	
   RETURN dwCount

METHOD GetSelectedArray() 
	
	LOCAL aRet AS ARRAY
	LOCAL oItem AS ListViewItem
	LOCAL nItem AS DWORD
	
	aRet := {}
	oItem := SELF:GetNextItem(LV_GNIBYITEM,,,,TRUE)
	DO WHILE oItem != NULL_OBJECT
		AAdd(aRet, oItem)
		nItem := oItem:ItemIndex
		oItem := SELF:GetNextItem(LV_GNIBYITEM,,,,TRUE, nItem)
	ENDDO

	RETURN aRet

METHOD GetSelectedCount() 
	
	LOCAL aRet AS ARRAY
	LOCAL oItem AS ListViewItem
	LOCAL nCount, nItem AS DWORD
	
	aRet := {}
	oItem := SELF:GetNextItem(LV_GNIBYITEM,,,,TRUE)
	nCount := 0
	DO WHILE oItem != NULL_OBJECT
		nCount += 1
		nItem := oItem:ItemIndex
		oItem := SELF:GetNextItem(LV_GNIBYITEM,,,,TRUE, nItem)
	ENDDO

	RETURN nCount


METHOD Open(oEmail AS CEmail) AS VOID PASCAL 

	LOCAL oItem AS ListViewItem
	LOCAL cPath, cFileName AS STRING
	LOCAL nPosn AS DWORD
	LOCAL lContinue, lVirusWarning AS LOGIC
	LOCAL oDlg AS AttachmentAskDialog

	lVirusWarning := TRUE	// maybe use the registry to save/modify this value

	lContinue := TRUE
	oItem := SELF:GetSelectedItem()
	IF oItem != NULL_OBJECT
	   nPosn := oItem:GetValue(#FilesAttached)
		cFileName := oEmail:GetAttachmentInfo(nPosn, ATTACH_FILENAME)
		
		IF lVirusWarning
			oDlg := AttachmentAskDialog{SELF:Owner, cPath+cFilename}
			oDlg:Show()
			IF oDlg:cResult = "O"
				lContinue := TRUE
			ELSEIF oDlg:cResult = "S"
				lContinue := FALSE	// no open but we can save
				SELF:Save(oEmail, cPath)
			ELSE
				lContinue := FALSE	// no, not allowed to do anything
			ENDIF
		ENDIF

		IF lContinue
			cPath := oEmail:GetAttachmentInfo(nPosn, ATTACH_FULLPATH)
			IF ! Empty(cPath)
				ShellExecute(NULL_PTR, String2Psz("OPEN"),String2Psz(cPath),NULL_PSZ,NULL_PSZ,SW_SHOWNORMAL)
			ELSE
				MessageBox(NULL_PTR, String2Psz("Can't open " + cFileName), String2Psz("File Open Error"), MB_ICONSTOP+MB_OK)
			ENDIF
		ENDIF

	ENDIF

	RETURN


METHOD Save(oEMail AS CEmail, cPath AS STRING) AS VOID PASCAL 
   LOCAL oProgWin  AS ProgressWindow
	LOCAL oDlg      AS StandardFolderDialog
	LOCAL nN        AS DWORD
   LOCAL nItems    AS DWORD
   LOCAL nPosn     AS DWORD
	LOCAL oItem     AS ListViewItem
	LOCAL cFileName AS STRING
	LOCAL aItems    AS ARRAY

   aItems := SELF:GetSelectedArray()
	
	nItems := ALen(aItems)
	IF nItems = 0
		RETURN // nothing was selected, or nothing is available to be selected
	ENDIF

   IF cPath == Null_String
      oDlg := StandardFolderDialog{,"Choose Save Location", aMailInfo[DEF_ATTACHPATH]}
   	oDlg:Show()
   	cPath := oDlg:FolderName
   	IF Empty(cPath)
   	   RETURN
      ENDIF
	ELSE
   	IF Right(cPath, 1) != "\"
   		cPath += "\"
   	ENDIF
   ENDIF

   oProgWin := ProgressWindow{SELF:Owner, "Saving Attachments", "", nItems * 2}
   oProgWin:AVIResource := "FileCopyAVI"
	oProgWin:Show()

	FOR nN := 1 UPTO nItems
		oItem := aItems[nN]
		nPosn := oItem:GetValue(#FilesAttached)
		cFileName := oEmail:GetAttachmentInfo(nPosn, ATTACH_FILENAME)
		oProgWin:Message(cPath+cFileName)
		oProgWin:StepIt()
		oEmail:SaveAs(cPath, cFileName, nPosn)
		oProgWin:StepIt()
	NEXT nN

	oProgWin:EndDialog()
	RETURN

METHOD SelectAll() 
	LOCAL nN, nItems AS DWORD
	
	nItems := SELF:ItemCount
	
	FOR nN := 1 UPTO nItems
		SELF:SelectItem(nN, TRUE)
	NEXT

	SELF:SetFocus()

	RETURN SELF


END CLASS
