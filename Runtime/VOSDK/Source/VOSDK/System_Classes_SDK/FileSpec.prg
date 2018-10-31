PARTIAL CLASS FileSpec
	// The FileSpec class contains the identification of a disk file
	// ( its drive, path and filename ), as well as a few useful methods.
	// The FileSpec class itself is intended mainly for keeping track of the file,
	// and doing some directory-type operations on it.
	//
	// The purpose of the FileSpec class is to help manage filenames and directories.
	// In order to avoid having explicit pathnames strewn through the program, which of
	// course cause maintenance problems and even makes the program hard to install
	// on a computer with a different disk or directory configurition, developers have come
	// up with different clever approaches, from use of the SET PATH statement, to runtime
	// parameters, to configuration files. While these approaches still work, the FileSpec
	// class and its derivatives offer some advantages.
	// In a practical example, a program might collect definition of files and directories
	// in one place, defining a FileSpec object for each file:
	//
	//                                              oFSCust := FileSpec{ "C:\DATA\CUSTDATA\CUST.DBF" }
	//                                              oFSCustIndex := FileSpec{ "C:\DATA\CUSTDATA\CUSTNO.NTX" }
	//                                              oFSOrders := FileSpec{ "C:\DATA\CUSTDATA\ORDERS.DBF" }
	//
	// Elsewhere in the program, these file specifications may be used to create data servers
	// or open databases for forms:
	//
	//                                              oDBCust := DbServer[ oFSCust }
	//                                              oDBCust:SetIndex( oFSCustIndex )
	//                                              oOrderForm:Use( oFSOrders )
	//
	// However, the FileSpec classes can do much more than simply store the path name.
	//
	// First, they help avoid the need for string manipulation to construct path names, in those
	// situations when the program wants to specify drives, directories and filenames separately:
	//
	//                                              oFSCust := FileSpec{ }  // contains no information yet!
	//                                              oFSCust:Drive := "C"
	//                                              oFSCust:Path := "\DATA\CUSTDATA"
	//                                              oFSCust:FileName := "CUST"
	//                                              oFSCust:Extension := "DBF"
	//
	// This technique obviously permits powerful and flexible programming techniques. ( Some examples
	// are illustrated below as examples for the specific methods. )
	//
	// The Find( ) method provides a particularly useful way of defining and storing path information.
	// Under Windows, the startup directory of an application may be specified as one of its properties in the
	// Program Manager. If the program starts with these lines:
	//
	//                                              oFSConfig := FileSpec{ "SOMEFILE.DBF" } // choose some necessary file
	//                                              IF !oFSConfig:Find( )
	//                                               <BREAK, or ask the user to specify where the information is>
	//                                              ENDIF
	//
	// the oFSConfig object now contains the full path of the startup directory.
	// Throughout the program, other FileSpec objects can be created from the components
	// of this object, and no explicit directory information needs to be included in the program
	// or stored in a configuration file. Installation of the program only requires that the program and
	// its files be copied to some suitable place, and the startup directory be specified in the Program Manager.
	//
	PROTECT cFSDrive        AS STRING
	PROTECT cFSPath         AS STRING
	PROTECT cFSFileName     AS STRING
	PROTECT cFSExtension    AS STRING
	PROTECT oErrorInfo      AS USUAL

	METHOD __DefaultFullPath ()  AS STRING STRICT 
	//Locates the first file using SetDefault(), CurDir() and SetPath()
	//If no file is found then the ASSIGNed drive, path, filename and extension is used.
	//
	LOCAL cDrive    AS STRING
	LOCAL cRet      AS STRING
	LOCAL lSearch   AS LOGIC
	LOCAL cPath     AS STRING
	LOCAL cSearch   AS STRING
	LOCAL siPath    AS DWORD
	LOCAL cTestPath AS STRING
	LOCAL aDirArray AS ARRAY
	LOCAL cDefault  AS STRING
	LOCAL aFullPath AS ARRAY


	cDrive  := SELF:cFSDrive
	cPath   := SELF:cFSPath


	//  UH 12/27/1999
	//  IF cDrive == NULL_STRING .OR. cPath == NULL_STRING
	IF cPath == NULL_STRING
		lSearch := .T. 
	ELSE
		IF cDrive == NULL_STRING
			IF ( SLen(cPath) > 1 ) .AND. ( Left(cPath, 2) =="\\" )
				//
				// UNC name given, no drive !!!
				//
				lSearch := .F. 
			ELSE
				lSearch := .T. 
			ENDIF
		ELSE
			//  UH 01/03/2000
			//  lSearch := .T.
			lSearch := .F. 
		ENDIF

		IF !SubStr2(cPath, SLen(cPath)) == "\"
		//ELSE
			cPath += "\"
		ENDIF
	ENDIF

	IF lSearch
		aFullPath := ArrayNew(4)
		cDefault := GetDefault()

		//Build search string of paths
        IF Empty(cDefault)
            //SE 090910
            cSearch := CurDir() + ";" + GetCurPath() + ";" 
            IF ! Left(cSearch, 2) == "\\"
               cSearch := CurDrive() +":\" + cSearch
            ENDIF    
        ELSE
            cSearch := cDefault + ";" + GetCurPath() + ";"
		ENDIF

		WHILE !Empty(cSearch)
			// get first path to search
			siPath := At2(";", cSearch)
			IF siPath = 0
				cTestPath := SubStr2(cSearch, 1)
			ELSE
				cTestPath := Left(cSearch, siPath - 1)
			ENDIF

			IF !Empty(cTestPath)
				IF InStr("\", cTestPath) .AND. RAt2("\", cTestPath) != SLen(cTestPath)
					cTestPath += "\"
				ENDIF

				__SplitPath(NIL, cTestPath, aFullPath)
				cDrive := aFullPath[ 1 ]
				cPath := aFullPath[ 2 ]

				IF SubStr2(cPath, SLen(cPath)) == "\"
					cTestPath := cDrive + cPath

				ELSE
					cTestPath := cDrive + cPath + "\"
				ENDIF

				// directory function defaults to "*.*"
				IF SELF:cFSFileName == NULL_STRING .AND. SELF:cFSExtension == NULL_STRING
					aDirArray := {}
				ELSE
					aDirArray := Directory(cTestPath + SELF:cFSFileName + SELF:cFSExtension, "HS")
				ENDIF

				// Check if the file was found
				IF ALen(aDirArray) > 0
					cDrive := Upper(cDrive)
					cPath := Upper(cPath)
					SELF:cFSDrive := cDrive
					SELF:cFSPath := cPath
					EXIT
				ENDIF
			ENDIF
			// get another path to search
			cSearch := SubStr2(cSearch, siPath + 1)
		ENDDO
	ELSE
		cSearch := cDrive + cPath
	ENDIF

	IF Empty(cSearch)
		cRet := ""
	ELSE
		cRet := cDrive + cPath + SELF:cFSFileName + SELF:cFSExtension
	ENDIF

	RETURN cRet


METHOD __FullPathAcc() AS STRING STRICT 
	LOCAL cPath AS STRING
	LOCAL cRetVal AS STRING
	cPath := SELF:cFSPath

	IF cPath == NULL_STRING .OR. SubStr2(cPath, SLen(cPath)) == "\"
		cRetVal := SELF:cFSDrive + cPath + SELF:cFSFileName + SELF:cFSExtension
	ELSE
		cRetVal := SELF:cFSDrive + cPath + "\" + SELF:cFSFileName + SELF:cFSExtension
	ENDIF
	RETURN cRetVal

METHOD __SetDrive(cTestDrive AS STRING)  AS STRING STRICT 

	//IF !IsString(cTestDrive)
	//	SELF:Error(FSError{ SELF, #__SetDrive, EG_ARG,__CavoStr(__CAVOSTR_DBFCLASS_BADDRIVE), cTestDrive, cTestDrive }, #__SetDrive)
   //
	//ELSE
		cTestDrive := Upper(cTestDrive)
		IF At2(":", cTestDrive) = 0
			cFSDrive := cTestDrive + ":"
		ELSE
			cFSDrive := cTestDrive
		ENDIF
	//ENDIF

	RETURN cFSDrive


METHOD __SetFileExt(cTestExt AS STRING) AS STRING STRICT      
	//
	// Test for a valid Extension specification and set cFSExtension is so.
	//
	IF Empty(cTestExt) // .OR. !IsString(cTestExt)
		RETURN cFSExtension := NULL_STRING
	ENDIF

        //RvdH 050314 Can't do this. Linux and others are case sensitive
	//cTestExt := Upper(cTestExt)
	IF At2(".", cTestExt) = 0
		cFSExtension := "." + cTestExt
	ELSE
		cFSExtension := cTestExt
	ENDIF
	RETURN cFSExtension

METHOD __SetFileName(cTestName AS STRING) AS STRING STRICT 
	//
	// Set the FileName
	//
	// 461@004 RJ - 10-07-95 - rewrite and/or cleanup of code
	//
	IF Empty(cTestName) // .OR. !IsString(cTestName)
		RETURN cFSFileName := NULL_STRING
	ENDIF

        //RvdH 050314 Can't do this. Linux and others are case sensitive
	//cTestName := Upper(cTestName)
	// strip off any file extension
	IF At2(".", cTestName) != 0
		cTestName := Left(cTestName, RAt2(".", cTestName) - 1)
	ENDIF

	// strip off any directory
	IF At2("\", cTestName) != 0
		cTestName := SubStr2(cTestName, RAt2("\", cTestName) + 1)
	ENDIF
	cFSFileName := cTestName
	RETURN cFSFileName

METHOD __SetFilePath(cTestPath AS STRING ) AS STRING STRICT  
	//
	// Set the path
	//
	// 461@004 RJ - 10-07-95 - rewrite and/or cleanup of code
	//
	LOCAL aFullPath     AS ARRAY

	//IF !IsString(cTestPath)
	//	RETURN NULL_STRING
	//ENDIF

        //RvdH 050314 Can't do this. Linux and others are case sensitive
	// cTestPath := Upper(cTestPath)
	// add a backslash to end of string to ensure that this gets parsed as a directory and not file name
	IF SubStr2(cTestPath, SLen(cTestPath)) != "\"
		cTestPath += "\"
	ENDIF

	aFullPath := ArrayNew(4)
	__SplitPath(NIL, cTestPath, aFullPath)
	cFSPath := aFullPath[ 2 ]
	RETURN cFSPath

METHOD AppendToPath(cDirectory)     
    // This method appends the specified directory or string of directories to the existing path.
    // For example:
    //
    //                      oFSCust := FileSpec{ "C:\DATA\CUST.DBF" }
    //                      ...
    //                      oFSOrders := FileSpec{ "ORDERS.DBF" }
    //                      oFSOrders:Path := oFSCust:Path
    //                      oFSOrders:AppendToPath( "ORDERS" )
    //
    // 461@004 RJ - 10-07-95 - rewrite and/or cleanup of code
    //

    LOCAL cPath     AS STRING
    LOCAL aFullPath AS ARRAY

    cPath := cFSPath

    IF Empty(cDirectory) .OR. !IsString(cDirectory)
       RETURN cPath
    ENDIF

        //RvdH 050314 Can't do this. Linux and others are case sensitive
	//cDirectory := Upper(cDirectory)
	// can't append a root directory or drive to an existing path
	IF cDirectory == "\" .OR. At2(":", cDirectory) != 0
		RETURN cPath
	ENDIF

    // get a starting path if there isn't any path to append to
    IF cPath == NULL_STRING
       //SE 090910
       cPath := CurDir()
       IF Left(cPath, 2) == "\\"  //UNC-Path
          cFSDrive := NULL_STRING
       ELSE
          cFSDrive := CurDrive()  
          // CurDir() returns NULL_STRING for ROOT directory and error. If error, default it to ROOT directory
          cPath := "\" + cPath
       ENDIF     
    ENDIF

	// place a backslash to begining of path to append
	IF Left(cDirectory, 1) != "\"
		cDirectory := "\" + cDirectory
	ENDIF

	// add trailing backslash so that __SplitPath knows that this is a path and not file name
	IF SubStr2(cDirectory, SLen(cDirectory)) != "\"
		cDirectory += "\"
	ENDIF

	aFullPath := ArrayNew(4)
	__SplitPath(NIL, cDirectory, aFullPath)

	// got our new path with any "..\" or ".\" taken into account
	cDirectory := aFullPath[ 2 ]

	IF SubStr2(cPath, SLen(cPath)) == "\"
		cPath += SubStr2(cDirectory, 2)
	ELSE
		cPath += cDirectory
	ENDIF

	RETURN cFSPath := cPath

ACCESS Attributes                   
	LOCAL cRet	AS STRING
	LOCAL cPath AS STRING

	cPath := SELF:__FullPathAcc()

	IF cPath != NULL_STRING
		IF FFirst(String2Psz(cPath), 0)
			cRet := FAttr2String( FAttrib() )
		ENDIF
	ENDIF

	RETURN cRet

METHOD Copy(oFSTarget, lName)       
	//
	// May be used to copy a file. For example,
	// oFSSource := FileSpec{ "C:\DATA\CUSTOMER.DBF" } // define original file
	// oFSTarget := FileSpec{ "A:\CUSTDATA" }          // specify target file
	// oFSSource:Copy( oFSTarget )             // copy the file
	//
	// 461@004 RJ - 10-07-95 - rewrite and/or cleanup of code
	// oFSTarget is assumed to be a file name. Include backslash char if only a path:
	// oFS:Copy("C:\TMP\")
	// copies SELF's file into C:\TMP
	//
	// lName enables "auto-rename" on file conflict. Default is disabled (FALSE)
	//

	LOCAL cNewName      AS STRING
	LOCAL cFileName     AS STRING
	LOCAL cFileNameExt  AS STRING
	LOCAL cTarget       AS STRING
	LOCAL cSourcePath   AS STRING
	LOCAL lRetCode      AS LOGIC
	LOCAL aFullPath     AS ARRAY
	LOCAL oSelf         AS FileSpec
	LOCAL cbOldErr      AS USUAL
	LOCAL oError            AS USUAL

	IF Empty(lName) .OR. !IsLogic(lName)
		lName := FALSE
	ENDIF

	IF IsInstanceOfUsual(oFSTarget, #FileSpec)
		cTarget := oFSTarget:FullPath
	ELSE
		IF Empty(oFSTarget) .OR. !IsString(oFSTarget)
			RETURN FALSE
		ENDIF
	        //RvdH 050314 Can't do this. Linux and others are case sensitive
		//cTarget := Upper(oFSTarget)
		cTarget := oFSTarget
	ENDIF

	cSourcePath := SELF:FullPath

	// resolve any ambiguous information with information from SELF
	oSelf := SELF
	aFullPath := ArrayNew(4)

	// clean up the source path because FileSpec allows NULL information
	__SplitPath(NIL, cSourcePath, aFullPath)
	IF SubStr2(aFullPath[ 2 ], SLen(aFullPath[ 2 ])) == "\"
		cSourcePath := aFullPath[ 1 ] + aFullPath[ 2 ] + aFullPath[ 3 ] + aFullPath[ 4 ]
	ELSE
		cSourcePath := aFullPath[ 1 ] + aFullPath[ 2 ] + "\" + aFullPath[ 3 ] + aFullPath[ 4 ]
	ENDIF

	// clean up the target path and resolve any ambiguous information with information from SELF
	__SplitPath(oSelf, cTarget, aFullPath)
	IF SubStr2(aFullPath[ 2 ], SLen(aFullPath[ 2 ])) == "\"
		cTarget := aFullPath[ 1 ] + aFullPath[ 2 ]
	ELSE
		cTarget := aFullPath[ 1 ] + aFullPath[ 2 ] + "\"
	ENDIF

	cFileName       := aFullPath[ 3 ]
	IF cFileName == NULL_STRING
		cFileName := SELF:cFSFileName
	ENDIF

	cFileNameExt    := aFullPath[ 4 ]
	IF cFileNameExt == NULL_STRING
		cFileNameExt := SELF:cFSExtension
	ENDIF

	IF lName
        // make a different file name if conflict exists
        IF (cNewName := __NewName(cTarget + cFileName + cFileNameExt)) != NULL_STRING
            cFileName := cNewName
        ENDIF              
        //SE 080414
        IF oFSTarget != NULL_OBJECT
            oFSTarget:FileName := cNewName
        ENDIF  
    ENDIF

    cTarget += cFileName + cFileNameExt
	cbOldErr := ErrorBlock( {|oErr|_Break(oErr)} )

	BEGIN SEQUENCE
		// UH 12/01/95
		// lRetCode := CopyFile(cSourcePath, cTarget)
		lRetCode := FCopy(cSourcePath, cTarget)
        ErrorBlock(cbOldErr)

	RECOVER USING oError
		ErrorBlock(cbOldErr)
		// get some kind of description for the DOS error
		IF oError:OsCode != 0
			oError:Description := VO_Sprintf(__CAVOSTR_SYSLIB_DOS_ERROR, NTrim(oError:OsCode) + ;
				" (" + DosErrString(oError:OsCode) + ")")
		ENDIF
		SELF:oErrorInfo := oError
		lRetCode := FALSE

	END SEQUENCE


	RETURN lRetCode

ACCESS DateChanged                  
	LOCAL cPath AS STRING
	LOCAL dRet	AS DATE

	cPath := SELF:__FullPathAcc()
	IF cPath != NULL_STRING
		IF FFirst(String2Psz(cPath), 0)
			dRet := FDate()
		ENDIF
	ENDIF

	RETURN dRet

METHOD Delete( )                    
	//
	// Used to delete a file
	//
	LOCAL cPath AS STRING

	cPath := SELF:__FullPathAcc()

	IF cPath != NULL_STRING
		RETURN FErase(cPath)
	ENDIF

	RETURN FALSE

ACCESS Drive                        
	// Returns the drive of the file, as a string
	// The string contain a trailing colon
	// Note that if the file was not specified with a drive, and the file could not be found
	// in the current directory and has not been found since, the Drive may be unknown and
	// in that case a null string is returned
	// Note that in some network environments, the "drive" may be represented by more than one character
	RETURN SELF:cFSDrive

ASSIGN Drive(cDrive)                
	// The drive may be assigned separately with this method.
	// The drive may be assigned with or without a trailing colon
	// For example, to copy selected records from a file to another drive, under the same directory and filename,
	// you might do this:
	// oFSSource := FileSpec{ "C:\DATA\CUSTOMER" }             // define original file
	// oFSTarget := FileSpec{ oFSSource:FullPath }             // copy path for target file
	// oFSTarget:Drive := "D"          // change drive for target file
	// oDBSource := DbServer{ oFSSource }              // open original file
	IF !Empty(cDrive) .AND. IsString(cDrive)

		cDrive := Upper(cDrive)
		// 461@003 RJ - 10-06-95 - Added single drive letter support
		IF At2(":", cDrive) = 0
			cDrive += ":"
		ENDIF
		SELF:cFSDrive := Left(cDrive, RAt2(":", cDrive))
	ELSE
		SELF:cFSDrive := NULL_STRING
	ENDIF

	RETURN SELF:cFSDrive

ACCESS ErrInfo  
	RETURN SELF:oErrorInfo

METHOD Error( oError, symMethod )   
	LOCAL cErrorValType AS STRING

	IF !IsInstanceOfUsual(oError, #Error)
		cErrorValType := ValType(oError)
		oError := Error{ }
		oError:GenCode := EG_ERRORBUILD

		//418@JFS001 changed CAVO_Str_BadErrorObject --> __CavoStr(__CAVOSTR_SYSCLASS_BADERROROBJECT)
		oError:Description := VO_Sprintf(__CAVOSTR_SYSCLASS_BADERROROBJECT,cErrorValType) //419@TR017

	ENDIF

	oError:MethodSelf := SELF

	IF symMethod = NIL .OR. UsualType( symMethod ) != SYMBOL
		oError:FuncSym := #Unknown
	ELSE
		oError:FuncSym := symMethod
	ENDIF

	//oError:choice := EC_BREAK
	Eval( ErrorBlock( ), oError )
	RETURN NIL
	//359@003 end

ACCESS Extension                    
	//
	// Returns the file extension as a string, with a leading period
	//
	RETURN SELF:cFSExtension

ASSIGN Extension(cExtension)        
	//
	// Assign file extension
	//
	// 461@004 RJ - 10-07-95 - rewrite and/or cleanup of code
	//

	IF Empty(cExtension) .OR. !IsString(cExtension)
		SELF:cFSExtension := NULL_STRING
	ELSE
	        //RvdH 050314 Can't do this. Linux and others are case sensitive
		//cExtension := Upper(cExtension)
		// someone sent in a path+filename
		IF At2("\", cExtension) != 0
			cExtension := SubStr2(cExtension, RAt2("\", cExtension) + 1)
		ENDIF

		IF At2(".", cExtension) != 0
			SELF:cFSExtension := SubStr2(cExtension, RAt2(".", cExtension))
		ELSE
			SELF:cFSExtension := "." + cExtension
		ENDIF

	ENDIF

	RETURN SELF:cFSExtension

ACCESS FileName                     
	//
	// Returns the file name as a string, excluding extension
	//
	RETURN SELF:cFSFileName

ASSIGN FileName( cFileName )        
	// The filename may be changed.
	// A simple filename may be specified, or a filename with extension, or a full path complete with drive.
	// Only those parts that are specified are changed.
	// For example,
	// oFS:FileName := "CUST"  changes only the filename
	// oFS:FileName := "CUST.DBX"      changes filename and extension
	// oFS:FileName := "\DATA\CUST"    changes path and filename
	// oFS:FileName := "D:CUST"        changes drive, path and filename
	// oFS:FileName := "DATA\CUST"     changes path and filename
	//
	// For example, to copy selected records from a file to another filename,
	// you might do this:
	// oFSSource := FileSpec{ "C:\DATA\CUSTOMER" }             // define original file
	// oFSTarget := FileSpec{ oFSSource:FullPath }             // copy path for target file
	// oFSTarget:FileName := "NYCUST"          // change filename for target file
	// oDBSource := DbServer{ oFSSource }              // open original file
	// oDBSource:CopyDB( oFSTarget , , { | |STATE = "NY" } )   // copy selected records to target file
	LOCAL aFullPath         AS ARRAY

	IF !Empty(cFileName) .AND. IsString(cFileName)

		//  UH 09/04/2000
		cFileName := __Normalize(cFileName)

		//  UH 09/12/2000
		//  cFileName := Upper(cFileName)

		aFullPath := ArrayNew(4)
		__SplitPath(NIL, cFileName, aFullPath)

		// set the drive and path anyway if SetDefault() has been set

		// UH
		// cGetDefault := GetDefault()
		//RvdH 050610 Don't remove the spaces.  They could be correct nowadays.
		//cFileName := AdjustFName(cFileName)

		// 464@002 RJ   11-06-95 - only use SetDefault() if drive/path was used in SetDefault()
		// only set the drive is it was sent in as an argument or SetDefault() includes a driver letter
		// and cFSDrive is NULL
		IF At2(":", cFileName) != 0
			SELF:cFSDrive       := aFullPath[ 1 ]
		ENDIF

		// only set the path is it was sent in as an argument or SetDefault() includes a path
		// and cFSPath is NULL
		IF At2("\", cFileName) != 0
			SELF:cFSPath        := aFullPath[ 2 ]
		ENDIF

		SELF:cFSFileName    := aFullPath[ 3 ]

		IF RAt2(".", cFileName) > 0
			SELF:cFSExtension   := aFullPath[ 4 ]
		ENDIF
	ELSE
		SELF:cFSFileName    := NULL_STRING
	ENDIF

	RETURN 

METHOD Find( )                      
	//
	// This method looks for the file and returns a logic indicating whether the file is found or not
	// If it finds it, it also stores the drive and path for future reference.
	//
	LOCAL lRet  AS LOGIC
	LOCAL cPath AS STRING

	lRet := File(SELF:Fullpath)

	IF !lRet
		cPath := SELF:__DefaultFullPath()

		IF cPath != NULL_STRING
			lRet := File(cPath)
		ELSE
			lRet := File(SELF:FullPath)
		ENDIF
	ENDIF

	IF lRet
		SELF:FullPath := FPathName()
	ENDIF

	RETURN lRet


ACCESS FullPath                     
	RETURN SELF:__FullPathAcc() //359@003


ASSIGN FullPath(cFullPath)          
	// This ASSIGN method allows the entire path to be changed.
	// Normally it is set through the instantiation parameters
	//
	// New: oFS:FullPath := "FileName"  ==> <current drive> + <current dir> + FileName
	// Old: oFS:FullPath := "FileName" ==> <cFSDrive> + <cFSPath> + FileName
	// NOTE: we already have this behavior in the oFS:FileName ASSIGN method which would only replace the FileName
	LOCAL aFullPath     AS ARRAY

	IF !Empty(cFullPath) .AND. IsString(cFullPath)
		//  UH 09/20/2000 What a Bullshit
		//  cFullPath := Upper(cFullPath)
		aFullPath := ArrayNew(4)
		__SplitPath(NIL, cFullPath, aFullPath)

		SELF:cFSDrive       := aFullPath[ 1 ]
		SELF:cFSPath        := aFullPath[ 2 ]
		SELF:cFSFileName    := aFullPath[ 3 ]
		SELF:cFSExtension   := aFullPath[ 4 ]

	ELSE
		SELF:cFSDrive       := NULL_STRING
		SELF:cFSPath        := NULL_STRING
		SELF:cFSFileName    := NULL_STRING
		SELF:cFSExtension   := NULL_STRING

	ENDIF

	RETURN SELF:__FullPathAcc()


CONSTRUCTOR(cFullPath)              

	IF !Empty(cFullPath)
		IF IsString(cFullPath) .OR. IsSymbol(cFullPath)
			IF IsSymbol(cFullPath)
				cFullPath := Symbol2String(cFullPath)
			ENDIF

			//  UH 09/12/2000
			//  SELF:FileName := Upper(cFullPath)
			SELF:FileName := cFullPath

			// UH 09/12/96
			SELF:Find()
		ENDIF
	ENDIF

	RETURN 


METHOD Move(oFSTarget, lName)       
	//
	// May be used to move a file. For example,
	//        oFSSource := FileSpec{ "C:\DATA\CUSTOMER.DBF" } // define original file
	//        oFSTarget := FileSpec{ "A:\CUSTDATA\CUSTOMER.BAK" }     // specify new file
	//        oFSSource:Move( oFSTarget )             // move the file
	//
	LOCAL oSelf         AS FILESPEC
	LOCAL aFullPath     AS ARRAY
	LOCAL cNewName      AS STRING
	LOCAL cFileName     AS STRING
	LOCAL cFileNameExt  AS STRING
	LOCAL cSourcePath   AS STRING
	LOCAL cTarget       AS STRING
	LOCAL lRetCode      AS LOGIC

	LOCAL cbOldErr      AS USUAL
	LOCAL oError            AS USUAL

	IF Empty(lName) .OR. !IsLogic(lName)
		lName := FALSE

	ENDIF

	IF IsInstanceOfUsual(oFSTarget, #FileSpec)
		cTarget := oFSTarget:FullPath

	ELSE
		IF Empty(oFSTarget) .OR. !IsString(oFSTarget)
			RETURN FALSE

		ENDIF
	        //RvdH 050314 Can't do this. Linux and others are case sensitive
		//cTarget := Upper(oFSTarget)
		cTarget   := oFSTarget

	ENDIF

	cSourcePath := SELF:FullPath

	oSelf := SELF
	aFullPath := ArrayNew(4)

	// clean up the source path because FileSpec allows NULL information
	__SplitPath(NIL, cSourcePath, aFullPath)
	IF SubStr2(aFullPath[ 2 ], SLen(aFullPath[ 2 ])) == "\"
		cSourcePath := aFullPath[ 1 ] + aFullPath[ 2 ] + aFullPath[ 3 ] + aFullPath[ 4 ]

	ELSE
		cSourcePath := aFullPath[ 1 ] + aFullPath[ 2 ] + "\" + aFullPath[ 3 ] + aFullPath[ 4 ]

	ENDIF

	// clean up the target path and resolve any ambiguous information with information from SELF
	__SplitPath(oSelf, cTarget, aFullPath)
	IF SubStr2(aFullPath[ 2 ], SLen(aFullPath[ 2 ])) == "\"
		cTarget := aFullPath[ 1 ] + aFullPath[ 2 ]

	ELSE
		cTarget := aFullPath[ 1 ] + aFullPath[ 2 ] + "\"

	ENDIF

	cFileName       := aFullPath[ 3 ]
	IF cFileName == NULL_STRING
		cFileName := SELF:cFSFileName
	ENDIF

	cFileNameExt    := aFullPath[ 4 ]
	IF cFileNameExt == NULL_STRING
		cFileNameExt := SELF:cFSExtension
	ENDIF

	IF lName
		// make a different file name if conflict exists
		IF (cNewName := __NewName(cTarget + cFileName + cFileNameExt)) != NULL_STRING
			cFileName := cNewName

		ENDIF
        //SE 080414
        IF oFSTarget != NULL_OBJECT
            oFSTarget:FileName := cNewName
        ENDIF  
    ENDIF

	cTarget += cFileName + cFileNameExt

	cbOldErr := ErrorBlock( {|oErr|_Break(oErr)} )

	BEGIN SEQUENCE
		// If moving only to the same drive, FRENAME() will do. This is much faster than a COPY/DELETE.
		IF Left(cTarget, At2(":", cTarget)) == Left(cSourcePath,  At2(":", cSourcePath))

			// FRename() may damage FAT entry for file if source and target are the same
			// I know I'd never do that...
			IF cSourcePath == cTarget
			   ErrorBlock(cbOldErr)
				RETURN FALSE

			ENDIF

			// FRename() will not rename the file if it exists
			IF !lName .AND. File(cTarget)
				IF (lRetCode := FErase(cTarget))
					lRetCode := FRename(cSourcePath, cTarget)
				ENDIF
			ELSE
				lRetCode := FRename(cSourcePath, cTarget)

			ENDIF

			IF lRetCode
				SELF:FullPath := cTarget
			ENDIF

		ELSE
			// UH 12/01/95
			// IF lRetCode := CopyFile(cSourcePath, cTarget)
			IF lRetCode := FCopy(cSourcePath, cTarget)
				IF lRetCode := FErase(cSourcePath)
					IF lRetCode
						SELF:FullPath := cTarget

					ENDIF

				ENDIF

			ENDIF

		ENDIF
        ErrorBlock(cbOldErr)
	RECOVER USING oError
		ErrorBlock(cbOldErr)
		// get some kind of description for the DOS error
		IF oError:OsCode != 0
			oError:Description := VO_Sprintf(__CAVOSTR_SYSLIB_DOS_ERROR, NTrim(oError:OsCode) + ;
				" (" + DosErrString(oError:OsCode) + ")")
		ENDIF

		SELF:oErrorInfo := oError
		lRetCode := FALSE

	END SEQUENCE


	RETURN lRetCode

ACCESS Path                         
	RETURN SELF:cFSPath

ASSIGN Path(cPath)                  
	//
	// The path ( directory ) of a FileSpec may be changed.
	// For example, to copy selected records from a file to another directory, under the same filename,
	// you might do this:
	//            oFSSource := FileSpec{ "C:\DATA\CUSTOMER" }             // define original file
	//            oFSTarget := FileSpec{ oFSSource:FullPath }             // copy path for target file
	//            oFSTarget:Path := "\DATA\NY"            // change directory for target file
	//            oDBSource := DbServer{ oFSSource }              // open original file
	//            oDBSource:CopyDB( oFSTarget , , { | |STATE = "NY" } )   // copy selected records to target file
	//
	// Note: although ACCESS Path returns the path with a leading slash but without a trailing slash, they may
	// be specified here in any order
	LOCAL aFullPath AS ARRAY

	IF !Empty(cPath) .AND. IsString(cPath)

		//  UH 09/04/2000
		cPath := __Normalize(cPath)

	        //RvdH 050314 Can't do this. Linux and others are case sensitive
		//cPath := Upper(cPath)
		aFullPath := ArrayNew(4)
		// check for ROOT directory or add a backslash if trailing backslash is omitted
        	IF Right(cPath, 1) == "\"
			__SplitPath(NIL, cPath, aFullPath)

		ELSE
			__SplitPath(NIL, cPath + "\", aFullPath)

		ENDIF

		// no drive was given, so don't assign a drive letter
		IF At2(":", cPath) != 0
			SELF:cFSDrive := aFullPath[ 1 ]

		ENDIF

		SELF:cFSPath := aFullPath[ 2 ]

	ELSE
		SELF:cFSDrive   := NULL_STRING
		SELF:cFSPath    := NULL_STRING

	ENDIF

	RETURN 

METHOD PathUp( )                    
	//
	// This method strips off the last subdirectory from the end of the path, in effect moving up to the
	// parent directory.
	// For example, to store selected data in a backup directory at a peer level to the actual
	// data directory:
	//
	LOCAL cPath AS STRING

	cPath := cFSPath

	// don't change cFSPath if it is not set
	IF cPath == NULL_STRING
		RETURN cFSPath

	ENDIF

	IF cPath != NULL_STRING
		//	UH: 09/01/1999 Recommended from Herntz
		//	cPath := SubStr3(cPath, 1, RAt2("\", cPath) - 1)
		cPath := AllTrim(cPath)
		IF Right(cPath,1) == "\"
			cPath := Left(cPath,SLen(cPath)-1)
		ENDIF
		cPath := Left(cPath, RAt2("\", cPath) )
	ENDIF

	IF cPath == NULL_STRING
		cFSPath := "\"
	ELSE
		cFSPath := cPath

	ENDIF

	RETURN cFSPath



METHOD Rename(oFSTarget, lName)     
	//
	// May be used to rename a file. For example,
	// oFSSource := FileSpec{ "C:\DATA\CUSTOMER.DBF" } // define original file
	// oFSSource:Rename( "CUSTOMER.BAK" )              // rename the file
	//
	LOCAL aFullPath     AS ARRAY
	LOCAL cTarget       AS STRING
	LOCAL cFileName     AS STRING
	LOCAL cFileNameExt  AS STRING
	LOCAL cSourcePath   AS STRING
	LOCAL cTargetPath   AS STRING
	LOCAL cNewName      AS STRING
	LOCAL lRetCode      AS LOGIC

	IF Empty(lName) .OR. !IsLogic(lName)
		lName := FALSE

	ENDIF

	IF IsInstanceOfUsual(oFSTarget, #FileSpec)
		cTarget := oFSTarget:FileName + oFSTarget:Extention

	ELSE
		IF Empty(oFSTarget) .OR. !IsString(oFSTarget)
			RETURN FALSE

		ENDIF

	        //RvdH 050314 Can't do this. Linux and others are case sensitive
		//cTarget := Upper(oFSTarget)
		cTarget   := oFSTarget


		// check for drive and/or directory in target name
		IF At2(":", cTarget) != 0 .OR. At2("\", cTarget) != 0
			RETURN FALSE

		ENDIF

	ENDIF

	cSourcePath := SELF:FullPath

	aFullPath := ArrayNew(4)

	// clean up the source path because FileSpec allows NULL information
	__SplitPath(NIL, cSourcePath, aFullPath)
	IF SubStr2(aFullPath[ 2 ], SLen(aFullPath[ 2 ])) == "\"
		cSourcePath := aFullPath[ 1 ] + aFullPath[ 2 ] + aFullPath[ 3 ] + aFullPath[ 4 ]
		cTargetPath := aFullPath[ 1 ] + aFullPath[ 2 ]

	ELSE
		cSourcePath := aFullPath[ 1 ] + aFullPath[ 2 ] + "\" + aFullPath[ 3 ] + aFullPath[ 4 ]
		cTargetPath := aFullPath[ 1 ] + aFullPath[ 2 ] + "\"

	ENDIF

	IF RAt2(".", cTarget) != 0
		cFileName 	 := Left(cTarget, RAt2(".", cTarget) - 1)
		cFileNameExt := SubStr2(cTarget, RAt2(".", cTarget))

	ELSE
		cFileName := cTarget

	ENDIF

	IF lName
		// make a different file name if conflict exists
		IF (cNewName := __NewName(cTargetPath + cFileName + cFileNameExt)) != NULL_STRING
			cFileName := cNewName

		ENDIF

	ENDIF

	cTargetPath += cFileName + cFileNameExt

	IF lRetCode := FRename(cSourcePath, cTargetPath)
		SELF:FullPath := cTargetPath

	ENDIF

	RETURN lRetCode

ACCESS Size                         
	LOCAL dwSize    AS DWORD
	LOCAL cPath     AS STRING

	cPath := SELF:__FullPathAcc()

	IF cPath != NULL_STRING
		//	09/12/96 UH
		//	aDir := Directory(cPath, "HS")
		//	IF ALen(aDir) > 0
		//    RETURN aDir[ 1, F_SIZE ]
		//	ENDIF
		IF FFirst(String2Psz(cPath), 0)
			dwSize := FSize()
		ENDIF
	ENDIF

	RETURN dwSize

ACCESS TimeChanged                  

	//	UH 09/12/96
	//	LOCAL aDir      AS ARRAY
	LOCAL cRet		AS STRING
	LOCAL cPath     AS STRING

	cPath := SELF:__FullPathAcc()

	IF cPath != NULL_STRING
		//	UH 09/12/96
		//aDir := Directory(cPath, "HS")
		//	IF ALen(aDir) > 0
		//		RETURN aDir[ 1, F_TIME ]
		IF FFirst(String2Psz(cPath), 0)
			cRet := FTime()
		ELSE
			cRet := ConTime(0,0,0)
		ENDIF
	ELSE
		cRet := ConTime(0,0,0)
	ENDIF

	RETURN cRet

END CLASS

PARTIAL CLASS FSError           INHERIT Error   //359@003

CONSTRUCTOR( oOriginator, symMethod, wErrorType, oHLErrorMessage, uMisc1, uMisc2 ) 
    //RvdH 080609 Added call to super:Init to correctly fill the callstack
    SUPER()
	SELF:SubSystem := "FileSpec"
	IF oOriginator# NIL
		SELF:MethodSelf := oOriginator

	ENDIF

	IF symMethod# NIL
		SELF:FuncSym := symMethod
		SELF:CallFuncSym := symMethod

	ENDIF

	IF wErrorType# NIL
		SELF:GenCode := wErrorType

	ELSE
		SELF:GenCode := EG_NOTABLE

	ENDIF

	IF oHLErrorMessage# NIL
		IF IsInstanceOfUsual( oHLErrorMessage, #HyperLabel )
			SELF:Description := oHLErrorMessage:Description
		ELSE
			SELF:Description := oHLErrorMessage

		ENDIF

	ENDIF

	IF GenCode = EG_ARG
		SELF:Args := { uMisc1 }
		SELF:Arg := uMisc2

	ENDIF

	SELF:Severity := ES_ERROR
	RETURN 

END CLASS

FUNCTION __GetPath      (cPath)
	LOCAL cNewPath  AS STRING
	LOCAL cTempPath AS STRING
	LOCAL cDrive    AS STRING
	LOCAL cCurDir   AS STRING
	LOCAL nPathLen  AS DWORD
	LOCAL i         AS DWORD
    	LOCAL dwPos     AS DWORD  

	IF !Empty(cPath)
	        //RvdH 050314 Can't do this. Linux and others are case sensitive
		//cPath := Upper(AllTrim(cPath))
		cPath := AllTrim(cPath)

		// check for NT "\\" device name format
        IF ! Left(cPath, 2) == "\\"

			// check string for DOS's current dir (".\") and parent dir ("..\") path directives
			IF At2(".\", cPath) = 0

				// drive was given
                IF (dwPos := At2(":", cPath)) != 0

                    // path was given
                    IF At2("\", cPath) != 0

                        // check if there is a root dir after drive ":"
                        // if so, then return the drive and path
                        IF SubStr3(cPath, dwPos + 1, 1) != "\"
                           // path looks something like this: D:DATA\
                           cCurDir := CurDir()
                           IF Left(cPath, 1) != CurDrive() 
                              cCurDir := NULL_STRING
                           ENDIF   

                           // CurDir() returns NULL_STRING for ROOT directory
                           // or on error - either way, default it to the ROOT directory
                           IF ! cCurDir == NULL_STRING
                              cCurDir := "\" + cCurDir
                           ENDIF 
                           cPath := Left(cPath, dwPos) + cCurDir + "\" + SubStr2(cPath, dwPos + 1)
                        ENDIF

                        // don't strip off the root dir 
                        dwPos := SLen(cPath)
                        IF Right(cPath, 1) == "\" .AND. SubStr3(cPath, dwPos - 1, 1) != ":"
                           cPath := Left(cPath, dwPos - 1)
                        ENDIF
                        
                        RETURN cPath

                    ELSE
                        // no path was given, only a drive and possibly a directory name
                        // get the current directory and add directory name onto it

                        cCurDir := CurDir()
                        IF Left(cPath, 1) != CurDrive() 
                           cCurDir := NULL_STRING
                        ENDIF
                        
                        // CurDir() returns NULL_STRING for ROOT directory
                        // or on error - either way, default it to the ROOT directory 
                        IF ! cCurDir == NULL_STRING
                           cCurDir := "\" + cCurDir
                        ENDIF 
                        
                        RETURN Left(cPath, dwPos) + cCurDir + "\" + SubStr2(cPath, dwPos + 1)

                    ENDIF

                ELSE
                     // no drive was given   
                     
                     // check for trailing backslash and strip it off
                     IF SLen(cPath) > 1 .AND. Right(cPath, 1) == "\"
                        cPath := Left(cPath, SLen(cPath) - 1)
                     ENDIF 
                     
                     cCurDir := CurDir()

                     // Get current directory on current drive if ROOT directory was not specified
                     IF Left(cPath, 1) != "\"
                        IF Left(cCurDir,2) == "\\"
                           cCurDir += "\"
                        ELSE
                           IF ! cCurDir == NULL_STRING
                              cCurDir += "\"
                           ENDIF 
                           cCurDir := CurDrive() + ":\" + cCurDir
                        ENDIF
                     ELSE
                        IF Left(cCurDir,2) == "\\"
                           dwPos := At3("\", cCurDir, 2) //server name
                           dwPos := At3("\", cCurDir, dwPos) //shared name
                           IF dwPos > 0
                              cCurDir := Left(cCurDir, dwPos-1)
                           ENDIF 
                           IF cPath == "\"
                              RETURN cCurDir
                           ENDIF   
                        ELSE   
                           cCurDir := CurDrive() + ":"
                        ENDIF       
                     ENDIF  
                     
                     RETURN cCurDir + cPath

                ENDIF

            ELSE
                // here we go...
                // get drive letter and path length excluding drive letter and ":"  
                cCurDir := CurDir()
                
                IF SubStr3(cPath, 2, 1) != ":" 
                   IF Left(cCurDir,2) == "\\"
                      dwPos := At3("\", cCurDir, 2) //server name
                      dwPos := At3("\", cCurDir, dwPos) //shared name
                      IF dwPos > 0
                         cDrive  := Left(cCurDir, dwPos-1)
                         cCurDir := SubStr2(cCurDir, dwPos+1) 
                      ELSE
                         cDrive  := cCurDir
                         cCurDir := NULL_STRING
                      ENDIF
                   ELSE   
                      cDrive := CurDrive() + ":"
                   ENDIF
                   nPathLen := SLen(cPath)
                ELSE
                   cDrive := Left(cPath, 2)
                   IF Left(cDrive, 1) != CurDrive()
                      cCurDir := NULL_STRING
                   ENDIF
                   
                   nPathLen := SLen(cPath) - 2
                   // get rid of drive letter from cPath
                   cPath := SubStr2(cPath, 3)
                ENDIF

                IF Left(cPath, 1) != "\"
                    IF cCurDir == NULL_STRING
                       cTempPath := "\"
                    ELSE
                       cTempPath := "\" + cCurDir + "\"
                    ENDIF

                    i := 1
                    WHILE i <= nPathLen
                        DO CASE
						CASE SubStr3(cPath, i, 1) == "\"
							cTempPath := cTempPath + "\"
							i++

						CASE SubStr3(cPath, i, 2) == ".\"
							i += 2

						CASE SubStr3(cPath, i, 3) == "..\"
							cTempPath := Left(cTempPath, SLen(cTempPath) - 1)
							cTempPath := Left(cTempPath, RAt2( "\", cTempPath))
							i += 3

						OTHERWISE
							cTempPath := cTempPath + SubStr3(cPath, i, 1)
							i++

						ENDCASE
						// if there's nothing left of the path, then default it to the root
						IF cTempPath == NULL_STRING
							cTempPath :=  "\"

						ENDIF

					ENDDO
					cNewPath := cDrive + cTempPath

				ELSE
					IF  Left(cPath, 1) == "\"
						i := 1
						WHILE i <= nPathLen
							DO CASE
							CASE SubStr3(cPath, i, 1) == "\"
								cTempPath := cTempPath + "\"
								i++

							CASE SubStr3(cPath, i, 2) == ".\"
								i += 2

							CASE SubStr3(cPath, i, 3) == "..\"
								cTempPath := Left(cTempPath, SLen(cTempPath) - 1)
								cTempPath := Left(cTempPath, RAt2( "\", cTempPath))
								i += 3

							OTHERWISE
								cTempPath := cTempPath + SubStr3(cPath, i, 1)
								i++

							ENDCASE
							// if there's nothing left of the path, then default it to the root
							IF cTempPath == NULL_STRING
								cTempPath :=  "\"

							ENDIF

						ENDDO
						cNewPath := cDrive + cTempPath

					ENDIF

                ENDIF

            ENDIF

        ELSE
            // just return UNC-Path 
            IF Right(cPath, 1) == "\" 
               RETURN Left(cPath, SLen(cPath)-1)
            ENDIF
            RETURN cPath   
        ENDIF
    ELSE
        // empty path given - default it to current drive and directory 
        cCurDir := CurDir()
        IF Left(cCurDir, 1) == "\\"
           RETURN cCurDir
        ENDIF   
        RETURN CurDrive() + ":\" + cCurDir

    ENDIF

    // don't strip off the root dir
    IF Right(cNewPath, 1) == "\" .AND. SubStr3(cNewPath, SLen(cNewPath) - 1, 1) != ":"
       cNewPath := Left(cNewPath, SLen(cNewPath) - 1)
    ENDIF
	RETURN cNewPath
FUNCTION __NewName      (cFullPath)
	//
	// cFullPath: a fully qualified PATH such as C:\TEMP\FILENAME.DBF
	//
	// RETURNS: a base file name. 8 character file name with 5 or 6 chars of original name plus a suffix
	//          ranging from "_1" to "_99" if a name conflict occurs. Otherwise returns NULL_STRING
	//
	//
	LOCAL i, w          AS DWORD
	LOCAL wLen          AS DWORD
	LOCAL wStart        AS DWORD
	LOCAL cPath         AS STRING
	LOCAL cFileName     AS STRING
	LOCAL cFileNameExt  AS STRING

        //RvdH 050314 Can't do this. Linux and others are case sensitive
	//cFullPath := Upper(cFullPath)
	IF File(cFullPath)
		cPath   := Left(cFullPath, RAt2("\", cFullPath))
		wStart  := RAt2("\", cFullPath) + 1
		wLen    := RAt2(".", cFullPath) - wStart
		cFileName := SubStr3(cFullPath, wStart, wLen)
		cFileNameExt := SubStr2(cFullPath, RAt2(".", cFullPath))

		IF SLen(cFileName) > 6
			w := SLen(cFileName) - 2

		ELSE
			w := SLen(cFileName)

		ENDIF

		FOR i := 1 UPTO 99
			IF i < 10
				cFileName := Left(cFileName, w) + "_" + Str3( i, 1, 0)

			ELSE
				IF w >= 6
					w := 5

				ENDIF

				cFileName := Left(cFileName, w) + "_" + Str3( i, 2, 0)

			ENDIF

			IF !File(cPath + cFileName + cFileNameExt)
				EXIT

			ENDIF

		NEXT

		RETURN cFileName

	ENDIF

	RETURN NULL_STRING

FUNCTION __SplitPath    (oFS , cString , aFullPath )
	//
	// cString is the string to parse into DRIVE/PATH/FILENAME information
	// Optional oFS parameter will cause ambiguous references to default to oFS's information
	//
	// oFS can be a FileSpec or inherited class from FileSpec
	//
	// If oFS is NIL, then all ambiguous information such as Drive, Directory
	// will default to the current drive, directory under Windows.
	//
	// aFullPath is a 1 dimensional, 4 element  array
	//
	//  aFullPath[ 1 ] = drive plus ":"
	//  aFullPath[ 2 ] = path including root "\" minus ending "\"
	//  aFullPath[ 3 ] = file name minus extension
	//  aFullPath[ 4 ] = extension including "."
	//

	LOCAL i         AS DWORD
    LOCAL DIM abDrive[MAXDRIVENAME] AS BYTE
    LOCAL DIM abDir	 [MAXDIRNAME]   AS BYTE
    LOCAL DIM abName [MAXFILENAME]  AS BYTE
    LOCAL DIM abExt  [MAXEXTNAME]	AS BYTE

	FOR i := 1 UPTO ALen(aFullPath)
		aFullPath[ i ] := NULL_STRING
	NEXT
       

    SplitPath(cString, PSZ(_CAST, @abDrive[1]), PSZ(_CAST,@abDir[1]), PSZ(_CAST,@abName[1]), PSZ(_CAST,@abExt[1]))

    aFullPath[1] := Psz2String(PSZ(_CAST, @abDrive[1]))
    aFullPath[2] := Psz2String(PSZ(_CAST, @abDir[1]))
    aFullPath[3] := Psz2String(PSZ(_CAST, @abName[1]))
    aFullPath[4] := Psz2String(PSZ(_CAST, @abExt[1]))

	IF IsInstanceOfUsual(oFS, #FileSpec)
	
	   //SE-081211 fixes a problem with UNC pathnames
       IF aFullPath[2] == NULL_STRING
          aFullPath[2] := oFS:Path
       ENDIF
      
       IF aFullPath[1] == NULL_STRING
          IF ! Left(aFullPath[2],2) == "\\"  //SE-081211 copy drive only if path is no UNC path
             aFullPath[1] := oFS:Drive 
          ENDIF   
       ENDIF

	ENDIF

	RETURN NIL



FUNCTION __Normalize        (cPath AS STRING)         AS STRING STRICT
	LOCAL nPos  AS DWORD
	LOCAL cFile AS STRING
	LOCAL cRet  AS STRING

	//  UH 09/04/2000
	nPos := RAt(".\", cPath)
	IF nPos > 0
		cFile := SubStr2(cPath, nPos + 2)
		cPath := Left(cPath, nPos + 1)
		cPath := __GetPath(cPath)
		IF SLen(cFile) > 0
			cRet := cPath
			IF Right(cPath, 1) != "\"
				cRet += "\"
			ENDIF
			cRet += cFile
		ELSE
			cRet := cPath
		ENDIF
	ELSE
		cRet := cPath
	ENDIF

	RETURN cRet



