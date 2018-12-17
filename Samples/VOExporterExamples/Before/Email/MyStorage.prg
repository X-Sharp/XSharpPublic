CLASS MyStorage INHERIT CStorage

METHOD CreateNewEmail() AS CEMail PASCAL 
    RETURN CEmail{,SELF}

CONSTRUCTOR() 
   LOCAL cPath AS STRING
   LOCAL cCurPath AS STRING

   cCurPath := CurDrive()+":\"+CurDir(CurDrive())

   cPath := "C:\Cavo28SP3\Samples\Email"

   IF DirChange(Cast2Psz(cPath)) = 0
      cPath += "\Data"
      IF DirChange(Cast2Psz(cPath)) != 0
         DirMake(Cast2Psz(cPath))
      ENDIF
      DirChange(Cast2Psz(cCurPath))
   ELSE
      cPath := WorkDir()
   ENDIF

   SUPER(cPath)

   RETURN SELF


END CLASS
