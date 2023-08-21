// 726. warning XS9085: Unbalanced textmerge delimiters '<<' and '>>'.
#pragma warnings(219, off) //   assigned but not used
FUNCTION Start() AS VOID
LOCAL lcBatch AS STRING
LOCAL lcOutputPath as STRING
lcOutputPath := "C:\Tmp"
TEXT TO lcBatch TEXTMERGE NOSHOW PRETEXT 1+2
	@echo off


	REM ----------------------------------------
	REM variable declaration
	REM ----------------------------------------
	set PATH1="<<JUSTPATH( ADDBS( lcOutputPath ) )>>"
	set OLDERTHEN=-14
	set FILEFILTER=*.*
	set LOGFILE="<<PROPER( ADDBS( JUSTPATH( FULLPATH( 'cleanup.exe' ) ) ) ) >> _log\%DATE:~6,4%%DATE:~3,2%%DATE:~0,2%\cleanupbatch.log"

	REM ----------------------------------------
	REM starting batch run
	REM ----------------------------------------
	echo starting batch > %LOGFILE%
	echo ************************** >> %LOGFILE%
	echo deleting protocol >> %LOGFILE%

	REM ----------------------------------------
	REM select files
	REM ----------------------------------------
	forfiles /P "%PATH1%" /S /M %FILEFILTER% /D %OLDERTHEN% /C "cmd /c del @file | echo loesche @file vom @fdate :: %LOGFILE%"

	REM ----------------------------------------
	REM END of batch run
	REM ----------------------------------------
	echo ************************** >> %LOGFILE%
	echo END of batch run >> %LOGFILE%

ENDTEXT

? lcBatch


FUNCTION FullPath(cFilename as string) AS STRING
    if file(cFilename)
        return FPathName()
    endif
    return "C:\SomePath\"+cFileName

