

"C:\Program Files (x86)\Windows Kits\10\bin\x86\signtool.exe" %*

set SIGN_RESULT=%ERRORLEVEL%

if %SIGN_RESULT% equ 0 (
  echo Signing succeeded
  exit /B 0
)

echo Signing failed with %SIGN_RESULT%
pause

exit /B %SIGN_RESULT%