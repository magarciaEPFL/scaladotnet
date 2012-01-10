rem =======================================
echo bringing DirB and DirC to a pristine state
rem =======================================

mkdir %DIRB_FOLDER%
mkdir %DIRC_FOLDER%
del /s /q %DIRB_FOLDER%\*.* >nul 2>&1
del /s /q %DIRC_FOLDER%\*.* >nul 2>&1

copy %DIRA_FOLDER%\*.dll %DIRB_FOLDER% >nul 2>&1
copy %DIRA_FOLDER%\*.dll %DIRC_FOLDER% >nul 2>&1


