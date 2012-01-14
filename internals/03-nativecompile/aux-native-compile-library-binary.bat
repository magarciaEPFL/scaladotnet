rem =======================================
rem use exe (compiled by the exe that was compiled by the cross-compiler) to compile the library
rem =======================================

del /q %MSIL_OUT%\scalalib.dll >nul 2>&1
del /q %MSIL_OUT%\scalalib.pdb >nul 2>&1

%DIRC_FOLDER%\scalacompiler.exe -sourcepath %OUT_TOP%\src\library -d %MSIL_OUT% @%OUT_TOP%\out-src-library.j2klst -target:library -Ystruct-dispatch:no-cache -Xassem-name scalalib -Xassem-extdirs %DIRA_FOLDER% 

rem del %OUT_TOP%\out-src-library.j2klst >nul 2>&1

copy %MSIL_OUT%\scalalib.dll %DIRB_FOLDER% /y 
copy %MSIL_OUT%\scalalib.pdb %DIRB_FOLDER% /y 

call aux-merge-support-dlls.bat

rem peverify %DIRB_FOLDER%\scalalib.dll


