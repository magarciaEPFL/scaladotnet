rem =======================================
rem exe (compiled by the cross-compiler) compiles itself 
rem =======================================

del /q %MSIL_OUT%\scalacompiler.exe >nul 2>&1
del /q %MSIL_OUT%\scalacompiler.pdb >nul 2>&1

%DIRC_FOLDER%\scalacompiler.exe -d %MSIL_OUT% @%OUT_TOP%\out-src-compiler.j2klst -target:exe -Ystruct-dispatch:no-cache -Xassem-name scalacompiler -Xassem-extdirs %DIRB_FOLDER% -Xshow-class scala.tools.nsc.Main 

rem del %OUT_TOP%\out-src-compiler.j2klst >nul 2>&1
rem del %OUT_TOP%\out-src-msil.j2klst >nul 2>&1

copy %MSIL_OUT%\scalacompiler.exe %DIRC_FOLDER% /y 
copy %MSIL_OUT%\scalacompiler.pdb %DIRC_FOLDER% /y 

peverify %DIRC_FOLDER%\scalacompiler.exe


