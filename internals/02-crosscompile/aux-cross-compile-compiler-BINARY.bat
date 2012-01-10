rem =======================================
echo crosscompile scalacompiler
rem =======================================

del %MSIL_OUT%\scalacompiler.msil >nul 2>&1 
del %MSIL_OUT%\scalacompiler.exe >nul 2>&1 
del %MSIL_OUT%\scalacompiler.pdb >nul 2>&1 

echo using: -Xassem-extdirs %DIRB_FOLDER%

dir %OUT_TOP%\src\compiler\*.scala /s /b > %OUT_TOP%\out-src-compiler.j2klst

%JAVA_EXE% -Dscala.timings=true -Dfile.encoding=UTF-8 -Xbootclasspath/a:%LIBRARIES% -Xms512M -Xmx1236M -Xss1M -XX:MaxPermSize=128M scala.tools.nsc.Main -d %MSIL_OUT% @%OUT_TOP%\out-src-compiler.j2klst -target:msil -Ystruct-dispatch:no-cache -Xassem-name scalacompiler -Xassem-extdirs %DIRB_FOLDER% -Xshow-class scala.tools.nsc.Main 

ilasm /QUIET /DEBUG %MSIL_OUT%\scalacompiler.msil /output=%DIRC_FOLDER%\scalacompiler.exe

del %MSIL_OUT%\scalacompiler.msil >nul 2>&1 

peverify %DIRC_FOLDER%\scalacompiler.exe


