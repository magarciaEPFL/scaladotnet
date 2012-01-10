rem =======================================
echo crosscompile scalalib
rem =======================================

rd /s /q %MSIL_OUT% >nul 2>&1
mkdir %MSIL_OUT%

echo using: -Xbootclasspath/a:%LIBRARIES%
echo using: -sourcepath %OUT_TOP%\src\library
echo using: -Xassem-extdirs %DIRA_FOLDER%

dir %OUT_TOP%\src\library\*.scala /s /b > %OUT_TOP%\out-src-library.j2klst

%JAVA_EXE% -Dfile.encoding=UTF-8 -Xbootclasspath/a:%LIBRARIES% -Xms512M -Xmx1236M -Xss1M -XX:MaxPermSize=128M scala.tools.nsc.Main -sourcepath %OUT_TOP%\src\library -d %MSIL_OUT% @%OUT_TOP%\out-src-library.j2klst -target:msil -Ystruct-dispatch:no-cache -Xassem-name scalalib -Xassem-extdirs %DIRA_FOLDER% 

del %DIRB_FOLDER%\scalalib.dll >nul 2>&1 
del %DIRB_FOLDER%\scalalib.pdb >nul 2>&1 

ilasm /QUIET /DLL /PDB %MSIL_OUT%\scalalib.msil /output=%DIRB_FOLDER%\scalalib.dll 

copy %DIRB_FOLDER%\scalalib.dll %DIRC_FOLDER%\scalalib.dll /y
copy %DIRB_FOLDER%\scalalib.pdb %DIRC_FOLDER%\scalalib.pdb /y

del %MSIL_OUT%\scalalib.msil >nul 2>&1 

call aux-merge-support-dlls.bat

rem peverify %DIRB_FOLDER%\scalalib.dll 
