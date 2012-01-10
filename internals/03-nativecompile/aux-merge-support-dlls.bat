rem =======================================
echo merging scalaruntime.dll and CSharpFilesForBootstrap.dll into scalalib.dll
rem =======================================

"C:\Program Files (x86)\Microsoft\ILMerge\ILMerge.exe" /out:%MSIL_OUT%\scalalib.dll %DIRB_FOLDER%\scalalib.dll %DIRA_FOLDER%\CSharpFilesForBootstrap.dll %DIRA_FOLDER%\scalaruntime.dll

rem repack with 32bitpreferred
ildasm %MSIL_OUT%\scalalib.dll /NOBAR /out=%MSIL_OUT%\scalalib.msil /linenum

ilasm /32BITPREFERRED /QUIET /DLL /PDB %MSIL_OUT%\scalalib.msil /out=%MSIL_OUT%\scalalib.dll

copy %MSIL_OUT%\scalalib.dll %DIRB_FOLDER% /y >nul 2>&1
copy %MSIL_OUT%\scalalib.pdb %DIRB_FOLDER% /y >nul 2>&1

copy %MSIL_OUT%\scalalib.dll %DIRC_FOLDER% /y >nul 2>&1
copy %MSIL_OUT%\scalalib.pdb %DIRC_FOLDER% /y >nul 2>&1

del %MSIL_OUT%\scalalib.dll
del %MSIL_OUT%\scalalib.pdb

del %DIRB_FOLDER%\scalaruntime.dll >nul 2>&1
del %DIRB_FOLDER%\CSharpFilesForBootstrap.dll >nul 2>&1

del %DIRC_FOLDER%\scalaruntime.dll >nul 2>&1
del %DIRC_FOLDER%\CSharpFilesForBootstrap.dll >nul 2>&1

peverify %DIRB_FOLDER%\scalalib.dll

