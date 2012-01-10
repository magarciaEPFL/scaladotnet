rem =======================================
rem before running this script, don't forget to run:
rem (1)
rem     pull-binary-libs.sh
rem     ant all.clean build // leaves scala-compiler.jar and scala-library.jar in build\pack\lib
rem     newlibs build       // leaves msil.jar and fjbg.jar in build\libs
rem (2)
rem     migrate-src-to-ikvm.bat (this populates patches/gen-src)
rem (3)
rem     patching, i.e. go to patches/src
rem       git fetch origin
rem       git merge origin/master
rem =======================================

cls
echo on

rem =======================================
rem variables 
rem =======================================

call aux-init-vars.bat 

rem contains the scala distribution given as input to jdk2ikvm
SET J2K_BASE=C:\scala
SET LIB_FOLDER=%J2K_BASE%\build\pack\lib
SET MSILANDFJBG=%J2K_BASE%\build\libs\msil.jar;%J2K_BASE%\build\libs\fjbg.jar
SET LIBRARIES=%MSILANDFJBG%;%LIB_FOLDER%\scala-compiler.jar;%LIB_FOLDER%\scala-library.jar;%J2K_BASE%\lib\jline.jar;%J2K_BASE%\lib\ant\ant.jar
set JAVA_EXE="C:\jdk\bin\java"

rem =======================================
echo bringing DirB and DirC to a pristine state
rem =======================================

call aux-prepare-DirsBandC.bat

del %DIRB_FOLDER%\msil.dll
del %DIRC_FOLDER%\msil.dll

rem =======================================
rem compilation proper starts here
rem =======================================

call aux-cross-compile-library.bat

call aux-cross-compile-compiler-BINARY.bat


