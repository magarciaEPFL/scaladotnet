rem contains the scala distribution given as input to jdk2ikvm
SET J2K_BASE=C:\scala
SET LIB_FOLDER=%J2K_BASE%\build\pack\lib
SET MSILANDFJBG=%J2K_BASE%\build\libs\msil.jar;%J2K_BASE%\build\libs\fjbg.jar
SET LIBRARIES=%MSILANDFJBG%;%LIB_FOLDER%\scala-compiler.jar;%LIB_FOLDER%\scala-library.jar;%J2K_BASE%\lib\jline.jar;%J2K_BASE%\lib\ant\ant.jar

rem contains the output of jdk2ikvm (src\library , src\compiler , and src\msil) as well as 'patches' folder
SET OUT_TOP=C:\patches

call aux-build-jdk2ikvm

rem use "start /B" to run in parallel, or use powershell's Start-Job and Wait-Job
call aux-j2k-cvt library 
call aux-j2k-cvt msil
call aux-j2k-cvt compiler

rem actors, partest
call aux-j2k-cvt actors
SET FOR_PARTEST=%LIB_FOLDER%\scala-partest.jar;%LIB_FOLDER%\scalap.jar
SET LIBRARIES=%LIBRARIES%;%FOR_PARTEST%
call aux-j2k-cvt partest

rem removing stuff that won't ever run in -target:msil mode, 

rd  /s /q %OUT_TOP%\gen-src\compiler\scala\tools\ant                     >nul 2>&1
del    /q %OUT_TOP%\gen-src\compiler\scala\tools\nsc\backend\jvm\*.scala >nul 2>&1


