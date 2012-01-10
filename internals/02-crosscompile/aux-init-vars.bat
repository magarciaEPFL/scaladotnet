SET MSIL_OUT=C:\temp\ilasm

rem contains the output of jdk2ikvm (src\library , src\compiler , and src\msil)
SET OUT_TOP=C:\patches

dir %OUT_TOP%\src\compiler\*.scala /s /b > %OUT_TOP%\out-src-compiler.j2klst
dir %OUT_TOP%\src\library\*.scala  /s /b > %OUT_TOP%\out-src-library.j2klst
dir %OUT_TOP%\src\msil\*.scala     /s /b > %OUT_TOP%\out-src-msil.j2klst

SET DIRA_FOLDER=C:\temp\test\DirA
SET DIRB_FOLDER=C:\temp\test\DirB
SET DIRC_FOLDER=C:\temp\test\DirC


