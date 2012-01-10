echo off

set SRC_FOLDER=%J2K_BASE%\src\%1
set SRCFILELIST_FILE=%OUT_TOP%\in-src-%1.j2klst

set OUT_FOLDER=%OUT_TOP%\gen-src\%1

echo updating file-list for %1
del %SRCFILELIST_FILE% >nul 2>&1 
dir %SRC_FOLDER%\*.scala /s /b > %SRCFILELIST_FILE%

echo deleting files to be overwritten 
rd /s /q   %OUT_FOLDER%   >nul 2>&1
mkdir      %OUT_FOLDER%   >nul 2>&1

echo running jdk2ikvm on %1
"C:\jdk\bin\java" -Xbootclasspath/a:%LIBRARIES% -Xms512M -Xmx1236M -Xss1M -XX:MaxPermSize=128M scala.tools.nsc.Main -Ystop-after:superaccessors -sourcepath %SRC_FOLDER% -P:jdk2ikvm:output-directory:%OUT_FOLDER% -Xplugin %OUT_TOP%\jdk2ikvm.jar -d %OUT_FOLDER% @%SRCFILELIST_FILE% -Yrangepos 

del %OUT_FOLDER%\*.java /s /q

del %SRCFILELIST_FILE% >nul 2>&1 


