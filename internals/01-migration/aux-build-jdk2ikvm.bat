echo off
cls

rem contains the src and resources folders in the jdk2ikm distribution
SET J2K_TOOL_FOLDER=C:\plugins\jdk2ikvm
SET J2K_PACKAGE_FOLDER=%J2K_TOOL_FOLDER%\src\
SET J2K_TEMP_FOLDER=.\classes

rem making room for classfiles
rd /s /q   %J2K_TEMP_FOLDER%    >nul 2>&1
mkdir      %J2K_TEMP_FOLDER%    >nul 2>&1

echo building jdk2ikvm 
"C:\jdk\bin\java" -Xbootclasspath/a:%LIBRARIES% scala.tools.nsc.Main -classpath %J2K_BASE%\lib\scala-compiler.jar;%J2K_BASE%\lib\scala-library.jar -d %J2K_TEMP_FOLDER% %J2K_PACKAGE_FOLDER%\JDK2IKVM.scala %J2K_PACKAGE_FOLDER%\JDK2IKVMPlugin.scala %J2K_PACKAGE_FOLDER%\FileUtil.scala %J2K_PACKAGE_FOLDER%\patchcmds\Patching.scala %J2K_PACKAGE_FOLDER%\patchcmds\Generating.scala  

del jdk2ikvm.jar
jar -cf jdk2ikvm.jar -C %J2K_TEMP_FOLDER% scala -C %J2K_TOOL_FOLDER%\resources\ . 

rd /s /q   %J2K_TEMP_FOLDER%    >nul 2>&1

