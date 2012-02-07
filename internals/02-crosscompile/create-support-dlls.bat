set IKVM_FOLDER=c:\ikvm
set DIRA_FOLDER=c:\temp\test\DirA
set J2K_BASE=C:\scala

rem assemblies are emitted with corflags 0x00020003 //  ILONLY 32BITPREFERRED
rem (by giving -platform:anycpu32bitpreferred to ikvmc, and /platform:anycpu32bitpreferred to csc)
rem for the reasons discussed at http://weblog.ikvm.net/CommentView.aspx?guid=7c760276-fc97-4b90-9208-5d67797b21a7

rem ---------------------------------
rem scalaruntime.dll
rem ---------------------------------
rd /s /q %DIRA_FOLDER%\classes >nul 2>&1
mkdir %DIRA_FOLDER%\classes

javac -d %DIRA_FOLDER%\classes %J2K_BASE%\src\library\scala\math\ScalaNumber.java %J2K_BASE%\src\library\scala\reflect\ScalaSignature.java %J2K_BASE%\src\library\scala\reflect\ScalaLongSignature.java %J2K_BASE%\src\library\scala\runtime\ArrayRuntime.java %J2K_BASE%\src\library\scala\runtime\BoxesRuntime.java %J2K_BASE%\src\library\scala\runtime\TraitSetter.java

del additional_java_sources_in_scalaruntime.txt >nul 2>&1 
dir %J2K_BASE%\src\library\scala\runtime\*Ref.java /s /b         >  additional_java_sources_in_scalaruntime.txt
dir %J2K_BASE%\src\library\scala\runtime\BoxedUnit.java /s /b    >> additional_java_sources_in_scalaruntime.txt
dir %J2K_BASE%\src\library\scala\collection\mutable\*.java /s /b >> additional_java_sources_in_scalaruntime.txt
javac -d %DIRA_FOLDER%\classes @additional_java_sources_in_scalaruntime.txt
del additional_java_sources_in_scalaruntime.txt >nul 2>&1 

del %DIRA_FOLDER%\scalaruntime.jar >nul 2>&1
jar -cf %DIRA_FOLDER%\scalaruntime.jar -C %DIRA_FOLDER%\classes scala 

%IKVM_FOLDER%\bin\ikvmc -target:library -platform:anycpu32bitpreferred -out:%DIRA_FOLDER%\scalaruntime.dll %DIRA_FOLDER%\scalaruntime.jar
del %DIRA_FOLDER%\scalaruntime.jar >nul 2>&1

rem ---------------------------------
rem fjbg.dll , forkjoin.dll , jline.dll
rem ---------------------------------

%IKVM_FOLDER%\bin\ikvmc -target:library -platform:anycpu32bitpreferred -out:%DIRA_FOLDER%\fjbg.dll     %J2K_BASE%\lib\fjbg.jar
%IKVM_FOLDER%\bin\ikvmc -target:library -platform:anycpu32bitpreferred -out:%DIRA_FOLDER%\forkjoin.dll %J2K_BASE%\lib\forkjoin.jar
%IKVM_FOLDER%\bin\ikvmc -target:library -platform:anycpu32bitpreferred -out:%DIRA_FOLDER%\jline.dll    %J2K_BASE%\lib\jline.jar

rem ---------------------------------
rem CSharpFilesForBootstrap.dll contains scala.runtime.Comparator and scala.runtime.SymtabAttribute
rem ---------------------------------

csc /r:%IKVM_FOLDER%\bin\IKVM.Reflection.dll /debug /platform:anycpu32bitpreferred /out:%DIRA_FOLDER%\CSharpFilesForBootstrap.dll /target:library Comparator.cs SymtabAttribute.cs

rem ---------------------------------
rem TODO copy to %DIRA_FOLDER%
rem
rem from IKVM bin folder:
rem   IKVM.OpenJDK.Beans.dll
rem   IKVM.OpenJDK.Core.dll
rem   IKVM.OpenJDK.Media.dll
rem   IKVM.OpenJDK.Misc.dll
rem   IKVM.OpenJDK.Security.dll
rem   IKVM.OpenJDK.SwingAWT.dll
rem   IKVM.OpenJDK.Text.dll
rem   IKVM.OpenJDK.Util.dll
rem   IKVM.OpenJDK.XML.API.dll
rem   IKVM.OpenJDK.XML.Bind.dll
rem   IKVM.OpenJDK.XML.Crypto.dll
rem   IKVM.OpenJDK.XML.Parse.dll
rem   IKVM.OpenJDK.XML.Transform.dll
rem   IKVM.OpenJDK.XML.WebServices.dll
rem   IKVM.OpenJDK.XML.XPath.dll
rem   IKVM.Reflection.dll
rem   IKVM.Runtime.dll
rem
rem from  "C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETPortable\v4.0\"
rem   mscorlib.dll
rem   System.dll
rem   System.Core.dll
rem   System.Xml.dll
rem
rem Given that the IKVM assemblies list v2 of mscorlib.dll as dependency, 
rem a .NET framework version below NETPortable v4.0 would also do.
rem
rem ---------------------------------

