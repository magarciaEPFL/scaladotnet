/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend

import util.{ ClassPath, MsilClassPath }
import msil.GenMSIL
import io.{ AbstractFile, MsilFile }

trait MSILPlatform extends Platform {
  import global._
  import definitions.{ ObjectClass, ValueTypeClass, ComparatorClass, BoxedNumberClass, getMember }

  type BinaryRepr = MsilFile

  if (settings.verbose.value)
    inform("[AssemRefs = " + settings.assemrefs.value + "]")

  // phaseName = "msil"
  object genMSIL extends {
    val global: MSILPlatform.this.global.type = MSILPlatform.this.global
    val runsAfter = List[String]("dce")
    val runsRightAfter = None
  } with GenMSIL

  lazy val classPath = MsilClassPath.fromSettings(settings)
  def rootLoader = new loaders.PackageLoader(classPath.asInstanceOf[ClassPath[platform.BinaryRepr]])
    // See discussion in JavaPlatForm for why we need a cast here.

  def platformPhases = List(
    genMSIL   // generate .msil files
  )

  lazy val externalEquals = getMember(ComparatorClass.companionModule, nme.equals_)

  def isMaybeBoxed(sym: Symbol) = {
    (sym == ObjectClass) ||
    (sym isNonBottomSubClass BoxedNumberClass)
  }

  def newClassLoader(bin: MsilFile): loaders.SymbolLoader =  new loaders.MsilFileLoader(bin)

  /**
   * Tells whether a class should be loaded and entered into the package
   * scope. On .NET, this method returns `false` for all synthetic classes
   * (anonymous classes, implementation classes, module classes), their
   * symtab is encoded in the pickle of another class.
   */
  def doLoad(cls: ClassPath[BinaryRepr]#ClassRep): Boolean = {
    if (cls.binary.isDefined) {
      val typ = cls.binary.get.msilType
      // the helper below prevents eager loading of non-constr-info parts (and always loads non-scala types)
      scala.runtime.SymtabAttribute.helperDoLoad(typ, loaders.clrTypes.SYMTAB_CONSTR)
    } else true // always load source
  }

  def needCompile(bin: MsilFile, src: AbstractFile) =
    false // always use compiled file on .net
}
