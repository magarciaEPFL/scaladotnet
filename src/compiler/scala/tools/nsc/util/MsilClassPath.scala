/* NSC -- new Scala compiler
 * Copyright 2006-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc
package util

import scala.util.Sorting
import scala.collection.mutable
import scala.tools.nsc.io.{ AbstractFile, MsilFile }
import IKVM.Reflection.{ Type => MSILType, Assembly }
import ClassPath.{ ClassPathContext, isTraitImplementation }

object MsilClassPath {

  val assemUniverse = new IKVM.Reflection.Universe // TODO per Run?
  private val loadedAssems  = new mutable.HashMap[String, Assembly]

  /* canonical entry point to load an assembly */
  def getAssembly(path: String): Assembly = {
    loadedAssems.getOrElseUpdate(path, assemUniverse.LoadFile(path))
  }

  /* canonical entry point to obtain an MsilType */
  def getMsilType(name: String): MSILType = {
    assemUniverse.GetType(name)
  }

  def collectTypes(assemFile: AbstractFile) = {
    var res: Array[MSILType] = MSILType.EmptyTypes
    val assem = getAssembly(assemFile.path)
    if (assem != null) {
      // DeclaringType == null: true for non-inner classes
      val res0: Array[MSILType] = assem.GetTypes() filter (_.DeclaringType == null)
      /* filtering out those MSILTypes that are IsDefinitelyInternal
         makes IKVM's java.lang.StringBuilder non-instantiable because one of its parents (AbstractStringBuilder)
         IsDefinitelyInternal (although it has default access in the JDK version).
         So we filter out only what we must, ie mscorlib's System.Internal,
         ("we must" because otherwise its companion object will name-clash with package System.Internal,
         which shows up in the popular System.Drawing.dll). */
      res = if(assem.GetName.Name == "mscorlib") res0 filterNot (t => t.FullName == "System.Internal") else res0
      Sorting.stableSort(res, (t1: MSILType, t2: MSILType) => (_root_.java.lang.String.instancehelper_compareTo(t1.FullName, t2.FullName)) < 0)
    }
    res
  }

  /** On the java side this logic is in PathResolver, but as I'm not really
   *  up to folding MSIL into that, I am encapsulating it here.
   */
  def fromSettings(settings: Settings): MsilClassPath = {
    val context =
      if (settings.inline.value) new MsilContext
      else new MsilContext { override def isValidName(name: String) = !isTraitImplementation(name) }

    import settings._
    new MsilClassPath(assemextdirs.value, assemrefs.value, sourcepath.value, context)
  }

  class MsilContext extends ClassPathContext[MsilFile] {
    def toBinaryName(rep: MsilFile) = rep.msilType.Name
    def newClassPath(assemFile: AbstractFile) = new AssemblyClassPath(MsilClassPath collectTypes assemFile, "", this, assemFile)
  }

  private def assembleEntries(ext: String, user: String, source: String, context: MsilContext): List[ClassPath[MsilFile]] = {
    import ClassPath._
    val etr = new mutable.ListBuffer[ClassPath[MsilFile]]
    val names = new mutable.HashSet[String]

    // 1. Assemblies from -Xassem-extdirs
    for (dirName <- expandPath(ext, expandStar = false)) {
      val dir = AbstractFile.getDirectory(dirName)
      if (dir ne null) {
        for (file <- dir) {
          val name = file.name.ToLower
          if (name.EndsWith(".dll") || name.EndsWith(".exe")) {
            names += name
            etr += context.newClassPath(file)
          }
        }
      }
    }

    // 2. Assemblies from -Xassem-path
    for (fileName <- expandPath(user, expandStar = false)) {
      val file = AbstractFile.getFile(fileName)
      if (file ne null) {
        val name = file.name.ToLower
        if (name.EndsWith(".dll") || name.EndsWith(".exe")) {
          names += name
          etr += context.newClassPath(file)
        }
      }
    }

    def check(n: String) {
      if (!names.contains(n))
      throw new java.lang.AssertionError("Cannot find assembly "+ n +
         ". Use -Xassem-extdirs or -Xassem-path to specify its location")
    }
    check("mscorlib.dll")
    // check("scalaruntime.dll")

    // 3. Source path
    for (dirName <- expandPath(source, expandStar = false)) {
      val file = AbstractFile.getDirectory(dirName)
      if (file ne null) etr += new SourcePath[MsilFile](file, context)
    }

    etr.toList
  }
}
import MsilClassPath._

/**
 * A assembly file (dll / exe) containing classes and namespaces
 */
class AssemblyClassPath(types: Array[MSILType], namespace: String, val context: MsilContext, val assemFile: AbstractFile) extends ClassPath[MsilFile] {
  def name = {
    val i = namespace.LastIndexOf('.')
    if (i < 0) namespace
    else namespace drop (i + 1)
  }
  def asURLs = List(new java.net.URL(name))
  def asClasspathString = "TODO"  // TODO

  private lazy val first: Int = {
    var m = 0
    var n = types.length - 1
    while (m < n) {
      val l = (m + n) / 2
      val res = _root_.java.lang.String.instancehelper_compareTo(types(l).FullName, namespace)
      if (res < 0) m = l + 1
      else n = l
    }
    val resFirst = if (types(m).FullName.StartsWith(namespace)) m else types.length;
    resFirst
  }

  lazy val classes = {
    val cls = new mutable.ListBuffer[ClassRep]
    var i = first
    while (i < types.length) {
      if( types(i).Namespace != null && types(i).Namespace.StartsWith(namespace) ) {
        // CLRTypes used to exclude java.lang.Object and java.lang.String (no idea why..)
        if (types(i).Namespace == namespace)
          cls += ClassRep(Some(new MsilFile(types(i))), None)
      }
      i += 1
    }
    val resClasses = cls.toIndexedSeq;
    resClasses
  }

  lazy val packages = {
    val nsSet = new mutable.HashSet[String]
    var i = first
    while (i < types.length) {
      if( types(i).Namespace != null && types(i).Namespace.StartsWith(namespace) ) {
        val subns = types(i).Namespace
        if (subns.Length > namespace.Length) {
          // example: namespace = "System", subns = "System.Reflection.Emit"
          //   => find second "." and "System.Reflection" to nsSet.
          val end = subns.IndexOf('.', namespace.Length + 1)
          nsSet += (if (end < 0) subns
                    else _root_.java.lang.String.instancehelper_substring(subns, 0, end))        
        }
      }
      i += 1
    }
    val xs = for (ns <- nsSet.toList)
      yield new AssemblyClassPath(types, ns, context, assemFile)

    val resPkgs = xs.toIndexedSeq;
    resPkgs
  }

  val sourcepaths: IndexedSeq[AbstractFile] = IndexedSeq()

  override def ToString() = "AssemCP("+assemFile.name+")"
}

/**
 * The classpath when compiling with target:msil.
 * Binary files are represented as MsilFile values.
 */
class MsilClassPath(ext: String, user: String, source: String, context: MsilContext)
extends MergedClassPath[MsilFile](MsilClassPath.assembleEntries(ext, user, source, context), context) { }
