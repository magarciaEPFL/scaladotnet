/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 */

package scala.tools.nsc
package interpreter

import scala.tools.nsc.io.{ File, AbstractFile }
import util.ScalaClassLoader
import java.net.URL
import scala.collection.{ mutable, immutable }

/**
 * A class loader that loads files from a {@link scala.tools.nsc.io.AbstractFile}.
 *
 * @author Lex Spoon
 */
class AbstractFileClassLoader(root: AbstractFile, parent: java.lang.ClassLoader)
    extends java.lang.ClassLoader(parent)
    with ScalaClassLoader
{
  // private val defined = mutable.Map[String, Class[_]]()

  override protected def trace =
    sys.props contains "scala.debug.classloader"

  protected def classNameToPath(name: String): String =
    if (name EndsWith ".class") name
    else _root_.java.lang.String.instancehelper_replace(name, '.', '/') + ".class"

  protected def findAbstractFile(name: String): AbstractFile = {
    var file: AbstractFile = root
    val pathParts          = classNameToPath(name) split '/'

    for (dirPart <- pathParts.init) {
      file = file.lookupName(dirPart, true)
      if (file == null)
        return null
    }

    file.lookupName(pathParts.last, false) match {
      case null   => null
      case file   => file
    }
  }

  protected def dirNameToPath(name: String): String =
    _root_.java.lang.String.instancehelper_replace(name, '.', '/')

  protected def findAbstractDir(name: String): AbstractFile = {
    var file: AbstractFile = root
    val pathParts          = dirNameToPath(name) split '/'

    for (dirPart <- pathParts) {
      file = file.lookupName(dirPart, true)
      if (file == null)
        return null
    }

    return file
  }

  override def getResourceAsStream(name: String) = findAbstractFile(name) match {
    case null => super.getResourceAsStream(name)
    case file => file.input
  }
  override def classBytes(name: String): Array[Byte] = findAbstractFile(name) match {
    case null => super.classBytes(name)
    case file => file.toByteArray
  }
  override def loadClass(name: String, resolve: Boolean) = {
    classLoaderLog("load " + name + ".")
    super.loadClass(name, resolve)
  }
  override def findClass(name: String): JClass = {
    val bytes = classBytes(name)
    classLoaderLog("find %s: %s".format(name,
      if (bytes.isEmpty) "failed."
      else bytes.size + " bytes."
    ))
    if (bytes.isEmpty)
      throw new java.lang.ClassNotFoundException(name)
    else {
      val clazz = defineClass(name, bytes, 0, bytes.length)
      // defined(name) = clazz
      clazz
    }
  }
  // Don't know how to construct an URL for something which exists only in memory
  // override def getResource(name: String): URL = findAbstractFile(name) match {
  //   case null   => super.getResource(name)
  //   case file   => new URL(...)
  // }

  private val packages = mutable.Map[String, java.lang.Package]()

  override def definePackage(name: String, specTitle: String, specVersion: String, specVendor: String, implTitle: String, implVersion: String, implVendor: String, sealBase: URL): java.lang.Package = {
    throw new java.lang.UnsupportedOperationException()
  }

  override def getPackage(name: String): java.lang.Package = {
    findAbstractDir(name) match {
      case null => super.getPackage(name)
      case file => packages.getOrElseUpdate(name, {
        val ctor = classOf[java.lang.Package].getDeclaredConstructor(scala.Array( classOf[String], classOf[String], classOf[String], classOf[String], classOf[String], classOf[String], classOf[String], classOf[URL], classOf[java.lang.ClassLoader] ) )
        ctor.setAccessible(true)
        ctor.newInstance(scala.Array( name, null, null, null, null, null, null, null, this ) ).asInstanceOf[java.lang.Package]
      })
    }
  }

  override def getPackages(): Array[java.lang.Package] =
    root.iterator.filter(_.isDirectory).map(dir => getPackage(dir.name)).toArray
}
