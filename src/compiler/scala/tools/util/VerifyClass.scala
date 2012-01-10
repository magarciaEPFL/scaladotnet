package scala.tools.util

import scala.tools.nsc.io._
import java.net.URLClassLoader
import collection.JavaConverters._


object VerifyClass {

  // Returns the error if there's a failure
  private def checkClass(name : String, cl: java.lang.ClassLoader) : (String, Option[String]) = {
    try   {
      java.lang.Class.forName(name, true, cl)
      (name, None)
    } catch {
      case x => (name, Some(x.ToString))
    }
  }

  def checkClassesInJar(name: String, cl: java.lang.ClassLoader) = new Jar(File(name)) filter (je => je.getName.EndsWith(".class")) map { x =>
    checkClass(_root_.java.lang.String.instancehelper_replace(x.getName.stripSuffix(".class"), '/', '.'), cl)
  } toMap

  def checkClassesInDir(name: String, cl: java.lang.ClassLoader) = (for {
    file <- Path(name).walk
    if file.name EndsWith ".class"
  } yield checkClass(name, cl)) toMap

  def checkClasses(name: String, cl: java.lang.ClassLoader) =
    if (name EndsWith ".jar")  checkClassesInJar(name, cl)
    else checkClassesInDir(name, cl)

  /** Attempts to load all classes on the classpath defined in the args string array.  This method is meant to be used via reflection from tools like SBT or Ant. */
  def run(args: Array[String]): java.util.Map/*[String, String]*/ = {
    val urls = args.map(Path.apply).map(_.toFile.toURI.toURL).toArray
    println("As urls: " + urls.mkString(","))
    val cl = URLClassLoader.newInstance(urls, null)
    val results = args.flatMap(n => checkClasses(n, cl)).toMap
    (for { (name, result) <- results } yield (name, result.getOrElse(null))).asJava
  }


  def main(args: Array[String]): Unit = {
    val results = run(args).asScala
    println("Processed " + results.size + " classes.")
    val errors = results.filter(_._2 != null)
    for( (name, result) <- results; if result != null) {
      println(name + " had error: " + result)
    }
    java.lang.System.exit(if(errors.size > 0) 1 else 0)
  }
}
