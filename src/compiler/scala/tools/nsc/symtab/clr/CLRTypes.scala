/* NSC -- new scala compiler
 * Copyright 2004-2011 LAMP/EPFL
 */


package scala.tools.nsc
package symtab
package clr

import IKVM.Reflection.{ Type => MsilType, ConstructorInfo, MethodInfo, FieldInfo }
import scala.collection._
import util.MsilClassPath

/**
 * Collects all types from all reference assemblies.
 */
abstract class CLRTypes {

  val global: Global
  import global.Symbol

  //##########################################################################

  var SBYTE:    MsilType = _
  var UBYTE:    MsilType = _
  var SHORT:    MsilType = _
  var USHORT:   MsilType = _
  var CHAR:     MsilType = _
  var INT:      MsilType = _
  var UINT:     MsilType = _
  var LONG:     MsilType = _
  var ULONG:    MsilType = _
  var FLOAT:    MsilType = _
  var DOUBLE:   MsilType = _
  var BOOLEAN:  MsilType = _
  var VOID:     MsilType = _
  var ENUM:     MsilType = _
  var DELEGATE: MsilType = _

  var OBJECT:       MsilType = _
  var STRING:       MsilType = _
  var STRING_ARRAY: MsilType = _

  var VALUE_TYPE: MsilType = _

  var SCALA_SYMTAB_ATTR: MsilType = _
  var SYMTAB_CONSTR: ConstructorInfo = _
  var SYMTAB_DEFAULT_CONSTR: ConstructorInfo = _

  var DELEGATE_COMBINE: MethodInfo = _
  var DELEGATE_REMOVE: MethodInfo = _

  val types: mutable.Map[Symbol,MsilType]               = new mutable.HashMap
  val constructors: mutable.Map[Symbol,ConstructorInfo] = new mutable.HashMap
  val methods: mutable.Map[Symbol,MethodInfo]           = new mutable.HashMap
  val fields: mutable.Map[Symbol, FieldInfo]            = new mutable.HashMap
  val sym2type: mutable.Map[MsilType,Symbol]            = new mutable.HashMap
  val addressOfViews = new mutable.HashSet[Symbol]
  val mdgptrcls4clssym: mutable.Map[ /*cls*/ Symbol, /*cls*/ Symbol] = new mutable.HashMap

  def isAddressOf(msym : Symbol) = addressOfViews.contains(msym)

  def isNonEnumValuetype(cls: Symbol) = {
    val msilTOpt = types.get(cls)
    val res = msilTOpt.isDefined && {
      val msilT = msilTOpt.get
      msilT.IsValueType && !msilT.IsEnum
    }
    res
  }

  def isValueType(cls: Symbol): Boolean = {
    val opt = types.get(cls)
    opt.isDefined && opt.get.IsValueType
  }

  def init() = try { // initialize
    // the MsilClasspath (nsc/util/Classpath.scala) initializes the msil-library by calling
    // Assembly.LoadFrom("mscorlib.dll"), so these types should be found

    // TODO for this to work, the object util.MsilClasspath for the should have been constructed by now.

    SBYTE    = getTypeSafe("System.SByte")
    UBYTE    = getTypeSafe("System.Byte")
    CHAR     = getTypeSafe("System.Char")
    SHORT    = getTypeSafe("System.Int16")
    USHORT   = getTypeSafe("System.UInt16")
    INT      = getTypeSafe("System.Int32")
    UINT     = getTypeSafe("System.UInt32")
    LONG     = getTypeSafe("System.Int64")
    ULONG    = getTypeSafe("System.UInt64")
    FLOAT    = getTypeSafe("System.Single")
    DOUBLE   = getTypeSafe("System.Double")
    BOOLEAN  = getTypeSafe("System.Boolean")
    VOID     = getTypeSafe("System.Void")
    ENUM     = getTypeSafe("System.Enum")
    DELEGATE = getTypeSafe("System.MulticastDelegate")

    OBJECT = getTypeSafe("System.Object")
    STRING = getTypeSafe("System.String")
    STRING_ARRAY = getTypeSafe("System.String[]")
    VALUE_TYPE = getTypeSafe("System.ValueType")

    SCALA_SYMTAB_ATTR = {
      val name = "scala.runtime.SymtabAttribute"
      var t = MsilClassPath.getMsilType(name)
      if(t == null) { // TODO bootstrap case not necessary in release
        val as = MsilClassPath.assemUniverse.GetAssemblies() filter { a => 
          a.FullName.StartsWith("CSharpFilesForBootstrap") || a.FullName.StartsWith("scalalib")
        }
        val ai = as.iterator
        while(ai.hasNext && t == null) { 
          t = MsilClassPath.assemUniverse.GetType(ai.next, name, false)
        }
      }
      assert(t != null, name)
      t
    }
    SYMTAB_CONSTR = SCALA_SYMTAB_ATTR.GetConstructor(Array(UBYTE.MakeArrayType()))
    SYMTAB_DEFAULT_CONSTR = SCALA_SYMTAB_ATTR.GetConstructor(MsilType.EmptyTypes)

    val delegate: MsilType = getTypeSafe("System.Delegate")
    val dargs: Array[MsilType] = Array(delegate, delegate)
    DELEGATE_COMBINE = delegate.GetMethod("Combine", dargs)
    DELEGATE_REMOVE = delegate.GetMethod("Remove", dargs)
  }
  catch {
    case e: RuntimeException =>
      Console.println(e.Message)
      throw e
  }

  //##########################################################################
  // type mapping and lookup

  def getType(name: String): MsilType = MsilClassPath.getMsilType(name)

  private def getTypeSafe(name: String): MsilType = {
    val t = MsilClassPath.getMsilType(name)
    assert(t != null, name)
    t
  }

  def lookupEverywhere(toLookup: String): MsilType = {
    var t: MsilType = null
    val ai = MsilClassPath.assemUniverse.GetAssemblies().iterator
    while(ai.hasNext && t == null) { 
      t = MsilClassPath.assemUniverse.GetType(ai.next, toLookup, false)
    }
    t
  }

  def isDelegateType(t: MsilType): Boolean = { t.BaseType == DELEGATE }

  import IKVM.Reflection._

  def HasPtrParamOrRetType(mb: MethodBase): Boolean = {
    val iter: Iterator[ParameterInfo] = mb.GetParameters.iterator
    while (iter.hasNext) {
      var pT: Type = iter.next.ParameterType
      if (pT.IsPointer) return true;
      if (pT.IsByRef && !CanBeTakenAddressOf(pT.GetElementType())) return true;
    }
    return false;
  }

  def CanBeTakenAddressOf(mt: MsilType): Boolean = ( mt.IsValueType && (mt != ENUM) )

  def IsInstance(mb: MethodBase): Boolean = (!mb.IsStatic && !mb.IsVirtual)

}  // CLRTypes
