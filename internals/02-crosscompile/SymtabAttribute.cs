/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


namespace scala.runtime {

  using System;
  using System.Collections.Generic;

  public class SymtabAttribute : Attribute {
    public byte[] symtab;

    public SymtabAttribute(byte[] symtab) { this.symtab = symtab; }
    public SymtabAttribute() {}

    public static byte[] helperGetPickle(
        IKVM.Reflection.Type typ, 
        IKVM.Reflection.ConstructorInfo ikSymtabCtor) {
      // note: ikSymtabCtor stands here for loaders.clrTypes.SYMTAB_CONSTR
      // ie. the result of getTypeSafe("scala.runtime.SymtabAttribute").GetConstructor(Array(UBYTE.MakeArrayType()))
      IList<IKVM.Reflection.CustomAttributeData> cads = typ.__GetCustomAttributes(null, false);  // this prevents ReadFixedArg from being called
      foreach(IKVM.Reflection.CustomAttributeData cad in cads) {
        if (cad.Constructor == ikSymtabCtor) {
          byte[] blob = cad.__GetBlob();
          // blob starts with 
          //   prolog 01 00
          //   length LL LH HL HH
          // where 
          //   int pos = 2;
          //   int length = ((int) blob[pos++] | (int) blob[pos++] << 8 | (int) blob[pos++] << 16 | (int) blob[pos++] << 24);
          // and then comes the real data starting with blob[6] inclusive. That's why we give 6 as offset to unpickle.
          //   byte[] dest = new byte[length];
          //   Array.Copy(blob, 6, dest, 0, length);
          return blob;
        }
      }
      return null;
    }

    public static bool helperDoLoad(
        IKVM.Reflection.Type typ,
        IKVM.Reflection.ConstructorInfo ikSymtabCtor)
    {
      // note (1): ikSymtabAttr stands here for loaders.clrTypes.SCALA_SYMTAB_ATTR
      //           ie. the result of getTypeSafe("scala.runtime.SymtabAttribute")
      // note (2): ikSymtabCtor stands here for loaders.clrTypes.SYMTAB_CONSTR
      //           ie. the result of ikSymtabAttr.GetConstructor(Array(UBYTE.MakeArrayType()))

      IKVM.Reflection.Type ikSymtabAttr = ikSymtabCtor.DeclaringType;
      IList<IKVM.Reflection.CustomAttributeData> cads = typ.__GetCustomAttributes(null, false); // this prevents ReadFixedArg from being called
      foreach(IKVM.Reflection.CustomAttributeData cad in cads) {
        if (cad.Constructor.DeclaringType == ikSymtabAttr) {
          bool res = (cad.Constructor == ikSymtabCtor);
          return res;
        }
      }
      return true; // always load non-scala types
    }

  }

}
