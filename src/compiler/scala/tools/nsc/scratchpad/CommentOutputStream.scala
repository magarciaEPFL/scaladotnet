package scala.tools.nsc.scratchpad

import java.io.OutputStream

class CommentOutputStream(out: CommentWriter, encoding: String = "") extends OutputStream {

  override def write(bs: Array[Byte]) =
    out.write(if (_root_.java.lang.String.instancehelper_isEmpty(encoding)) java.lang.String.newhelper(bs)  else java.lang.String.newhelper(bs, encoding) )

  override def write(bs: Array[Byte], off: Int, len: Int) =
    out.write(if (_root_.java.lang.String.instancehelper_isEmpty(encoding)) java.lang.String.newhelper(bs, off, len)  else java.lang.String.newhelper(bs, off, len, encoding) )

  override def write(ch: Int) =
    write(Array(ch.toByte))

  override def close() = out.close()
  override def flush() = out.flush()
  override def Dispose { this.close }
}