/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Miguel Garcia, http://lamp.epfl.ch/~magarcia
 * @author Nikolay Mihaylov
 */

package scala.tools.nsc
package backend.msil

import scala.collection._
import scala.tools.nsc.symtab._

import IKVM.Reflection.{Type => MsilType}

abstract class GenMSIL extends SubComponent {
  import global._
  import loaders.clrTypes

  import icodes._
  import icodes.opcodes._

  override def newPhase(p: Phase) = new MsilPhase(p)
  val phaseName = "msil"

  /** MSIL code generation phase */
  class MsilPhase(prev: Phase) extends GlobalPhase(prev) {
    def name = phaseName

    override def erasedTypes = true

    override def apply(unit: CompilationUnit) {
      abort("MSIL works on icode classes, not on compilation units!")
    }

    /**
     * Several passes on all classes:
     *   (1) find the entry point (if given),
     *   (2) create a TypeBuilder for each class,
     *   (3) create class members,
     *   (4) emit bytecode for each method.
     * Finally, the assembly is written out to disk.
     */
    override def run() {

      if (settings.debug.value) inform("[running phase " + name + " on icode]")

      val codeGenerator = new BytecodeGenerator

      val entryClassName = opt.showClass.getOrElse("").ToString
      if(opt.showClass.isDefined) { // TODO introduce dedicated setting instead
        classes.values foreach { iclass => codeGenerator.findEntryPoint(iclass, entryClassName) }
      }
      val needsEntryPoint = Set("exe", "winexe") contains settings.target.value
      if(needsEntryPoint && (codeGenerator.entryPoint == null)) {
        if(opt.showClass.isDefined) warning("Couldn't find entry point in class " + entryClassName + ", emitting .dll instead.")
        else warning("No entry point was given (missing -Xshow-class), emitting .dll instead.")
      }

      codeGenerator.initAssembly

      val iclassesSorted = classes.values.toList.sortBy(c => c.symbol.id) // simplifies comparing cross-compiler vs. .exe output
      iclassesSorted foreach { iclass => codeGenerator.createTypeBuilder(iclass.symbol) }
      iclassesSorted foreach codeGenerator.createClassMembers
      iclassesSorted foreach codeGenerator.genClass
      codeGenerator.writeAssembly
    }

  } // end of MsilPhase

  class BytecodeGenerator extends Helpers {

    import IKVM.Reflection.{FieldInfo, MethodInfo};
    import IKVM.Reflection.AssemblyName
    import IKVM.Reflection.Emit.{AssemblyBuilderAccess, AssemblyBuilder, ModuleBuilder}
    import IKVM.Reflection.Emit.TypeBuilder

    /* ------------------------ (1.A) entry point of assembly ------------------------*/

    var entryPoint: Symbol = _
    var outputFileName     = ""

    val firstSourceName = (global.icodes.classes.keys find { csym =>  csym.sourceFile != null }).get.sourceFile.name;

    val notInitializedModules = mutable.HashSet[Symbol]()

    def findEntryPoint(cls: IClass, entryClassName: String) {

        def isEntryPoint(msym: Symbol):Boolean = {
          if (    isStaticModule(msym.owner)
               && msilName(msym) == "main"
               && (msym.tpe.paramTypes.length == 1) ) {
            val tk = toTypeKind(msym.tpe.paramTypes(0))
            tk match {
              case ARRAY(elem)
              if (elem.toType.typeSymbol == definitions.StringClass) => true
              case _ => false
            }
          } else false
        }

      if(entryPoint == null) {
        val cfn = cls.symbol.fullName
        if(cfn == entryClassName) {
          for (m <- cls.methods; if isEntryPoint(m.symbol)) { entryPoint = m.symbol }
          if(entryPoint != null) { inform("Found main method in class " + cfn) }
        }
      }

    }

    /* ------------------------ (1.B) assembly initialization ------------------------*/

    var assemName: String = _
    val outDir: java.io.File = {
      val dirName =
        if(settings.outdir.isSetByUser && (settings.outdir.value != ".") ) settings.outdir.value
        else System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location);

      new java.io.File(dirName)
    }

    var massembly: AssemblyBuilder = _
    var mmodule: ModuleBuilder = _

    def initAssembly() {

      assemName = settings.assemname.value

      if (assemName == "") {
        if (entryPoint != null) {
          assemName = msilName(entryPoint.enclClass)
          // remove the $ at the end (from module-name)
          assemName = assemName.Substring(0, assemName.Length - 1)
        } else {
          // take assembly name to be the filename of first source file
          assert(firstSourceName.EndsWith(".scala"), firstSourceName)
          assemName = firstSourceName.Substring(0, firstSourceName.Length - 6)
        }
      } else {
        if (assemName.EndsWith(".dll") || assemName.EndsWith(".exe")) {
          assemName = assemName.Substring(0, assemName.Length-4)
        }
        val f: java.io.File = new java.io.File(assemName)
        assemName = f.getName()
      }

      // val srcPath = new java.io.File(settings.sourcedir.value)
      val fileExtension =
        if(entryPoint == null) {
          if(settings.target.value == "module") ".netmodule"
          else ".dll"
        } else {
          settings.target.value match {
            case "exe" | "winexe" => ".exe"
            case "library"        => ".dll"
            case "module"         => ".netmodule"
          }
        }

      outputFileName   = outDir.getPath() + java.io.File.separatorChar + assemName + fileExtension

      val assemblyName = new AssemblyName(assemName)
      massembly =
        ikvmuniverse.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Save, outDir.getPath)
      // TODO massembly.__SetAssemblyFlags
      // Mono's mcs does it differently, not hardcoded but based on the runtime on which the compiler runs.
      // http://permalink.gmane.org/gmane.gnome.mono.patches/176029
      massembly.__SetImageRuntimeVersion("v4.0.30319", 0x20000)

      /* Quoting from docs: "If you want the module to contain the assembly manifest, name should be the same as the name of the assembly ...
         and filename should be the same as the filename you specify when you save the assembly" */
      mmodule = massembly.DefineDynamicModule(assemName, outputFileName, true /* instructs to track debug info */)

    } // end of initAssembly

    /* ------------------------ (2) create type builders ------------------------*/

    override def createTypeBuilder(csym: Symbol) {
      if(typebldrs.isDefinedAt(csym)) { return; }

      val parents: List[Type] =
        if (csym.info.parents.isEmpty) List(definitions.ObjectClass.tpe)
        else csym.info.parents.distinct

      val isIface = (csym.isTrait && !csym.isImplClass)
      val superType : MsilType = if (isIface) null else getReflType(parents.head.typeSymbol);

      val minIfaces = minimizeInterfaces(parents.tail map (p => p.typeSymbol))
      val interfaces: Array[MsilType] = minIfaces.map(ts => getReflType(ts)).toArray

      val tBuilder =
        if (csym.isNestedClass) {
          val ownerT = getTypeBuilder(csym.owner)
          ownerT.DefineNestedType(msilName(csym), msilTypeFlags(csym), superType, interfaces)
        } else {
          mmodule.DefineType(msilName(csym), msilTypeFlags(csym), superType, interfaces)
        }

      typebldrs(csym) = tBuilder

    } // end of createTypeBuilder

    /** Drop redundant interfaces (ones which are implemented by some
     *  other parent) from the immediate parents.  This is important on
     *  android because there is otherwise an interface explosion.
     */
    private def minimizeInterfaces(interfaces: List[Symbol]): List[Symbol] = {
      // TODO a superclass may already extend interfaces also extended by this type.
      // TODO can we minimize interfaces already in GenICode?
      var rest   = interfaces
      var leaves = List.empty[Symbol]
      while(!rest.isEmpty) {
        val candidate = rest.head
        val nonLeaf = leaves exists { lsym => lsym isSubClass candidate }
        if(!nonLeaf) {
          leaves = candidate :: (leaves filterNot { lsym => candidate isSubClass lsym })
        }
        rest = rest.tail
      }

      leaves
    }

    /* ------------------------ (3) create class members ------------------------*/

    def createClassMembers(iclass: IClass) {

      if (iclass.symbol eq definitions.ArrayClass) { return; }

      val tBuilder = typebldrs(iclass.symbol)

      for (ifield <- iclass.fields) {
        val fsym     = ifield.symbol
        val fname    = msilName(fsym)
        val ftype    = msilType(toTypeKind(fsym.tpe))
        val attribs  = msilFieldFlags(fsym)
        val reqMods  = requiredCustomMods(fsym.annotations)
        val fBuilder = tBuilder.DefineField(fname, ftype, reqMods, Array(), attribs)
        // TODO addAttributes(fBuilder, fsym.annotations)
        fldbldrs(fsym) = fBuilder
      }

      if (isStaticModule(iclass.symbol)) {
        val sc = iclass.lookupStaticCtor
        if (sc.isDefined) {
          val m = sc.get
          val oldLastBlock = m.lastBlock
          val lastBlock = m.newBlock()
          oldLastBlock.replaceInstruction(oldLastBlock.length - 1, JUMP(lastBlock))
          // call object's private ctor from static ctor
          lastBlock.emit(CIL_NEWOBJ(iclass.symbol.primaryConstructor))
          lastBlock.emit(DROP(toTypeKind(iclass.symbol.tpe)))
          lastBlock emit RETURN(UNIT)
          lastBlock.close
        }
      }

      for (imethod <- iclass.methods) {
        val msym = imethod.symbol

        val ownerType = typebldrs(msym.enclClass)
        assert(tBuilder == ownerType, "mtype = " + tBuilder + "; ownerType = " + ownerType)
        val paramTypes = msilParamTypes(msym)
        val attribs = msilMethodFlags(msym)

        import IKVM.Reflection.{CallingConventions, ParameterAttributes}

        if (msym.isClassConstructor) {
          val constrBldr = ownerType.DefineConstructor(attribs, CallingConventions.Standard, paramTypes)
          for (i <- 0.until(paramTypes.length)) {
            val pname = msilName(imethod.params(i).sym)
            // DefineParam expects the first param to have number 1 as index; number 0 represents the return value.
            constrBldr.DefineParameter(i + 1, ParameterAttributes.None, pname)
          }
          // TODO addAttributes(constrBldr, msym.annotations)
          constrbldrs(msym) = constrBldr
        } else {
          var resType = msilType(imethod.returnType)
          val methodBldr = ownerType.DefineMethod(msilName(msym), attribs, resType, paramTypes)
          for (i <- 0.until(paramTypes.length)) {
            val pname = msilName(imethod.params(i).sym)
            // DefineParam expects the first param to have number 1 as index; number 0 represents the return value.
            methodBldr.DefineParameter(i + 1, ParameterAttributes.None, pname)
          }
          // TODO addAttributes(methodBldr, msym.annotations)
          methodbldrs(msym) = methodBldr
        }
      }

      if (isStaticModule(iclass.symbol)) {
        import IKVM.Reflection.FieldAttributes;
        // add field for module instance
        val modsym = iclass.symbol
        val tBuilder = getTypeBuilder(modsym)
        val attribs: FieldAttributes = (FieldAttributes.Public | /* FieldAttributes.InitOnly | */ FieldAttributes.Static)
        val fb = tBuilder.DefineField(MODULE_INSTANCE_NAME, tBuilder, attribs)
        fldbldrs(modsym) = fb

        notInitializedModules += modsym

        if (iclass.lookupStaticCtor.isEmpty) { addStaticInit(modsym) }
      }

    } // end of createClassMembers

    /* ------------------------ (4) emit bytecode ------------------------*/

    var currIClazz : IClass  = _
    var currIMethod: IMethod = _
    var currDoc : System.Diagnostics.SymbolStore.ISymbolDocumentWriter = _
    val localBuilders = mutable.HashMap[Local, IKVM.Reflection.Emit.LocalBuilder]()
    var mcode: IKVM.Reflection.Emit.ILGenerator = _

    def genClass(iclass: IClass) {
      currIClazz = iclass
      val tBuilder = getTypeBuilder(iclass.symbol)
      // IMethod.sourceFile contains just the filename
      val ilasmFileName = _root_.java.lang.String.instancehelper_replace(iclass.cunit.source.file.ToString, "\\", "\\\\")
      currDoc =
        mmodule.DefineDocument(ilasmFileName,
                                /* language */       System.Guid.Empty,
                                /* languageVendor */ System.Guid.Empty,
                                /* documentType */   System.Diagnostics.SymbolStore.SymDocumentType.Text)

      // skipping generating Clone method

      if (isTopLevelModule(iclass.symbol)) {
        if (iclass.symbol.companionClass == NoSymbol)
          generateMirrorClass(iclass)
        else
          log("No mirror class for module with linked class: " + iclass.symbol.fullName)
      }

      addSymtabAttribute(iclass.symbol, tBuilder)
      // TODO addAttributes(tBuilder, iclass.symbol.annotations)

      if (iclass.symbol != definitions.ArrayClass) {
        iclass.methods foreach genMethod
      }

    } // end of genClass

    /**
     *  modsym is a module-symbol without a companion class.
     *  The "mirror class" added here is not cosmetic because
     *  it gets a pickle for the sourceModule.
     */
    private def generateMirrorClass(iclass: IClass) {

      import IKVM.Reflection.{MethodAttributes, ParameterAttributes, TypeAttributes}
      import IKVM.Reflection.Emit.OpCodes;

      val modsym = iclass.symbol
      val tBuilder = getTypeBuilder(modsym)
      assert(modsym.isModuleClass, "Can't generate mirror-class for non-module class " + modsym)
      val moduleName = msilName(modsym)
      val mirrorName = _root_.java.lang.String.instancehelper_substring(moduleName, 0, _root_.java.lang.String.instancehelper_length(moduleName) - 1)
      val cattribs = (TypeAttributes.Class | TypeAttributes.Public | TypeAttributes.Sealed)
      val mirrorTypeBuilder =
        mmodule.DefineType(mirrorName, cattribs, MOBJECT, EmptyTypesArr)

      /* TODO In order to emit debug info, we need instruction positions, which aren't there. */

      for (m <- modsym.tpe.nonPrivateMembers
           if m.owner != definitions.ObjectClass && !m.isProtected &&
           m.isMethod && !m.isClassConstructor && !m.isStaticMember && !m.isCase &&
           !m.isDeferred)
        {
          val paramTypes = msilParamTypes(m)
          val paramNames: Array[String] = new Array[String](paramTypes.length)
          for (i <- 0 until paramTypes.length) { paramNames(i) = "x_" + i }

          // CHECK: verify if getMethodName is better than msilName
          val mattribs = (MethodAttributes.Public | MethodAttributes.Static)
          val mirrorMethod =
            mirrorTypeBuilder.DefineMethod(msilName(m), mattribs, msilType(toTypeKind(m.tpe.resultType)), paramTypes)

          var i = 0
          while (i < paramTypes.length) {
            mirrorMethod.DefineParameter(i+1, ParameterAttributes.None, paramNames(i))
            i += 1
          }

          val mirrorCode = mirrorMethod.GetILGenerator()
          mirrorCode.Emit(OpCodes.Ldsfld, getModuleInstanceField(modsym))
          val mInfo = getMethodInfo(m)
          for (paramidx <- 0.until(paramTypes.length)) {
            val mInfoParams = mInfo.GetParameters
            val loadAddr = mInfoParams(paramidx).ParameterType.IsByRef
            loadArg(mirrorCode, loadAddr)(paramidx)
          }

          mirrorCode.Emit(OpCodes.Callvirt, getMethodInfo(m))
          mirrorCode.Emit(OpCodes.Ret)
        }

      addSymtabAttribute(modsym.sourceModule, mirrorTypeBuilder)
      // no pre-req for CreateType invocation because the mirror class has no parents other than Object
      mirrorTypeBuilder.CreateType()

    } // end of generateMirrorClass

    /**
     * Form of the custom Attribute parameter (Ecma-335.pdf)
     *      - p. 163 for CustomAttrib Form,
     *      - p. 164 for FixedArg Form (Array and Element) (if array or not is known!)
     *  !! least significant byte first if values longer than one byte !!
     *
     * 1: Prolog (unsigned int16, value 0x0001) -> symtab[0] = 0x01, symtab[1] = 0x00
     * 2: FixedArgs (directly the data, get number and types from related constructor)
     *  2.1: length of the array (unsigned int32, 4 bytes, least significant first)
     *  2.2: the byte array data
     * 3: NumNamed (unsigned int16, number of named fields and properties, 0x0000)
     */
    private def addSymtabAttribute(csym: Symbol, tBuilder: TypeBuilder) {

        def addMarker() {
          val markerSymtab = new Array[Byte](4)
          markerSymtab(0) = 1.toByte
          tBuilder.SetCustomAttribute(SYMTAB_ATTRIBUTE_EMPTY_CONSTRUCTOR, markerSymtab)
        }

      // both conditions are needed (why exactly..?)
      if (_root_.java.lang.String.instancehelper_endsWith(tBuilder.Name, "$") || csym.isModuleClass) {
        addMarker()
      } else {
        currentRun.symData.get(csym) match {
          case Some(pickle) =>
            var size = pickle.writeIndex
            val symtab = new Array[Byte](size + 8)
            symtab(0) = 1.toByte
            for (i <- 2 until 6) {
              symtab(i) = (size & 0xff).toByte
              size = size >> ((8) & 0x1f)
            }
            java.lang.System.arraycopy(pickle.bytes, 0, symtab, 6, pickle.writeIndex)

            tBuilder.SetCustomAttribute(SYMTAB_ATTRIBUTE_CONSTRUCTOR, symtab)

            currentRun.symData -= csym
            currentRun.symData -= csym.companionSymbol

          case _ =>
            addMarker()
        }
      }
    }

    private def genMethod(m: IMethod) {
      currIMethod = m
      localBuilders.clear
      computeLocalVarsIndex(m)

      mcode =
        if (m.symbol.isClassConstructor) {
          constrbldrs(m.symbol).GetILGenerator()
        } else {
          val mBuilder = methodbldrs(m.symbol)
          import IKVM.Reflection.MethodImplAttributes;
          var mflags = ( MethodImplAttributes.IL | MethodImplAttributes.Managed )
          if (m.symbol.hasFlag(Flags.SYNCHRONIZED)) {
            mflags |= MethodImplAttributes.Synchronized
          }
          mBuilder.SetImplementationFlags(mflags) // "Call the SetImplementationFlags method before you call the SetCustomAttribute method."
          if (!mBuilder.IsAbstract) mBuilder.GetILGenerator()
          else null
        }

      if (mcode != null) {
        for (local <- m.locals ; if !(m.params contains local)) {
          val t: MsilType = msilType(local.kind)
          val localBuilder = mcode.DeclareLocal(t)
          localBuilder.SetLocalSymInfo(msilName(local.sym))
          localBuilders(local) = localBuilder
        }
        genCodeForCurrentMethod()
      }

    } // end of genMethod

    private def computeLocalVarsIndex(m: IMethod) {
      var idx = if (m.symbol.isStaticMember) 0 else 1
      val params = m.params
      for (l <- params) { l.index = idx; idx += 1 }

      idx = 0
      val locvars = m.locals filterNot (params contains)
      for (l <- locvars) { l.index = idx; idx += 1 }
    }

    /** Special linearizer for methods with at least one exception handler.
     *  This linearizer brings all basic blocks in the right order so that
     *  nested try-catch and try-finally blocks can be emitted.
     */
    val msilLinearizer = new MSILLinearizer()

    val labels = mutable.HashMap[BasicBlock, IKVM.Reflection.Emit.Label]()

    import IKVM.Reflection.Emit.{ILGenerator, Label, OpCodes}

    def genCodeForCurrentMethod() {

      labels.clear

      var linearization =
        if(!currIMethod.exh.isEmpty) msilLinearizer.linearize(currIMethod)
        else linearizer.linearize(currIMethod)

      if (!currIMethod.exh.isEmpty) {
        // TODO move this instr into the then-branch above if that branch doesn't side-effect "!currIMethod.exh.isEmpty"
        linearization = computeExceptionMaps(linearization, currIMethod)
      }

      for (bb <- linearization) { labels(bb) = mcode.DefineLabel() }

      // debug val blocksInM = m.code.blocks.toList.sortBy(bb => bb.label)
      // debug val blocksInL = linearization.sortBy(bb => bb.label)
      // debug val MButNotL  = (blocksInM.toSet) diff (blocksInL.toSet) // if non-empty, a jump to B fails to find a label for B (case CJUMP, case CZJUMP)
      // debug if(!MButNotL.isEmpty) { }

      try {
        genBlocks(linearization)
      } catch {
        case ex: System.NotSupportedException if (ex.Source == "IKVM.Reflection") => 
          // for example, 'Backward branch constraints' violated
          val locMsg = 
            if(currIMethod.symbol.pos.isDefined) ""
            else " for method " + currIMethod.ToString();
          currIClazz.cunit.error(currIMethod.symbol.pos,  "(in GenMSIL phase) " + ex.Message + locMsg)
      }

      // RETURN inside exception blocks are replaced by Leave. The target of the
      // leave is a `Ret` outside any exception block (generated here).
      if (handlerReturnMethod == currIMethod) {
        mcode.MarkLabel(handlerReturnLabel)
        if (handlerReturnKind != UNIT) {
          mcode.Emit(OpCodes.Ldloc, handlerReturnLocal)
        }
        mcode.Emit(OpCodes.Ret)
      }

      beginExBlock.clear()
      beginCatchBlock.clear()
      endExBlock.clear()
      endFinallyLabels.clear()
    }


    def genBlocks(blocks: List[BasicBlock], previous: BasicBlock = null) {
      blocks match {
        case Nil => ()
        case x :: Nil     => genBlock(x, prev = previous, next = null)
        case x :: y :: ys => genBlock(x, prev = previous, next = y); genBlocks(y :: ys, previous = x)
      }
    }

    // the try blocks starting at a certain BasicBlock
    val beginExBlock = mutable.HashMap[BasicBlock, List[ExceptionHandler]]()

    // the catch blocks starting / endling at a certain BasicBlock
    val beginCatchBlock = mutable.HashMap[BasicBlock, ExceptionHandler]()
    val endExBlock = mutable.HashMap[BasicBlock, List[ExceptionHandler]]()

    /** When emitting the code (genBlock), the number of currently active try / catch
     *  blocks. When seeing a `RETURN` inside a try / catch, we need to
     *   - store the result in a local (if it's not UNIT)
     *   - emit `Leave handlerReturnLabel` instead of the Return
     *   - emit code at the end: load the local and return its value
     */
    var currentHandlers = new mutable.Stack[ExceptionHandler]
    // The IMethod the Local/Label/Kind below belong to
    var handlerReturnMethod: IMethod = _
    // Stores the result when returning inside an exception block
    var handlerReturnLocal: IKVM.Reflection.Emit.LocalBuilder = _
    // Label for a return instruction outside any exception block
    var handlerReturnLabel: IKVM.Reflection.Emit.Label = _
    // The result kind.
    var handlerReturnKind: TypeKind = _

    def returnFromHandler(kind: TypeKind): (IKVM.Reflection.Emit.LocalBuilder, IKVM.Reflection.Emit.Label) = {
      if (handlerReturnMethod != currIMethod) {
        handlerReturnMethod = currIMethod
        if (kind != UNIT) {
          handlerReturnLocal = mcode.DeclareLocal(msilType(kind))
          handlerReturnLocal.SetLocalSymInfo("$handlerReturn")
        }
        handlerReturnLabel = mcode.DefineLabel()
        handlerReturnKind = kind
      }
      (handlerReturnLocal, handlerReturnLabel)
    }

    /** For try/catch nested inside a finally, we can't use `Leave OutsideFinally`, the
     *  Leave target has to be inside the finally (and it has to be the `endfinally` instruction).
     *  So for every finalizer, we have a label which marks the place of the `endfinally`,
     *  nested try/catch blocks will leave there.
     */
    val endFinallyLabels = mutable.HashMap[ExceptionHandler, IKVM.Reflection.Emit.Label]()

    /** Computes which blocks are the beginning / end of a try or catch block */
    private def computeExceptionMaps(blocks: List[BasicBlock], m: IMethod): List[BasicBlock] = {
      val visitedBlocks = new mutable.HashSet[BasicBlock]()

      // handlers which have not been introduced so far
      var openHandlers = m.exh


      /** Example
       *   try {
       *     try {
       *         // *1*
       *     } catch {
       *       case h1 =>
       *     }
       *   } catch {
       *     case h2 =>
       *     case h3 =>
       *       try {
       *
       *       } catch {
       *         case h4 =>  // *2*
       *         case h5 =>
       *       }
       *   }
       */

      // Stack of nested try blocks. Each bloc has a List of ExceptionHandler
      // (multiple catch statements). Example *1*: Stack(List(h2, h3), List(h1))
      val currentTryHandlers = new mutable.Stack[List[ExceptionHandler]]()

      // Stack of nested catch blocks. The head of the list is the current catch block.
      // The tail is all following catch blocks. Example *2*: Stack(List(h3), List(h4, h5))
      val currentCatchHandlers = new mutable.Stack[List[ExceptionHandler]]()

      for (b <- blocks) {

        // are we past the current catch blocks?
        def endHandlers(): List[ExceptionHandler] = {
          var res: List[ExceptionHandler] = Nil
          if (!currentCatchHandlers.isEmpty) {
            val handler = currentCatchHandlers.top.head
            if (!handler.blocks.contains(b)) {
              // all blocks of the handler are either visited, or not part of the linearization (i.e. dead)
              assert(handler.blocks.forall(b => visitedBlocks.contains(b) || !blocks.contains(b)),
                     "Bad linearization of basic blocks inside catch. Found block not part of the handler\n"+
                     b.fullString +"\nwhile in catch-part of\n"+ handler)

              val rest = currentCatchHandlers.pop.tail
              if (rest.isEmpty) {
                // all catch blocks of that exception handler are covered
                res = handler :: endHandlers()
              } else {
                // there are more catch blocks for that try (handlers covering the same)
                currentCatchHandlers.push(rest)
                beginCatchBlock(b) = rest.head
              }
            }
          }
          res
        }
        val end = endHandlers()
        if (!end.isEmpty) endExBlock(b) = end

        // are we past the current try block?
        if (!currentTryHandlers.isEmpty) {
          val handler = currentTryHandlers.top.head
          if (!handler.covers(b)) {
            // all of the covered blocks are visited, or not part of the linearization
            assert(handler.covered.forall(b => visitedBlocks.contains(b) || !blocks.contains(b)),
                   "Bad linearization of basic blocks inside try. Found non-covered block\n"+
                   b.fullString +"\nwhile in try-part of\n"+ handler)

            assert(handler.startBlock == b,
                   "Bad linearization of basic blocks. The entry block of a catch does not directly follow the try\n"+
                   b.fullString +"\n"+ handler)

            val handlers = currentTryHandlers.pop
            currentCatchHandlers.push(handlers)
            beginCatchBlock(b) = handler
          }
        }

        // are there try blocks starting at b?
        val (newHandlers, stillOpen) = openHandlers.partition(_.covers(b))
        openHandlers = stillOpen

        val newHandlersBySize = newHandlers.groupBy(_.covered.size)
        // big handlers first, smaller ones are nested inside the try of the big one
        // (checked by the assertions below)
        val sizes = newHandlersBySize.keys.toList.sortWith(_ > _)

        val beginHandlers = new mutable.ListBuffer[ExceptionHandler]
        for (s <- sizes) {
          val sHandlers = newHandlersBySize(s)
          for (h <- sHandlers) {
            assert(h.covered == sHandlers.head.covered,
                   "bad nesting of exception handlers. same size, but not covering same blocks\n"+
                   h +"\n"+ sHandlers.head)
            assert(h.resultKind == sHandlers.head.resultKind,
                   "bad nesting of exception handlers. same size, but the same resultKind\n"+
                   h +"\n"+ sHandlers.head)
          }
          for (bigger <- beginHandlers; h <- sHandlers) {
            assert(h.covered.subsetOf(bigger.covered),
                   "bad nesting of exception handlers. try blocks of smaller handler are not nested in bigger one.\n"+
                   h +"\n"+ bigger)
            assert(h.blocks.toSet.subsetOf(bigger.covered),
                   "bad nesting of exception handlers. catch blocks of smaller handler are not nested in bigger one.\n"+
                   h +"\n"+ bigger)
          }
          beginHandlers += sHandlers.head
          currentTryHandlers.push(sHandlers)
        }
        beginExBlock(b) = beginHandlers.toList
        visitedBlocks += b
      }

      // if there handlers left (i.e. handlers covering nothing, or a
      // non-existent (dead) block), remove their catch-blocks.
      val liveBlocks = if (openHandlers.isEmpty) blocks else {
        blocks.filter(b => openHandlers.forall(h => !h.blocks.contains(b)))
      }

      /** There might be open handlers, but no more blocks.
       *  This happens when try/catch end with `throw` or `return`
       *     def foo() { try { .. throw } catch { _ => .. throw } }
       *
       *  In this case we need some code after the catch block for the auto-generated `leave` instruction.
       *  So we're adding a (dead) `throw new Exception`.
       */
      val rest = currentCatchHandlers.map(handlers => {
        assert(handlers.length == 1, handlers)
        handlers.head
      }).toList

      if (rest.isEmpty) {
        liveBlocks
      } else {
        val b = m.code.newBlock
        b.emit(Seq(
          NEW(REFERENCE(definitions.ThrowableClass)),
          DUP(REFERENCE(definitions.ObjectClass)),
          CALL_METHOD(definitions.ThrowableClass.primaryConstructor, Static(true)),
          THROW(definitions.ThrowableClass)
        ))
        b.close
        endExBlock(b) = rest
        liveBlocks ::: List(b)
      }
    } // end of computeExceptionMaps

    /**
     *  @param block the BasicBlock to emit code for
     *  @param next  the following BasicBlock, `null` if `block` is the last one
     */
    def genBlock(block: BasicBlock, prev: BasicBlock, next: BasicBlock) {

      def loadLocalOrAddress(local: Local, msg : String , loadAddr : Boolean) {
        val isArg = local.arg
        val i = local.index
        if (isArg)
          loadArg(mcode, loadAddr)(i)
        else
          loadLocal(i, local, mcode, loadAddr)
      }

      def loadFieldOrAddress(field: Symbol, isStatic: Boolean, msg: String, loadAddr : Boolean) {
        var fieldInfo = getFieldInfo(field)
        if (isVolatile(fieldInfo)) {
          mcode.Emit(OpCodes.Volatile)
        }
        if (!fieldInfo.IsLiteral) {
          if (loadAddr) {
            mcode.Emit(if (isStatic) OpCodes.Ldsflda else OpCodes.Ldflda, fieldInfo)
          } else {
            mcode.Emit(if (isStatic) OpCodes.Ldsfld else OpCodes.Ldfld, fieldInfo)
          }
        } else {
          assert(!loadAddr, "can't take AddressOf a literal field (not even with readonly. prefix) because no memory was allocated to such field ...")
          // TODO the above can be overcome by loading the value, boxing, and finally unboxing. An address to a copy of the raw value will be on the stack.
         /*  We perform `field inlining' as required by CLR.
          *  Emit as for a CONSTANT ICode stmt, with the twist that the constant value is available
          *  as a java.lang.Object and its .NET type allows constant initialization in CLR, i.e. that type
          *  is one of I1, I2, I4, I8, R4, R8, CHAR, BOOLEAN, STRING, or CLASS (in this last case,
          *  only accepting nullref as value). See Table 9-1 in Lidin's book on ILAsm. */
          val value = fieldInfo.GetRawConstantValue
          if (value == null) {
            mcode.Emit(OpCodes.Ldnull)
          } else {
            val typ =
              if (fieldInfo.FieldType.IsEnum) {
                // now comes a .NET 4.0 API call, and UnderlyingSystemType doesn't do the trick. Can another (old) API be used instead?
                fieldInfo.FieldType.GetEnumUnderlyingType
              } else {
                fieldInfo.FieldType
              }
            if (typ == MSTRING) {
              mcode.Emit(OpCodes.Ldstr, value.asInstanceOf[String])
            } else if (typ == MBOOL) {
                mcode.Emit(if (value.asInstanceOf[Boolean]) OpCodes.Ldc_I4_1
                           else OpCodes.Ldc_I4_0)
            } else if (typ == MSBYTE || typ == MUBYTE) {
              loadI4(value.asInstanceOf[Byte], mcode)
            } else if (typ == MSSHORT || typ == MUSHORT) {
              loadI4(value.asInstanceOf[Int], mcode)
            } else if (typ == MCHAR) {
              loadI4(value.asInstanceOf[Char], mcode)
            } else if (typ == MINT || typ == MUINT) {
              loadI4(value.asInstanceOf[Int], mcode)
            } else if (typ == MLONG || typ == MULONG) {
              mcode.Emit(OpCodes.Ldc_I8, value.asInstanceOf[Long])
            } else if (typ == MFLOAT) {
              mcode.Emit(OpCodes.Ldc_R4, value.asInstanceOf[Float])
            } else if (typ == MDOUBLE) {
              mcode.Emit(OpCodes.Ldc_R8, value.asInstanceOf[Double])
            } else {
              /* TODO one more case is described in Partition II, 16.2: bytearray(...) */
              abort("Unknown type for static literal field: " + fieldInfo)
            }
          }
        }
      }

      /** Creating objects works differently on .NET. On the JVM
       *  - NEW(type) => reference on Stack
       *  - DUP, load arguments, CALL_METHOD(constructor)
       *
       * On .NET, the NEW and DUP are ignored, but we emit a special method call
       *  - load arguments
       *  - NewObj(constructor) => reference on stack
       *
       * This variable tells whether the previous instruction was a NEW,
       * we expect a DUP which is not emitted. */
      var previousWasNEW = false

      var lastLineNr: Int = 0
      var lastPos: Position = NoPosition

      // EndExceptionBlock must happen before MarkLabel because it adds the Leave instruction.
      // Otherwise, labels(block) points to the Leave (inside the catch) instead of the instruction afterwards.
      for (handlers <- endExBlock.get(block); exh <- handlers) {
        currentHandlers.pop()
        for (l <- endFinallyLabels.get(exh))
          mcode.MarkLabel(l)
        mcode.EndExceptionBlock()
      }

      mcode.MarkLabel(labels(block))

      for (handler <- beginCatchBlock.get(block)) {
        if (!currentHandlers.isEmpty && currentHandlers.top.covered == handler.covered) {
          currentHandlers.pop()
          currentHandlers.push(handler)
        }
        if (handler.cls == NoSymbol) {
          // `finally` blocks are represented the same as `catch`, but with no catch-type
          mcode.BeginFinallyBlock()
        } else {
          val t = getReflType(handler.cls)
          mcode.BeginCatchBlock(t)
        }
      }
      for (handlers <- beginExBlock.get(block); exh <- handlers) {
        currentHandlers.push(exh)
        mcode.BeginExceptionBlock()
      }

      for (instr <- block) {

        if(instr.pos.isDefined) {
          val iPos = instr.pos
          val currentLineNr = iPos.line
          val skip = if(iPos.isRange) iPos.sameRange(lastPos) else (currentLineNr == lastLineNr);
          if(!skip) {
            if(iPos.isRange) {
              val startLine = iPos.focusStart.line
              val endLine   = iPos.focusEnd.line
              val startCol  = iPos.focusStart.column
              val endCol    = iPos.focusEnd.column
              mcode.MarkSequencePoint(currDoc, startLine, startCol, endLine, endCol)
            } else {
              val linPos = iPos.toSingleLine
              val linNr  = linPos.line
              val endCol = linPos.endOrPoint
              mcode.MarkSequencePoint(currDoc, linNr, 0, linNr, endCol)
            }
            lastLineNr = currentLineNr
            lastPos = iPos
          }
        }

        if (previousWasNEW) { assert(instr.isInstanceOf[DUP], block) }

        instr match {
          case THIS(_) =>
            mcode.Emit(OpCodes.Ldarg_0)

          case CONSTANT(const) =>
            const.tag match {
              case UnitTag    => ()
              case BooleanTag => mcode.Emit(if (const.booleanValue) OpCodes.Ldc_I4_1
                                            else OpCodes.Ldc_I4_0)
              case ByteTag    => loadI4(const.byteValue, mcode)
              case ShortTag   => loadI4(const.shortValue, mcode)
              case CharTag    => loadI4(const.charValue, mcode)
              case IntTag     => loadI4(const.intValue, mcode)
              case LongTag    => mcode.Emit(OpCodes.Ldc_I8, const.longValue)
              case FloatTag   => mcode.Emit(OpCodes.Ldc_R4, const.floatValue)
              case DoubleTag  => mcode.Emit(OpCodes.Ldc_R8, const.doubleValue)
              case StringTag  => mcode.Emit(OpCodes.Ldstr, const.stringValue)
              case NullTag    => mcode.Emit(OpCodes.Ldnull)
              case ClassTag   =>
                mcode.Emit(OpCodes.Ldtoken, msilType(toTypeKind(const.typeValue)))
                mcode.Emit(OpCodes.Call, TYPE_FROM_HANDLE)
              case _          => abort("Unknown constant value: " + const)
            }

          case LOAD_ARRAY_ITEM(kind) =>
            (kind: @unchecked) match {
              case BOOL           => mcode.Emit(OpCodes.Ldelem_I1)
              case BYTE           => mcode.Emit(OpCodes.Ldelem_I1) // I1 for System.SByte, i.e. a scala.Byte
              case SHORT          => mcode.Emit(OpCodes.Ldelem_I2)
              case CHAR           => mcode.Emit(OpCodes.Ldelem_U2)
              case INT            => mcode.Emit(OpCodes.Ldelem_I4)
              case LONG           => mcode.Emit(OpCodes.Ldelem_I8)
              case FLOAT          => mcode.Emit(OpCodes.Ldelem_R4)
              case DOUBLE         => mcode.Emit(OpCodes.Ldelem_R8)
              case REFERENCE(cls) => mcode.Emit(OpCodes.Ldelem_Ref)
              case ARRAY(elem)    => mcode.Emit(OpCodes.Ldelem_Ref)

              // case UNIT is not possible: an Array[Unit] will be an
              //  Array[scala.runtime.BoxedUnit] (-> case REFERENCE)
            }

          case LOAD_LOCAL(local) => loadLocalOrAddress(local, "load_local", false)

          case CIL_LOAD_LOCAL_ADDRESS(local) => loadLocalOrAddress(local, "cil_load_local_address", true)

          case LOAD_FIELD(field, isStatic) => loadFieldOrAddress(field, isStatic, "load_field", false)

          case CIL_LOAD_FIELD_ADDRESS(field, isStatic) => loadFieldOrAddress(field, isStatic, "cil_load_field_address", true)

          case CIL_LOAD_ARRAY_ITEM_ADDRESS(kind) => mcode.Emit(OpCodes.Ldelema, msilType(kind))

          case CIL_NEWOBJ(msym) =>
            assert(msym.isClassConstructor)
            val constructorInfo: IKVM.Reflection.ConstructorInfo = getConstrInfo(msym)
            mcode.Emit(OpCodes.Newobj, constructorInfo)

          case LOAD_MODULE(module) =>
            mcode.Emit(OpCodes.Ldsfld, getModuleInstanceField(module))

          case STORE_ARRAY_ITEM(kind) =>
            (kind: @unchecked) match {
              case BOOL           => mcode.Emit(OpCodes.Stelem_I1)
              case BYTE           => mcode.Emit(OpCodes.Stelem_I1)
              case SHORT          => mcode.Emit(OpCodes.Stelem_I2)
              case CHAR           => mcode.Emit(OpCodes.Stelem_I2)
              case INT            => mcode.Emit(OpCodes.Stelem_I4)
              case LONG           => mcode.Emit(OpCodes.Stelem_I8)
              case FLOAT          => mcode.Emit(OpCodes.Stelem_R4)
              case DOUBLE         => mcode.Emit(OpCodes.Stelem_R8)
              case REFERENCE(cls) => mcode.Emit(OpCodes.Stelem_Ref)
              case ARRAY(elem)    => mcode.Emit(OpCodes.Stelem_Ref) // @TODO: test this! (occurs when calling a Array[Object]* vararg param method)

              // case UNIT not possible (see comment at LOAD_ARRAY_ITEM)
            }

          case STORE_LOCAL(local) =>
            val isArg = local.arg
            val i = local.index

            // there are some locals defined by the compiler that are isArg and that need to be stored.
            if (isArg) {
              if (i >= -128 && i <= 127)
                mcode.Emit(OpCodes.Starg_S, i)
              else
                mcode.Emit(OpCodes.Starg, i)
            } else {
              i match {
                case 0 => mcode.Emit(OpCodes.Stloc_0)
                case 1 => mcode.Emit(OpCodes.Stloc_1)
                case 2 => mcode.Emit(OpCodes.Stloc_2)
                case 3 => mcode.Emit(OpCodes.Stloc_3)
                case _      =>
                  if (i >= -128 && i <= 127)
                    mcode.Emit(OpCodes.Stloc_S, localBuilders(local))
                  else
                    mcode.Emit(OpCodes.Stloc, localBuilders(local))
              }
            }

          case STORE_THIS(_) =>
            // this only works for impl classes because the self parameter comes first
            // in the method signature. If that changes, this code has to be revisited.
            mcode.Emit(OpCodes.Starg_S, 0)

          case STORE_FIELD(field, isStatic) =>
            val fieldInfo = getFieldInfo(field)
            mcode.Emit(if(isStatic) OpCodes.Stsfld else OpCodes.Stfld, fieldInfo)

          case CALL_PRIMITIVE(primitive) =>
            genPrimitive(primitive, instr.pos)

          case CALL_METHOD(msym, style) =>
            if (msym.isClassConstructor) {
              val constructorInfo: IKVM.Reflection.ConstructorInfo = getConstrInfo(msym)
              (style: @unchecked) match {
                // normal constructor calls are Static..
                case Static(_) =>
                  if (currIMethod.symbol.isClassConstructor && currIMethod.symbol.owner == msym.owner)
                    // we're generating a constructor (method: IMethod is a constructor), and we're
                    // calling another constructor of the same class.

                    // @LUC TODO: this can probably break, namely when having: class A { def this() { new A() } }
                    // instead, we should instruct the CALL_METHOD with additional information, know whether it's
                    // an instance creation constructor call or not.
                    mcode.Emit(OpCodes.Call, constructorInfo)
                  else
                    mcode.Emit(OpCodes.Newobj, constructorInfo)
                case SuperCall(_) =>
                  mcode.Emit(OpCodes.Call, constructorInfo)
                  if (isStaticModule(currIClazz.symbol) &&
                      notInitializedModules.contains(currIClazz.symbol) &&
                      currIMethod.symbol.isClassConstructor)
                    {
                      notInitializedModules -= currIClazz.symbol
                      mcode.Emit(OpCodes.Ldarg_0)
                      mcode.Emit(OpCodes.Stsfld, getModuleInstanceField(currIClazz.symbol))
                    }
              }

            } else {

              var doEmit = true
              val mtypeOwner = getReflType(msym.owner)
              if(mtypeOwner.IsEnum) {
                def negBool() = {
                  mcode.Emit(OpCodes.Ldc_I4_0)
                  mcode.Emit(OpCodes.Ceq)
                }
                doEmit = false
                val name = msym.name
                if (name eq nme.EQ)       { mcode.Emit(OpCodes.Ceq) }
                else if (name eq nme.NE)  { mcode.Emit(OpCodes.Ceq); negBool }
                else if (name eq nme.LT)  { mcode.Emit(OpCodes.Clt) }
                else if (name eq nme.LE)  { mcode.Emit(OpCodes.Cgt); negBool }
                else if (name eq nme.GT)  { mcode.Emit(OpCodes.Cgt) }
                else if (name eq nme.GE)  { mcode.Emit(OpCodes.Clt); negBool }
                else if (name eq nme.OR)  { mcode.Emit(OpCodes.Or) }
                else if (name eq nme.AND) { mcode.Emit(OpCodes.And) }
                else if (name eq nme.XOR) { mcode.Emit(OpCodes.Xor) }
                else
                  doEmit = true
              }

              // method: implicit view(FunctionX[PType0, PType1, ...,PTypeN, ResType]):DelegateType
              val (isDelegateView, paramType, resType) = beforeTyper {
                msym.tpe match {
                  case MethodType(params, resultType)
                  if (params.length == 1 && msym.name == nme.view_) =>
                    val paramType = params(0).tpe
                    val isDel = definitions.isCorrespondingDelegate(resultType, paramType)
                    (isDel, paramType, resultType)
                  case _ => (false, null, null)
                }
              }
              if (doEmit && isDelegateView) {
                doEmit = false
                createDelegateCaller(paramType, resType)
              }

              if (doEmit &&
                  (msym.name == nme.PLUS || msym.name == nme.MINUS)
                  && isDelegateType(msilType(toTypeKind(msym.owner.tpe))))
                {
                doEmit = false
                val methodInfo: MethodInfo = getMethodInfo(msym)
                // call it as a static method, even if the compiler (symbol) thinks it's virtual
                mcode.Emit(OpCodes.Call, methodInfo)
                mcode.Emit(OpCodes.Castclass, msilType(toTypeKind(msym.owner.tpe)))
              }

              if (doEmit && definitions.Delegate_scalaCallers.contains(msym)) {
                doEmit = false
                val methodSym: Symbol = definitions.Delegate_scalaCallerTargets(msym)
                val delegateType: Type = msym.tpe match {
                  case MethodType(_, retType) => retType
                  case _ => abort("not a method type: " + msym.tpe)
                }
                val methodInfo: MethodInfo = getMethodInfo(methodSym)
                val delegCtor = msilType(toTypeKind(delegateType)).GetConstructor(Array(MOBJECT, INT_PTR))
                if (methodSym.isStatic) {
                  mcode.Emit(OpCodes.Ldftn, methodInfo)
                } else {
                  mcode.Emit(OpCodes.Dup)
                  mcode.Emit(OpCodes.Ldvirtftn, methodInfo)
                }
                mcode.Emit(OpCodes.Newobj, delegCtor)
              }

              if (doEmit) {
                val methodInfo: MethodInfo = getMethodInfo(msym)
                (style: @unchecked) match {
                  case SuperCall(_) =>
                    mcode.Emit(OpCodes.Call, methodInfo)
                  case Dynamic =>
                    // methodInfo.DeclaringType is null for global methods
                    val isValuetypeMethod = (methodInfo.DeclaringType ne null) && (methodInfo.DeclaringType.IsValueType)
                    val isValuetypeVirtualMethod = isValuetypeMethod && (methodInfo.IsVirtual)
                    /* if (dynToStatMapped(msym)) { mcode.Emit(OpCodes.Call, methodInfo) } else */
                    if (isValuetypeVirtualMethod) {
                      mcode.Emit(OpCodes.Constrained, methodInfo.DeclaringType)
                      mcode.Emit(OpCodes.Callvirt, methodInfo)
                    } else if (isValuetypeMethod) {
                      // otherwise error "Callvirt on a value type method" ensues
                      mcode.Emit(OpCodes.Call, methodInfo)
                    } else {
                      mcode.Emit(OpCodes.Callvirt, methodInfo)
                    }
                  case Static(_) =>
                    if(methodInfo.IsVirtual /* TODO && !mcode.Ldarg0WasJustEmitted */ ) {
                      mcode.Emit(OpCodes.Callvirt, methodInfo)
                    } else mcode.Emit(OpCodes.Call, methodInfo)
              }
            }
            }

          case BOX(boxType)   => emitBox(mcode, boxType)

          case UNBOX(boxType) => emitUnbox(mcode, boxType)

          case CIL_UNBOX(boxType) => mcode.Emit(OpCodes.Unbox, msilType(boxType))

          case CIL_INITOBJ(valueType) => mcode.Emit(OpCodes.Initobj, msilType(valueType))

          case NEW(REFERENCE(cls)) =>
            // the next instruction must be a DUP, see comment on `var previousWasNEW`
            previousWasNEW = true

          // works also for arrays and reference-types
          case CREATE_ARRAY(elem, dims) =>
            // TODO: handle multi dimensional arrays
            assert(dims == 1, "Can't handle multi dimensional arrays")
            mcode.Emit(OpCodes.Newarr, msilType(elem))

          // works for arrays and reference-types
          case IS_INSTANCE(tpe) =>
            mcode.Emit(OpCodes.Isinst, msilType(tpe))
            mcode.Emit(OpCodes.Ldnull)
            mcode.Emit(OpCodes.Ceq)
            mcode.Emit(OpCodes.Ldc_I4_0)
            mcode.Emit(OpCodes.Ceq)

          // works for arrays and reference-types
          // Quoting from the spec: "S <: T does not imply
          //  Array[S] <: Array[T] in Scala. However, it is possible
          //  to cast an array of S to an array of T if such a cast
          //  is permitted in the host environment."
          case CHECK_CAST(tpknd) =>
            val tMSIL = msilType(tpknd)
            if(tMSIL.IsValueType) mcode.Emit(OpCodes.Unbox_Any, tMSIL)
            else mcode.Emit(OpCodes.Castclass, tMSIL)

          // no SWITCH is generated when there's
          //  - a default case ("case _ => ...") in the matching expr
          //  - OR is used ("case 1 | 2 => ...")
          case SWITCH(tags, branches) =>
            // tags is List[List[Int]]; a list of integers for every label.
            //    if the int on stack is 4, and 4 is in the second list => jump to second label
            // branches is List[BasicBlock]
            //    the labels to jump to (the last one is the default one)

            val switchLocal = mcode.DeclareLocal(MINT)
            // several switch vars will be emitted with the same name, but they referred by index, not by name.
            switchLocal.SetLocalSymInfo("$switch_var")

            mcode.Emit(OpCodes.Stloc, switchLocal)
            var i = 0
            for (l <- tags) {
              var targetLabel = labels(branches(i))
              for (i <- l) {
                mcode.Emit(OpCodes.Ldloc, switchLocal)
                loadI4(i, mcode)
                mcode.Emit(OpCodes.Beq, targetLabel)
              }
              i += 1
            }
            val defaultTarget = labels(branches(i))
            if (next != branches(i))
              mcode.Emit(OpCodes.Br, defaultTarget)

          case JUMP(whereto) =>
            val (leaveHandler, leaveFinally, lfTarget) = leavesHandler(block, whereto)
            if (leaveHandler) {
              if (leaveFinally) {
                if (lfTarget.isDefined) mcode.Emit(OpCodes.Leave, lfTarget.get)
                else mcode.Emit(OpCodes.Endfinally)
              } else
                mcode.Emit(OpCodes.Leave, labels(whereto))
            } else if (next != whereto)
              mcode.Emit(OpCodes.Br, labels(whereto))

          case CJUMP(success, failure, cond, typknd) =>
            // cond is TestOp (see Primitives.scala), and can take values EQ, NE, LT, GE LE, GT
            val isFloat = typknd == FLOAT || typknd == DOUBLE
            val emit = (c: TestOp, l: Label) => emitBr(c, l, isFloat)
            emitCondBr(block, cond, success, failure, next, emit)

          case CZJUMP(success, failure, cond, typknd) =>
            emitCondBr(block, cond, success, failure, next, emitBrBool(_, _))

          case RETURN(typknd) =>
            if (currentHandlers.isEmpty)
              mcode.Emit(OpCodes.Ret)
            else {
              val (local, label) = returnFromHandler(typknd)
              if (typknd != UNIT)
                mcode.Emit(OpCodes.Stloc, local)
              mcode.Emit(OpCodes.Leave, label)
            }

          case THROW(_) =>
            mcode.Emit(OpCodes.Throw)

          case DROP(typknd) => mcode.Emit(OpCodes.Pop)

          case DUP(typknd) => // see comment on `var previousWasNEW`
            if (!previousWasNEW) mcode.Emit(OpCodes.Dup)
            else previousWasNEW = false

          case MONITOR_ENTER() => mcode.Emit(OpCodes.Call, MMONITOR_ENTER)

          case MONITOR_EXIT() => mcode.Emit(OpCodes.Call, MMONITOR_EXIT)

          case SCOPE_ENTER(_) | SCOPE_EXIT(_) | LOAD_EXCEPTION(_) => ()
        }

      } // end of `for (instr <- b) { .. }`
    } // end of genBlock

    def genPrimitive(primitive: Primitive, pos: Position) {

      primitive match {
        case Negation(kind) =>
          kind match {
            // CHECK: is ist possible to get this for BOOL? in this case, verify.
            case BOOL | BYTE | CHAR | SHORT | INT | LONG | FLOAT | DOUBLE =>
              mcode.Emit(OpCodes.Neg)

            case _ => abort("Impossible to negate a " + kind)
          }

        case Arithmetic(op, kind) =>
          op match {
            case ADD => mcode.Emit(OpCodes.Add)
            case SUB => mcode.Emit(OpCodes.Sub)
            case MUL => mcode.Emit(OpCodes.Mul)
            case DIV => mcode.Emit(OpCodes.Div)
            case REM => mcode.Emit(OpCodes.Rem)
            case NOT => mcode.Emit(OpCodes.Not) //bitwise complement (one's complement)
            case _ => abort("Unknown arithmetic primitive " + primitive )
          }

        case Logical(op, kind) => op match {
          case AND => mcode.Emit(OpCodes.And)
          case OR => mcode.Emit(OpCodes.Or)
          case XOR => mcode.Emit(OpCodes.Xor)
        }

        case Shift(op, kind) => op match {
          case LSL => mcode.Emit(OpCodes.Shl)
          case ASR => mcode.Emit(OpCodes.Shr)
          case LSR => mcode.Emit(OpCodes.Shr_Un)
        }

        case Conversion(src, dst) =>
          debuglog("Converting from: " + src + " to: " + dst)

          dst match {
            case BYTE =>   mcode.Emit(OpCodes.Conv_I1) // I1 for System.SByte, i.e. a scala.Byte
            case SHORT =>  mcode.Emit(OpCodes.Conv_I2)
            case CHAR =>   mcode.Emit(OpCodes.Conv_U2)
            case INT =>    mcode.Emit(OpCodes.Conv_I4)
            case LONG =>   mcode.Emit(OpCodes.Conv_I8)
            case FLOAT =>  mcode.Emit(OpCodes.Conv_R4)
            case DOUBLE => mcode.Emit(OpCodes.Conv_R8)
            case _ =>
              Console.println("Illegal conversion at: " + currIClazz + " at: " + pos.source + ":" + pos.line)
          }

        case ArrayLength(_) => mcode.Emit(OpCodes.Ldlen)

        case StartConcat => mcode.Emit(OpCodes.Newobj, MSTRING_BUILDER_CONSTR)


        case StringConcat(el) =>
          val elemType : MsilType = el match {
            case REFERENCE(_) | ARRAY(_) => MOBJECT
            case _ => msilType(el)
          }

          val argTypes:Array[MsilType] = Array(elemType)
          val stringBuilderAppend = MSTRING_BUILDER.GetMethod("Append", argTypes )
          mcode.Emit(OpCodes.Callvirt,  stringBuilderAppend)

        case EndConcat => mcode.Emit(OpCodes.Callvirt, MSTRING_BUILDER_TOSTRING)

        case _ => abort("Unimplemented primitive " + primitive)
      }
    } // end genPrimitive


    ////////////////////// loading ///////////////////////

    def loadI4(value: Int, code: ILGenerator): Unit = value match {
      case -1 => code.Emit(OpCodes.Ldc_I4_M1)
      case 0  => code.Emit(OpCodes.Ldc_I4_0)
      case 1  => code.Emit(OpCodes.Ldc_I4_1)
      case 2  => code.Emit(OpCodes.Ldc_I4_2)
      case 3  => code.Emit(OpCodes.Ldc_I4_3)
      case 4  => code.Emit(OpCodes.Ldc_I4_4)
      case 5  => code.Emit(OpCodes.Ldc_I4_5)
      case 6  => code.Emit(OpCodes.Ldc_I4_6)
      case 7  => code.Emit(OpCodes.Ldc_I4_7)
      case 8  => code.Emit(OpCodes.Ldc_I4_8)
      case _  =>
        if (value >= -128 && value <= 127) {
          // (OpCodes.Ldc_I4_S, value.toByte) would be the right thing to do
          // but right now TypeParser maps both System.{Byte, SByte} to scala.Byte
          code.Emit(OpCodes.Ldc_I4, value)
        } else {
          code.Emit(OpCodes.Ldc_I4, value)
        }
    }

    def loadArg(code: ILGenerator, loadAddr: Boolean)(i: Int) =
      if (loadAddr) {
        if (i >= -128 && i <= 127)
          code.Emit(OpCodes.Ldarga_S, i)
        else
          code.Emit(OpCodes.Ldarga, i)
      } else {
        i match {
          case 0 => code.Emit(OpCodes.Ldarg_0)
          case 1 => code.Emit(OpCodes.Ldarg_1)
          case 2 => code.Emit(OpCodes.Ldarg_2)
          case 3 => code.Emit(OpCodes.Ldarg_3)
          case _      =>
            if (i >= -128 && i <= 127) code.Emit(OpCodes.Ldarg_S, i)
            else code.Emit(OpCodes.Ldarg, i)
        }
      }

    def loadLocal(i: Int, local: Local, code: ILGenerator, loadAddr: Boolean) =
      if (loadAddr) {
        if (i >= -128 && i <= 127) code.Emit(OpCodes.Ldloca_S, localBuilders(local))
        else code.Emit(OpCodes.Ldloca, localBuilders(local))
      } else {
        i match {
          case 0 => code.Emit(OpCodes.Ldloc_0)
          case 1 => code.Emit(OpCodes.Ldloc_1)
          case 2 => code.Emit(OpCodes.Ldloc_2)
          case 3 => code.Emit(OpCodes.Ldloc_3)
          case _      =>
            if (i >= -128 && i <= 127) code.Emit(OpCodes.Ldloc_S, localBuilders(local))
            else code.Emit(OpCodes.Ldloc, localBuilders(local))
        }
      }

    ////////////////////// branches ///////////////////////

    /** Returns a Triple (Boolean, Boolean, Option[Label])
     *   - whether the jump leaves some exception block (try / catch / finally)
     *   - whether it leaves a finally handler (finally block, but not it's try / catch)
     *   - a label where to jump for leaving the finally handler
     *     . None to leave directly using `endfinally`
     *     . Some(label) to emit `leave label` (for try / catch inside a finally handler)
     */
    def leavesHandler(from: BasicBlock, to: BasicBlock): (Boolean, Boolean, Option[Label]) =
      if (currentHandlers.isEmpty) (false, false, None)
      else {
        val h = currentHandlers.head
        val leaveHead = { h.covers(from) != h.covers(to) ||
                          h.blocks.contains(from) != h.blocks.contains(to) }
        if (leaveHead) {
          // we leave the innermost exception block.
          // find out if we also leave som e `finally` handler
          currentHandlers.find(e => {
            e.cls == NoSymbol && e.blocks.contains(from) != e.blocks.contains(to)
          }) match {
            case Some(finallyHandler) =>
              if (h == finallyHandler) {
                // the finally handler is the innermost, so we can emit `endfinally` directly
                (true, true, None)
              } else {
                // we need to `Leave` to the `endfinally` of the next outer finally handler
                val l = endFinallyLabels.getOrElseUpdate(finallyHandler, mcode.DefineLabel())
                (true, true, Some(l))
              }
            case None =>
              (true, false, None)
          }
        } else (false, false, None)
      }

    def emitCondBr(block: BasicBlock, cond: TestOp, success: BasicBlock, failure: BasicBlock,
                   next: BasicBlock, emitBrFun: (TestOp, Label) => Unit) {
      val (sLeaveHandler, sLeaveFinally, slfTarget) = leavesHandler(block, success)
      val (fLeaveHandler, fLeaveFinally, flfTarget) = leavesHandler(block, failure)

      if (sLeaveHandler || fLeaveHandler) {
        val sLabelOpt = if (sLeaveHandler) {
          val leaveSLabel = mcode.DefineLabel()
          emitBrFun(cond, leaveSLabel)
          Some(leaveSLabel)
        } else {
          emitBrFun(cond, labels(success))
          None
        }

        if (fLeaveHandler) {
          if (fLeaveFinally) {
            if (flfTarget.isDefined) mcode.Emit(OpCodes.Leave, flfTarget.get)
            else mcode.Emit(OpCodes.Endfinally)
          } else
            mcode.Emit(OpCodes.Leave, labels(failure))
        } else
          mcode.Emit(OpCodes.Br, labels(failure))

        sLabelOpt.map(l => {
          mcode.MarkLabel(l)
          if (sLeaveFinally) {
            if (slfTarget.isDefined) mcode.Emit(OpCodes.Leave, slfTarget.get)
            else mcode.Emit(OpCodes.Endfinally)
          } else
            mcode.Emit(OpCodes.Leave, labels(success))
        })
      } else {
        if (next == success) {
          emitBrFun(cond.negate, labels(failure))
        } else {
          emitBrFun(cond, labels(success))
          if (next != failure) {
            mcode.Emit(OpCodes.Br, labels(failure))
          }
        }
      }
    }

    def emitBr(condition: TestOp, dest: Label, isFloat: Boolean) {
      condition match {
        case EQ => mcode.Emit(OpCodes.Beq, dest)
        case NE => mcode.Emit(OpCodes.Bne_Un, dest)
        case LT => mcode.Emit(if (isFloat) OpCodes.Blt_Un else OpCodes.Blt, dest)
        case GE => mcode.Emit(if (isFloat) OpCodes.Bge_Un else OpCodes.Bge, dest)
        case LE => mcode.Emit(if (isFloat) OpCodes.Ble_Un else OpCodes.Ble, dest)
        case GT => mcode.Emit(if (isFloat) OpCodes.Bgt_Un else OpCodes.Bgt, dest)
      }
    }

    def emitBrBool(cond: TestOp, dest: Label) {
      cond match {
        // EQ -> Brfalse, NE -> Brtrue; this is because we come from
        // a CZJUMP. If the value on the stack is 0 (e.g. a boolean
        // method returned false), and we are in the case EQ, then
        // we need to emit Brfalse (EQ Zero means false). vice versa
        case EQ => mcode.Emit(OpCodes.Brfalse, dest)
        case NE => mcode.Emit(OpCodes.Brtrue, dest)
      }
    }

    // #####################################################################
    // delegate callers

    var delegateCallers: TypeBuilder = _
    var nbDelegateCallers: Int = 0

    private def initDelegateCallers() = {
      import IKVM.Reflection.TypeAttributes;
      delegateCallers =
        mmodule.DefineType("$DelegateCallers", (TypeAttributes.Public | TypeAttributes.Sealed))
    }

    private def createDelegateCaller(functionType: Type, delegateType: Type) = {

      import IKVM.Reflection.{FieldAttributes, MethodAttributes, ParameterAttributes}
      import IKVM.Reflection.Emit.{FieldBuilder, MethodBuilder}

      if (delegateCallers == null)
        initDelegateCallers()
      // create a field an store the function-object
      val mFunctionType: MsilType = msilType(toTypeKind(functionType))
      val anonfunField: FieldBuilder = delegateCallers.DefineField(
        "$anonfunField$$" + nbDelegateCallers, mFunctionType,
        (FieldAttributes.InitOnly | FieldAttributes.Public | FieldAttributes.Static))
      mcode.Emit(OpCodes.Stsfld, anonfunField)


      // create the static caller method and the delegate object
      val (params, returnType) = delegateType.member(nme.apply).tpe match {
        case MethodType(delParams, delReturn) => (delParams, delReturn)
        case _ => abort("not a delegate type: "  + delegateType)
      }
      val caller: MethodBuilder = delegateCallers.DefineMethod(
        "$delegateCaller$$" + nbDelegateCallers,
        (MethodAttributes.Final | MethodAttributes.Public | MethodAttributes.Static),
        msilType(toTypeKind(returnType)),
        (params map (_.tpe) map (toTypeKind(_)) map (msilType(_)) ).toArray)
      for (i <- 0 until params.length)
        caller.DefineParameter(i+1, ParameterAttributes.None, "arg" + i) // FIXME: use name of parameter symbol
      val delegCtor = msilType(toTypeKind(delegateType)).GetConstructor(Array(MOBJECT, INT_PTR))
      mcode.Emit(OpCodes.Ldnull)
      mcode.Emit(OpCodes.Ldftn, caller)
      mcode.Emit(OpCodes.Newobj, delegCtor)


      // create the static caller method body
      val functionApply: MethodInfo = getMethodInfo(functionType.member(nme.apply))
      val dcode: ILGenerator = caller.GetILGenerator()
      dcode.Emit(OpCodes.Ldsfld, anonfunField)
      for (i <- 0 until params.length) {
        loadArg(dcode, false /* TODO confirm whether passing actual as-is to formal is correct wrt the ByRef attribute of the param */)(i)
        emitBox(dcode, toTypeKind(params(i).tpe))
      }
      dcode.Emit(OpCodes.Callvirt, functionApply)
      emitUnbox(dcode, toTypeKind(returnType))
      dcode.Emit(OpCodes.Ret)

      nbDelegateCallers = nbDelegateCallers + 1

    } //def createDelegateCaller

    // #####################################################################
    // emitBox, emitUnbox

    def emitBox(code: ILGenerator, boxType: TypeKind) = (boxType: @unchecked) match {
      // doesn't make sense, unit as parameter..
      case UNIT   => code.Emit(OpCodes.Ldsfld, boxedUnit)
      case BOOL | BYTE | SHORT | CHAR | INT | LONG | FLOAT | DOUBLE =>
        code.Emit(OpCodes.Box, msilType(boxType))
      case REFERENCE(cls) if clrTypes.isValueType(cls) =>
        code.Emit(OpCodes.Box, (msilType(boxType)))
      case REFERENCE(_) | ARRAY(_) =>
        warning("Tried to BOX a non-valuetype.")
        ()
    }

    def emitUnbox(code: ILGenerator, boxType: TypeKind) = (boxType: @unchecked) match {
      case UNIT   => code.Emit(OpCodes.Pop)
      /* (1) it's essential to keep the code emitted here (as of now plain calls to System.Convert.ToBlaBla methods)
             behaviorally.equiv.wrt. BoxesRunTime.unboxToBlaBla methods
             (case null: that's easy, case boxed: track changes to unboxBlaBla)
         (2) See also: asInstanceOf to cast from Any to number,
             tracked in http://lampsvn.epfl.ch/trac/scala/ticket/4437  */
      case BOOL   => code.Emit(OpCodes.Call, toBool)
      case BYTE   => code.Emit(OpCodes.Call, toSByte)
      case SHORT  => code.Emit(OpCodes.Call, toShort)
      case CHAR   => code.Emit(OpCodes.Call, toChar)
      case INT    => code.Emit(OpCodes.Call, toInt)
      case LONG   => code.Emit(OpCodes.Call, toLong)
      case FLOAT  => code.Emit(OpCodes.Call, toFloat)
      case DOUBLE => code.Emit(OpCodes.Call, toDouble)
      case REFERENCE(cls) if clrTypes.isValueType(cls) =>
        code.Emit(OpCodes.Unbox, msilType(boxType))
        code.Emit(OpCodes.Ldobj, msilType(boxType))
      case REFERENCE(_) | ARRAY(_) =>
        warning("Tried to UNBOX a non-valuetype.")
        ()
    }

    // #####################################################################
    // module-instance

    /**
     * This method is called when emitting bytecode for LOAD_MODULE
     * fsym may refer either to be an object-symbol (module-symbol), or a module-class-symbol.
     *
     */
    private def getModuleInstanceField(modsym: Symbol): FieldInfo = {
      assert(modsym.isModule || modsym.isModuleClass, "Expected module: " + showsym(modsym))

      val moduleClassSym = if (modsym.isModule) modsym.moduleClass else modsym

      // TODO: get module field for modules not defined in the source currently compiling (e.g. Console)

      if(fldbldrs.contains(moduleClassSym)) {
        fldbldrs(moduleClassSym)
      } else if(clrTypes.fields.contains(moduleClassSym)) {
        clrTypes.fields(moduleClassSym)
      } else {
        val nameInMetadata = nestingAwareFullClassname(moduleClassSym)
        val mClass = clrTypes.lookupEverywhere(nameInMetadata)
        val mfield = mClass.GetField("MODULE$")
        assert(mfield ne null, "module not found " + showsym(moduleClassSym))
        clrTypes.fields(moduleClassSym) = mfield
        mfield
      }
    }

    def nestingAwareFullClassname(csym: Symbol) : String = {
      val suffix = csym.moduleSuffix
      val res =
        if (csym.isNestedClass) nestingAwareFullClassname(csym.owner) + "+" + csym.encodedName
        else csym.fullName;
      res + suffix
    }

    /** Adds a static initializer which creates an instance of the module
     *  class (calls the primary constructor). A special primary constructor
     *  will be generated (notInitializedModules) which stores the new instance
     *  in the MODULE$ field right after the super call.
     */
    private def addStaticInit(modsym: Symbol) {
      val tBuilder = getTypeBuilder(modsym)
      import IKVM.Reflection.{CallingConventions, MethodAttributes};
      val attribs = (MethodAttributes.Static | MethodAttributes.Public)
      val staticInit = tBuilder.DefineConstructor(attribs, CallingConventions.Standard, EmptyTypesArr)
      val sicode = staticInit.GetILGenerator()
      val instanceConstructor = constrbldrs(modsym.primaryConstructor)
      // there are no constructor parameters. assuming the constructor takes no parameter
      // is fine: we call (in the static constructor) the constructor of the module class,
      // which takes no arguments - an object definition cannot take constructor arguments.
      sicode.Emit(OpCodes.Newobj, instanceConstructor)
      // the stsfld is done in the instance constructor, just after the super call.
      sicode.Emit(OpCodes.Pop)
      sicode.Emit(OpCodes.Ret)
    }

    /* ------------------------ (5) write assembly to disk ------------------------*/

    def writeAssembly() {

      import IKVM.Reflection.{MethodAttributes, ParameterAttributes}
      import IKVM.Reflection.Emit.OpCodes

      if (entryPoint != null) {
        assert(entryPoint.enclClass.isModuleClass, entryPoint.enclClass)
        val mainMethod = methodbldrs(entryPoint)
        val attribs = (MethodAttributes.Public | MethodAttributes.Static)
        val globalMain = mmodule.DefineGlobalMethod("Main", attribs, null, Array(MSTRING_ARRAY))
        globalMain.DefineParameter(1, ParameterAttributes.None, "args")
        massembly.SetEntryPoint(globalMain)
        val code = globalMain.GetILGenerator()
        val moduleField = getModuleInstanceField(entryPoint.enclClass)
        code.Emit(OpCodes.Ldsfld, moduleField)
        code.Emit(OpCodes.Ldarg_0)
        code.Emit(OpCodes.Callvirt, mainMethod)
        code.Emit(OpCodes.Ret)
        mmodule.CreateGlobalFunctions()
      }

      bakeTypeBuilders()

      try {
        import IKVM.Reflection.{PortableExecutableKinds, ImageFileMachine}
        massembly.Save(outputFileName, 
                       (PortableExecutableKinds.ILOnly | PortableExecutableKinds.Preferred32Bit), 
                       ImageFileMachine.I386)
        // see "Managed PE File Types" at http://weblog.ikvm.net/CommentView.aspx?guid=7c760276-fc97-4b90-9208-5d67797b21a7
      } catch {
        case e: java.lang.Exception => abort("Could not write to " + outputFileName + ": " + java.lang.Throwable.instancehelper_getMessage(e))
      }

    } // end of writeAssembly

    private def bakeTypeBuilders() {
      val baked = scala.collection.mutable.HashSet.empty[Symbol]

        def bake(csym: Symbol) {
          if(baked(csym)) { return; }
          if(!isBeingCompiled(csym)) { return; }
          for(t <- csym.info.parents) { bake(t.typeSymbol) }
          val res = getTypeBuilder(csym).CreateType()
          if(res == null) { warning("Reflection.Emit.TypeBuilder.CreateType failed for " + showsym(csym)) }
          baked += csym
        }

      for (csym <- classes.keys) { bake(csym) }
    }

  } // end of BytecodeGenerator

  abstract class Helpers {

    import IKVM.Reflection.{FieldInfo, ConstructorInfo, MethodInfo}
    import IKVM.Reflection.Emit.{TypeBuilder, FieldBuilder, ConstructorBuilder, MethodBuilder}

    val EmptyTypesArr = Array.empty[MsilType]

    val ikvmuniverse = util.MsilClassPath.assemUniverse // TODO remove

    val typebldrs   : mutable.Map[Symbol,TypeBuilder]  = new mutable.HashMap
    val fldbldrs    : mutable.Map[Symbol,FieldBuilder] = new mutable.HashMap
    val constrbldrs : mutable.Map[Symbol,ConstructorBuilder] = new mutable.HashMap
    val methodbldrs : mutable.Map[Symbol,MethodBuilder]      = new mutable.HashMap

    val MODULE_INSTANCE_NAME = "MODULE$"

    val MVOID    = clrTypes.VOID
    val MBOOL    = clrTypes.BOOLEAN
    val MSBYTE   = clrTypes.SBYTE
    val MUBYTE   = clrTypes.UBYTE
    val MSSHORT  = clrTypes.SHORT
    val MUSHORT  = clrTypes.USHORT
    val MCHAR    = clrTypes.CHAR
    val MINT     = clrTypes.INT
    val MUINT    = clrTypes.UINT
    val MLONG    = clrTypes.LONG
    val MULONG   = clrTypes.ULONG
    val MFLOAT   = clrTypes.FLOAT
    val MDOUBLE  = clrTypes.DOUBLE
    val MOBJECT  = clrTypes.OBJECT
    val MSTRING  = clrTypes.STRING
    val MSTRING_ARRAY  = MSTRING.MakeArrayType()

    val SYMTAB_ATTRIBUTE_CONSTRUCTOR = clrTypes.SYMTAB_CONSTR // ie. clrTypes.SCALA_SYMTAB_ATTR.GetConstructor(Array(MUBYTE.MakeArrayType())

    val SYMTAB_ATTRIBUTE_EMPTY_CONSTRUCTOR = clrTypes.SYMTAB_DEFAULT_CONSTR // ie. clrTypes.SCALA_SYMTAB_ATTR.GetConstructor(EmptyTypesArr)

    val MEXCEPTION   = mscorlibType("System.Exception")
    val MUBYTE_ARRAY = MUBYTE.MakeArrayType()

    // Map a class or method from the Scala world to the closest .net counterpart, for use during IL instruction generation.
    mapType(definitions.AnyClass,     MOBJECT)
    mapType(definitions.AnyRefClass,  MOBJECT)
    mapType(definitions.NullClass,    MEXCEPTION)
    mapType(definitions.NothingClass, MEXCEPTION)

    val objParam = Array(MOBJECT)

    val ICLONEABLE       = mscorlibType("System.ICloneable")
    val MEMBERWISE_CLONE = MOBJECT.GetMethod("MemberwiseClone", EmptyTypesArr)

    val MMONITOR       = mscorlibType("System.Threading.Monitor")
    val MMONITOR_ENTER = MMONITOR.GetMethod("Enter", objParam)
    val MMONITOR_EXIT  = MMONITOR.GetMethod("Exit",  objParam)

    val MSTRING_BUILDER          = mscorlibType("System.Text.StringBuilder")
    val MSTRING_BUILDER_CONSTR   = MSTRING_BUILDER.GetConstructor(EmptyTypesArr)
    val MSTRING_BUILDER_TOSTRING = MSTRING_BUILDER.GetMethod("ToString", EmptyTypesArr)

    val TYPE_FROM_HANDLE =
      mscorlibType("System.Type").GetMethod("GetTypeFromHandle", Array(mscorlibType("System.RuntimeTypeHandle")))
    val INT_PTR = mscorlibType("System.IntPtr")
    val SystemConvert = mscorlibType("System.Convert")

    val JOBJECT = definitions.ObjectClass
    val JSTRING = definitions.StringClass

    import IKVM.Reflection.{FieldInfo, MethodInfo};
    val toBool:   MethodInfo = SystemConvert.GetMethod("ToBoolean", objParam) // see comment in emitUnbox
    val toSByte:  MethodInfo = SystemConvert.GetMethod("ToSByte",   objParam)
    val toShort:  MethodInfo = SystemConvert.GetMethod("ToInt16",   objParam)
    val toChar:   MethodInfo = SystemConvert.GetMethod("ToChar",    objParam)
    val toInt:    MethodInfo = SystemConvert.GetMethod("ToInt32",   objParam)
    val toLong:   MethodInfo = SystemConvert.GetMethod("ToInt64",   objParam)
    val toFloat:  MethodInfo = SystemConvert.GetMethod("ToSingle",  objParam)
    val toDouble: MethodInfo = SystemConvert.GetMethod("ToDouble",  objParam)

    val boxedUnit: FieldInfo = getFieldInfo(definitions.BoxedUnit_UNIT)

    // Scala attributes
    // symtab.Definitions -> object (singleton..)
    val SerializableAttr = definitions.SerializableAttr.tpe
    val CloneableAttr    = definitions.getClass(newTypeName("scala.cloneable")).tpe
    val TransientAtt     = definitions.getClass(newTypeName("scala.transient")).tpe
    // remoting: the architectures are too different, no mapping (no portable code possible)

    // java instance methods that are mapped to static methods in .net
    // these will need to be called with OpCodes.Call (not Callvirt)
    // Update: looks like not needed anymore. (otherwise, getMethodInfo should do the trick its GenMSIL counterpart does).
    // val dynToStatMapped = mutable.HashSet[Symbol]()

    def mapType(csym: Symbol, mType: MsilType) {
      assert(mType != null, showsym(csym))
      assert(!clrTypes.types.contains(csym), showsym(csym))
      clrTypes.types(csym) = mType
    }

    /* ------------------------ helpers ------------------------*/

    def isBeingCompiled(csym: Symbol): Boolean = {
      icodes.classes.isDefinedAt(csym)
      // currentRun.compiles(csym) won't help bc SpecializeType adds to currentRun.symSource an entry for spezd types (not being compiled)
    }

    // if the module is lifted it does not need to be initialized in
    // its static constructor, and the MODULE$ field is not required.
    // the outer class will care about it.
    def isStaticModule(sym: Symbol): Boolean = {
      // .net inner classes: removed '!sym.hasFlag(Flags.LIFTED)', added
      // 'sym.isStatic'. -> no longer compatible without skipping flatten!
      sym.isModuleClass && sym.isStatic && !sym.isImplClass
    }

    def isTopLevelModule(sym: Symbol): Boolean =
      beforeRefchecks {
        sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
      }

    /** Return the a name of this symbol that can be used on .NET (e.g., spaces are removed).
     *
     * scala.Nothing and scala.Null are not VM-level classes
     * (rather, they denote (a) abrupt termination upon evaluation or (b) 'null', resp.)
     * They are 'mapped' by GenICode to scala.runtime.Nothing$ and scala.runtime.Null$.
     * Given an argument denoting scala.Nothing or scala.Null,
     * we return a name in agreement with the VM-level class that GenICode mapped to.
     */
    def msilName(sym: Symbol): String = {
      val suffix = sym.moduleSuffix
      // Flags.JAVA: "symbol was not defined by a scala-class" (java, or .net-class)

      if (sym == definitions.NothingClass)   { return "scala.runtime.Nothing$" }
      else if (sym == definitions.NullClass) { return "scala.runtime.Null$" }

      (if (sym.isClass || (sym.isModule && !sym.isMethod)) {
        if (sym.isNestedClass) sym.simpleName
        else sym.fullName
       } else
         _root_.java.lang.String.instancehelper_trim(sym.simpleName.ToString)) + suffix
    }

    def showsym(sym: Symbol): String = (sym.ToString +
      "\n  symbol = " + Flags.flagsToString(sym.flags) + " " + sym +
      "\n  owner  = " + Flags.flagsToString(sym.owner.flags) + " " + sym.owner
    )

    ////////////////////// flags ///////////////////////

    def msilTypeFlags(sym: Symbol): IKVM.Reflection.TypeAttributes = {

      import IKVM.Reflection.TypeAttributes
      import TypeAttributes.{NotPublic => Zero}

      var mf : TypeAttributes = TypeAttributes.AutoLayout | TypeAttributes.AnsiClass

      if(sym.isNestedClass) {
        mf = mf | (if (sym hasFlag Flags.PRIVATE) TypeAttributes.NestedPrivate else TypeAttributes.NestedPublic)
      } else {
        mf = mf | (if (sym hasFlag Flags.PRIVATE) TypeAttributes.NotPublic else TypeAttributes.Public)
      }
      mf = mf | (if (sym hasFlag Flags.ABSTRACT) TypeAttributes.Abstract else Zero)
      mf = mf | (if (sym.isTrait && !sym.isImplClass) TypeAttributes.Interface else TypeAttributes.Class)
      mf = mf | (if (sym isFinal) TypeAttributes.Sealed else Zero)

      val isSerializ = sym.annotations exists {
        case ai @ AnnotationInfo(SerializableAttr, _, _) => true
        case _ => false
      }
      if(isSerializ) { mf = mf | TypeAttributes.Serializable };

      // TODO: add the Serializable TypeAttribute also if the annotation
      // System.SerializableAttribute is present (.net annotation, not scala)
      //  Best way to do it: compare with
      //  definitions.getClass("System.SerializableAttribute").tpe
      //  when frontend available

      mf
      // static: not possible (or?)
    }

    def msilMethodFlags(sym: Symbol): IKVM.Reflection.MethodAttributes = {

      import IKVM.Reflection.MethodAttributes

      var mf: MethodAttributes = MethodAttributes.HideBySig

      if (sym hasFlag Flags.PRIVATE) {
        mf = mf | MethodAttributes.Private
      } else if (sym.isPrimaryConstructor && isTopLevelModule(sym.owner)) {
        // constructors of module classes should be private
        mf |= MethodAttributes.Private
      } else {
        mf = mf | MethodAttributes.Public
      }

      if (!sym.isClassConstructor) {
        if (sym.isStaticMember)
          mf = mf | MethodAttributes.Static
        else {
          mf = mf | MethodAttributes.Virtual
          if (sym.isFinal && !getReflType(sym.owner).IsInterface)   { mf = mf | MethodAttributes.Final   }
          if (sym.isDeferred || getReflType(sym.owner).IsInterface) { mf = mf | MethodAttributes.Abstract }
        }
      }

      if (sym.isStaticMember) { mf = mf | MethodAttributes.Static }

      mf
    }

    def msilFieldFlags(sym: Symbol): IKVM.Reflection.FieldAttributes = {

      import IKVM.Reflection.FieldAttributes

      var mf: FieldAttributes =
        if (sym hasFlag Flags.PRIVATE) FieldAttributes.Private
        else if (sym hasFlag Flags.PROTECTED) FieldAttributes.FamORAssem
        else FieldAttributes.Public

      if (sym hasFlag Flags.FINAL) { mf = mf | FieldAttributes.InitOnly }

      if (sym.isStaticMember) { mf = mf | FieldAttributes.Static }

      // TRANSIENT: "not serialized", VOLATILE: doesn't exist on .net
      // TODO: add this annotation also if the class has the custom attribute
      // System.NotSerializedAttribute

      val isTransient = sym.annotations exists {
        case ai @ AnnotationInfo(TransientAtt, _, _) => true
        case _ => false
      }

      if(isTransient) { mf = mf | FieldAttributes.NotSerialized };

      mf
    }

    /**
     *  A no-preconds method to find the .Net representative for the argument
     *  (which can be a class being compiled or just being referred somewhere in the program).
     *  This method can be invoked multiple times.
     */
    def getReflType(csym: Symbol): MsilType = {
      if(isBeingCompiled(csym)) {
        getTypeBuilder(csym)
      } else {
        getRefdMsilType(csym)
      }
    }

    /**
     *  Precond: the argument denotes a class being compiled.
     *  This method can be invoked multiple times.
     */
    def getTypeBuilder(csym: Symbol): TypeBuilder = {
      assert(isBeingCompiled(csym), "Expected a class-symbol for a class being compiled but found " + showsym(csym))
      if(!typebldrs.isDefinedAt(csym)){
        createTypeBuilder(csym)
      }
      typebldrs(csym)
    }

    def createTypeBuilder(csym: Symbol);

    /**
     *  This method should be used only when about to emit bytecode and no class-symbol is available
     *  (that happens in just a few occassions).
     */
    def mscorlibType(fullName: String): MsilType = clrTypes.getType(fullName)

    /**
     * csym points to a type not being compiled (thus, 'referenced')
     */
    def getRefdMsilType(csym: Symbol): MsilType = {
      assert(!isBeingCompiled(csym), "Expected a class-symbol for a class NOT being compiled but found " + showsym(csym))
      val attemptA = clrTypes.types.get(csym)
      if(attemptA.isDefined) return attemptA.get;

      /* 
       * if we get here it's because 
       *    enterClass(root: Symbol, name: String, completer: SymbolLoader): Symbol
       * was never invoked for csym (e.g., scala.reflect.api.Trees+Tree , due to the symtab in scala.reflect.api.Trees)
       * TODO perhaps the SymbolLoader can be made to grab the MsilType for the static nested class Tree 
       *      (the MsilType for its owner, trait Trees, is already available).
       */

          def fqName(s: Symbol): String = {
            val n = if (s.isNestedClass) fqName(s.owner) +"+"+ s.simpleName
                    else s.fullName
            if (s.isModuleClass && !s.isTrait && !s.isJavaDefined) n + "$" else n
          }

      // lookup by name among all loaded assemblies
      val t: MsilType = clrTypes.lookupEverywhere(fqName(csym))
      assert(t != null)
      clrTypes.types(csym) = t
      t
    }

    /**
     * What's a CLR custom modifier?
     * It's basically a marker (in the form of a MsilType) associated with a location (think of FieldInfo, ParameterInfo, and PropertyInfo)
     * and thus that marker (be it optional or required) becomes part of the signature of that location.
     * Example: "System.Runtime.CompilerServices.IsVolatile"
     *
     * TODO Other sym.annotations (e.g., definitions.TransientAttr) should be reflected in bytecode via
     * SetCustomAttribute(CustomAttributeBuilder). For that, use addAttributes().
     */
    def requiredCustomMods(annotations: List[AnnotationInfo]): Array[MsilType] = {
      annotations.map(_.atp.typeSymbol).collect {
        case definitions.VolatileAttr  => VolatileMarker
      } toArray
    }

    private lazy val VolatileMarker = mscorlibType("System.Runtime.CompilerServices.IsVolatile")

    def isVolatile(fi: FieldInfo): Boolean = {
      fi.GetRequiredCustomModifiers contains VolatileMarker
    }

    def isDelegateType(t: MsilType): Boolean = { t.BaseType == clrTypes.DELEGATE }

    // #####################################################################

    /**
     *  This method can be invoked multiple times.
     *  Add binding (fsym -> fInfo) to clrTypes.fields map if fsym not in both of fldbldrs and clrTypes.fields.
     */
    def getFieldInfo(fsym: Symbol): FieldInfo = {
      if(fldbldrs.contains(fsym)) {
        fldbldrs(fsym)
      } else if(clrTypes.fields.contains(fsym)) {
        clrTypes.fields(fsym)
      } else {
        import IKVM.Reflection.BindingFlags;
        var bindflags = (if(fsym.isStaticMember) BindingFlags.Static else BindingFlags.Instance)
        bindflags |= (BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.DeclaredOnly)
        val fInfo =
          if(clrTypes.fields.contains(fsym)) {
            val legaField = clrTypes.fields(fsym)
            legaField
          } else {
            getReflType(fsym.owner).GetField(msilName(fsym), bindflags)
          }
        assert(fInfo != null, showsym(fsym))
        clrTypes.fields(fsym) = fInfo
        fInfo
      }
    }

    // #####################################################################

    /**
     *  This method can be invoked multiple times.
     *  Adds binding (ctorsym -> cInfo) to clrTypes.constructors map if ctorsym neither in both of constrbldrs nor clrTypes.constructors.
     */
    def getConstrInfo(ctorsym: Symbol): ConstructorInfo = {
      if(constrbldrs.contains(ctorsym)) {
        constrbldrs(ctorsym)
      } else if(clrTypes.constructors.contains(ctorsym)) {
        clrTypes.constructors(ctorsym)
      } else {
        val cInfo =
          if(clrTypes.constructors.contains(ctorsym)) {
            val legaConstr = clrTypes.constructors(ctorsym)
            val declType = legaConstr.DeclaringType
            val searchParamTypes : Array[MsilType] = legaConstr.GetParameters map (_.ParameterType)
            // TODO also include custom req mods in search criteria bc that's part of the method signature (and the ret type for good measure).
            helperGetConstrInfo(declType, searchParamTypes)
          } else {
            val declType = getReflType(ctorsym.owner)
            val searchParamTypes = msilParamTypes(ctorsym)
            // TODO also include custom req mods in search criteria bc that's part of the method signature (and the ret type for good measure).
            helperGetConstrInfo(declType, searchParamTypes)
          }
        assert(cInfo != null, showsym(ctorsym))
        clrTypes.constructors(ctorsym) = cInfo
        cInfo
      }
    }

    private def helperGetConstrInfo(declType: MsilType,
                                    searchParamTypes: Array[MsilType]): ConstructorInfo = {
      import IKVM.Reflection.BindingFlags;
      val bindflags = (BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.DeclaredOnly)
      var found : ConstructorInfo = null;
      // now we walk up the base-class hierarchy till we find the method.
      val declConstrs = declType.GetConstructors(bindflags)
      var i = 0;
      while(found == null && i < declConstrs.length) {
        val declConstr = declConstrs(i)
        val declParamInfos = declConstr.GetParameters()
        if(helperEqualParameters(declParamInfos, searchParamTypes)) {
          return declConstr;
        }
        i += 1;
      }
      // assert(false)
      null
    }

    // #####################################################################

    /**
     *  This method can be invoked multiple times.
     *  Add binding (msym -> mInfo) to clrTypes.methods map if msym not found in both of methodbldrs and clrTypes.methods.
     */
    def getMethodInfo(msym: Symbol): MethodInfo = {
      if(methodbldrs.contains(msym)) {
        methodbldrs(msym)
      } else if(clrTypes.methods.contains(msym)) {
        clrTypes.methods(msym)
      } else {
        import IKVM.Reflection.BindingFlags;
        var bindflags = (if(msym.isStaticMember) BindingFlags.Static else BindingFlags.Instance)
        bindflags |= (BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.DeclaredOnly)
        val mInfo =
          if(clrTypes.methods.contains(msym)) {
            val legaMeth = clrTypes.methods(msym)
            val declType = legaMeth.DeclaringType
            val searchParamTypes: Array[MsilType] = legaMeth.GetParameters map (_.ParameterType)
            val searchResType = legaMeth.ReturnType
            // what about custom req mods in search criteria bc that's part of the ctor signature.
            helperGetMethodInfo(declType, legaMeth.Name, bindflags, searchParamTypes, searchResType)
          } else {
            var declType = getReflType(msym.owner)
            val mName = msilName(msym)
            val searchParamTypes = msilParamTypes(msym)
            val searchResType    = msilType(toTypeKind(msym.tpe.resultType))
            helperGetMethodInfo(declType, mName, bindflags, searchParamTypes, searchResType)
          }
        assert(mInfo != null, showsym(msym))
        clrTypes.methods(msym) = mInfo
        mInfo
      }
    }

    /** looks up in assembly metadata the way ch.epfl.lamp.compiler.msil used to do it. */
    private def helperGetMethodInfo(declType0: MsilType,
                                     mName: String,
                                     bindflags: IKVM.Reflection.BindingFlags,
                                     searchParamTypes: Array[MsilType],
                                     searchResType: MsilType): MethodInfo = {
      var declType = declType0
      // now we walk up the base-class hierarchy till we find the method.
      do {
        val declMethods = declType.GetMethods(bindflags)
        var i = 0;
        while(i < declMethods.length) {
          val declMethod = declMethods(i)
          if( (declMethod.Name == mName) && (searchResType == declMethod.ReturnType) ) {
            val declParamInfos = declMethod.GetParameters()
            if(helperEqualParameters(declParamInfos, searchParamTypes)) {
              return declMethod;
            }
          }
          i += 1;
        }
        declType = declType.BaseType
      } while (declType != null)
      // assert(false)
      null
    }

    private def helperEqualParameters(declParamInfos: Array[IKVM.Reflection.ParameterInfo], searchParamTypes: Array[MsilType]): Boolean = {
      if(declParamInfos.size != searchParamTypes.size) return false;
      // custom req mods are part of the ctor signature, but they shouldn't have been emitted for libraries compiled by Scala.Net.
      var i = 0;
      while(i < declParamInfos.length) {
        val pi = declParamInfos(i)
        val t  = searchParamTypes(i)
        if(pi.ParameterType != t) {
          return false;
        }
        i += 1
      }
      true
    }

    def msilType(t: TypeKind): MsilType = (t: @unchecked) match {
      case UNIT           => MVOID
      case BOOL           => MBOOL
      case BYTE           => MSBYTE
      case SHORT          => MSSHORT
      case CHAR           => MCHAR
      case INT            => MINT
      case LONG           => MLONG
      case FLOAT          => MFLOAT
      case DOUBLE         => MDOUBLE
      case REFERENCE(cls) => getReflType(cls)
      case ARRAY(elem)    => msilType(elem).MakeArrayType()
    }

    def msilParamTypes(sym: Symbol): Array[MsilType] = {
      sym.tpe.paramTypes.map(t => msilType(toTypeKind(t))).toArray
    }

  } // end of Helpers

} // end of GenMSIL
