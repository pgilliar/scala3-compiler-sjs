package dotty.tools.dotc.sjsmacros.host

import dotty.tools.sjs.ir.{Names, Serializers, Traversers, Trees, Types}

import java.nio.ByteBuffer
import scala.collection.mutable

object SjsMacroIRClosure:
  import SjsMacroArtifacts.IRFile

  def collect(entryPointsIR: Seq[IRFile], availableIR: Seq[IRFile]): Seq[IRFile] =
    val availableByClassName = mutable.LinkedHashMap.empty[Names.ClassName, IRFile]
    val availableClassDefs = mutable.HashMap.empty[Names.ClassName, Trees.ClassDef]

    for file <- availableIR do
      val classDef = deserialize(file)
      availableByClassName.getOrElseUpdate(classDef.className, file)
      availableClassDefs.getOrElseUpdate(classDef.className, classDef)

    val selected = mutable.LinkedHashSet.empty[Names.ClassName]
    val enqueued = mutable.HashSet.empty[Names.ClassName]
    val queue = mutable.Queue.empty[Names.ClassName]

    def enqueue(className: Names.ClassName): Unit =
      if availableByClassName.contains(className) && !selected.contains(className) && enqueued.add(className) then
        queue.enqueue(className)

    for entryPoint <- entryPointsIR do
      collectClassReferences(deserialize(entryPoint)).foreach(enqueue)

    while queue.nonEmpty do
      val className = queue.dequeue()
      if selected.add(className) then
        collectClassReferences(availableClassDefs(className)).foreach(enqueue)

    selected.toList.flatMap(availableByClassName.get)

  private def deserialize(file: IRFile): Trees.ClassDef =
    Serializers.deserialize(ByteBuffer.wrap(file.bytes))

  private def collectClassReferences(classDef: Trees.ClassDef): List[Names.ClassName] =
    val collector = new ClassReferenceCollector
    collector.traverseClassDef(classDef)
    collector.references.toList

  private final class ClassReferenceCollector extends Traversers.Traverser:
    val references = mutable.LinkedHashSet.empty[Names.ClassName]

    private def add(className: Names.ClassName): Unit =
      references += className

    private def collectParam(param: Trees.ParamDef): Unit =
      collectType(param.ptpe)

    private def collectMethodName(name: Names.MethodName): Unit =
      name.paramTypeRefs.foreach(collectTypeRef)
      collectTypeRef(name.resultTypeRef)

    private def collectType(tpe: Types.Type): Unit = tpe match
      case Types.ClassType(className, _) =>
        add(className)
      case Types.ArrayType(arrayTypeRef, _) =>
        collectTypeRef(arrayTypeRef)
      case Types.ClosureType(paramTypes, resultType, _) =>
        paramTypes.foreach(collectType)
        collectType(resultType)
      case Types.RecordType(fields) =>
        fields.foreach(field => collectType(field.tpe))
      case _ =>
        ()

    private def collectTypeRef(typeRef: Types.TypeRef): Unit = typeRef match
      case Types.ClassRef(className) =>
        add(className)
      case Types.ArrayTypeRef(base, _) =>
        collectTypeRef(base)
      case transientTypeRef: Types.TransientTypeRef =>
        collectType(transientTypeRef.tpe)
      case _ =>
        ()

    override def traverse(tree: Trees.Tree): Unit =
      collectType(tree.tpe)
      tree match
        case Trees.VarDef(_, _, vtpe, _, _) =>
          collectType(vtpe)
        case Trees.New(className, ctor, _) =>
          add(className)
          collectMethodName(ctor.name)
        case Trees.LoadModule(className) =>
          add(className)
        case Trees.SelectStatic(field) =>
          add(field.name.className)
        case Trees.SelectJSNativeMember(className, member) =>
          add(className)
          collectMethodName(member.name)
        case Trees.Apply(_, _, method, _) =>
          collectMethodName(method.name)
        case Trees.ApplyStatically(_, _, className, method, _) =>
          add(className)
          collectMethodName(method.name)
        case Trees.ApplyStatic(_, className, method, _) =>
          add(className)
          collectMethodName(method.name)
        case Trees.ApplyDynamicImport(_, className, method, _) =>
          add(className)
          collectMethodName(method.name)
        case Trees.NewArray(typeRef, _) =>
          collectTypeRef(typeRef)
        case Trees.ArrayValue(typeRef, _) =>
          collectTypeRef(typeRef)
        case Trees.IsInstanceOf(_, testType) =>
          collectType(testType)
        case Trees.AsInstanceOf(_, tpe) =>
          collectType(tpe)
        case Trees.LoadJSConstructor(className) =>
          add(className)
        case Trees.LoadJSModule(className) =>
          add(className)
        case Trees.ClassOf(typeRef) =>
          collectTypeRef(typeRef)
        case Trees.Closure(_, captureParams, params, restParam, resultType, _, _) =>
          captureParams.foreach(collectParam)
          params.foreach(collectParam)
          restParam.foreach(collectParam)
          collectType(resultType)
        case Trees.CreateJSClass(className, _) =>
          add(className)
        case _ =>
          ()
      super.traverse(tree)

    override def traverseClassDef(classDef: Trees.ClassDef): Unit =
      classDef.superClass.foreach(classIdent => add(classIdent.name))
      classDef.interfaces.foreach(classIdent => add(classIdent.name))
      classDef.jsClassCaptures.foreach(_.foreach(collectParam))
      super.traverseClassDef(classDef)

    override def traverseAnyFieldDef(fieldDef: Trees.AnyFieldDef): Unit =
      collectType(fieldDef.ftpe)
      fieldDef match
        case Trees.JSFieldDef(_, name, _) =>
          traverse(name)
        case _ =>
          ()

    override def traverseMethodDef(methodDef: Trees.MethodDef): Unit =
      collectMethodName(methodDef.name.name)
      methodDef.args.foreach(collectParam)
      collectType(methodDef.resultType)
      super.traverseMethodDef(methodDef)

    override def traverseJSConstructorDef(jsConstructor: Trees.JSConstructorDef): Unit =
      jsConstructor.args.foreach(collectParam)
      jsConstructor.restParam.foreach(collectParam)
      super.traverseJSConstructorDef(jsConstructor)

    override def traverseJSMethodPropDef(jsMethodPropDef: Trees.JSMethodPropDef): Unit =
      jsMethodPropDef match
        case Trees.JSMethodDef(_, name, args, restParam, _) =>
          traverse(name)
          args.foreach(collectParam)
          restParam.foreach(collectParam)
        case Trees.JSPropertyDef(_, name, getterBody, setterArgAndBody) =>
          traverse(name)
          getterBody.foreach(tree => collectType(tree.tpe))
          setterArgAndBody.foreach { case (arg, body) =>
            collectParam(arg)
            collectType(body.tpe)
          }
      super.traverseJSMethodPropDef(jsMethodPropDef)
