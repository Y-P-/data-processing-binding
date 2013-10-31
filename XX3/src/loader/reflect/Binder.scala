package loader.reflect

import java.lang.reflect.ParameterizedType
import loader.{Element,Named,NamedField,Context}
import loader.core.exceptions.DynamicInvocation
  
final class Binder(actor:FldActor,ana:Analyze,cv:(String)=>Any,tagEnd:Null) {
  @inline private def named(ld:Element,r:Any):Any = {if (r.isInstanceOf[Named]) r.asInstanceOf[Named].name_=(ld.name); r}
  @inline private def end(ld:Element,r:Any):Any   = if (ana.isNamed && !ld.parentStc.fd.isList) new NamedField(r,ld.name) else named(ld,r)
  final def namedAny(ld:Element,value:Any)        = end(ld,value)
  final def namedString(ld:Element,value:String)  = end(ld,cv(value))
  final def namedStruct(ld:Element,value:AnyRef)  = null //end(ld,tagEnd(named(ld,value).asInstanceOf[AnyRef],ld))
  final def finalValue(value:AnyRef)              = null //tagEnd(value,null)
  final def set(on:AnyRef,value:Any)              = actor.set(on,value)
  final def get(on:AnyRef)                        = actor.get(on)
  final def seq(fd:Context#FieldMapping)          = ana.seq(fd)
  final def newItem                               = ana.getLoadableClass.newInstance
  final def rawClass:Class[_]                     = Analyze.getClass(actor.expected)
  final def paramTypes:Array[Class[_]]            = actor.expected.asInstanceOf[ParameterizedType].getActualTypeArguments.map(Analyze.getClass)
}
object Binder {
  val debug = true
  def apply(ld:Element):Binder = {/*
    val fd = ld.fd.load(ld.parentStc)._1
    if (debug) println(s"binding ${ld.parentStc.fd.loader.id} with ${fd.annot.name}")
    val czL    = if (fd.loader!=null) fd.loader.annot.clzz else null
    val actor  = FldActor(ld.parentStc.fd.loader.annot.clzz,fd.annot.name)
    val ana    = try { Analyze(actor.expected,czL,fd.isList,fd.isSeq) }
                 catch { case d:DynamicInvocation => fd.dynamic(ld.parentStc) }
    val annot  = ld.fd.annot
    val cv     = ana.getConverter match {
      case Some(c) => val converter = c.withParam(annot.param);
                      val validStr  = c.validStr(annot.regex)
                      val valid     = converter.valid(annot.valid)
                      (s:String)=>Converter(converter,fd.annot.fmtin,validStr(s),valid)
      case None    => null  //happens on non terminal fields
    }
    val tagEnd = if (fd.loader!=null) TagEndInvoker(ana.getLoadableClass,annot.tagEnd) else null
    new Binder(actor,ana,cv,tagEnd)*/
    null //XXX à refaire
  }
  def apply(fd:Context#FieldMapping):Binder = {
    new Binder(null,null,null,null /*TagEndInvoker(fd.loader.annot.clzz,""/*XXX fd.annot.tagEnd*/)*/)
  }
}
