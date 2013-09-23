package loader.core.context

import scala.collection.mutable.HashMap
import utils.RegexMap

/** Tag manager.
 *  It behaves roughly like a map.
 */
trait TagMap {
  def fetch(name: String): Option[loader.core.context.Context#FieldMapping]
  def put(mapping:(String,loader.core.context.Context#FieldMapping)):Unit
  def asXml(tab:Int):String
  def hasNonContig:Boolean
  def size:Int
  def values:Iterable[loader.core.context.Context#FieldMapping]
}

/**
 *  A TagMap that can handle regex.
 */
final protected class RegexTagMap extends TagMap { self=>
  //fields or field/attribute definitions
  //implementation choice: standard fields are expected to be much more common that partitioned ones (such as attributes)
  //the following map is direct for standard fields and carries no overhead.
  //partitioned fields are prefixed by their partition name followed by '°'
  private var flds = RegexMap(HashMap.empty[String,Context#FieldMapping])
  /** indicates sequences are expected */
  lazy val hasSeqs = flds!=null && flds.exists(_._2.annot.isSeq)
  /** indicates that non contiguous sequences are expected */
  lazy val hasNonContig = hasSeqs && (flds.exists ((x:(String,Context#FieldMapping))=>x._2.annot.isSeq && !x._2.annot.contiguous))
  
  def size   = flds.size
  def values = flds.values
  def fetch(name:String) = flds.get(name)
  def put(mapping:(String,loader.core.context.Context#FieldMapping)) = flds.put(mapping._1,mapping._2)
  
  def asXml(tab:Int):String = s"${flds.values.foldLeft("")((x,y)=>x+XmlHelper.t(tab)+y.annot.asXml)}"
}