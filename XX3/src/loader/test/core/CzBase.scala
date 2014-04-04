package loader.test.core

import loader.annotations.{TagField,TagList,TagSeq}

/** A minimal class to test most object spawning cases, including collections, deep collections,
 *  object nesting, empty lists...
 *  Note that we will not check list/sequence nesting combinations (List[Array[Seq]] or Array[List[List]] etc.
 *  as we rely on the utils.reflect utilities which are tested independently.
 */
abstract class CzBase {
  type Cz<:CzBase
  import utils.Indent
  def id:Int                        //standard scalar field with auto conversion
  def ok:Boolean                    //same, different conversion
  def idA:Array[Double]             //array of scalars
  def idL:List[Integer]             //collection of scalars
  def id2:Array[Array[Double]]      //deep collection of scalars
    
  def cz:Cz                         //object
  def czA:Array[Cz]                 //array of objects
  def czL:List[Cz]                  //collection of objects
  def cz2:Array[List[Cz]]           //deep collection of objects
  def cz3:Array[List[Array[Cz]]]    //deep sequence of objects
    
  def p1 = if (idA==null) "" else if (idA.length==0) "-, " else s"[${idA.mkString(",")}], "
  def p2 = if (idL==null) "" else if (idL.isEmpty) "-, "   else s"$idL, "
  def p3 = if (id2==null) "" else if (id2.length==0) "-, " else s"[${id2.deep.mkString(",")}], "
  def p4(x:Indent) = if (cz==null) "" else s"${cz.p(x)}, "
  def p5(x:Indent):String = if (czA==null) "" else if (czA.length==0) "-, " else {
    val b = new StringBuffer
    b.append(s"${x}czA=[")
    for (v<-czA) b.append(v.p(x(2)))
    b.append(s"${x}],").toString
  }
  def p6(x:Indent):String = if (czL==null) "" else if (czL.isEmpty) "-, " else {
    val b = new StringBuffer
    b.append(s"${x}czL=[")
    for (v<-czL) b.append(v.p(x(2)))
    b.append(s"${x}],").toString
  }
  def p7(x:Indent):String = if (cz2==null) "" else if (cz2.length==0) "-, " else {
    val b = new StringBuffer
    b.append(s"${x}cz2=[")
    for (v1<-cz2) {
      b.append(s"${x(2)}[")
      for (v<-v1) {
        b.append(v.p(x(4)))
      }
      b.append(s"${x(2)}]")
    }
    b.append(s"${x}],").toString
  }
  def p8(x:Indent):String = if (cz3==null || cz3.length==0) "" else {
    val b = new StringBuffer
    b.append(s"${x}cz3=[")
    for (v1<-cz3) {
      b.append(s"${x(2)}[")
      for (v2<-v1) {
        b.append(s"${x(4)}[")
        for (v<-v2) {
          b.append(v.p(x(6)))
        }
        b.append(s"${x(4)}]")
      }
      b.append(s"${x(2)}]")
    }
    b.append(s"${x}],").toString
  }
  def p(x:Indent):String = s"$x{$id, ${if(ok)"true, "else""}$p1$p2$p3${p4(x(2))}${p5(x(2))}${p6(x(2))}${p7(x(2))}${p8(x(2))}}"
  override def toString = p(new Indent(0))
}

object CzBase {

  //annotation fully filled
  class FullAnnot extends CzBase {
    type Cz = FullAnnot
    @TagField          val id:Int = 0                           //standard scalar field with auto conversion
    @TagField          val ok:Boolean = false                   //same, different conversion
    @TagList(depth=1)  val idA:Array[Double] = null             //array of scalars
    @TagSeq(depth=0)   val idL:List[Integer] = null             //collection of scalars
    @TagList(depth=2)  val id2:Array[Array[Double]] = null      //deep collection of scalars
    
    @TagField(loader=classOf[Cz])         val cz:Cz = null                       //object
    @TagSeq(loader=classOf[Cz], depth=0)  val czA:Array[Cz] = null               //array of objects
    @TagList(loader=classOf[Cz], depth=1) val czL:List[Cz] = null                //collection of objects
    @TagList(loader=classOf[Cz], depth=2) val cz2:Array[List[Cz]] = null         //deep collection of objects
    @TagSeq(loader=classOf[Cz], depth=2)  val cz3:Array[List[Array[Cz]]] = null  //deep sequence of objects
  }
  //class to load inferred
  class InferAnnot extends CzBase {
    type Cz = InferAnnot
    @TagField          val id:Int = 0                           //standard scalar field with auto conversion
    @TagField          val ok:Boolean = false                   //same, different conversion
    @TagList(depth=1)  val idA:Array[Double] = null             //array of scalars
    @TagSeq(depth=0)   val idL:List[Integer] = null             //collection of scalars
    @TagList(depth=2)  val id2:Array[Array[Double]] = null      //deep collection of scalars
    
    @TagField         val cz:Cz = null                        //object
    @TagSeq(depth=0)  val czA:Array[Cz] = null                //array of objects
    @TagList(depth=1) val czL:List[Cz] = null                 //collection of objects
    @TagList(depth=2) val cz2:Array[List[Cz]] = null          //deep collection of objects
    @TagSeq(depth=2)  val cz3:Array[List[Array[Cz]]] = null   //deep sequence of objects    
  }
  //class to load and list/seq depth inferred
  class FullInferAnnot extends CzBase {
    type Cz = FullInferAnnot
    @TagField val id:Int = 0                           //standard scalar field with auto conversion
    @TagField val ok:Boolean = false                   //same, different conversion
    @TagList  val idA:Array[Double] = null             //array of scalars
    @TagSeq   val idL:List[Integer] = null             //collection of scalars
    @TagList  val id2:Array[Array[Double]] = null      //deep collection of scalars
    
    @TagField val cz:Cz = null                         //object
    @TagSeq   val czA:Array[Cz] = null                 //array of objects
    @TagList  val czL:List[Cz] = null                  //collection of objects
    @TagList  val cz2:Array[List[Cz]] = null           //deep collection of objects
    @TagSeq   val cz3:Array[List[Array[Cz]]] = null    //deep sequence of objects    
  }
  //class to load and missing annotations inferred
  class TotalInferAnnot extends CzBase {
    type Cz = TotalInferAnnot
    val id:Int = 0                                     //standard scalar field with auto conversion
    val ok:Boolean = false                             //same, different conversion
    val idA:Array[Double] = null                       //array of scalars
    @TagSeq   val idL:List[Integer] = null             //collection of scalars
    val id2:Array[Array[Double]] = null                //deep collection of scalars
    
    val cz:Cz = null                                   //object
    @TagSeq   val czA:Array[Cz] = null                 //array of objects
    val czL:List[Cz] = null                            //collection of objects
    val cz2:Array[List[Cz]] = null                     //deep collection of objects
    @TagSeq   val cz3:Array[List[Array[Cz]]] = null    //deep sequence of objects    
  }
  //everything inferred! (no sequences)
  class TotalInferExt extends CzBase {
    type Cz = TotalInferExt
    val id:Int = 0                                     //standard scalar field with auto conversion
    val ok:Boolean = false                             //same, different conversion
    val idA:Array[Double] = null                       //array of scalars
    val idL:List[Integer] = null                       //collection of scalars
    val id2:Array[Array[Double]] = null                //deep collection of scalars
    
    val cz:Cz = null                                   //object
    val czA:Array[Cz] = null                           //array of objects
    val czL:List[Cz] = null                            //collection of objects
    val cz2:Array[List[Cz]] = null                     //deep collection of objects
    val cz3:Array[List[Array[Cz]]] = null              //deep sequence of objects    
  }}