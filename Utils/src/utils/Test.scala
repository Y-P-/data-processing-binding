package utils

import scala.collection.mutable.ArrayBuffer
import scala.collection.AbstractSeq
import scala.collection.IndexedSeq
import scala.collection.SeqLike
import scala.collection.mutable.Builder

class Véhicule(val nbRoues:Int,val prix:Int,val propriétaire:String,val poids:Int)

class Camion(nbRoues:Int,prix:Int,propriétaire:String,poids:Int, val contenance:Int)
      extends Véhicule(nbRoues,prix,propriétaire,poids)
object Camion {
  def apply(nbRoues:Int,prix:Int,propriétaire:String,poids:Int, contenance:Int) =
    new Camion(nbRoues,prix,propriétaire,poids,contenance)
}

class Voiture(nbRoues:Int,prix:Int,propriétaire:String,poids:Int, val nbPortes:Int)
      extends Véhicule(nbRoues,prix,propriétaire,poids)
object Voiture {
  def apply(nbRoues:Int,prix:Int,propriétaire:String,poids:Int, nbPortes:Int) =
    new Voiture(nbRoues,prix,propriétaire,poids,nbPortes)
}

class Véhicules[+V<:Véhicule](list:ArrayBuffer[V]) extends AbstractSeq[V] with IndexedSeq[V] with SeqLike[V,Véhicules[V]] {
  def apply(idx:Int) = list(idx)
  def length = list.size
  override protected [this] def newBuilder:Builder[V,Véhicules[V]] = new Builder[V,Véhicules[V]] {
    protected val l = ArrayBuffer.empty[V]
    def +=(elem: V): this.type = { l += elem; this }
    def clear(): Unit = l.clear
    def result(): Véhicules[V] = new Véhicules(l)
  }
}
object Véhicules {
  def apply = new Véhicules(ArrayBuffer.empty)
  def apply[V<:Véhicule](v:V*) = new Véhicules(ArrayBuffer(v:_*))
}


object Test {
  val c = Véhicules(Camion(6,50000,"Albert",6000,50),Camion(4,30000,"Bernard",3000,25),Camion(8,100000,"Albert",9000,120))
  val v = Véhicules(Voiture(4,12000,"Albert",1400,5),Voiture(4,8000,"Bernard",800,3))
  val h = c ++ v
}

final class Angle protected (val a:Double) extends AnyVal {
  def sin  = Math.sin(a)
  def cos  = Math.cos(a)
  def tan  = Math.tan(a)
  def +(x:Angle)  = new Angle(a+x.a)
  def -(x:Angle)  = new Angle(a-x.a)
  def *(x:Double) = new Angle(a*x)
  def /(x:Double) = new Angle(a/x)
  def <(x:Angle)  = a<x.a
  def <=(x:Angle) = a<=x.a
  def >(x:Angle)  = a>x.a
  def >=(x:Angle) = a>=x.a
  def toRadian:Double = a
  def toGrade:Double  = a*Angle.gr
  def toDegree:Double = a*Angle.dg
}
object Angle {
  val pi = new Angle(Math.PI)
  protected val gr = 200.0/Math.PI
  protected val dg = 180.0/Math.PI
  def atan(d:Double) :Angle  = new Angle(Math.atan(d))
  def degree(d:Double):Angle = new Angle(d/dg)
  def radian(d:Double):Angle = new Angle(d)
  def grade(d:Double):Angle  = new Angle(d/gr)
}

object X {
  import Angle._
  def main(args:Array[String]):Unit = {
    val x = (degree(125) - grade(25)).cos   //calcule cos(125 degrés - 25 grades) => résultat Double
    val y = (pi-degree(45))/2               //calcule (PI-45°)/2                  => résultat Angle
    val z = pi/3 + degree(45)               //calcule l'angle PI/3 rd + 45 °      => résultat Angle
    val t = (y-z)*x                         //soustrait l'angle z de l'angle y et multiplie par le coefficient x => résultat Angle
   // val u = 2*z                             //interdit: il existe Angle*Double, mais pas Double*Angle!
    val b = y>z                             //vérifie si y est supérieur à z
  }
}
