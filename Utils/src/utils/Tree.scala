package utils

import scala.collection.mutable.{ListBuffer,LinkedHashMap}
import scala.collection.{TraversableLike,Map}
import scala.collection.MapProxy
import scala.collection.DefaultMap
import java.io.Writer


/**
 * This class lets define a named tree (not binary.)
 * Each node and leaf has a name. A sequence from top to leaf forms a path.
 * The order is defined by the underlying map type used. This is defined
 * by the empty map in the builder.
 * 
 * @param name  the local name of the current node
 * @param cur   the value for the current node
 * @param next  the list of children for that node, by their names
 * @param X     the type of the name
 * @param T     the type of the value associated with a leaf/node
 * @param This  the type for the implementing class
 */
trait TreeLike[X,+T,+This<:TreeLike[X,T,This]] extends Map[X,This] { this:This=>
  protected[this] val builder:TreeLikeBuilder[X,T,This]
  def name:X
  def cur:Option[T]
  def self:Map[X,This]
  def parent:This
  /** an utility to get an empty tree of the same class */
  def emptyTree = builder.empty
  /** builds the path to go from the top to the current element; the last element is itself */
  def path:List[X] = { val l=new ListBuffer[X]; var c=this; do { c.name +=: l; c=c.parent } while (c.parent!=null); l.toList }
  /** builds the list of ascendants */
  def parents:List[This] = { val l=new ListBuffer[This]; var c=this; while (c.parent.name!=null) { val x=c.parent; l+=x; c=x } ; l.toList }
  /** runs through all entries which hold data ; first param is the reverse path to that entry */
  def forDefined[U](f:(This)=>U):Unit = {
    if (cur!=None) f(this)
    for (x <- self) x._2.forDefined(f)
  }
  /** flattens the tree to it's canonical state */
  def canonical:List[(List[X],T)] = {
    var l = new ListBuffer[(List[X],T)]
    forDefined { (t:This) => l += (((t::t.parents).reverse.map(_.name),t.cur.get)) }
    l.toList
  }
  /** flattens the tree to it's canonical state */
  def flatten:Iterable[This] = self.values
  /** Tries to turn a one level deep hierarchy to a map*/
  def toMap:Map[X,T] = try {
    val m = builder.emptyMap[T]
    for (v <- self) {
      if (v._2.self!=None) throw new IllegalStateException("cannot transform hierarchy to map if not exactly 1 level deep")
      m += ((v._1,v._2.cur.get))
    }
    m
  } catch {
    case _:Throwable => throw new IllegalStateException("cannot transform hierarchy to a map"+this)
  }
  /** 'Removes' elements */
  def filter(f:(X,T)=>Boolean):This = {
    builder.fromCanonical(canonical.filter(x=>f(x._1.head,x._2)))
  }
  /** 'Replaces' T with f(this):S in all nodes */
  def map[S](f:(This)=>Option[S]):builder.Repr[S] = {
    def doIt(t:This,prev: =>builder.Repr[S]):builder.Repr[S] = {
      var res = null.asInstanceOf[builder.Repr[S]]
      val n = builder.emptyMap[builder.Repr[S]]
      t.self.foreach { u => n.put(u._1,doIt(u._2,res)) }
      res = builder.mapBuild(t.name,f(t),n,prev)
      res
    }
    doIt(this,null.asInstanceOf[builder.Repr[S]])
  }
  /** 'Develops' f(this) in all nodes, enlarging the tree */
  def flatMap[S](f:(This)=>builder.Repr[S]):builder.Repr[S] =
    builder.mapFromCanonical(for (x <- flatten; z <- f(x).canonical) yield (x.parents.map(_.name) ::: z._1, z._2))  
  /** String representation */
  override def toString = s"${if (cur.isEmpty) "" else s"${cur.get}"}${if (self.isEmpty) "" else s" => $self"}"
  def print(prefix:String, out:Writer):Unit = {
    if (cur!=None) { out.write(prefix); out.write(" = "); out.write(cur.get.toString); out.write("\n") }
    for (t <- self) {
      val p = if (prefix.isEmpty) t._1.toString else prefix+"."+t._1.toString
      t._2.print(p,out)
    }
  }
}

trait TreeLikeBuilder[X,T,This<:TreeLike[X,T,This]] {
  import scala.language.higherKinds
  import scala.collection.mutable.Map
  type Repr[t]<:TreeLike[X,t,_]
  /** Builder for this class ; useful for map etc operations*/
  def mapBuild[S](name:X,cur:Option[S],next:Map[X,Repr[S]],prev: =>Repr[S]):Repr[S]
  def mapBuildBind[S](name:X,cur:Option[S],next:Map[X,Repr[S]],prev: =>Repr[S]):Repr[S] = {val r=mapBuild(name,cur,next,prev); prev.self.asInstanceOf[Map[X,Repr[S]]].put(name,r); r}
  /** Builder for this class with no type change ; useful for map etc operations*/
  def build(name:X,cur:Option[T],next:Map[X,This],prev: =>This):This
  def buildBind(name:X,cur:Option[T],next:Map[X,This],prev: =>This):This = {val r=build(name,cur,next,prev); prev.self.asInstanceOf[Map[X,This]].put(name,r); r}
  /** Empty map representing the underlying map class to use. LinkedHashMap by default */
  def emptyMap[Y]:scala.collection.mutable.Map[X,Y] = LinkedHashMap.empty
  def empty=build(null.asInstanceOf[X],None,emptyMap,null.asInstanceOf[This])
  /** inner utility : develops one level of data by tearing out the first elt of all inner iterables. */
  protected def develop[S](data:Traversable[(Traversable[X],S)]):Map[X,_<:(Option[S],Traversable[(Traversable[X],S)])] = {
    val h = emptyMap[(Option[S],List[(Traversable[X],S)])]
    for (x <- data; first=x._1.head; value=(x._1.tail,x._2)) h.put(first,(value._1.isEmpty,h.get(first)) match {
      case (false,None)    => (None,List(value))    //create entry: intermediate leaf, init first child
      case (true, None)    => (Some(value._2),Nil)  //create entry: final leaf, put value, no children
      case (false,Some(l)) => (l._1,value::l._2)    //update entry: intermediate leaf, keep current value, update children
      case (true, Some(l)) => (Some(value._2),l._2) //update entry: final leaf, put value, keep children
    })
    h //note that children lists are in reverse order
  }
  /** Develops data to build a 'Tree' beginning with the (name,cur) leaf, associated with data expanded as subtree */
  def fromCanonical[U,V<:TreeLike[X,U,_]](name:X,cur:Option[U],data:Traversable[(Traversable[X],U)],prev: =>V,builder:(X,Option[U],Map[X,V],=>V)=>V):V = {
    if (data.isEmpty)
      builder(name,cur,Map.empty,prev)
    else {
      var res:V = null.asInstanceOf[V]
      val r = emptyMap[V]
      develop(data).foreach { x =>  //don't use map: wrong Map type due to no canBuildFrom
        r.put(x._1,fromCanonical(x._1,x._2._1,x._2._2,res,builder))
      }
      res=builder(name,cur,r,prev)  //important! sets the captured res in the previous closure ( =>res )
      res
    }
  }
  /** Develops the flat representation of the tree into the hierarchical representation. Inverse of flatten. */
  def fromCanonical(data:Traversable[(Traversable[X],T)]):This =
    fromCanonical(null.asInstanceOf[X],None,data,null.asInstanceOf[This],build)
  /** Develops the flat representation of the tree into the hierarchical representation. Inverse of flatten. */
  def mapFromCanonical[S](data:Traversable[(Traversable[X],S)]):Repr[S] =
    mapFromCanonical(null.asInstanceOf[X],None,data,null.asInstanceOf[Repr[S]])
  def mapFromCanonical[S](name:X,cur:Option[S],data:Traversable[(Traversable[X],S)],prev: =>Repr[S]):Repr[S] =
    fromCanonical[S,Repr[S]](null.asInstanceOf[X],None,data,null.asInstanceOf[Repr[S]],mapBuild)
  /** 'Replaces' T with f(t) in all nodes ; cannot place in TreeLike as it breaks the covariance. */
  def map(tree:This,f:(This)=>T):This =
    fromCanonical(for (x <- tree.values) yield (x.parents.map(_.name),f(x)))
}

/** A TreeLike that binds strongly the types. It forbids mapping operations.
 *  It allows put operations and warrants that subtrees are of the same class as the current tree.
 */
trait StrongTreeLike[X,T,This<:StrongTreeLike[X,T,This]] extends TreeLike[X,T,This] with Mutable[X,T,This] { this:This=>
}

/** Breaks covariance ; mixin where no covariance involved
 */
trait Mutable[X,T,This<:StrongTreeLike[X,T,This]] { this:This=>
  /** puts an entry in the map with a value and an empty element map */
  def put(name:X,value:Option[T]):This = builder.buildBind(name, value, builder.emptyMap, this)  
}

/** A standard implementation, likely sufficient for most needs.
 *  Subclassing it is tricky ; better to write your own class if necessary!
 */
final class Tree[X,+T](val name:X,val cur:Option[T],val self:Map[X,Tree[X,T]],prv: =>Tree[X,T]) extends MapProxy[X,Tree[X,T]] with TreeLike[X,T,Tree[X,T]] {
  protected[this] val builder = Tree.builder[X,T]()
  def parent = prv
}

/** Factory for Tree
 */
object Tree {
  import scala.collection.mutable.Map
  
  //interesting type
  //type XTree[+T<:XTree[T]] = Traversable[(String,Either[T,String])]
  
  def builder[X,T]() = new TreeLikeBuilder[X,T,Tree[X,T]] {
    type Repr[t] = Tree[X,t]
    type This    = Repr[T]
    def mapBuild[S](name:X,cur:Option[S],next:Map[X,Repr[S]],prev: =>Repr[S]):Repr[S] = Tree(name,cur,next,prev)
    def build(name:X,cur:Option[T],next:Map[X,This],prev: =>This):This = Tree(name,cur,next,prev)
  }
  def apply[X,T](name:X,cur:Option[T],next:Map[X,Tree[X,T]],prev: =>Tree[X,T]):Tree[X,T] = new Tree(name,cur,next,prev)
  def apply[X,T](data:Traversable[(Traversable[X],T)]):Tree[X,T] = builder().fromCanonical(data)
  def empty[X,T] = builder[X,T]().empty
  
  /** for testing purposes */
  def main(args:Array[String]):Unit = {
    val l = List((List("x","y","z"),"0"),(List("x","y"),"1"),(List("x","u","v"),"2"),(List("x","y","t"),"3"))
    val r = Tree(l)
    val r1 = r.map[Int](_.cur.map(Integer.parseInt))
    val r2 = r1.filter((s,i)=>i>=2)
    println(r)
    println(r.canonical)
    println(r1)
    println(r1.canonical)
    println(r2)
    println(r2.canonical)
    //TreeBuilder.map and flatMap untested yet
  }
}

/** A standard implementation for strong immutable tree bindings, likely sufficient for most needs.
 */
class StrongTree[X,T](val name:X,val cur:Option[T],val self:Map[X,StrongTree[X,T]],prv: =>StrongTree[X,T]) extends MapProxy[X,StrongTree[X,T]] with StrongTreeLike[X,T,StrongTree[X,T]] with Mutable[X,T,StrongTree[X,T]] {
  val builder = StrongTree.builder[X,T]()
  def parent = prv
}

/** The builder for strong trees
 */
object StrongTree {
  def builder[X,T]() = new TreeLikeBuilder[X,T,StrongTree[X,T]] {
    import scala.collection.mutable.Map
    type Repr[t] = StrongTree[X,t]
    def mapBuild[S](name:X,cur:Option[S],next:Map[X,Repr[S]],prev: =>Repr[S]):Repr[S] = StrongTree(name,cur,next,prev)
    def build(name:X,cur:Option[T],next:Map[X,StrongTree[X,T]],prev: =>StrongTree[X,T]):StrongTree[X,T]  = StrongTree(name,cur,next,prev)
  }
  def apply[X,T](name:X,cur:Option[T],next:Map[X,StrongTree[X,T]],prev: =>StrongTree[X,T]):StrongTree[X,T] = new StrongTree(name,cur,next,prev)
  def apply[X,T](data:Traversable[(Traversable[X],T)]):StrongTree[X,T] = builder().fromCanonical(data)
  def empty[X,T] = builder[X,T]().empty
}

/** A standard implementation for strong mutable tree bindings, likely sufficient for most needs.
 */
class StrongMutableTree[X,T](val name:X,val cur:Option[T],val self:scala.collection.mutable.Map[X,StrongMutableTree[X,T]],prv: =>StrongMutableTree[X,T]) extends scala.collection.mutable.MapProxy[X,StrongMutableTree[X,T]] with StrongTreeLike[X,T,StrongMutableTree[X,T]] with Mutable[X,T,StrongMutableTree[X,T]] {
  val builder = StrongMutableTree.builder[X,T]()
  def parent = prv
}

/** The builder for strong trees
 */
object StrongMutableTree {
  import scala.collection.mutable.Map
  def builder[X,T]() = new TreeLikeBuilder[X,T,StrongMutableTree[X,T]] {
    type Repr[t] = StrongMutableTree[X,t]
    def mapBuild[S](name:X,cur:Option[S],next:Map[X,Repr[S]],prev: =>Repr[S]):Repr[S] = StrongMutableTree(name,cur,next,prev)
    def build(name:X,cur:Option[T],next:Map[X,StrongMutableTree[X,T]],prev: =>StrongMutableTree[X,T]):StrongMutableTree[X,T]  = StrongMutableTree(name,cur,next,prev)
  }
  def apply[X,T](name:X,cur:Option[T],next:Map[X,StrongMutableTree[X,T]],prev: =>StrongMutableTree[X,T]):StrongMutableTree[X,T] = new StrongMutableTree(name,cur,next,prev)
  def apply[X,T](data:Traversable[(Traversable[X],T)]):StrongMutableTree[X,T] = builder().fromCanonical(data)
  def empty[X,T] = builder[X,T]().empty
}


object StringTree {
    //finds the next char c. -1 if not found. 'until' excluded.
    protected def find(s:CharSequence,c:Char,from:Int,until:Int):Int = {
      var i=from; if (i<until) do { if (s.charAt(i)==c) return i else i+=1 } while (i<until); -1
    }
    //finds the next closing char c that matches the opening char o. assumes that s(from-1)=o. -1 if not found. 'until' excluded.
    protected def findMatch(s:CharSequence,o:Char,c:Char,from:Int,until:Int):Int = {
      var i=from; var k=1; if (i<until) do { val x=s.charAt(i); if (x==c) { k-=1; if (k==0) return i; } else if (x==o) k+=1; i+=1 } while (i<until); -1
    }
    //returns the param value: (composite,beg,end) ; end excluded, until excluded.
    protected def param(s:CharSequence,o:Char,c:Char,e:Char,sep:Char,from:Int,until:Int):(Boolean,String,Int,Int) = {
      val n = find(s,e,from,until)
      if (n<0) throw new IllegalStateException(s"<$e> was expected in <$s>")
      val name = s.subSequence(from,n).toString
      if (s.charAt(n+1)=='(') {
        val r = findMatch(s,'(',')',n+2,until)
        if (r<0) throw new IllegalStateException(s"opening <$o> unmatched in <$s>")
        (true,name,n+2,r)
      } else {
        val r=find(s,sep,n+1,until)
        (false,name,n+1,if (r<0) until else r)
      }
    }
    protected def params(prev: =>StrongTree[String,String],name:String,cur:Option[String],s:CharSequence,o:Char,c:Char,e:Char,sep:Char,from:Int,until:Int):StrongTree[String,String] = {
      var i = from
      var res:StrongTree[String,String] = null
      val r = scala.collection.mutable.Map.empty[String,StrongTree[String,String]]
      do {
        val recur  = param(s,o,c,e,sep,i,until)
        val cur0   = if (recur._1) None else Some(s.subSequence(recur._3, recur._4).toString)
        val next   = if (recur._1) params(res,recur._2,cur0,s,o,c,e,sep,recur._3,recur._4) else StrongTree.builder[String,String].build(recur._2,cur0,scala.collection.mutable.Map.empty,res)
        r.put(recur._2,next)
        i = find(s,sep,recur._4,until)+1
      } while (i<until && i>0);
      res = StrongTree.builder[String,String].build(name,cur,r,prev)
      res
    }
    /** reads a hierarchical string tree in developed form */
    def apply(s:CharSequence) = params(null,null,None,s,'(',')','=',';',0,s.length)
}
