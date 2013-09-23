package validator

object BorderFactory {
  val defFmt = new Formatter()
  def apply[T](comp:(T,T)=>Int,succ:(T,T)=>Boolean=null,format:Formatter=defFmt) = new BorderFactory[T] {
    def compare(t1:T,t2:T):Int = comp(t1,t2)
    def discret:Boolean = succ!=null
    def isSucc(t1:T,t2:T):Boolean = succ(t1,t2)
    val fmt:Formatter = format
  }
}

abstract class BorderFactory[-T] {
  def compare(t1:T,t2:T):Int     // ~ t1-t2
  def discret:Boolean            // discreet space for T and each element has a successor, except last
  def isSucc(t1:T,t2:T):Boolean  // t2 = t1+1
  val fmt:Formatter              // data for analysis/formatting

  sealed protected[this] trait AbstractLimit {
    /** The limit for this border */
    protected[this] def limit:T
    /** type for the reverse border, the one that accept exactly the opposite of this one */
    protected[BorderFactory] type R<:AbstractLimit
    
    /** compares l with this border and returns the pair (min,max) ; if both are equal, then min eq max */
    protected[BorderFactory] def order[X>:this.type<:Side](l:X):(X,X)  //the curious generic signature warrants that only same sides can be compared
    /**@return the min between this and l */
    final def min[X>:this.type<:Side](l:X):X = order(l)._1 
    /**@return the max between this and l */
    final def max[X>:this.type<:Side](l:X):X = order(l)._2
    
    /** tells whether t is the predecessor of limit */
    protected def isPred(t:T):Boolean = BorderFactory.this.isSucc(limit,t)
    /** tells whether l.limit is the successor of limit */
    protected[BorderFactory] def isSucc(l:AbstractLimit):Boolean = l.isPred(limit)
    /** tells whether this limit is accepted by l */
    protected[BorderFactory] def isAcceptedBy(l:AbstractLimit):Boolean = l.check(limit)
    
    /** Comparison between borders : negative if l greater than limit, zero if equal */
    final protected[BorderFactory] def minus(l:AbstractLimit):Int = l.cmp(limit)
    /** How s compares to the border : positive if t greater than limit, zero if equal */
    protected[BorderFactory] def cmp(t:T):Int
    /** How s compares to the border : true if s is accepted (on the good side) */
    protected[BorderFactory] def check(t:T):Boolean
    /** The reverse border ; see R's definition */
    protected[BorderFactory] def reverse:R
    /** True if the limit is on the good side of the border */
    def closed:Boolean
    /** String representing for the border type */
    def symbol(fmt:Formatter):String
    /** Limit's value as string representation */
    def limitAsString(fmt:Formatter):String
    /** How to format this */
    protected def stringOrder:(Formatter=>String,Formatter=>String)
    /** formatting using any formatter*/
    final def mkString(fmt:Formatter):String = { val so=stringOrder; so._1(fmt)+so._2(fmt) }
    /** formatting using default formatter*/
    override def toString = mkString(fmt)
  }

  trait Side extends AbstractLimit
  trait Closed { protected[BorderFactory] type R<:Open;   final def closed=true;  }
  trait Open   { protected[BorderFactory] type R<:Closed; final def closed=false; }
  abstract class Left  extends Side { protected[BorderFactory] type R<:Right;  final protected def stringOrder = (symbol _, limitAsString _) }
  abstract class Right extends Side { protected[BorderFactory] type R<:Left;   final protected def stringOrder = (limitAsString _, symbol _) }

  /**
   * This trait provides common behaviors on infinite borders.
   */
  trait InfiniteLimit extends AbstractLimit {
    final protected[this] val limit:T = null.asInstanceOf[T]
    final def closed = false
    final def check(t:T):Boolean = true  //any value is accepted
    final override def isSucc(l:AbstractLimit) = false
    final override def equals(that: Any):Boolean = that match {
      case l:InfiniteLimit => eq(l)
      case _ => false
    }        
  }
  /**
   * This trait provides common behaviors on finite borders.
   */
  protected[this] trait ValuedLimit extends Side {
    protected[this] val limit:T
    protected[BorderFactory] type R<:ValuedLimit
    final def cmp(t:T):Int = compare(t,limit)
    /** compares l with this border and returns the pair (min,max) ; if both are equal, then min eq max */
    final protected[BorderFactory] def order[X>:this.type<:Side](l:X):(X,X) = {
      val r = this minus l
      if      (r<0)                          (asInstanceOf[X],l)  //these cast are safe! X>:this.type
      else if (r>0)                          (l,asInstanceOf[X])
      else if (closed == l.closed)           (l,l)
      else if (closed ^ isInstanceOf[Right]) (asInstanceOf[X],l)
      else                                   (l,asInstanceOf[X])
    }
    def limitAsString(fmt:Formatter) = limit.toString
    override val hashCode = limit.hashCode
    override def equals(that: Any):Boolean = that match {
      case i:ValuedLimit => i.cmp(limit)==0
      case _ => false
    }
  }  
  final class LeftOpen(protected[this] val limit:T) extends Left with Open with ValuedLimit {
    protected[BorderFactory] type R = RightClosed
    final def reverse = new RightClosed(limit)
    final def symbol(fmt:Formatter) = fmt.leftOpen 
    final def check(t:T):Boolean = cmp(t)>0
  }
  final class RightClosed(protected[this] val limit:T) extends Right with Closed with ValuedLimit {
    protected[BorderFactory] type R = LeftOpen
    final def reverse = new LeftOpen(limit)
    final def symbol(fmt:Formatter) = fmt.leftOpen 
    final def check(t:T):Boolean = cmp(t)<=0
  }
  final class LeftClosed(protected[this] val limit:T) extends Left with Closed with ValuedLimit {
    protected[BorderFactory] type R = RightOpen
    final def reverse = new RightOpen(limit)
    final def symbol(fmt:Formatter) = fmt.rightOpen 
    final def check(t:T):Boolean = cmp(t)>=0
  }
  final class RightOpen(protected[this] val limit:T) extends Right with Open with ValuedLimit {
    protected[BorderFactory] type R = LeftClosed
    final def reverse = new LeftClosed(limit)
    final def symbol(fmt:Formatter) = fmt.rightOpen 
    final def check(t:T):Boolean = cmp(t)<0
  }
  /**
   * This object specifies an infinite border on the left side (i.e. -oo).
   * It doesn't depend on the type because infinity always behaves in the same way.
   */
  object InfiniteLeft extends Left with InfiniteLimit  {
    protected[BorderFactory] type R = InfiniteRight.type
    final def cmp(t:T):Int = 1
    final def reverse = InfiniteRight
    final protected[BorderFactory] def order[X>:this.type<:Side](l:X):(X,X) = (asInstanceOf[X],l)
    final def limitAsString(fmt:Formatter) = fmt.leftInfinite 
    final def symbol(fmt:Formatter) = fmt.leftOpen
  }
  /**
   * This object specifies an infinite border on the right side (i.e. +oo).
   * It doesn't depend on the type because infinity always behaves in the same way.
   */
  object InfiniteRight extends Right with InfiniteLimit {
    protected[BorderFactory] type R = InfiniteLeft.type
    final def cmp(t:T):Int = -1
    final def reverse = InfiniteLeft
    final protected[BorderFactory] def order[X>:this.type<:Side](l:X):(X,X) = (l,asInstanceOf[X])
    final def limitAsString(fmt:Formatter) = fmt.rightInfinite
    final def symbol(fmt:Formatter) = fmt.rightOpen
  }

  /**
   * Interval constructor.
   */
  object Interval {
    implicit def fmt = BorderFactory.this.fmt
    /**
     * The empty interval is common to all types : it doesn't match anything!
     */
    val empty = new Interval(null,null) {
      override def union(i:Interval):Intervals    = Intervals(i)
      override def intersect(i:Interval):Interval = this
      override def mkString(fmt:Formatter) = fmt.leftOpen + fmt.rightOpen 
      override def toString = mkString(fmt) 
    }
        
    /**@return true if the borders |x,r|,|l,y| can be merged and both intervals reduced to |x,y| */
    private def canMerge(r:Right, l:Left):Boolean = {
      val d = l minus r
      return d<0 || d==0 && (r.closed || l.closed) ||
            (discret && r.closed && l.closed && l.isSucc(r))
    }
        
    /**@returns -1 is |l,r| is empty, 0 if singleton, 1 if an interval */
    private def joining(l:Left, r:Right):Int = {
      val d = l minus r
      if (d>0)  return -1
      if (d==0) return if (r.closed && l.closed) 0 else -1
      if (!discret) return 1
      if (!r.isSucc(l)) return 1
      if (r.closed ^ l.closed) return 0
      if (r.closed && l.closed) return 1
      return -1
    }
    
    /**
     * Constructor using borders<br>
     * This is the only accessible constructor for Intervals from borders.<br>
     * @return the associated interval.
     */
    def apply(left:Left,right:Right):Interval = {
      val iL = left  eq InfiniteLeft
      val iR = right eq InfiniteRight
      if (iL && iR) return new Interval(left,right) { override def check(t:T) = true }
      if (iL)       return new Interval(left,right) { override def check(t:T) = right.check(t) }
      if (iR)       return new Interval(left,right) { override def check(t:T) = left.check(t) }
      joining(left,right) match {
        case -1 => return empty
        case  0 => return new Interval(left,right) {
                     override def check(t:T) = left.cmp(t)==0
                     override def check(x:AbstractLimit) = (left minus x)==0
                   }
        case 1 => return new Interval(left,right)
      }
    }
    /**
     * Constructor using a parser and a String.<br>
     * Format 1: [a,b[ (for example for the semi open (on b) interval a,b)<br>
     * Format 2: >=a (for a semi infinite interval)<br>
     * Format 3: a the singleton interval [a,a]<br>
     * It is possible to use +oo or -oo instead of the format 2.<br>
     * The constructor assumes that a given value represented as a string contains no space.<br>
     * This is the second way to build an interval.
     * 
     * @param s  string to parse
     * @param p  base parser for the values
     * @param separator the separator between interval definitions
     * @return (unmatched section, interval read)
     */
    def parse(s:String, p:(String)=>T,fmt:Formatter=BorderFactory.this.fmt):(String,Interval) = {
      val m1 = fmt.f1.matcher(s)
      if (m1.matches) {
         val leftOpen      = m1.group(1)==fmt.leftOpen 
         val leftInfinite  = m1.group(2)!=null
         val leftVal       = m1.group(3)
         val rightOpen     = m1.group(6)==fmt.rightOpen 
         val rightInfinite = m1.group(4)!=null
         val rightVal      = m1.group(5)
         val left  = if (leftInfinite)  InfiniteLeft  else if (leftOpen)  new LeftOpen(p(leftVal))   else new LeftClosed(p(leftVal))
         val right = if (rightInfinite) InfiniteRight else if (rightOpen) new RightOpen(p(rightVal)) else new RightClosed(p(rightVal))
         return (if (m1.group(8)==null) "" else m1.group(8),Interval(left,right))
      } else {
        val m2 = fmt.f2.matcher(s)
        if (m2.matches) {
          val isLeft = m2.group(4)!=null || m2.group(5)!=null
          val isOpen = m2.group(3)!=null || m2.group(5)!=null
          val v      = m2.group(6)
          val left  = if (!isLeft) InfiniteLeft  else if (isOpen) new LeftOpen(p(v))  else new LeftClosed(p(v))
          val right = if (isLeft)  InfiniteRight else if (isOpen) new RightOpen(p(v)) else new RightClosed(p(v))
          return (if (m2.group(7)==null) "" else m2.group(7),Interval(left,right))
        } else {
          val m3 = fmt.f3.matcher(s)
          if (m3.matches)
            return (if (m3.group(2)==null) "" else m3.group(2),Interval(new LeftClosed(p(m3.group(1))),new RightClosed(p(m3.group(1)))))
          else
            throw new java.text.ParseException(s,0)
        }
      }
    }
    /**
     * Constructor from a String.<br>
     * The string is parsed from the beginning until an interval is read. Any excess characters are ignored.<br>
     * @param s  string to parse
     * @param p  base parser for the values
     * @return the interval at the start of s
     */
    def apply(s:String, p:(String)=>T,fmt:Formatter=BorderFactory.this.fmt):Interval = parse(s,p,fmt)._2
    def unapply(i:Interval):Option[(Left,Right)] = Some((i.left,i.right))
  }
  
  /**
   * The interval class.<br>
   */
  class Interval(val left:Left,val right:Right) {
    import Interval.unapply
    def check(v:T):Boolean = left.check(v) && right.check(v) 
    protected def check(l:AbstractLimit):Boolean =
      l.isInstanceOf[ValuedLimit] && l.isAcceptedBy(left) && l.isAcceptedBy(right) 
    /**
     * Complementary 
     * @return ordered liste of the intervals forming the complementary
     */
    def reverse:Intervals = {
      if ((left eq InfiniteLeft) && (right eq InfiniteRight)) return Intervals.empty
      if (left eq InfiniteLeft)   return Intervals(Interval(right.reverse,InfiniteRight))
      if (right eq InfiniteRight) return Intervals(Interval(InfiniteLeft,left.reverse))
      return Intervals(Interval(InfiniteLeft,left.reverse),Interval(right.reverse,InfiniteRight))
    }
    /**
     * Intersection de deux intervalles. 
     * @param i  second intervalle de l'union
     * @return l'intervalle intersection
     */
    def intersect(i:Interval):Interval = {
      if (i eq Interval.empty) return i;
      Interval(i.left max left,i.right min right)
    }
    /**
     * Union de deux intervalles. 
     * @param i  second intervalle de l'union
     * @return la liste ordonnée des intervalles composant l'union
     */
    def union(i:Interval):Intervals = {
      if (i eq Interval.empty) return Intervals(this)
      val r = this order i
      if (!Interval.canMerge(r._1.right,r._2.left))
        return new Intervals(List(r._1,r._2))
      return Intervals(new Interval(r._1.left,r._1.right max r._2.right))
    }
        
    /**
     * @param i
     * @return le couple ordonné ('petit','grand') (au sens borne gauche la plus petite)
     */
    def order(i:Interval):(Interval,Interval) = {
      val x = left order i.left
      if (x._1 eq x._2) {
        val y = right order i.right
        if (y._1 eq right)   return (this,i)
        else                 return (i,this)
      } else if (x._1 eq left) return (this,i)
      else                     return (i,this)
    }
        
    def mkString(fmt:Formatter) = left.mkString(fmt)+fmt.internSep+right.mkString(fmt)
    override def toString = mkString(fmt)
    override def hashCode = left.hashCode ^ right.hashCode
    override def equals(that: Any):Boolean = that match {
      case Interval(this.left,this.right) => true
      case _ => false
    }
  }
  
  object Intervals {
    implicit def fmt = BorderFactory.this.fmt
    /** order for intervals by checking position of left */
    protected def lt(x:Interval,y:Interval):Boolean = (x order y)._1 eq x
    val empty:Intervals = new Intervals(List[Interval]())
    /**
     * Constructor from a String and a parser.<br>
     * Individual intervals are separated by externSep<br>
     * Intervals are normalized (regrouped, simplified, ordonned).<br> 
     * @return (unmatched string, intervals read)
     */
    def parse(s:String, p:(String)=>T,fmt:Formatter=BorderFactory.this.fmt):(String,Intervals) = {
      var v:(String,Intervals,Boolean) = (s,empty,false);
      //reads an element and separator. The boolean is true if we must exit.
      def readone(x:(String,Intervals,Boolean)):(String,Intervals,Boolean) = {
        try {
          val r = Interval.parse(x._1,p,fmt)
          val res = x._2 union r._2
          val m = fmt.f4.matcher(r._1)
          try {
            if (m.matches) return (m.group(1), res, false)
            else           return (r._1,       res, true)
          } catch {
             case e:java.text.ParseException => return (x._1,res,true)
          }
        } catch {
          case e:java.text.ParseException => return (x._1,x._2,true)
        }
      }
      while (!v._3) v=readone(v)
      (v._1,v._2)
    }
    /**
     * Different constructors
     */
    def apply(s:String, p:(String)=>T,fmt:Formatter=BorderFactory.this.fmt):Intervals = parse(s,p,fmt)._2
    def apply(i:Interval):Intervals = if (i eq Interval.empty) empty else new Intervals(List(i))
    def apply(i:Interval,j:Interval):Intervals = if (i eq Interval.empty) Intervals(j) else if (j eq Interval.empty) Intervals(i) else { val x=(i order j); new Intervals(List(x._1,x._2)) }
    def apply(i:Interval*):Intervals = Intervals(i.filterNot(_ eq Interval.empty).toList)
    def apply(list:List[Interval]):Intervals = list.filterNot(_ eq Interval.empty) match {
      case Nil => empty
      case l:List[_] => new Intervals(l.sortWith(Intervals.lt))
    }
  }
  /**
   * List of disjoint intervals in increasing order (left side)
   */
  class Intervals(val list:List[Interval]) {
    /**
     * Intersection of Intervals with an Interval.<br>
     * @param i
     * @return normalized resulting Intervals
     */
    def intersect(i:Interval):Intervals = (reverse union i.reverse).reverse
    /**
     * Intersection of Intervals with Intervals.<br>
     * @param i
     * @return normalized resulting Intervals
     */
    def intersect(i:Intervals):Intervals = (reverse union i.reverse).reverse
    /**
     * Complementery for an Intervals.<br>
     * @param i
     * @return normalized resulting Intervals
     */
    def reverse:Intervals = {
      val first = if (list.head.left eq InfiniteLeft) Nil else List(new Interval(InfiniteLeft,list.head.left.reverse))
      val r = list.tail.foldLeft((first,list.head.right))((x:(List[Interval],Right),i:Interval)=>(new Interval(x._2.reverse,i.left.reverse) :: x._1,i.right))
      new Intervals(if (r._2 eq InfiniteRight) r._1.reverse else (new Interval(r._2.reverse,InfiniteRight) :: r._1).reverse)
    }
    /**
     * Union of an Intervals with an Interval.<br>
     * @param i
     * @return normalized resulting Intervals
     */
    def union(i:Interval):Intervals = this union Intervals(List(i))
    /**
     * Union of two Intervals.<br>
     * @param i
     * @return normalized resulting Intervals
     */
    def union(i:Intervals):Intervals = {
      if (list.isEmpty)   return i
      if (i.list.isEmpty) return this
      val l = utils.Algo.merge(list,i.list,Intervals.lt)
      // reduces the intervals list
      // by construction xs contains only consolidated intervals lesser than x
      def reduce(xs:Intervals,x:Interval) = {
        if (xs.list.isEmpty) Intervals(List(x))
        else                 Intervals(xs.list.init ::: (xs.list.last union x).list)
      }
      l.foldLeft(Intervals.empty)(reduce)
    }
    /**
     * Valids the presence of t inside one of the Interval inside Intervals.<br>
     * This is optimzed for as few comparisons as possible (dichotomy).<br>
     * @param t
     * @return
     */
    def check(t:T):Boolean = utils.Algo.dicho[Interval](list,_.left.check(t)) match {
      case Some(i) => i.right.check(t)
      case None    => false
    }
      
    override def equals(that: Any):Boolean = that match {
      case i:Intervals => i.list==list
      case _ => false
    }
    def mkString(fmt:Formatter) = list.tail.foldLeft(list.head.mkString(fmt))((s:String,i:Interval)=>s+fmt.externSep+i.mkString(fmt))
    override def toString = list.mkString(fmt.externSep)
    override def hashCode = list.hashCode
  }
}
