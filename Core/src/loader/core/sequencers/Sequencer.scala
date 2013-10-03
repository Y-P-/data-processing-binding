package loader.core.sequencers

import loader.core.definition.Def
import scala.annotation.tailrec


trait Sequencer[-E<:Def#Elt] {
  /** Rank where this item is expected. This normally begins at 1. There should be no hole for best effect.
   *  However: -1 => always processed last
   *           note that it is not possible to have some items always processed first unless they are always all stored.
   */
  def rank(e:E):Int
  protected[this] def toDo:scala.collection.mutable.SortedSet[(Int,E,E=>Unit)]
  protected var n:Int = 1  //index for next item expected
  //an inactive Sequencer can only store data.
  //That data will only be acted on when the pull() method is invoked.
  protected var isActive = true
  //depending on the state, this will either immediately process e or store it for delayed processing
  def pull0(e:E):Unit           = { val r=Sequencer.mkRank(rank(e)); if (isActive && r<=n) { e.pullX();  update(r>0) } else toDo += ((r,e,_.pullX())) }
  //depending on the state, this will either immediately process e or store it for delayed processing
  def pull1(e:E)(v:e.Kind):Unit = { val r=Sequencer.mkRank(rank(e)); if (isActive && r<=n) { e.pullX(v); update(r>0) } else toDo += ((r,e,_.pullX(v))) }
  //this will process all stored items
  def pull():Unit = for (x <- toDo) x._3(x._2)
  //this will process all eligible stored items.
  //e.g. you receive 3,4,6,2,1,5,7
  //  3 => store (3) ; 1 expected
  //  4 => store (3,4) ; 1 expected
  //  6 => store (3,4,6) ; 1 expected
  //  2 => store (2,3,4,6) ; 1 expected
  //  1 => process 1. 2 now expected => update now processes 2 (etc). store (6) ; 5 expected
  //  5 => process 5. process 6 in store. store empty ; 7 expected
  //  7 => process
  @tailrec protected def update(inc:Boolean):Unit = {
    if (inc) n+=1
    if (!toDo.isEmpty) {
      val h = toDo.head
      if (h._1<n) { h._3(h._2); toDo-=h; update(true) }
    }
  }
}

object Sequencer {
  def ordering[E<:Def#Elt](cmp:(E,E)=>Int) = new Ordering[(Int,E,E=>Unit)] {
    def compare(x: (Int,E,E=>Unit), y: (Int,E,E=>Unit)) = if (x._1<y._1) -1 else if (x._1>y._1) 1 else if (cmp==null) 0 else cmp(x._2,y._2)
  }
  def mkRank(r:Int) = r match {
    case -1 => Int.MaxValue
    case  x => x
  }
}


