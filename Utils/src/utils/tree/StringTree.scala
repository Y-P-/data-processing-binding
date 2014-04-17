package utils.tree

class StringTree(value:Option[String]) extends MapTree[String,String](value) {
  def this(value:Option[String],tree:StringTree) = { this(value); self = tree }
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
    protected def params(prev: =>StringTree,name:String,cur:Option[String],s:CharSequence,o:Char,c:Char,e:Char,sep:Char,from:Int,until:Int):StringTree = {
      var i = from
      val res:StringTree = new StringTree(cur)
      do {
        val recur  = param(s,o,c,e,sep,i,until)
        val cur0   = if (recur._1) None else Some(s.subSequence(recur._3, recur._4).toString)
        val next   = if (recur._1) params(res,recur._2,cur0,s,o,c,e,sep,recur._3,recur._4) else new StringTree(cur0)
        res.put(recur._2,next)
        i = find(s,sep,recur._4,until)+1
      } while (i<until && i>0);
      res
    }
    /** reads a hierarchical string tree in developed form */
    def apply(s:CharSequence) = params(null,null,None,s,'(',')','=',';',0,s.length)
}