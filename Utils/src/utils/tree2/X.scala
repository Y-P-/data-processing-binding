package utils.tree2


object X {
  val c1 = StringTree(1)
  val c2 = StringTree(2)
  val c3 = StringTree(3)
  val c11 = StringTree(4,Map("a"->c1,"b"->c2))
  val c12 = StringTree(5,Map("c"->c3))
  val c13 = StringTree(5,"d"->c2,"x"->c2)
  val c111 = StringTree(6,Map("d"->c11,"e"->c12))
    
  def main(a:Array[String]):Unit = {
    println(c111)
    val r = c111.map[String,PrefixTree[String,String]]((_:Int).toString+"y")
    println(r)
    val t = c111.map[Int,String,PrefixTree[Int,String]](_(0)-'a',_.toString+"y")  //full mapping to other tree
    println(t)
    val s = c111.filterAll(_._1!="b")                    //one level
    println(s)
    val u = c111.map[String,StringTree[String]]((_:Int).toString+"x")
    println(u)
    for (z<-u.iterator(true,false)) println(z)
    println(c111.get("d"))
    println(c111.seqView.get("d","a"))
    println(c111.seqView.get("d","a","c"))
    println(c111("d"))
    println(c111.seqView(Seq("d","a")))
    try { println(c111.seqView(Seq("d","a","c"))) } catch { case e:java.util.NoSuchElementException => println("not found")}
    println(StringTree.builder[Int](Seq(
      (Seq("x","y"),1),
      (Seq("x","y","z"),2),
      (Seq("x"),3),
      (Seq("x","v"),4),
      (Seq("z"),5),
      (Seq("z","a","b"),6),
      (Seq("z"),7)
    )))
  }
}