package utils.tree2


object X {
  val c1 = StringTree(1)
  val c2 = StringTree(2)
  val c3 = StringTree(3)
  val c11 = StringTree(4,Map("a"->c1,"b"->c2))
  val c12 = StringTree(5,Map("c"->c3))
  val c13 = StringTree(5,"d"->c2,"x"->c2)
  val c111 = StringTree(6,Map("d"->c11,"e"->c12))
    
  def f(x:StringTree[Int]):StringTree[String] = StringTree(x.value.map(_.toString+"x"),x.tree.mapValues(f))
  def g(x:(String,PrefixTree[String,_])):Boolean = x._1!="b"
  def h(x:StringTree[Int]):PrefixTree[Int,String] = PrefixTree(x.value.map(_.toString+"x"),x.tree.map(x=>(x._1(0)-'a',h(x._2))))
  
  def main(a:Array[String]):Unit = {
    println(c111)
    val r = c111.map[StringTree[String]](f)      //deep
    println(r)
    val s = c111.filterAll(g)                    //one level
    println(s)
    val t = c111.map[PrefixTree[Int,String]](h)  //full mapping to other tree
    println(t)
    val q = c111.map(_.toString+"x",_(0) - 'a')
    println(q)
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