package utils.tests.events
import utils.events._

object MouseClicks extends Event
object MenuSelect extends Event
object Quit extends Event


class Button(val label:String) {
	object clicks extends EventSource(this) {
	  val m = attach[Int](MouseClicks)
	  val q = attach[Null](Quit)
	  def left()   = m.emit(0)
	  def right()  = m.emit(1)
	  def middle() = q.emit(null)
	}
}

class Menu(val label:String, val subMenu:Array[Menu]) {
	def this(label:String) = this(label,null)
	object select extends EventSource(this) {
	   val v = attach[Menu](MenuSelect)
		 def select(idx:Int) = v.emit(subMenu(idx))
	}
}

object Test extends Observer {
	  var u = 0
	  val mouseButton = new Button("mouse")
	  val menu        = new Menu("menu", Array(new Menu("fichier"), new Menu("quitter")))
	  var o:Watcher[_,_,Unit] = null
	  
	  {
		  obs(mouseButton.clicks.m)    { (e,a) => println("received "+a) }
		  obs(Quit)                    { (e,a) => println("Quit detected") }
		  obs(mouseButton.clicks.self) { (e,a) => println("mouse detected") }
		}
		  //{ case (src,ev,a) =>
		  //  if (a.label.equals("quitter")) src.emit(Quit,null)
		  /*
		  observe(mouseButton.clicks, new Func<SourceEvent<Button,Object>>() {
			  public void apply(SourceEvent<Button,Object> ev) {
				  System.out.println("spying Click on mouse: "+ev.event+"/"+ev.value);
			  }
		  });
		  observe(Quit.r, new Func<Object>() {
			  public void apply(Object e) { System.out.println("finished!!!"); u=3; }
		  });
		  observe(MouseClicks.r, new Func<Integer>() {
			  public void apply(Integer e) { switch(e) {
			  	case 1 : System.out.println("yoho!");  break;
			  	case 0 : System.out.println("flute!"); break;  
			  }};
		  });
		  o = observe(mouseButton.clicks, MouseClicks.r, new Func2<Button.Clicks,Button,Integer>() {
			  public void apply(Button.Clicks c, Integer e) { switch(e) {
			  	case 1 : System.out.println("right from "+c.source.label); break;
			  	case 0 : System.out.println("left  from "+c.source.label); break;
			  }};
		  });
		  observe(mouseButton.clicks, MouseClicks.r, new Func<Integer>() {
			  public void apply(Integer e) { switch(e) {
			  	case 1 : System.out.println("yeah!"); break;
			  	case 0 : mouseButton.clicks.emit(Quit.r,null); break;
			  }};
		  });
		  */
	  
	  def main(args:Array[String]):Unit = {
	    mouseButton.clicks.right()
	    println("-----------------")
	    mouseButton.clicks.left()
	    println("-----------------")
	    mouseButton.clicks.middle()
	    println("-----------------")
	    menu.select.select(1)
	    println(u)
	    println("-----------------")
	    //mouseButton.clicks.send(MouseClicks, 1, this)
	    println("-----------------")
//	    mouseButton.clicks.send(MouseClicks, 0, o)
	  }
}