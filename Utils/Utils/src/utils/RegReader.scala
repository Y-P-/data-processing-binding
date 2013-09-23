package utils

object Hive {
  import scala.language.implicitConversions
  val HKCR = new Hive(0x80000000)
  val HKCU = new Hive(0x80000001)
  val HKLM = new Hive(0x80000002)
  val HKU  = new Hive(0x80000003)
  val HKCC = new Hive(0x80000005)
  
  implicit protected final def toCstr(s:String):Array[Byte] = {
    val r = new Array[Byte](s.length + 1)
    val r1 = s.getBytes
    System.arraycopy(r1,0,r,0,r1.length)
    r(s.length) = 0
    r
  }
  implicit protected final def toStr(b:Array[Byte]) = if (b==null) null else new String(b,0,b.length-1)
    
  protected final class M[X](m:java.lang.reflect.Method) {
    def apply(args:AnyRef*)= m.invoke(null,args:_*).asInstanceOf[X]
  }
  protected object M {
   private val clz = java.util.prefs.Preferences.userRoot.getClass
   def apply[X](name:String,args:Class[_]*):M[X] = {
      val m = clz.getDeclaredMethod(name,args:_*)
      m.setAccessible(true)
      new M[X](m)
    }
  }
  
  private val cI = classOf[Int]
  private val cB = classOf[Array[Byte]]
  
  protected val regOpenKey      = M[Array[Int]]("WindowsRegOpenKey",cI,cB,cI)
  protected val regCloseKey     = M[Int]("WindowsRegCloseKey",cI)
  protected val regQueryValueEx = M[Array[Byte]]("WindowsRegQueryValueEx",cI,cB)
  protected val regEnumValue    = M[Array[Byte]]("WindowsRegEnumValue",cI,cI,cI)
  protected val regQueryInfoKey = M[Array[Int]]("WindowsRegQueryInfoKey",cI)
  protected val regEnumKeyEx    = M[Array[Byte]]("WindowsRegEnumKeyEx",cI,cI,cI)  
  protected val regCreateKeyEx  = M[Array[Int]]("WindowsRegCreateKeyEx",cI,cB)
  protected val regSetValueEx   = M[Int]("WindowsRegSetValueEx",cI,cB,cB)
  protected val regDeleteValue  = M[Int]("WindowsRegDeleteValue",cI,cB)
  protected val regDeleteKey    = M[Int]("WindowsRegDeleteKey",cI,cB)
  protected val regFlushKey     = M[Int]("WindowsRegFlushKey",cI)

  
  def main(args:Array[String]):Unit = {
	val key = HKLM("SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\AppCompatFlags")
	println(key.readString("ApphelpUIExe"))
	println(key.readAllValues)
	println(key.getSubKeys)
	println(key.parent.getSubKeys)
  }

}

final class Hive protected (val hkey:Int) {
  import Hive._
  protected val hive:Integer = hkey
    
  def isRoot(h:Int) = hkey==HKCR.hkey || hkey==HKLM.hkey || hkey==HKCU.hkey || hkey==HKCC.hkey || hkey==HKU.hkey
   
  final class Key(key:String) {
    def this(parent:String,name:String) = this(parent+"\\"+name)
    import Key._
    protected val ck:Array[Byte] = key
    //analyzes the key for (parent,name)
    lazy val analyze       = { val m=p.matcher(key); if (!m.matches) ("",key) else (m.group(1),m.group(5)) }
    def parent             = if (analyze._1.length==0) null else new Key(analyze._1)
    def name               = analyze._2
    def child(name:String) = new Key(key,name)
    def hive               = Hive.this
    //wrapper to closes the handle after use
    protected def withKey[X](mode:Integer,name:Array[Byte])(f:(Integer,Array[Byte])=>X) =
      regOpenKey(hive.hive,ck,mode) match {
        case Array(r,0)  => val h = new Integer(r)
                            try     { f(h,name) }
                            finally { if (!isRoot(r)) regCloseKey(h) }
        case _           => throw new IllegalStateException
      }
    def readString(name:String):String = withKey(KEY_READ,name) { regQueryValueEx(_,_) }
    def foreachValue[X](f:(String,String)=>X) = withKey(KEY_READ,null) { (h,name)=>
      val x = regQueryInfoKey(h)
      val Array(_,r,nb,_,lg) = regQueryInfoKey(h)
      if (r!=0) throw new IllegalStateException
      val l = new Integer(lg+1)
      for (i<- 0 until nb) {
        val name = regEnumValue(h,new Integer(i),l)
        f(name,regQueryValueEx(h,name))
      }
    }
    def foreachSubKey[X](f:(Key)=>X) = withKey(KEY_READ,null) { (h,name)=>
      val Array(nb,r,_,lg,_) = regQueryInfoKey(h)
      if (r!=0) throw new IllegalStateException
      val l = new Integer(lg+1)
      for (i<- 0 until nb) {
        f(child(regEnumKeyEx(h,new Integer(i),l)))
      }
    }
    def readAllValues = {
      val r=new scala.collection.mutable.HashMap[String,String]
      foreachValue { r.put(_,_) }
      r.toMap
    }
    def getSubKeys = {
      val r=new scala.collection.mutable.ListBuffer[Key]
      foreachSubKey { r += _ }
      r.toList
    }
    def create() = {
      val Array(h,r) = regCreateKeyEx(hive, ck)
      if (!isRoot(r)) regCloseKey(new Integer(h))
      if (r!=0) throw new IllegalStateException
    }
    def delete() = {
      val r = regDeleteKey(hive, ck)
      if (r!=0) throw new IllegalStateException
    }
    def flush() = {
      val r = regFlushKey(hive, ck)
      if (r!=0) throw new IllegalStateException
    }
    def writeString(name:String,value:String):Unit =
      withKey(KEY_ALL_ACCESS,name) { regSetValueEx(_,_,toCstr(value)) }
    def deleteValue(name:String):Unit =
      withKey(KEY_ALL_ACCESS,name) { regDeleteValue(_,_) }
    override def toString = key
  }
  object Key {
    private final val KEY_ALL_ACCESS = new Integer(0xf003f)
    private final val KEY_READ       = new Integer(0x20019)
    private val p = java.util.regex.Pattern.compile("(((([^\\\\]+)\\\\)*)[^\\\\]+)\\\\([^\\\\]*)")
  }
  def apply(key:String) = new Key(key)
}
