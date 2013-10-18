package utils.parsers

import scala.annotation.tailrec
import scala.annotation.switch
import utils.CharReader


final object ParserUtilities {
  
  protected[parsers] class Jump(val n:Int) extends Exception {
    override def fillInStackTrace = this    
  }
  protected[parsers] class Internal(val msg:String) extends Exception {
    override def fillInStackTrace = this    
  }
  protected[parsers] object exit extends Exception {
    override def fillInStackTrace = this    
  }
    
  protected[parsers] val noSpy = new Spy {
    def line:Int=0
    def depth:Int=0
    def line_=(l:Int) = {}
    def depth_=(l:Int) = {}
  }
}