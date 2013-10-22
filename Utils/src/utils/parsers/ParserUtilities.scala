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
}