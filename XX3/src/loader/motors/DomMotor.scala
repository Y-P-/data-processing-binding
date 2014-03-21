package loader.motors

import loader.core.definition.Processor

object DomMotor {

  implicit class XNode(e:Processor#EltBase) extends org.w3c.dom.Node {
    import org.w3c.dom._
    //unimplemented
    def appendChild(x$1: Node): Node = ???
    def cloneNode(x$1: Boolean): Node = ???
    def getBaseURI(): String = null
    def getNodeValue(): String = ???
    def getUserData(x$1: String): Object = ???
    def insertBefore(x$1: Node,x$2: Node): Node = ???
    def removeChild(x$1: Node): Node = ???
    def replaceChild(x$1: Node,x$2: Node): Node = ???
    def setNodeValue(x$1: String): Unit = ???
    def setPrefix(x$1: String): Unit = ??? 
    def setTextContent(x$1: String): Unit = ???
    def setUserData(x$1: String,x$2: Any,x$3: UserDataHandler): Object = ???  
    //standard implementation
    def isDefaultNamespace(x$1: String): Boolean = true
    def isEqualNode(x$1: Node): Boolean = x$1 eq this
    def isSameNode(x$1: Node): Boolean = x$1 eq this
    def getFeature(x$1: String,x$2: String): Object = null
    def getTextContent(): String = ""
    def getLocalName(): String = e.name
    def getNamespaceURI(): String = null
    def getNodeName(): String = e.name
    def getNodeType(): Short = Node.ELEMENT_NODE
    def getOwnerDocument(): Document = null
    def getParentNode(): Node = e.parent
    def getPrefix(): String = null
    def isSupported(x$1: String,x$2: String): Boolean = false
    def normalize(): Unit = ()
    def lookupNamespaceURI(x$1: String): String = null
    def lookupPrefix(x$1: String): String = null
    //implemented
    def compareDocumentPosition(x$1: Node): Short = ???
    def getAttributes(): NamedNodeMap = ??? 
    def getChildNodes(): NodeList = ???
    def getFirstChild(): Node = ???
    def getLastChild(): Node = ???
    def getNextSibling(): Node = ???
    def getPreviousSibling(): Node = ???
    def hasAttributes(): Boolean = ???
    def hasChildNodes(): Boolean = ???    
  }
  
  /*
  trait LXNode extends XNode { this:CtxCore#List=>
    
  }
  trait TXNode extends XNode { this:CtxCore#Terminal=>
    import org.w3c.dom._
    def getChildNodes(): NodeList = empty
    def getFirstChild(): Node = null
    def getLastChild(): Node = null
    def hasChildNodes(): Boolean = false
    def compareDocumentPosition(x$1: Node): Short = ???
    def getAttributes(): NamedNodeMap = ???
    def getNextSibling(): Node = ???
    def getPreviousSibling(): Node = ???
    def hasAttributes(): Boolean = ???
  }
  val empty = new org.w3c.dom.NodeList {
    def getLength = 0
    def item(i:Int) = null
  }*/
  
}