package loader.core.callbacks

import loader.core.definition._
import loader.core.ParserBuilder

/** Defines a Callback, which is some code which executes in place of the original code in a Motor#Element.
 *  Methods are redirected to the function values with the same name. The general contract for these functions is:
 *  - name identical to method it 'overloads'
 *  - last parameter = function that is being overloaded ; unless you explicitely want to disable that call, you
 *                       should call it somewhere in your implementation!
 *  - other parameters  = parameters for the original method, in the same order
 *  Some specific case may be needed (see onName for example)
 *
 *  It is possible to stack callbacks by means of the apply call; the stack executes in the 'logical' order in which
 *  it was created. For example:
 *      c1.onBeg = { a1; f; b1; }
 *      c2.onBeg = { a2; f; b2; }
 *      then c1(c2).onBeg is equivalent to { a1; a2; f; b1; b2; }
 *
 *  It is only necessary to redefine the methods that one needs. null functions will be ignored (they will even be
 *  discarded when the apply call is done to mix callbacks.)
 *  If a given method is not 'overloaded' in any callback in the stack, then the original method is invoked with
 *  very little overload (one check against null.)
 *
 *  The combine method along with a bunch of predefined types is very helpful to easily define the result of a
 *  combination between Callbacks.
 *
 *  This trait defines a basic pattern common to all motors. Some motors may define additional methods for their
 *  Elements, which may then require a specific Callback class to tap into. The common methods redefined
 *  here are:
 *  - onName
 *  - onBeg
 *  - onEnd
 *  - onVal
 *  - onChild
 *  
 *  Note the global contra-variance on all parameters.
 *  
 *  TODO: There are ways to improve the overall performance on Callbacks, but we do not
 *        implement them yet:
 *        1) breakdown WithCallback into five traits, each managing a call.
 *           use these specific traits as appropriate in the client classes (e.g. StructCbk etc in CtxCore)
 *        2) keep track in a mask inside Callback of which calls are actually overloaded
 *           use that mask in WithCallback to prevent going inside a callback that does nothing (default implementation)
 *        Usure whether any of this has any significant impact.
 */
abstract class Callback[-E0,-S0,-R0,K,V>:Null] { self=>
  protected class Inner(protected[this] val elt:E0) {
    def onName[S<:S0](key:K, f: (K)=>S):S                      = f(key)
    def onInit(f: =>Unit):Unit                                 = f
    def onBeg(f: =>Unit):Unit                                  = f
    def onVal[R<:R0](s:V,f: (V) =>R):R                         = f(s)
    def onSolver[R<:R0](s:V, r: ()=>R, f: (V,()=>R) =>R):R     = f(s,r)
    def onEnd[R<:R0](f: =>R):R                                 = f
    def onChild[E<:E0,R<:R0](child:E,r:R,f: (E,R) =>Unit):Unit = f(child,r)  
  }
  def apply(elt:E0):Callback[E0,S0,R0,K,V]#Inner = new Inner(elt)
  
  def apply[E1<:E0,S1<:S0,R1<:R0](c: Callback[E1,S1,R1,K,V]): Callback[E1,S1,R1,K,V] = new Callback[E1,S1,R1,K,V] {
    override def apply(elt:E1) = {
      val cbIn = self(elt) //executed inside cbOut
      val cbOut = c(elt)   //executed as outer context
      if (cbIn == null)     cbOut
      else if (cbOut==null) cbIn
      else                  new Inner(elt) {
        override def onName[S<:S1](key:K, f: (K)=>S):S                      = cbOut.onName(key,cbIn.onName(_,f))
        override def onBeg(f: =>Unit):Unit                                  = cbOut.onBeg(cbIn.onBeg(f))
        override def onVal[R<:R1](s:V,f: (V)=>R):R                          = cbOut.onVal(s,cbIn.onVal(_,f))
        override def onSolver[R<:R1](s:V, r: ()=>R, f: (V,()=>R)=>R):R      = cbOut.onSolver(s,r,cbIn.onSolver(_:V,_:()=>R,f))
        override def onEnd[R<:R1](f: =>R):R                                 = cbOut.onEnd(cbIn.onEnd(f))
        override def onChild[E<:E1,R<:R1](child:E,r:R,f: (E,R)=>Unit):Unit  = cbOut.onChild(child,r,cbIn.onChild(_:E,_:R,f))
      }
    }
  }
  //will easily turn a single callback to a recursive one, where the implicit fails
  def * = (new CallbacksBuilder[E0,S0,R0,K,V])(this)
}
object Callback {
  implicit def toCbks[E0,S0,R0,K,V>:Null](cbk:Callback[E0,S0,R0,K,V]):Callbacks[E0,S0,R0,K,V] = cbk*
}
