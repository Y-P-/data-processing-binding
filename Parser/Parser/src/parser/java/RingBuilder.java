package parser.java;

public abstract class RingBuilder<T extends Ring<T>> {  
  
  abstract protected T newToken();
  
  @SuppressWarnings("unchecked")
  public T build(int stkSz) {
	Object[] a = new Object[stkSz];
    T prev=null;
    T init=null;
	for (int i=0; i<stkSz; i++) {
      if (prev==null) {
        prev=newToken();
        init=prev;
      }
      else {
  	    T t = newToken();
  	    prev.setNext(t);
  	    t.setPrev(prev);
  	    prev=t;
      }
      a[i] = prev;
	}
	init.setPrev(prev);
	prev.setNext(init);
	for (Object tk : a) ((T)tk).bind();
	return init;
  }
}
