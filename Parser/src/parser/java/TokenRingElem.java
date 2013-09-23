package parser.java;

public abstract class TokenRingElem<T extends TokenRingElem<T> & Ring<T>> extends TokenReader.TokenBuf implements Ring<T> {
  public T  next;   //Token following this token
  public T  prev;   //Token preceding this token
  public T  toFill; //Token to fill when this token is consummed ; fill.read = this
  public T  toRead; //Token to use when this token is filled up  ; read.fill = this

  public TokenRingElem(TokenReader tr) {
	tr.super();
  }
  
  final public void setPrev(T prev) { this.prev = prev; }
  final public void setNext(T next) { this.next = next; }
  abstract public int getStkFw();
  
  /** Called for late init of remaining fields */
  @SuppressWarnings("unchecked")
  public void bind() {
	T x = (T)this;
	for (int i=0; i<getStkFw(); i++) x=x.next;
	toFill = x;
	toFill.toRead = (T)this;
  }
  
}
