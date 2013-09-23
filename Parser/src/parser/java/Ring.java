package parser.java;

interface Ring<T extends Ring<T>> {
  public void setNext(T t);  //Token following this token
  public void setPrev(T t);  //Token preceding this token
  public void bind();        //Late initialization for tokens, after the ring is closed!
}