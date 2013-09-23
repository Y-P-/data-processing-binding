package parser.java;

public abstract class TokenReader {
  final public static int End   = -1;
  final public static int Error = -2;
  
  final public byte[] data;
  final public String encoding;

  public int  position = -1;
  public int  line     =  0;
  
  public TokenReader(parser.Source src) {
    this.data = utils.Reader.apply(src.uri());
    this.encoding = src.encoding();
  }
	  
  public TokenReader(byte[] data, String encoding) {
	this.data = data;
	this.encoding = encoding;
  }
  
  public void reset() {
	position = -1;
	line     =  0;
  }
  
  /**
   * Fills last with the next token.
   * Accounts for position, newline and struct level, removes comments and spaces
   * End of flow results in the End token.
   */
  protected abstract String tokenName(int kind);
  
  /** All fields should be seen as final for practical use. */
  public abstract class TokenBuf {
    public int pos    = 0;
    public int kind   = TokenReader.End;
    public int length = 0;
    public final int end()    { return pos+length-1; } //last  character position in data flow
    public final String infoString() { return baseString() + " ["+pos+","+end()+"]"; }
    public final String longString() { return infoString() + " " + tokenName(kind); }
    public String baseString() { //the String as read
      try {
    	if (kind==TokenReader.End) return "<<EOF>>";
        return new String(data,pos,length,encoding);
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
    }
    public String unquotedString() { //the String unquotted
      String s = baseString();
      return (s.charAt(0)=='"' && s.charAt(s.length()-1)=='"') ? s.substring(1,s.length()-1) : s;
    }
    public String toString() { return baseString(); }
    
    /** Fills this TokenBuf. */
    abstract public void fill();
  }

}