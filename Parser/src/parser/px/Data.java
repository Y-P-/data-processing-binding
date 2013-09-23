package parser.px;

/**
 * Java class ensuring direct array access.
 * @author yprelot
 */
public class Data {
	  // byte class table
	  // 0 : space
	  // 3 : \n
	  // 1 : character
	  // 2 : #
	  // 4 : =
	  // 5 : "
	  // 6 : {
	  // 7 : }
	static int[] bClass = new int[]{
		      0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,   //space, \n, space
		      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   //space
		      0,1,5,2,1,1,1,1,1,1,1,1,1,1,1,1,   //space, char, ", #, char
		      1,1,1,1,1,1,1,1,1,1,1,1,1,4,1,1,   //char, =, char
		      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,   //char
		      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,   //char
		      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,   //char
		      1,1,1,1,1,1,1,1,1,1,1,6,1,7,1,1    //char, {, char, }, char
	};
	static int[] bString = new int[]{
		      0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,   //\n
		      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   //
		      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,   //"
		      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   //
		      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   //
		      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   //
		      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   //
		      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0    //
	};
	
	static public boolean isChar(String s) {
		for (char c : s.toCharArray()) if (bClass[c]!=1) return false;
	    return true;
	}
}
