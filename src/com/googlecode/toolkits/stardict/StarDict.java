/*
  Copyright 2009 http://code.google.com/p/toolkits/. All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:
  * Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.
  * Redistributions in binary form must reproduce the above
  copyright notice, this list of conditions and the following
  disclaimer in the documentation and/or other materials provided
  with the distribution.
  * Neither the name of http://code.google.com/p/toolkits/ nor the names of its
  contributors may be used to endorse or promote products derived
  from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package com.googlecode.toolkits.stardict;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

public class StarDict {

    static class IndexEntry {
	public String mName;
	public ArrayList<Pair<Integer, Integer>> start_ends;

	public IndexEntry(String name) {
	    mName = name;
	}

	public void add(int start, int end) {
	    start_ends.add(new Pair(start, end));
	}
    }

    static int packI(byte[] buffer, int offset) {
	return 
	    ((0XFF & buffer[offset]) << 24) +
	    ((0XFF & buffer[offset+1]) << 16) +
	    ((0XFF & buffer[offset+2]) << 8) +
	    ((0XFF & buffer[offset+3]) << 0);
    }	    
	
    /**/
    RandomAccessFile index;
    RandomAccessFile dict;

    HashMap<String, ArrayList<Pair<Integer, Integer>>> entries_map = new HashMap<String, ArrayList<Pair<Integer, Integer>>>();    
  
    /**
     * 
     * @param dictname
     */
    void die(String s) {
	System.out.println(s);
	System.exit(-1);
    }

    public StarDict(String dictname) {
	try {
	    int length = (int) new File(dictname+".idx").length();

	    this.index = new RandomAccessFile(dictname+".idx", "r");
	    this.dict = new RandomAccessFile(dictname+".dz", "r");

	    byte[] buffer = new byte[length];
	    index.readFully(buffer, 0, length);

	    int start = 0;
	    for (int l = 0; l < length; l++) {
		if (buffer[l] == 0) {
		    String s = new String(buffer, start, l - start, "UTF8");
		    int n = buffer[l + 1];
		    ArrayList<Pair<Integer, Integer>> start_ends = new ArrayList<Pair<Integer, Integer>>(n);
		    for (int e = 0; e < n; e++) {
			int b1 = l + 2 + e * 8;
			int b2 = l + 2 + e * 8 + 4;

			int e_start = packI(buffer, b1);
			int e_end = packI(buffer, b2);

			start_ends.add(new Pair(e_start, e_end));
		    }
		    l++;
		    l += 8 * n;
		    start = l + 1;
		    entries_map.put(s, start_ends);
		}
	    }		
	}
	catch(FileNotFoundException e) {
	    e.printStackTrace();
	}
	catch(Exception e) {
	    e.printStackTrace();
	}
    }
  
    /**
     * 
     * @param word
     * @return the explanation of the word
     */
    public ArrayList<String> getExplanation(String word) {
	
	ArrayList<Pair<Integer, Integer>> start_ends = entries_map.get(word);

	if (start_ends != null) {
	    ArrayList<String> ret = new ArrayList<String>();
	    for (Pair<Integer, Integer> p : start_ends) {
		int start = (int) p.first;
		int end = (int) p.second;

		byte[] buffer = new byte[end - start];

		try {
		    dict.seek(start);
		    dict.readFully(buffer, 0, buffer.length);
		    ret.add(new String(buffer, "UTF8"));
		    
		} catch (Exception e) {
		    System.out.printf("start for %s is %d, stop is %d\n", word, start, end);
		    e.printStackTrace();
		}
	    }
	    return ret;
	}
	return null;
    }
  
    public static void main(String[] args) {
	StarDict dict = new StarDict("/sdcard/ahd/ahd");
	dict.getExplanation("hello");
    }
}

class Pair<F, S> {
    public final F first;
    public final S second;

    /**
     * Constructor for a Pair. If either are null then equals() and hashCode() will throw
     * a NullPointerException.
     * @param first the first object in the Pair
     * @param second the second object in the pair
     */
    public Pair(F first, S second) {
        this.first = first;
        this.second = second;
    }

    /**
     * Checks the two objects for equality by delegating to their respective equals() methods.
     * @param o the Pair to which this one is to be checked for equality
     * @return true if the underlying objects of the Pair are both considered equals()
     */
    public boolean equals(Object o) {
        if (o == this) return true;
        if (!(o instanceof Pair)) return false;
        final Pair<F, S> other;
        try {
            other = (Pair<F, S>) o;
        } catch (ClassCastException e) {
            return false;
        }
        return first.equals(other.first) && second.equals(other.second);
    }

    /**
     * Compute a hash code using the hash codes of the underlying objects
     * @return a hashcode of the Pair
     */
    public int hashCode() {
        int result = 17;
        result = 31 * result + first.hashCode();
        result = 31 * result + second.hashCode();
        return result;
    }

    /**
     * Convenience method for creating an appropriately typed pair.
     * @param a the first object in the Pair
     * @param b the second object in the pair
     * @return a Pair that is templatized with the types of a and b
     */
    public static <A, B> Pair <A, B> create(A a, B b) {
        return new Pair<A, B>(a, b);
    }
}
