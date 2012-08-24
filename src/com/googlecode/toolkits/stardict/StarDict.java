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

class Location {
    public int offset;
    public int size;
}

public class StarDict {  
    /**/
    RandomAccessFile index;
    RandomAccessFile yaindex;
    DictZipFile dz;
    String dictname;
    public String last_error = "";
  
    /**
     * 
     * @param dictname
     */
    public StarDict(String dictname) {
	try {
	    this.dictname = dictname;
	    this.index = new RandomAccessFile(dictname+".idx", "r");
	    this.dz = new DictZipFile(dictname+".dict.dz");
	    this.yaindex = new RandomAccessFile(dictname+".yaidx", "r");
	    //this.dz.runtest();
	}
	catch(FileNotFoundException e) {
	    last_error = e.toString();
	    e.printStackTrace();
	}
	catch(Exception e) {
	    last_error = e.toString();
	    e.printStackTrace();
	}
    }
  
    public String getWord(int p, Location l) {
	if(l==null) {
	    l = new Location();
	}
	String word = null;
	byte [] buffer = new byte[1024];
	int dataoffset = 0;
	int datasize = 0;
	int offset = 0; // the offset of the p-th word in this.index
	try {
	    this.yaindex.seek(p*4);
	    int size = this.yaindex.read(buffer, 0, 4);
	    if (size!=4) {
		throw new Exception("Read Index Error");
	    }
	    for(int i=0;i<4;i++) {
		offset<<=8;
		offset|=buffer[i]&0xff;
	    }
	    this.index.seek(offset);
	    size = this.index.read(buffer, 0, 1024);
	    for(int i=0;i<size;i++) {
		if (buffer[i]==0) {
		    word = new String(buffer, 0, i, "UTF8");
		    dataoffset = 0;
		    datasize = 0;
		    for (int j=i+1;j<i+5;j++) {
			dataoffset<<=8;
			dataoffset|=buffer[j]&0xff;
		    }
		    for (int j=i+5;j<i+9;j++) {
			datasize<<=8;
			datasize|=buffer[j]&0xff;
		    }
		    break;
		}
	    }
	    //System.out.println(datasize);
	    //buffer = new byte[datasize];
	    //this.dz.seek(dataoffset);
	    //this.dz.read(buffer, datasize);
	    l.offset = dataoffset;
	    l.size = datasize;
	}
	catch(Exception e) {
	    last_error = e.toString();
	    e.printStackTrace();
	}
	return word;
    }
  
    /**
     * 
     * @param word
     * @return the explanation of the word
     */
    public String getExplanation(String word) {
	int i = 0;
	int max = getWordNum();
	String w = "";
	int mid = 0;
	Location l = new Location();
	String exp = null;
	//return this.dz.test()+this.dz.last_error;
	///*
	while( i<=max ) {
	    mid = (i + max)/2;
	    w = getWord(mid, l);
	    if (w.compareTo(word)>0) {
		max = mid-1;
	    }
	    else if(w.compareTo(word)<0) {
		i = mid+1;
	    } 
	    else {
		break;
	    }
	}
	//get explanation
	byte [] buffer = new byte[l.size];
	this.dz.seek(l.offset);
	try {
	    this.dz.read(buffer, l.size);
	}
	catch(Exception e) {
	    last_error = e.toString();
	    buffer = null;
	    exp = e.toString();
	}
    
	try {
	    if (buffer == null) {
		exp = "Error when reading data\n"+exp;
	    }
	    else {
		exp = new String(buffer, "UTF8");
	    }
	}
	catch(Exception e) {
	    last_error = e.toString();
	    e.printStackTrace();
	}
	return w+"\n"+exp;
	//*/
    }
  
    /**
     * 
     * @return
     */
    public String getVersion() {
	try {
	    BufferedReader br = new BufferedReader(new FileReader(this.dictname+".ifo"));
	    String line = br.readLine();
	    while(line != null) {
		String [] version = line.split("=");
		if (version.length == 2 && version[0].equals("version")) {
		    return version[1];
		}
		line = br.readLine();
	    }
	}
	catch(IOException e) {
	    last_error = e.toString();
	    e.printStackTrace();
	}
	return "UNKNOWN VERSION";
    }
  
    public int getWordNum() {
	try {
	    BufferedReader br = new BufferedReader(new FileReader(this.dictname+".ifo"));
	    String line = br.readLine();
	    while(line != null) {
		String [] version = line.split("=");
		if (version.length == 2 && version[0].equals("wordcount")) {
		    return Integer.parseInt(version[1]);
		}
		line = br.readLine();
	    }
	}
	catch(IOException e) {
	    last_error = e.toString();
	    e.printStackTrace();
	}
	return 0;
    }

  
    public static void main(String[] args) {
	StarDict dict = new StarDict("/home/bhj/today/stardict.zip.unzip.24247/AHD4_2.8");
	System.out.println(dict.getVersion());
	Location l = new Location();
	String w = dict.getWord(1000, l);
	System.out.println(w);
	System.out.println(dict.getExplanation(w));
	System.out.println(dict.getExplanation("this"));
    }
}
