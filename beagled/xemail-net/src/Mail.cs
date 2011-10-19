/*
 * Mail.cs
 * Copyright (C) 2006 COLIN Cyrille.
 *
 */

using System;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Collections;
using System.Web.Mail;

namespace System.Net.Imap {

  public class Mail {
		
		public ArrayList    _attachments;
    private string      _body = String.Empty;
    private Encoding    _bodyEncoding;
    private MailFormat    _bodyFormat;
    private Flags     _flags = new Flags();
    private string      _header= String.Empty;
    private string      _inline = String.Empty; // Mime content that could be display inline insteado of attachments
    private MailPriority  _priority;
    private int       _size;
    const   int       _maxsize = 50000; // this is the max size to store body, elsewhere body keep at the server.
    private string      _uid;
    private bool      _headersonly; // set to true if only headers have been fetched.

    // Constructor
    public Mail () {
      _attachments = new ArrayList ();
      _bodyEncoding = Encoding.Default;
    }
    public string Bcc {
      get { return GetHeader("Bcc:");}
      set { SetHeader("Bcc:",value); }
    }
    public string Body {
      get { return _body; }
      set { _body = value; }
    }
    public string Cc {
      get { return GetHeader("Cc:");}
      set { SetHeader("Cc:",value); }
    }
    public string Date {
      get { return GetHeader("Date:");}
      set { SetHeader("Date:",value); }
    }
    public Flags Flags {
        get {return this._flags;}
        set {this._flags = value;}
    }
    public string From {
      get { return GetHeader("From:");}
      set { SetHeader("From:",value); }
    }
    public string Header {
      get { return _header; }
      set { _header = value; }
    }
    public string ReplyTo {
      get { return GetHeader("Reply-To:");}
      set { SetHeader("Reply-To:",value); }
    }
    public string Sender {
      get { return GetHeader("Sender:");}
      set { SetHeader("Sender:",value); }
      }
    public int Size {
            get { return _size; }
            set { _size = value; }
      }
    public string Subject {
      get { return GetHeader("Subject:");}
      set { SetHeader("Subject:",value); }
    }
    public string To {
      get { return GetHeader("To:");}
      set { SetHeader("To:",value); }
    }
    public string Uid {
      get { return _uid; }
      set { _uid = value; }
    }
    private void SetHeader(string header,string val) {
      Match m;
      m = Regex.Match(this._header,"^("+header+".*)\\r\\n",RegexOptions.Multiline);
      if(m.Groups.Count > 1) {
        this._header.Replace(m.Groups[0].ToString(), header+val+"\r\n");
      } else {
        this._header += header+val+"\r\n";
      }
    }
    private string GetHeader(string header) {
            Match m;
            m = Regex.Match(this._header,"^"+header+" (.*)\\r\\n",RegexOptions.Multiline);
            if(m.Groups.Count > 1) {
                return m.Groups[1].ToString();
            } else {
                return String.Empty;
            }
    }
    private string _boundary(string messagepart) {
      Match m;
            m = Regex.Match(messagepart,"boundary=[\"](.*?)[\"]\\r\\n",RegexOptions.Multiline);
            if(m.Groups.Count > 1) {
                return m.Groups[1].ToString();
            } else {
                return String.Empty;
            }
    }
    private string _contentType() {
      Match m;
            m = Regex.Match(this._header,"Content-Type: (.*?);",RegexOptions.Multiline);
            if(m.Groups.Count > 1) {
                return m.Groups[1].ToString();
            } else {
                return String.Empty;
            }
    }
    /*
     *
     *  Hum, what's this ugly think !! To read message we need to parse message to retrieve mime part.
     *  At this point a specialized library/treatement is need. In waiting time, i hack this :
     *  If in the message header there's a boundary field, I parse message in multiple attachments separate header : Content-type:.....
     *  and body : the data.
     *  Like this each parts of a Mime message is an attachment.
     *
     */
    public void Load(byte[] message,bool headersonly) {
          this._headersonly = headersonly;
      if(headersonly) {
        this._header = System.Text.Encoding.ASCII.GetString(message);
      } else {
        MemoryStream stream = new MemoryStream(message);
        string line = ReadLine(stream);
        while(line!="\r\n"){
          this._header+=line;
          line = ReadLine(stream);
        }
        string boundary = _boundary(this._header);
        if(boundary==String.Empty){
          line = ReadLine(stream);
          while(line!=""){
            this._body+=line;
            line = ReadLine(stream);
          }
        } else { //else this is a multipart Mime Message
            ParseMime(stream,boundary);
                }
      }
    }
    private void ParseMime(MemoryStream stream,string boundary) {
      byte[] data = ReadDataLine(stream);
            bool first = true;
            bool header = true;
            string part = "";
            while(!System.Text.Encoding.ASCII.GetString(data).StartsWith("--"+boundary)){data = ReadDataLine(stream);}
            while(!System.Text.Encoding.ASCII.GetString(data).StartsWith("--"+boundary+"--")){
                  data = ReadDataLine(stream);
                    Attachment a = new Attachment();
                    part = "";
                    // read part header
                    while(!System.Text.Encoding.ASCII.GetString(data).StartsWith("--"+boundary) && !(data[0]==13 && data[1]==10)) {
                        part+=System.Text.Encoding.ASCII.GetString(data);
                        data = ReadDataLine(stream);
                    }
                    a.Header=part;
                    // header body
                    data = ReadDataLine(stream);
                    ArrayList body = new ArrayList();
                    while(!System.Text.Encoding.ASCII.GetString(data).StartsWith("--"+boundary)) {
                        foreach (byte b in data) {body.Add(b);}
                      data = ReadDataLine(stream);
                    }
          // check for nested part
          string nestedboundary = _boundary(a.Header);
          if(nestedboundary == String.Empty) {
                    a.Content = (byte[])body.ToArray(typeof(byte));
                    this._attachments.Add(a);
          } else { // nested
            ParseMime(new MemoryStream((byte[])body.ToArray(typeof(byte))),nestedboundary);
          }
      }
    }
    public void SetFlags(string flags) {
      string[] f = flags.Split(' ');
      this._flags.Clear();
      foreach(string s in f){
        this._flags.Add(new Flag(s));
      }
    }
    private string ReadLine(MemoryStream stream){
      if(stream.Position==stream.Length)return String.Empty;
          StringBuilder line = new StringBuilder();
          char[] buf = new char[2];
          while(!(buf[0]==13 && buf[1]==10)) {
              buf[0] = buf[1];
              buf[1] = (char)stream.ReadByte();
              line.Append(buf[1]);
          }
      //stream.Seek(line.Length, SeekOrigin.Current);
          return line.ToString(0,line.Length);
      }
    private byte[] ReadDataLine(MemoryStream stream){
      byte[] buf = new byte[2];
      ArrayList result = new  ArrayList();
      while(!(buf[0]==13 && buf[1]==10)) {
         buf[0] = buf[1];
         buf[1] = (byte)stream.ReadByte();
    	   result.Add(buf[1]);
      }
      return (byte[])result.ToArray(typeof(byte));
		}
	}
}

