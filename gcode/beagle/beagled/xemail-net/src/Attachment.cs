/*
 * Attachment.cs
 * Copyright (C) 2006 COLIN Cyrille.
 *
 */

using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections;

namespace System.Net.Imap {
  public class Attachment {
      private string      _header = String.Empty;
    private byte[]      _content = new byte[0];
    private bool      _onserver;
    public Attachment() {}
    public string FileName {
         get { return GetHeader("filename=");}
      }
    private string Charset {
         get { return GetHeader("charset=");}
        }
    private string ContentDisposition {
         get { return GetHeader("Content-Disposition: ");}
        }
    public string ContentEncoding {
         get { return GetHeader("Content-Transfer-Encoding: ");}
        }
    public string ContentType {
         get { return GetHeader("Content-Type: ");}
      }
    public bool IsAttachment {
      get {
        return (ContentDisposition.ToLower() == "attachment" || ContentDisposition.ToLower() == "inline")?true:false;
      }
    }
    public void Save (string path) {
      if (File.Exists(path+this.FileName)) {
          }
          FileStream fs = new FileStream(path+this.FileName, FileMode.Create);
          BinaryWriter w = new BinaryWriter(fs);
      if(ContentEncoding == "base64") {
              w.Write(Convert.FromBase64String(System.Text.Encoding.ASCII.GetString(_content)));
      } else {
        w.Write(_content);
      }
          w.Close();
          fs.Close();
    }
    public byte[] Content {
         set { this._content = value;}
         get { return this._content;}
      }
    public string Header {
         set { this._header = value;}
         get { return this._header;}
      }
    public bool OnServer {
         set { this._onserver = value;}
         get { return this._onserver;}
    }
    private string GetHeader(string header) {
            Match m;
            m = Regex.Match(this._header,header+"[\"]?(.*?)[\"]?(\\r\\n|;)",RegexOptions.Multiline);
            if(m.Groups.Count > 1) {
                return m.Groups[1].ToString();
            } else {
                return String.Empty;
            }
        }

  }
}
