/*
 * ImapClient.cs.
 * Copyright (C) 2006 COLIN Cyrille.
 *
 */


using System;
using System.Collections;
using System.Net;
using System.Net.Sockets;
using System.IO;
using System.Text;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
#if MONO
using Mono.Security.Protocol.Tls;
#endif
using System.Text.RegularExpressions;


namespace System.Net.Imap {

	public class ImapClient {
		// Imap State
		private bool 			_connected = false;
		private bool            _authenticated = false;
		private bool			_selected = false;

		private string 			_authmethod = String.Empty;
		private string 			_selectedmailbox = String.Empty;
		private int	 			_port = 143;
		private string 			_connectedhostname = String.Empty;
		private bool 			_ssl = false;
		private int 			_tag = 0;
		private string 			_lasterror= String.Empty;
		private string			_capability = String.Empty;		
		
		private TcpClient  		_imapconnection;
		//private NetworkStream 	_imapnetworkstream;
		private Stream 	_imapnetworkstream;
		private StreamReader  	_imapstreamreader;
		private int 			_maxlen; 			// Max lenght of received data
		private int				_idletimeout = 6000; 		// Idle time out
		
/*
 *
 *		Properties
 *
 */

	public string AuthMethod {
		get {
			return this._authmethod;
		}
		set {
			this._authmethod = value;			
		}
	}

	public string LastError {
		get {
			return this._lasterror;
		}				
	}	

	public string ConnectedHostName {
		get {
			return this._connectedhostname;
		}					
   	}

	public int Port {
		get {
			return this._port;
		}
		set {
			this._port = value;
		}
	}

	public bool Ssl {
		get {
			return this._ssl;
		}
		set {
			this._ssl = value;
		}
	}

	private string Tag {
		get {
			this._tag++;
			if(this._tag<10)return "xm00"+this._tag.ToString()+" ";
			if(this._tag<100)return "xm0"+this._tag.ToString()+" ";
			if(this._tag<1000)return "xm"+this._tag.ToString()+" ";
			return "xm"+ this._tag;
		}

	}


	
/*
 *
 *		Methods
 *
 *
 */

	public bool AppendMail(string mailbox, Mail email) {
		try {
            if(!_connected) throw new Exception("You must connect first !");
			if(!_authenticated)throw new Exception("You must authenticate first !");
			string flags = String.Empty;
			string size = (email.Body.Length-1).ToString();
			if(email.Flags.Count>0){
				flags = "(";
				foreach(Flag f in email.Flags){ flags += f.Name + " "; }
				flags = flags.Substring(0,flags.Length-1);
				flags += ")";
			}
            string command = this.Tag + "APPEND "+mailbox+" "+flags+" {"+size+"}\r\n";
            SendCommand(command);
            string response = ReadLine();
			if(response.StartsWith("+")){	
				SendCommand(email.Body);	
    	        response = ReadLine();
			}
            return true;
        } catch (Exception e) {
            this._lasterror = "AppendMail : " + e.Message;
            return false;
        }	
	}

	public bool AppendMime(string mailbox, string mimeText) {
		throw new Exception("Not yet implemented !");
		return false;
	}

	public string Capability() {
		try {
	        if(!_connected) throw new Exception("You must connect first !");
    	    string command = this.Tag + "CAPABILITY\r\n";
        	SendCommand(command);
	        string response = ReadLine();
			if(response.StartsWith("* CAPABILITY "))response = response.Substring(13);
			this._capability = response;
        	ReadLine();
	        return response;
		} catch (Exception e) {
			this._lasterror = "Capability : " + e.Message;
			return null;
		}
    }

	public bool Connect(string hostname) {
    	try {
        	_imapconnection = new TcpClient(hostname, this.Port);
			_imapnetworkstream = _imapconnection.GetStream();
#if MONO
			if(this.Ssl) {
				 _imapnetworkstream = new SslClientStream(_imapconnection.GetStream(),hostname,true,Mono.Security.Protocol.Tls.SecurityProtocolType.Default);
				((SslClientStream)_imapnetworkstream).ClientCertSelectionDelegate = new CertificateSelectionCallback(DefaultCertificateSelectionCallback);
                ((SslClientStream)_imapnetworkstream).ServerCertValidationDelegate = new CertificateValidationCallback(DefaultCertificateValidationCallback);
                ((SslClientStream)_imapnetworkstream).PrivateKeyCertSelectionDelegate = new PrivateKeySelectionCallback(DefaultPrivateKeySelectionCallback); 
			}
#endif
			
      _imapstreamreader = new StreamReader(_imapnetworkstream,System.Text.Encoding.ASCII);
      string info = ReadLine();
			if(info.StartsWith("* OK") != true){
					throw new Exception(info);
			}
			this._connected = true;
			this._connectedhostname = hostname;
			return true;
		} catch(Exception e) {
			this._connected = false;
			this._connectedhostname = String.Empty;
      this._lasterror = "Connect : " + e.Message;
			return false;
		}
	}

	public bool Copy(string messageset, string destination) {
		try {
            if(!_connected) throw new Exception("You must connect first !");
            if(!_authenticated)throw new Exception("You must authenticate first !");
			if(!_selected)      throw new Exception("You must select first !");
            string command = this.Tag + "COPY "+messageset+" \""+destination+"\"\r\n";
            SendCommand(command);
            string response = ReadLine();
            response = response.Substring(response.IndexOf(" ")).Trim();
            if(!response.ToUpper().StartsWith("OK")){
                throw new Exception(response);
            }
            return true;
        } catch (Exception e) {
            this._lasterror = "Copy : " + e.Message;
            return false;
        }
	}

	public bool CreateMailbox(string mailbox) {
		try {
		    if(!_connected) throw new Exception("You must connect first !");	
		    if(!_authenticated)throw new Exception("You must authenticate first !");
			string command = this.Tag + "CREATE \""+mailbox+"\"\r\n";
			SendCommand(command);
			string response = ReadLine();
			response = response.Substring(response.IndexOf(" ")).Trim();
			if(!response.ToUpper().StartsWith("OK")){
				throw new Exception(response);
			}
	        return true;
		} catch (Exception e) {
			this._lasterror = "CreateMailbox : " + e.Message;
            return false;
		}
	}

	public bool DeleteMailbox(string mailbox) {
		try {
	        if(!_connected) throw new Exception("You must connect first !");
    	    if(!_authenticated)throw new Exception("You must authenticate first !");
	        string command = this.Tag + "DELETE \""+mailbox+"\"\r\n";
    	    SendCommand(command);
        	string response = ReadLine();
	        response = response.Substring(response.IndexOf(" ")).Trim();
    	    if(!response.ToUpper().StartsWith("OK")){
				throw new Exception(response);
        	}
	        return true;
		} catch (Exception e) {
            this._lasterror = "DeleteMailbox : " + e.Message;
            return false;
        }
	}

	public void Disconnect() {
		Logout();
	}

  public Mailbox Examine(string mailbox) {
		try {
	        if(!_connected) throw new Exception("You must connect first !");
   		    if(!_authenticated)throw new Exception("You must authenticate first !");
	        Mailbox x = null;
    	    string tag = this.Tag;
        	string command = tag + "EXAMINE \""+mailbox+"\"\r\n";
	        SendCommand(command);
    	    string response = ReadLine();
        	if(response.StartsWith("*")) {
            	x = new Mailbox(mailbox);
	            while(response.StartsWith("*")){
    	            Match m;
        	        m = Regex.Match(response,@"(\d+) EXISTS");
            	    if(m.Groups.Count>1){x.NumMsg = Convert.ToInt32(m.Groups[1].ToString());}
                	m = Regex.Match(response,@"(\d+) RECENT");
	                if(m.Groups.Count>1)x.NumNewMsg = Convert.ToInt32(m.Groups[1].ToString());
    	            m = Regex.Match(response,@"UNSEEN (\d+)");
        	        if(m.Groups.Count>1)x.NumUnSeen = Convert.ToInt32(m.Groups[1].ToString());
            	    m = Regex.Match(response,@" FLAGS \((.*?)\)");
                	if(m.Groups.Count>1)x.SetFlags(m.Groups[1].ToString());
	                response = ReadLine();
    	        }
				if(response.StartsWith(tag+"OK")){
					if(response.ToUpper().IndexOf("READ/WRITE") > -1) x.Rw = true;
				}
            	this._selected = true;
            	this._selectedmailbox = mailbox;
        	}
        	return x;
		 } catch (Exception e) {
		 	this._lasterror = "Examine : " + e.Message;
			return null;
		 }
    	}

	public bool Expunge() {
		try {
        	if(!_connected) throw new Exception("You must connect first !");
            if(!_authenticated)throw new Exception("You must authenticate first !");
			if(!_selected)      throw new Exception("You must select first !");
            string tag = this.Tag;
            string command = tag + "EXPUNGE\r\n";
            SendCommand(command);
            string response = ReadLine();
            while(response.StartsWith("*")){
				response = ReadLine();
			}
			if(response.StartsWith(tag+"OK"))return true;
        	return false;
		} catch (Exception e) {
            this._lasterror = "Expunge : " + e.Message;
            return false;
        }
	}

	public MailCollection FetchMessages(string start,string end,bool uid,bool headersonly,bool setseen) {
		try {
			if(!_connected) 	throw new Exception("You must connect first !");
   	     	if(!_authenticated)	throw new Exception("You must authenticate first !");
    	    if(!_selected)		throw new Exception("You must select first !");
			string UID,HEADERS,SETSEEN;
			UID = HEADERS = SETSEEN = String.Empty;
			if(uid)UID="UID ";
			if(headersonly)HEADERS="HEADER";
			if(setseen)SETSEEN=".PEEK";
			string tag = this.Tag;
			string command = tag + UID + "FETCH " + start + ":" + end + " (UID RFC822.SIZE FLAGS BODY" + SETSEEN + "[" + HEADERS + "])\r\n";
	        SendCommand(command);
			MailCollection x = new MailCollection();
			string reg = @"\* \d+ FETCH.*?BODY.*?\{(\d+)\}";
			string response = ReadLine();
			Match m = Regex.Match(response,reg);
			while(m.Groups.Count > 1){
				int bodylen = Convert.ToInt32(m.Groups[1].ToString());
				Mail mail = new Mail();
				byte[] body = ReadData(bodylen);
				Match m2 = Regex.Match(response,@"UID (\d+)");
				if(m2.Groups[1]!=null)mail.Uid = m2.Groups[1].ToString();
				m2 = Regex.Match(response,@"FLAGS \((.*?)\)");
				if(m2.Groups[1]!=null)mail.SetFlags(m2.Groups[1].ToString());
				m2 = Regex.Match(response,@"RFC822\.SIZE (\d+)");
				if(m2.Groups[1]!=null)mail.Size = Convert.ToInt32(m2.Groups[1].ToString());
				mail.Load(body,headersonly);
				x.Add(mail);
				response = ReadLine(); // read last line terminated by )
				response = ReadLine(); // read next line
		        m = Regex.Match(response,reg);
			}
			return x;
		} catch (Exception e) {
			this._lasterror = "FetchMessages " + e.ToString();//e.Message;
            return null;
		}
	 }
	
	public Quota GetQuota(string mailbox) {
        try {
            if(!_connected) throw new Exception("You must connect first !");
            if(!_authenticated)throw new Exception("You must authenticate first !");
            if(this._capability.IndexOf("NAMESPACE")== -1) 
					 new Exception("This command is not supported by the server or call Capability() is need !");

            string command = this.Tag + "GETQUOTAROOT " + mailbox + "\r\n";
            SendCommand(command);
            string response = ReadLine();
			string reg = "\\* QUOTA (.*?) \\((.*?) (.*?) (.*?)\\)";
            while(response.StartsWith("*")){
				Match m = Regex.Match(response,reg);
				if(m.Groups.Count > 1){
			 		return new Quota(	m.Groups[1].ToString(),
										m.Groups[2].ToString(),
										Int32.Parse(m.Groups[3].ToString()),
										Int32.Parse(m.Groups[4].ToString())
									);
				}
				response = ReadLine();
            }
			return null;
        } catch (Exception e) {
            this._lasterror = "Quota : " + e.Message;
            return null;
        }

	}
	public bool IsConnected() {
		return this._connected;
	}

	public bool IsLoggedIn() {
		return this._authenticated;
	}

	public MailboxCollection ListMailboxes(string reference, string pattern) {
		try {
	        if(!_connected) throw new Exception("You must connect first !");
    	    if(!_authenticated)throw new Exception("You must authenticate first !");
			MailboxCollection x = new MailboxCollection();
	        string command = this.Tag + "LIST \""+reference+"\" \""+pattern+"\"\r\n";
    	    SendCommand(command);
			string reg = "\\* LIST \\(([^\\)]*)\\) \\\"([^\\\"]+)\\\" \\\"([^\\\"]+)\\\"";
	        string response = ReadLine();
    	    Match m = Regex.Match(response,reg);
        	while(m.Groups.Count > 1){
				Mailbox mailbox = new Mailbox(m.Groups[3].ToString());
				x.Add(mailbox);
				response = ReadLine();
				m = Regex.Match(response,reg);		
			}
    	    return x;
		} catch (Exception e) {
			this._lasterror = "ListMailboxes : " + e.Message;
            return null;
		}
	}

    public MailboxCollection ListSuscribesMailboxes(string reference, string pattern) {
        if(!_connected) throw new Exception("You must connect first !");
        if(!_authenticated)throw new Exception("You must authenticate first !");
        MailboxCollection x = new MailboxCollection();
        string command = this.Tag + "LSUB \""+reference+"\" \""+pattern+"\"\r\n";
        SendCommand(command);
		string reg = "\\* LSUB \\(([^\\)]*)\\) \\\"([^\\\"]+)\\\" \\\"([^\\\"]+)\\\"";
        string response = ReadLine();
        Match m = Regex.Match(response,reg);
        while(m.Groups.Count > 1){
            Mailbox mailbox = new Mailbox(m.Groups[3].ToString());
            x.Add(mailbox);
            response = ReadLine();
            m = Regex.Match(response,reg);
        }
        return x;
    }

	public bool Login(string login, string password) {
		try {
			if(!_connected){
				throw new Exception("You must connect first !");
			}
			string command = String.Empty;
			string result = String.Empty;
			string tag = this.Tag;
			switch (this.AuthMethod) {
				case "CRAM-MD5" :
					command = tag + "AUTHENTICATE CRAM-MD5\r\n";
					SendCommand(command);
					// retrieve server key
					string key = ReadLine().Replace("+ ","");
					key = System.Text.Encoding.Default.GetString(Convert.FromBase64String(key));
					// calcul hash
					HMACMD5 kMd5 = new HMACMD5(System.Text.Encoding.ASCII.GetBytes(password));
					byte[] hash1 = kMd5.ComputeHash(System.Text.Encoding.ASCII.GetBytes(key));
					key = BitConverter.ToString(hash1).ToLower().Replace("-","");
					string response = Convert.ToBase64String(System.Text.Encoding.ASCII.GetBytes(login + " " + key))+"\r\n";
					SendCommand(response);
					result = ReadLine();
					if(result.StartsWith(tag+"OK")==true){
						this._authenticated = true;
						return true;
					}else{
						throw new Exception(result);
					}
					break;
				case "LOGIN" :
	                command = tag + "LOGIN "+login+" "+password+"\r\n";
					SendCommand(command);
					result = ReadLine();
       		        if(result.StartsWith(tag+"OK")){
   	 	    	        this._authenticated = true;
           	    	    return true;
                	}else{
						throw new Exception(result);
    	            }
					break;
			case "PLAIN" :
			default :
				throw new Exception("Authentication Mode Not Yet Supported !");
				break;
			}
		} catch (Exception e) {
			this._lasterror = "Login : " + e.Message;
            return false;
		}
			
	}

	public bool Logout() {
		try {
			if(!_connected) throw new Exception("You must connect first !");
			string command = this.Tag + "LOGOUT";
			SendCommand(command);
			this._connected = this._authenticated = false;
			_imapconnection = null;
			return true;
		} catch (Exception e) {
		    this._lasterror = "Logout : " + e.Message;
		    return false;
		}
	}

	public Namespaces Namespace(){
		try {
			if(!_connected) throw new Exception("You must connect first !");
			if(!_authenticated)throw new Exception("You must authenticate first !");
			if(this._capability.IndexOf("NAMESPACE")== -1) throw new Exception("This command is not supported by the server or call Capability() is need !");
			string command = this.Tag + "NAMESPACE\r\n";
			SendCommand(command);
			string response = ReadLine();
			//Console.WriteLine(response);
			if(response.StartsWith("* NAMESPACE")){
				response = response.Substring(12);
				Namespaces n = new Namespaces();
				//[TODO] be sure to parse correctly namespace when not all namespaces are present. NIL character
				string reg =@"\((.*?)\) \((.*?)\) \((.*?)\)$";
	            Match m = Regex.Match(response,reg);
 	           	if(m.Groups.Count != 4) {
				ReadLine();
				throw new Exception("En error occure, this command is not fully supported !");
			}
				string reg2 = "\\(\\\"(.*?)\\\" \\\"(.*?)\\\"\\)";
				Match m2 = Regex.Match(m.Groups[1].ToString(),reg2);
				while(m2.Groups.Count>1) {
					n.AddServerNamespace(m2.Groups[1].ToString(),m2.Groups[2].ToString());
					m2 = m2.NextMatch();
				}
				m2 = Regex.Match(m.Groups[2].ToString(),reg2);
				while(m2.Groups.Count>1) {
					n.AddUserNamespace(m2.Groups[1].ToString(),m2.Groups[2].ToString());
					m2 = m2.NextMatch();
				}
				m2 = Regex.Match(m.Groups[3].ToString(),reg2);
				while(m2.Groups.Count>1) {
					n.AddSharedNamespace(m2.Groups[1].ToString(),m2.Groups[2].ToString());
					m2 = m2.NextMatch();
				}
				ReadLine();
				return n;
			} else {
				throw new Exception("Unknow server response !");
			}
		} catch (Exception e) {
		    this._lasterror = "Namespace : " + e.Message;
			return null;
	    }
    }

	public int NumMails(string mailbox){
		try {
		    if(!_connected) throw new Exception("You must connect first !");	
		    if(!_authenticated)throw new Exception("You must authenticate first !");
			string command = this.Tag + "STATUS "+mailbox+" (MESSAGES)\r\n";
			SendCommand(command);
			string reg = @"\* STATUS.*MESSAGES (\d+)";
			string response = ReadLine();
			int result = 0;
			while(response.StartsWith("*")){
	    	    Match m = Regex.Match(response,reg);
			    if(m.Groups.Count > 1)result=Convert.ToInt32(m.Groups[1].ToString());
		        response = ReadLine();
	    	    m = Regex.Match(response,reg);
		    }
		    return result;
		} catch (Exception e) {
            this._lasterror = "NumMails : " + e.Message;
            return -1;
        }
	}

	public bool RefetchMailFlags(Mail email) {
			        throw new Exception("Not yet implemented !");
					        return false;
	}

	public bool RenameMailbox(string frommailbox, string tomailbox) {
		try {
	        if(!_connected) throw new Exception("You must connect first !");
    	    if(!_authenticated)throw new Exception("You must authenticate first !");
        	string command = this.Tag + "RENAME \""+frommailbox+"\" \"" +tomailbox+"\"\r\n";
	        SendCommand(command);
    	    string response = ReadLine();
        	response = response.Substring(response.IndexOf(" ")).Trim();
	        if(!response.ToUpper().StartsWith("OK")){
    	        throw new Exception(response);
        	}
        	return true;
		} catch (Exception e) {
            this._lasterror = "RenameMailbox : " + e.Message;
            return false;
        }
	}

    public MessageSet Search(string criteria,bool uid){
        try {
            if(!_connected) throw new Exception("You must connect first !");
            if(!_authenticated)throw new Exception("You must authenticate first !");
            if(!_selected)      throw new Exception("You must select first !");
            string isuid = uid?"UID ":"";
            string command = this.Tag + isuid + "SEARCH "+criteria+"\r\n";
            SendCommand(command);

            string reg = @"^\* SEARCH (.*)";
            string response = ReadLine();
	    ReadLine (); // Read the SUCCESS line
            MessageSet ms = new MessageSet();
            Match m = Regex.Match(response,reg);
            if(m.Groups.Count > 1){
                string[] uids = m.Groups[1].ToString().Trim().Split(' ');
                foreach(string s in uids){
                    ms.Messages.Add(s);
                }
                return ms;
            }else {
                throw new Exception(response);
            }
        } catch (Exception e) {
            this._lasterror = "Search : " + e.Message;
            return null;
        }
    }

	public Mailbox SelectMailbox(string mailbox) {
		try {
	        if(!_connected) throw new Exception("You must connect first !");
    	    if(!_authenticated)throw new Exception("You must authenticate first !");
        	Mailbox x = null;
			string tag = this.Tag;
    	    string command = tag + "SELECT \""+mailbox+"\"\r\n";
        	SendCommand(command);
	        string response = ReadLine();
			if(response.StartsWith("*")) {
				x = new Mailbox(mailbox);
			    while(response.StartsWith("*")){
					Match m;
					m = Regex.Match(response,@"(\d+) EXISTS");
					if(m.Groups.Count>1){x.NumMsg = Convert.ToInt32(m.Groups[1].ToString());}
					m = Regex.Match(response,@"(\d+) RECENT");
					if(m.Groups.Count>1)x.NumNewMsg = Convert.ToInt32(m.Groups[1].ToString());
					m = Regex.Match(response,@"UNSEEN (\d+)");
	                if(m.Groups.Count>1)x.NumUnSeen = Convert.ToInt32(m.Groups[1].ToString());
					m = Regex.Match(response,@" FLAGS \((.*?)\)");
	    	        if(m.Groups.Count>1)x.SetFlags(m.Groups[1].ToString());
    	    	    response = ReadLine();
        		}
				if(response.StartsWith(tag+"OK")){
					if(response.ToUpper().IndexOf("READ/WRITE") > -1) x.Rw = true;
				}
				this._selected = true;
				this._selectedmailbox = mailbox;
			}
	        return x;
		} catch (Exception e) {
            this._lasterror = "SelectMailbox : " + e.Message;
            return null;
        }
	}

	public bool Store(string messageset,bool replace,string flags){
		try {
            if(!_connected) throw new Exception("You must connect first !");
            if(!_authenticated)throw new Exception("You must authenticate first !");
            if(!_selected)      throw new Exception("You must select first !");
			string isreplace = replace?"+":"";
            string command = this.Tag + "STORE "+isreplace+"FLAGS.SILENT ("+flags+")\"\r\n";
            SendCommand(command);
            string response = ReadLine();
            response = response.Substring(response.IndexOf(" ")).Trim();
            if(!response.ToUpper().StartsWith("OK")){
                throw new Exception(response);
            }
            return true;
        } catch (Exception e) {
            this._lasterror = "Store : " + e.Message;
            return false;
        }
	}

	public bool SuscribeMailbox(string mailbox) {
		try {
	        if(!_connected) throw new Exception("You must connect first !");
    	    if(!_authenticated)throw new Exception("You must authenticate first !");
        	string command = this.Tag + "SUBSCRIBE \""+mailbox+"\"\r\n";
	        SendCommand(command);
    	    string response = ReadLine();
        	response = response.Substring(response.IndexOf(" ")).Trim();
	        if(!response.ToUpper().StartsWith("OK")){
    	        throw new Exception(response);
        	}
	        return true;
		} catch (Exception e) {
            this._lasterror = "SuscribeMailbox : " + e.Message;
            return false;
		}
	}

	public bool UnSuscribeMailbox(string mailbox) {
		try {
	        if(!_connected) throw new Exception("You must connect first !");
			if(!_authenticated)throw new Exception("You must authenticate first !");
			string command = this.Tag + "UNSUBSCRIBE \""+mailbox+"\"\r\n";
	        SendCommand(command);
    	    string response = ReadLine();
        	response = response.Substring(response.IndexOf(" ")).Trim();
	        if(!response.ToUpper().StartsWith("OK")){
				throw new Exception(response);
        	}
	        return true;
		 } catch (Exception e) {
	        this._lasterror = "UnSuscribeMailbox : " + e.Message;
	        return false;
	     }
    }

/*
 *
 *		Privates functions
 *
 */

	private void SendCommand(string command) {
	    byte [] data = System.Text.Encoding.ASCII.GetBytes(command.ToCharArray());
        try {
			//Console.Write("SendCommand : "+command);
        	_imapnetworkstream.Write(data, 0, data.Length);
		} catch (Exception e) {
       	   	throw new Exception("SendCommand error :" + e.Message);
        }
   	}
    internal X509Certificate DefaultCertificateSelectionCallback(X509CertificateCollection clientCertificates,
																X509Certificate serverCertificate,
																string targetHost,
																X509CertificateCollection serverRequestedCertificates) {
    	return null;
    }
    internal bool DefaultCertificateValidationCallback(X509Certificate certificate,int[] certificateErrors) {
    	return true;
    }
    internal AsymmetricAlgorithm DefaultPrivateKeySelectionCallback(X509Certificate certificate,string targetHost) {
        return null;
    }


	/*
	 *
	 *			ReadData(), ReadLine(), function to catch streamer idle timeout
	 *			TODO catch time out :) !!
	 *
	 * 
	 */
	
	private string ReadLine(){
		StringBuilder line = new StringBuilder();
		char[] buf = new char[2];
		while(!(buf[0]==13 && buf[1]==10)) {
			buf[0] = buf[1];
			buf[1] = (char)_imapnetworkstream.ReadByte();
			line.Append(buf[1]);
		}
		return line.ToString(0,line.Length-2); 
	}
	private byte[] ReadData (int len) {
		byte[] data = new byte[len];
        byte[] buf = new byte[1];
        Int32 numread = 0;
		int x = 0;
        while (x < len) {
            data[x++] = (byte)_imapnetworkstream.ReadByte();
        }
		return data;
	}
  }
}
