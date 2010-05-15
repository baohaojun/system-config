//
// GMailSearchDriver.cs
//
// A basic gmail email search driver. Search is performed in realtime
// using imap search functionality.
//
// Copyright (C) 2008 D Bera <dbera.web@gmail.com>
//
//
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//

using System;
using System.Collections;
using System.Net.Imap;
using System.Text;

using Beagle.Util;

[assembly: Beagle.Daemon.IQueryableTypes (typeof (Beagle.Daemon.GoogleBackend.GMailSearchDriver))]

namespace Beagle.Daemon.GoogleBackend {

	[QueryableFlavor (Name="GMailSearch", Domain=QueryDomain.Global, RequireInotify=false)]
	public class GMailSearchDriver : IQueryable {

		// Bunch of const strings
		const string GMAIL_SERVER = "imap.gmail.com";
		const string AUTH_METHOD = "LOGIN";
		const string GMAIL_DOMAIN = "mail"; // Google Apps users have their own domain
		const string GMAIL_HIT_URL = "https://mail.google.com/{0}/#search/{1}"; // 0 - domain, 1 - msgid
		const int PORT = 993;
		const bool USE_SSL = true;

		private string username = null;
		private string password = null;
		private string domain = GMAIL_DOMAIN;
		private string search_folder = null;
		private bool valid_account = false;

		private ImapClient imap_client = null;

		public GMailSearchDriver ()
		{
		}

		public void Start ()
		{
			try {
				GMime.Global.Init ();
			} catch (Exception e) {
				Log.Error (e, "Unable to initialize GMime");
				return;
			}

			Config config = Conf.Get ("GoogleBackends");
			ReadConf (config);
			Conf.WatchForUpdates ();
			Conf.Subscribe ("GoogleBackends", delegate (Config new_config) {
								ReadConf (new_config);
							    });
		}

		private void ReadConf (Config config)
		{
			if (config == null || config.Name != "GoogleBackends")
				return;

			username = config.GetOption ("GMailUsername", null);
			search_folder = config.GetOption ("GMailSearchFolder", null);

			string password_source = config.GetOption ("GMailPasswordSource", "conf-data");
			password_source = password_source.ToLower ();
			switch (password_source) {
			case "conf-file":
				password = config.GetOption ("GMailPassword", null);
				break;

			case "gnome-keyring":
				Log.Error ("GMailPasswordSource: gnome-keyring is not supported yet");
				break;

			case "kdewallet":
				try {
					password = KdeUtils.ReadPasswordKDEWallet ("beagle", username);
				} catch (Exception e) {
					Log.Error (e, "Error in reading password in KDE wallet");
				}
				break;

			default:
				Log.Error ("GMailPasswordSource should be one of 'kdewallet,gnome-keyring,conf-file");
				break;

			}

			valid_account = (
				! String.IsNullOrEmpty (username) &&
				! String.IsNullOrEmpty (password) &&
				! String.IsNullOrEmpty (search_folder));

			if (! valid_account)
				Log.Warn ("GMail account information not set. Search is disabled.");
			else
				Log.Debug ("GMail account information successfully read.");

			domain = config.GetOption ("GoogleAppDomainName", null);
			if (String.IsNullOrEmpty (domain))
				domain = GMAIL_DOMAIN;
			else
				// Google Apps domain is of form
				// a/mydomain.name
				domain = ("a/" + domain);
		}

		public bool AcceptQuery (Query query)
		{
			if (! valid_account)
				return false;

			bool has_text = false;
			foreach (QueryPart qp in query.Parts)
				if (qp is QueryPart_Text)
					has_text = true;

			if (! has_text) {
				Log.Error ("GMailSearchDriver can only search for text and does not support 'OR', 'NOT' queries.");
				return false;
			}
			    
			return Connect ();
		}

		public void DoQuery (Query query,
				     IQueryResult result,
				     IQueryableChangeData changeData)
		{
			// Program semantics guarantee that imap_client != null

			StringBuilder sb = new StringBuilder ();
			foreach (QueryPart qp in query.Parts) {
				if (qp is QueryPart_Text) {
					if (sb.Length > 0)
						sb.Append (' ');
					sb.Append (((QueryPart_Text) qp).Text);
				}
			}

			// IMAP SEARCH protocol
			sb.Insert (0, "TEXT \"");
			sb.Append ("\"");
			string imap_query = sb.ToString ();

			try {
				DoGMailQuery (sb.ToString (), query.MaxHits, result);
			} catch {
				Log.Error ("GMailSearchDriver IMAP error :{0}", imap_client.LastError);
			} finally {
				imap_client.Disconnect ();
			}
		}

#if ENABLE_RDF_ADAPTER
		public ICollection DoRDFQuery (Query query)
		{
			return null;
		}
#endif

		public int DoCountMatchQuery (Query query)
		{
			// FIXME: Implement this
			return -1;
		}

		public ISnippetReader GetSnippet (string[] query_terms, Hit hit, bool full_text, int context_length, int snippet_length)
		{
			return null;
		}

		public QueryableStatus GetQueryableStatus ()
		{
			QueryableStatus status = new QueryableStatus ();
			status.Name = "GMailSearch";
			// FIXME: Get number of emails from GMail
			return status;
		}

		///////////////////////////////////////////////////

		private bool Connect ()
		{
			if (imap_client != null)
				imap_client.Disconnect ();
			imap_client = null;
			bool success = false;

			try {
				imap_client = new ImapClient ();
				imap_client.AuthMethod = AUTH_METHOD;
				imap_client.Port = PORT;
				imap_client.Ssl = USE_SSL;
				imap_client.Connect (GMAIL_SERVER);

				success = imap_client.Login (username, password);

				if (! success) {
					Log.Error ("IMAP connection unsuccessful: {0}", imap_client.LastError);
				} else {
					Mailbox search_mailbox = imap_client.SelectMailbox (search_folder);
					success = (search_mailbox != null);
					if (! success)
						Log.Error ("Selection folder unsuccessful: {0}", imap_client.LastError);
				}
			} catch (Exception e) {
				Log.Error (e, "GMailSearchDriver: Error in connecting to {0} with username {1}", GMAIL_SERVER, username);
			}

			if (! success && imap_client != null)
				imap_client.Disconnect ();

			return success;
		}

		private void DoGMailQuery (string query, int maxhits, IQueryResult result)
		{
			Log.Debug ("GMailSearchDriver: Searching for [{0}]", query);
			MessageSet results = imap_client.Search (query, false);
			if (results == null) {
				return;
			}

			Log.Debug ("Recvd {0} messages", results.Messages.Count);

			// Get the messages in reverse order; latest first
			ArrayList matched_ids = new ArrayList (results.Messages);
			matched_ids.Reverse ();

			const int MAX_QUEUED_HITS = 25;
			int left = Math.Min (maxhits, matched_ids.Count);
			ArrayList result_batch = new ArrayList (MAX_QUEUED_HITS);

			MailCollection emails;
			GMime.StreamMem stream;
			GMime.Parser parser;
			GMime.Message message;
			Hit hit;

			foreach (string id in matched_ids) {
				if (left -- == 0)
					break;
				Log.Debug ("Fetching headers for message id {0}", id);

				emails = imap_client.FetchMessages (id, id, false,true,false);
				if (emails == null || emails.Count == 0) {
					Log.Error ("IMAP error: {0}", imap_client.LastError);
					continue;
				}

				foreach (Mail m in emails) {
					hit = null;

					using (stream = new GMime.StreamMem (m.Header))
					using (parser = new GMime.Parser (stream))
					using (message = parser.ConstructMessage ())
						hit = MessageToHit (message);

					if (hit == null) {
						Log.Error ("Bad IMAP email {0}: no msg-id", id);
						continue;
					} else {
						result_batch.Add (hit);
					}
				}

				if (result_batch.Count >= MAX_QUEUED_HITS) {
					result.Add (result_batch);
					result_batch.Clear ();
				}
			}

			result.Add (result_batch, matched_ids.Count);
		}

		// Copied from FilterMail.cs:DoPullProperties
		private Hit MessageToHit (GMime.Message message)
		{
			string msgid = message.GetHeader ("Message-Id");
			if (msgid == null)
				return null;

			msgid = GMime.Utils.DecodeMessageId (msgid);
			Hit hit = new Hit ();
			hit.Uri = new Uri (String.Format (GMAIL_HIT_URL, domain, msgid));
                        hit.AddProperty (Property.NewUnsearched ("beagle:HitType", "MailMessage"));
                        hit.AddProperty (Property.NewUnsearched ("beagle:MimeType", "text/html"));
                        hit.AddProperty (Property.NewUnsearched ("beagle:Source", "GMailSearch"));
			hit.Score = 1.0;

			hit.AddProperty (Property.NewUnsearched ("fixme:msgid", msgid));

			string subject = GMime.Utils.HeaderDecodePhrase (message.Subject);
			hit.AddProperty (Property.New ("dc:title", subject));
			hit.Timestamp = message.Date.ToUniversalTime ();
			hit.AddProperty (Property.NewDate ("fixme:date", message.Date.ToUniversalTime ()));

			GMime.InternetAddressList addrs;
			addrs = message.GetRecipients (GMime.Message.RecipientType.To);
			foreach (GMime.InternetAddress ia in addrs) {
				hit.AddProperty (Property.NewUnsearched ("fixme:to", ia.ToString (false)));
				if (ia.AddressType != GMime.InternetAddressType.Group)
					hit.AddProperty (Property.New ("fixme:to_address", ia.Addr));

				hit.AddProperty (Property.New ("fixme:to_name", ia.Name));
			}
			addrs.Dispose ();

			addrs = message.GetRecipients (GMime.Message.RecipientType.Cc);
			foreach (GMime.InternetAddress ia in addrs) {
				hit.AddProperty (Property.NewUnsearched ("fixme:cc", ia.ToString (false)));
				if (ia.AddressType != GMime.InternetAddressType.Group)
					hit.AddProperty (Property.New ("fixme:cc_address", ia.Addr));

				hit.AddProperty (Property.New ("fixme:cc_name", ia.Name));
			}
			addrs.Dispose ();

			addrs = GMime.InternetAddressList.ParseString (GMime.Utils.HeaderDecodePhrase (message.Sender));
			foreach (GMime.InternetAddress ia in addrs) {
				hit.AddProperty (Property.NewUnsearched ("fixme:from", ia.ToString (false)));
				if (ia.AddressType != GMime.InternetAddressType.Group)
					hit.AddProperty (Property.New ("fixme:from_address", ia.Addr));

				hit.AddProperty (Property.New ("fixme:from_name", ia.Name));
			}
			addrs.Dispose ();

			foreach (GMime.References refs in message.References)
				hit.AddProperty (Property.NewUnsearched ("fixme:reference", refs.Msgid));

			string list_id = message.GetHeader ("List-Id");
			if (list_id != null)
				hit.AddProperty (Property.New ("fixme:mlist", GMime.Utils.HeaderDecodePhrase (list_id)));

			return hit;
		}
	}
}

