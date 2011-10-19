//
// AvahiBrowser.cs
//
// Copyright (C) 2006 Kyle Ambroff <kwa@icculus.org>
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
using System.Threading;
using System.Collections;
using System.Collections.Generic;

using Avahi;

namespace Beagle.Util
{
        public delegate void AvahiEventHandler (object sender, AvahiEventArgs args);
        
        public class AvahiBrowser : IDisposable
        {
                public event AvahiEventHandler HostFound;
                public event AvahiEventHandler HostRemoved;
                
                private Avahi.Client avahi_client = null;
                private ServiceBrowser browser = null;
                private ServiceResolver resolver = null;
                
                private List<NetworkService> services = null;

                public AvahiBrowser ()
                {
                        this.services = new List<NetworkService> ();
                        this.avahi_client = new Avahi.Client ();
                }

                public void Dispose ()
                {
			if (browser == null)
				return;

                        browser.Dispose ();
                        browser = null;
                }

                public void Start ()
                {
                        browser = new ServiceBrowser (avahi_client, "_beagle._tcp");
                        browser.ServiceAdded += OnServiceAdded;
                        browser.ServiceRemoved += OnServiceRemoved;
		}

		private static NetworkService ParseServiceInfo (ServiceInfo info)
		{
                        Uri uri = new Uri (String.Format ("http://{0}:{1}", info.HostName, info.Port));
                        bool password_required = false;
                        string cookie = null;

			// Check if the network service is password protected
			// and store cookie when found
                        foreach (byte[] bytes in info.Text) {
                                string text = System.Text.Encoding.UTF8.GetString (bytes);
                                string [] split = text.Split ('=');
                                
                                if (split.Length < 2)
                                        continue;

                                if (split [0].ToLower () == "password") {
                                        password_required = (split [1].ToLower () == "true");
                                } else if (split [0].ToLower () == "org.freedesktop.avahi.cookie") {
                                        cookie = split [1];
				}
                        }

                        NetworkService service = new NetworkService (info.Name, uri, password_required, cookie);

			return service;
		}

                private void OnServiceAdded (object o, ServiceInfoArgs args)
                {
			// Ignore local services
                        if ((args.Service.Flags & LookupResultFlags.Local) > 0)
                                return;
                        
                        resolver = new ServiceResolver (avahi_client, args.Service);
                        resolver.Found += OnServiceResolved;
                        resolver.Timeout += OnServiceTimeout;
                }
                
                private void OnServiceResolved (object o, ServiceInfoArgs args)
                {
			NetworkService service = ParseServiceInfo (args.Service);
                        services.Add (service);

                        AvahiEventArgs event_args = new AvahiEventArgs (service);

                        if (HostFound != null)
                                HostFound (null, event_args);
                        
			((ServiceResolver)o).Dispose ();
                }

                private void OnServiceTimeout (object o, EventArgs args)
                {
                        Logger.Log.Info ("Failed to resolve service.");
			((ServiceResolver)o).Dispose ();
                }

                private void OnServiceRemoved (object sender, ServiceInfoArgs args)
                {
			foreach (NetworkService service in services) {
				if (service.Name != args.Service.Name)
					continue;
                        
                                services.Remove (service);
			                        
				AvahiEventArgs event_args = new AvahiEventArgs (service);
				
				if (HostRemoved != null)
					HostRemoved (this, event_args);

				return;
			}
                }

		public NetworkService GetServiceByName (string name)
		{
			foreach (NetworkService service in services) {
				if (service.Name == name)
					return service;
			}

			return null;
		}

		public ICollection<NetworkService> GetServicesBlocking ()
		{
			object sync_lock = new object ();

			lock (sync_lock) {
				browser = new ServiceBrowser (avahi_client, "_beagle._tcp");
				browser.ServiceAdded += OnServiceAdded;
				browser.AllForNow += delegate (object o, EventArgs args) {
					Console.WriteLine ("All for now!");
					lock (sync_lock) {
						Monitor.Pulse (sync_lock);
					}
				};
				
				Monitor.Wait (sync_lock);

				Console.WriteLine ("Return!");
			}
			
			return services;
                }

                public ICollection Services {
                        get { return services; }
                }
        }
       
        public class AvahiEventArgs : EventArgs
        {
                private NetworkService service;

                public AvahiEventArgs (NetworkService service) : base ()
                {
                        this.service = service;
                }

                public NetworkService Service {
                        get { return service; }
                }
                
                public Uri Address {
                        get { return service.GetUri (); }
                }
                
                public string Name {
                        get { return service.Name; }
                }
        }
}
