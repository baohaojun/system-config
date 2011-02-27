//
// FilterImage.cs
//
// Copyright (C) 2004 Novell, Inc.
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
using System.IO;
using System.Text;

using Beagle.Util;
using FSpot;
using FSpot.Xmp;
using SemWeb;

namespace Beagle.Filters {

	public abstract class FilterImage : Beagle.Daemon.Filter {

		// 1: Base
		// 2: Added fspot:IsIndexed field, added width & height properties
		// 3: Add Digikam tags and caption
		// 4: Index IPTC keywords
		private int version = 4;

		public FilterImage ()
		{
			base.SetVersion (Version);
			SetFileType ("image");
		}

		protected new void SetVersion (int version)
		{
			this.version += version;
			base.SetVersion (version);
		}

		protected virtual void PullImageProperties () { }

		private int width = -1;
		private int height = -1;
		private int depth = -1;

		protected int Width {
			set { width = value; }
			get { return width; }
		}

		protected int Height {
			set { height = value; }
			get { return height; }
		}

		protected int Depth {
			set { depth = value; }
			get { return depth; }
		}

		protected override void DoPullProperties ()
		{
			PullImageProperties ();

			if (width > 0)
				AddProperty (Beagle.Property.NewUnsearched ("fixme:width", width));

			if (height > 0)
				AddProperty (Beagle.Property.NewUnsearched ("fixme:height", height));

			if (depth > 0)
				AddProperty (Beagle.Property.NewUnsearched ("fixme:depth", depth));

			try {
				AddFSpotInformation (this.FileInfo.FullName);
				AddDigikamInformation (this.FileInfo.FullName);
			} catch (Exception ex) {
				Logger.Log.Error ("Exception trying to retrieve FSpot/Digikam information:" + ex);
			}


		}

		private void AddFSpotInformation (string path)
		{
			FSpotTools.Photo photo = FSpotTools.GetPhoto (this.FileInfo.FullName);

			if (photo == null)
				return;

			AddProperty (Beagle.Property.NewBool ("fspot:IsIndexed", true));
			
			AddProperty (Beagle.Property.New ("fspot:Description", photo.Description));
			
			foreach (FSpotTools.Tag tag in photo.Tags) {
				AddProperty (Beagle.Property.New ("fspot:Tag", tag.Name));
				AddProperty (Beagle.Property.NewUnstored ("image:tag", tag.Name));
			}
		}

		private void AddDigikamInformation (string path)
		{
			DigikamTags.DigikamData digikam_data = DigikamTags.GetDigikamData (this.FileInfo.FullName);

			if (digikam_data == null)
				return;

			AddProperty (Beagle.Property.NewBool ("digikam:IsIndexed", true));
			
			AddProperty (Beagle.Property.New ("digikam:caption", digikam_data.caption));
			
			foreach (string tag in digikam_data.Tags) {
				AddProperty (Beagle.Property.New ("digikam:Tag", tag));
				AddProperty (Beagle.Property.NewUnstored ("image:tag", tag));
			}
		}
		
		internal void AddXmpProperties (XmpFile xmp)
		{
			Resource subject_anon = null;
			Resource creator_anon = null;
			Resource rights_anon = null;
			Resource title_anon = null;

			foreach (Statement stmt in xmp.Store) {
				if (stmt.Predicate == MetadataStore.Namespaces.Resolve ("dc:subject")) {
					//Console.WriteLine ("found subject");
					subject_anon = stmt.Object;
				} else if (stmt.Predicate == MetadataStore.Namespaces.Resolve ("dc:creator")) {
					//Console.WriteLine ("found creator");
					creator_anon = stmt.Object;
				} else if (stmt.Predicate == MetadataStore.Namespaces.Resolve ("dc:rights")) {
					rights_anon = stmt.Object;
				} else if (stmt.Predicate == MetadataStore.Namespaces.Resolve ("dc:title")) {
					if (stmt.Object is Literal)
						AddProperty (Beagle.Property.New ("dc:title", ((Literal)stmt.Object).Value));
					else if (stmt.Object is BNode)
						title_anon = stmt.Object;
				} else if (stmt.Predicate == MetadataStore.Namespaces.Resolve ("tiff:Model")) {
					// NOTE: the namespaces for xmp and beagle don't always match up
					AddProperty (Beagle.Property.New ("exif:Model", ((Literal)stmt.Object).Value));
				} else if (stmt.Predicate == MetadataStore.Namespaces.Resolve ("cc:license")) {
					AddProperty (Beagle.Property.NewKeyword ("fixme:license", ((Literal)stmt.Object).Value));
				}
			}
			
			foreach (Statement stmt in xmp.Store) {
				if (stmt.Subject == subject_anon && 
				    stmt.Predicate != MetadataStore.Namespaces.Resolve ("rdf:type")) {
					AddProperty (Beagle.Property.New ("dc:subject", ((Literal)stmt.Object).Value));
				} else if (stmt.Subject == creator_anon &&  
					   stmt.Predicate != MetadataStore.Namespaces.Resolve ("rdf:type")) {
					AddProperty (Beagle.Property.New ("dc:creator", ((Literal)stmt.Object).Value));
				} else if (stmt.Subject == rights_anon &&  
					   stmt.Predicate != MetadataStore.Namespaces.Resolve ("rdf:type")) {
					AddProperty (Beagle.Property.New ("dc:rights", ((Literal)stmt.Object).Value));
				} else if (stmt.Subject == title_anon &&
					   stmt.Predicate != MetadataStore.Namespaces.Resolve ("rdf:type")) {
					AddProperty (Beagle.Property.New ("dc:title", ((Literal)stmt.Object).Value));
				}
			}
		}
	}
}
