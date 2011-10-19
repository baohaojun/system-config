//
// FilterPackage.cs
//
// Copyright (C) 2006 Debajyoti Bera <dbera.web@gmail.com>
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

using Beagle.Util;

namespace Beagle.Filters {

	public abstract class FilterPackage : Beagle.Daemon.Filter {

		// 1. Store packager information as a single field
		//    Store packager as dc:creator
		//    Store license as dc:rights
		//    Store homepage as searchable
		private int version = 1;

		public FilterPackage ()
		{
			SnippetMode = true;
			SetFileType ("package");

			base.SetVersion (version);
		}

		protected new void SetVersion (int version)
		{
			this.version += version;
			base.SetVersion (version);
		}

		protected abstract bool PullPackageProperties ();

		private string package_name, package_version, category;
		private string homepage, summary, packager, license;
		private long size;

		/* Some of the metadata common in all packages.
		 * Use them to display general package information in beagle frontends.
		 *  - PackageName - name of the package
		 *  - PackageVersion - package version
		 *  - Summary - short description of the package (if available)
		 *  - Category - category of the package (if available)
		 *  - Homepage - Homepage URL of the package/project (if available)
		 *  - Packager - Packager information of the package (if available)
		 *  - License - License of the package
		 *  - Size - Size of the package (in bytes)
		 */

		// Name of the package
		protected string PackageName  {
			get { return package_name; }
			set { package_name = value; }
		}

		// Version
		protected string PackageVersion {
			get { return package_version; }
			set { package_version = value; }
		}

		// A short summary. Some packages might not have this.
		// Some packages have a longer description which is not suitable for showing in the frontend.
		// Use summary instead.
		protected string Summary {
			get { return summary; }
			set { summary = value; }
		}

		// Category/section to which the package might belong. Not all packages might have this.
		protected string Category {
			get { return category; }
			set { category = value; }
		}

		// Homepage of the package
		protected string Homepage {
			get { return homepage; }
			set { homepage = value; }
		}

		// Packager. This is generally the name of a person or a company followed by the
		// email address or homepage.
		protected string Packager {
			get { return packager; }
			set { packager = value; }
		}

		// License of the package. Not present in all packages.
		protected string License {
			get { return license; }
			set { license = value; }
		}

		// Size of the package - in bytes.
		// Depending on package, its either the installed size or the size of the package.
		protected long Size {
			get { return size; }
			set { size = value; }
		}

		protected override void DoPullProperties ()
		{
			if (! PullPackageProperties ()) {
				Error ();
				return;
			}

			AddProperty (Beagle.Property.New ("dc:title", package_name));
			AddProperty (Beagle.Property.NewKeyword ("fixme:version", package_version));
			AddProperty (Beagle.Property.New ("dc:subject", summary));
			AddProperty (Beagle.Property.New ("pkg:group", category));
			AddProperty (Beagle.Property.New ("dc:source", homepage));
			AddProperty (Beagle.Property.New ("dc:creator", packager));
			AddProperty (Beagle.Property.New ("dc:rights", license));
			AddProperty (Beagle.Property.NewUnsearched ("fixme:size", size));

		}

		protected override void DoPull ()
		{
			Finished ();
		}
	}
}
