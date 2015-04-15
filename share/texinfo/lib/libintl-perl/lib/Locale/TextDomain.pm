#! /bin/false

# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: TextDomain.pm,v 1.1 2011-10-12 23:51:27 pertusus Exp $

# High-level interface to Perl i18n.
# Copyright (C) 2002-2009 Guido Flohr <guido@imperia.net>,
# all rights reserved.

# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU Library General Public License as published
# by the Free Software Foundation; either version 2, or (at your option)
# any later version.
                                                                                
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
                                                                                
# You should have received a copy of the GNU Library General Public 
# License along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, 
# USA.

package __TiedTextDomain;

use strict;

sub TIEHASH
{
    my ($class, $function) = @_;
    bless {
        __function => $function,
    }, $class;
}

sub FETCH
{
    my ($self, $msg) = @_;
    
    &{$self->{__function}} ($msg);
}

sub FIRSTKEY
{
    my $self = shift;
    my $reset_iterator = keys %$self;
    return scalar each %$self;
}

sub NEXTKEY
{
    my $self = shift;
    return scalar each %$self;
}

sub CLEAR {}
sub STORE {}
sub DELETE {}

1;

package Locale::TextDomain;

use strict;

use Locale::Messages qw (textdomain bindtextdomain dgettext dngettext dpgettext dnpgettext);
use Cwd qw (abs_path);

use vars qw ($VERSION);

$VERSION = '1.20';

require Exporter;

use vars qw (@ISA @EXPORT %__ $__);

@ISA = ('Exporter');
@EXPORT = qw (__ __x __n __nx __xn __p __px __np __npx $__ %__ 
              N__ N__n N__p N__np);

my %textdomains = ();
my %bound_dirs = ();
my @default_dirs = ();

sub __ ($);
	
sub __find_domain ($);
sub __expand ($%);
sub __tied_gettext ($$);

BEGIN {
    # Tie the hash to gettext().
    tie %__, '__TiedTextDomain', \&__tied_gettext;
    $__ = \%__;

	# Add default search directories, but only if they exist.
	for my $dir (qw (/usr/share/locale /usr/local/share/locale)) {
        if (-d $dir) {
            @default_dirs = ($dir);
            last;
        }
    }
}

# Normal gettext.
sub __ ($)
{
    my $msgid = shift;
	
    my $package = caller;
    
    my $textdomain = $textdomains{$package};
    
    __find_domain $textdomain if 
		defined $textdomain && defined $bound_dirs{$textdomain};
    
    return dgettext $textdomain => $msgid;
}

# Called from tied hash.
sub __tied_gettext ($$)
{
    my ($msgid) = @_;
    
    my ($package) = caller (1);
    
    my $textdomain = $textdomains{$package};
    unless (defined $textdomain) {
		my ($maybe_package, $filename, $line) = caller (2);
		if (exists $textdomains{$maybe_package}) {
			warn <<EOF;
Probable use of \$__ or \%__ where __() should be used at $filename:$line.
EOF
		}
	}
    __find_domain $textdomain if
		defined $textdomain && defined $bound_dirs{$textdomain};
    
    return dgettext $textdomain => $msgid;
}

# With interpolation.
sub __x ($@)
{
    my ($msgid, %vars) = @_;
    
    my $package = caller;
    
    my $textdomain = $textdomains{$package};
    
    __find_domain $textdomain if
		defined $textdomain && defined $bound_dirs{$textdomain};
    
    return __expand ((dgettext $textdomain => $msgid), %vars);
}

# Plural.
sub __n ($$$)
{
    my ($msgid, $msgid_plural, $count) = @_;
    
    my $package = caller;
    
    my $textdomain = $textdomains{$package};
    
    __find_domain $textdomain if
		defined $textdomain && defined $bound_dirs{$textdomain};
    
    return dngettext $textdomain, $msgid, $msgid_plural, $count;
}

# Plural with interpolation.
sub __nx ($$$@)
{
    my ($msgid, $msgid_plural, $count, %args) = @_;
    
    my $package = caller;
    
    my $textdomain = $textdomains{$package};
    
    __find_domain $textdomain if
		defined $textdomain && defined $bound_dirs{$textdomain};
    
    return __expand ((dngettext $textdomain, $msgid, $msgid_plural, $count),
					 %args);
}

# Plural with interpolation.
sub __xn ($$$@)
{
    my ($msgid, $msgid_plural, $count, %args) = @_;
    
    my $package = caller;
    
    my $textdomain = $textdomains{$package};
    
    __find_domain $textdomain if
		defined $textdomain && defined $bound_dirs{$textdomain};
    
    return __expand ((dngettext $textdomain, $msgid, $msgid_plural, $count),
					 %args);
}

# Context. (p is for particular or special)
sub __p ($$)
{
    my $msgctxt = shift;
    my $msgid = shift;
	
    my $package = caller;
    
    my $textdomain = $textdomains{$package};
    
    __find_domain $textdomain if 
		defined $textdomain && defined $bound_dirs{$textdomain};
    
    return dpgettext $textdomain => $msgctxt, $msgid;
}

# With interpolation.
sub __px ($$@)
{
    my ($msgctxt, $msgid, %vars) = @_;
    
    my $package = caller;
    
    my $textdomain = $textdomains{$package};
    
    __find_domain $textdomain if
		defined $textdomain && defined $bound_dirs{$textdomain};
    
    return __expand ((dpgettext $textdomain => $msgctxt, $msgid), %vars);
}

# Context + Plural.
sub __np ($$$$)
{
    my ($msgctxt, $msgid, $msgid_plural, $count) = @_;
    
    my $package = caller;
    
    my $textdomain = $textdomains{$package};
    
    __find_domain $textdomain if
		defined $textdomain && defined $bound_dirs{$textdomain};
    
    return dnpgettext $textdomain, $msgctxt, $msgid, $msgid_plural, $count;
}

# Plural with interpolation.
sub __npx ($$$$@)
{
    my ($msgctxt, $msgid, $msgid_plural, $count, %args) = @_;
    
    my $package = caller;
    
    my $textdomain = $textdomains{$package};
    
    __find_domain $textdomain if
		defined $textdomain && defined $bound_dirs{$textdomain};
    
    return __expand ((dnpgettext $textdomain, $msgctxt, $msgid, $msgid_plural, $count),
					 %args);
}

# Dummy functions for string marking.
sub N__($)
{
    return shift;
}

sub N__n($$$)
{
    return @_;
}

sub N__p($$) {
    return @_;
}

sub N__np($$$$) {
    return @_;
}

sub import
{
    my ($self, $textdomain, @search_dirs) = @_;
    
    # Check our caller.
    my $package = caller;
    return if exists $textdomains{$package};
    
    # Was a textdomain specified?
	$textdomain = textdomain unless defined $textdomain && length $textdomain;
    
    # Remember the textdomain of that package.
    $textdomains{$package} = $textdomain;
    
    # Remember that we still have to bind that textdomain to
    # a directory.
    unless (exists $bound_dirs{$textdomain}) {
		@search_dirs = map $_ . '/LocaleData', @INC, @default_dirs
			unless @search_dirs;
		$bound_dirs{$textdomain} = [@search_dirs];
    }
	
    Locale::TextDomain->export_to_level (1, $package, @EXPORT);
    
    return;
}

# Private functions.
sub __find_domain ($)
{
	my $domain = shift;
	
	my $try_dirs = $bound_dirs{$domain};
	
	if (defined $try_dirs) {
		my $found_dir = '';
		
		TRYDIR: foreach my $dir (map { abs_path $_ } grep { -d $_ } @$try_dirs) {
			# Is there a message catalog?  We have to search recursively
			# for it.  Since globbing is reported to be buggy under
			# MS-DOS, we roll our own version.
			local *DIR;
			if (opendir DIR, $dir) {
				my @files = map { "$dir/$_/LC_MESSAGES/$domain.mo" } 
					grep { ! /^\.\.?$/ } readdir DIR;

				foreach my $file (@files) {
					if (-f $file || -l $file) {
						# If we find a non-readable file on our way,
						# we access has been disabled on purpose.
						# Therefore no -r check here.
						$found_dir = $dir;
						last TRYDIR;
					}
				}
			}
		}
		
		# If there was no success, this will fall back to the default search
		# directories.
		bindtextdomain $domain => $found_dir;
    }
    
    # The search has completed.
    undef $bound_dirs{$domain};
    
    return 1;
}

sub __expand ($%)
{
    my ($translation, %args) = @_;
    
    my $re = join '|', map { quotemeta $_ } keys %args;
    $translation =~ s/\{($re)\}/defined $args{$1} ? $args{$1} : "{$1}"/ge;
    
    return $translation;
}

1;

__END__

=head1 NAME

Locale::TextDomain - Perl Interface to Uniforum Message Translation

=head1 SYNOPSIS

 use Locale::TextDomain ('my-package', @locale_dirs);
 
 use Locale::TextDomain qw (my-package);
 
 my $translated = __"Hello World!\n";
 
 my $alt = $__{"Hello World!\n"};
 
 my $alt2 = $__->{"Hello World!\n"};

 my @list = (N__"Hello",
             N__"World");
 
 printf (__n ("one file read", 
              "%d files read", 
              $num_files),
         $num_files);

 print __nx ("one file read", "{num} files read", $num_files,
             num => $num_files);

 my $translated_context = __p ("Verb, to view", "View");

 printf (__np ("Files read from filesystems",
               "one file read", 
               "%d files read", 
               $num_files),
         $num_files);

 print __npx ("Files read from filesystems",
              "one file read", 
              "{num} files read", 
              $num_files,
              num => $num_files);


=head1 DESCRIPTION

The module Locale::TextDomain(3pm) provides a high-level interface
to Perl message translation.

=head2 Textdomains

When you request a translation for a given string, the system used
in libintl-perl follows a standard strategy to find a suitable message
catalog containing the translation: Unless you explicitely define
a name for the message catalog, libintl-perl will assume that your
catalog is called 'messages' (unless you have changed the default
value to something else via Locale::Messages(3pm), method textdomain()).

You might think that his default strategy leaves room for optimization
and you are right.  It would be a lot smarter if multiple software
packages, all with their individual message catalogs, could be installed
on one system, and it should also be possible that third-party
components of your software (like Perl modules) can load their
message catalogs, too, without interfering with yours.

The solution is clear, you have to assign a unique name to your message
database, and you have to specify that name at run-time.  That unique
name is the so-called I<textdomain> of your software package.  The name is
actually arbitrary but you should follow these best-practice guidelines
to ensure maximum interoperability:

=over 8

=item File System Safety

In practice, textdomains get mapped into file names, and you should
therefore make sure that the textdomain you choose is a valid filename
on every system that will run your software.

=item Case-sensitivity

Textdomains are always case-sensitive (i. e. 'Package' and 'PACKAGE'
are not the same).  However, since the message catalogs will be stored
on file systems, that may or may not distinguish case when looking
up file names, you should avoid potential conflicts here.

=item Textdomain Should Match CPAN Name

If your software is listed as a module on CPAN, you should simply 
choose the name on CPAN as your textdomain.  The textdomain for 
libintl-perl is hence 'libintl-perl'.  But please replace all 
periods ('.') in your package name with an underscore because ...

=item Internet Domain Names as a Fallback

... if your software is I<not> a module listed on CPAN, as a last 
resort you should use the Java(tm) package scheme, i. e. choose
an internet domain that you are owner of (or ask the owner of an
internet domain) and concatenate your preferred textdomain with the
reversed internet domain.  Example: Your company runs the web-site
'www.foobar.org' and is the owner of the domain 'foobar.org'.  The
textdomain for your company's software 'barfoos' should hence be
'org.foobar.barfoos'.

=back

If your software is likely to be installed in different versions on
the same system, it is probably a good idea to append some version
information to your textdomain.

Other systems are less strict with the naming scheme for textdomains
but the phenomena known as Perl is actually a plethora of small,
specialized modules and it is probably wisest to postulate some
namespace model in order to avoid chaos.

=head2 Binding textdomains to directories

Once the system knows the I<textdomain> of the message that you 
want to get translated into the user's language, it still has to
find the correct message catalog.  By default, libintl-perl will
look up the string in the translation database found in the 
directories F</usr/share/locale> and F</usr/local/share/locale>
(in that order).

It is neither guaranteed that these directories exist on the target
machine, nor can you be sure that the installation routine has write 
access to these locations.  You can therefore instruct libintl-perl 
to search other directories prior to the default directories.  Specifying 
a differnt search directory is called I<binding> a textdomain to a 
directory.

B<Locale::TextDomain> extends the default strategy by a Perl
specific approach.  Unless told otherwise, it will look for a
directory F<LocaleData> in every component found in the standard
include path C<@INC> and check for a database containing the message
for your textdomain there.  Example: If the path 
F</usr/lib/perl/5.8.0/site_perl> is in your C<@INC>, you can
install your translation files in F</usr/lib/perl/5.8.0/site_perl/LocaleData>, 
and they will be found at run-time.

=head1 USAGE

It is crucial to remember that you use Locale::TextDoamin(3) as 
specified in the section L</SYNOPSIS>, that means you have to 
B<use> it, not B<require> it.  The module behaves quite differently 
compared to other modules.

The most significant difference is the meaning of the list passed
as an argument to the use() function.  It actually works like this:

    use Locale::TextDomain (TEXTDOMAIN, DIRECTORY, ...)

The first argument (the first string passed to use()) is the textdomain
of your package, optionally followed by a list of directories to search
I<instead> of the Perl-specific directories (see above: F</LocaleData>
appended to every part of C<@INC>).

If you are the author of a package 'barfoos', you will probably put
the line

    use Locale::TextDomain 'barfoos';

resp. for non-CPAN modules

    use Locale::TextDomain 'org.foobar.barfoos';

in every module of your package that contains translatable strings. If
your module has been installed properly, including the message catalogs,
it will then be able to retrieve these translations at run-time.

If you have not installed the translation database in a directory
F<LocaleData> in the standard include path C<@INC> (or in the system
directories F</usr/share/locale> resp. F</usr/local/share/locale>), you 
have to explicitely specify a search path by giving the names of
directories (as strings!) as additional arguments to use():

    use Locale::TextDomain qw (barfoos ./dir1 ./dir2);

Alternatively you can call the function bindtextdomain() with suitable
arguments (see the entry for bindtextdomain() in 
L<Locale::Messages/FUNCTIONS>).  If you do so, you should pass 
C<undef> as an additional argument in order to avoid unnecessary
lookups:

    use Locale::TextDomain ('barfoos', undef);

You see that the arguments given to use() have nothing to do with
what is imported into your namespace, but they are rather arguments
to textdomain(), resp. bindtextdomain().  Does that mean that 
B<Locale::TextDomain> exports nothing into your namespace? Umh, not
exactly ... in fact it imports I<all> functions listed below into
your namespace, and hence you should not define conflicting functions
(and variables) yourself.

So, why has Locale::TextDomain to be different from other modules?
If you have ever written software in C and prepared it for 
internationalization (i18n), you will probably have defined some
preprocessor macros like:

    #define _(String) dgettext ("my-textdomain", String)
    #define N_(String) String

You only have to define that once in C, and the textdomain for your
package is automatically inserted into all gettext functions.  In 
Perl there is no such mechanism (at least it is not portable,
option -P) and using the gettext functions could become quite
cumbersome without some extra fiddling:

    print dgettext ("my-textdomain", "Hello world!\n");

This is no fun.  In C it would merely be a

    printf (_("Hello world!\n"));

Perl has to be more concise and shorter than C ... see the next
section for how you can use B<Locale::TextDomain> to end up in Perl 
with a mere

    print __"Hello World!\n";

=head1 EXPORTED FUNCTIONS

All functions have quite funny names on purpose.  In fact the 
purpose for that is quite clear: They should be short, operator-like,
and they should not yell for conflicts with existing functions in
I<your> namespace.  You will understand it, when you internationalize
your first Perl program or module.  Preparing it is more like marking
strings as being translatable than inserting function calls.  Here
we go:

=over 4

=item B<__ MSGID>

B<NOTE:> This is a I<double> underscore!

The basic and most-used function.  It is a short-cut for a call
to gettext() resp. dgettext(), and simply returns the translation for
B<MSGID>.  If your old code reads like this:

    print "permission denied";
    
You will now write:

    print __"permission denied";

That's all, the string will be output in the user's preferred language,
provided that you have installed a translation for it.

Of course you can also use parentheses:

    print __("permission denied");

Or even:

    print (__("permission denied"));

In my eyes, the first version without parentheses looks best.

=item B<__x MSGID, ID1 =E<gt> VAL1, ID2 =E<gt> VAL2, ...>

One of the nicest features in Perl is its capability to interpolate
variables into strings:

    print "This is the $color $thing.\n";

This nice feature might con you into thinking that you could now
write

    print __"This is the $color $thing.\n";

Alas, that would be nice, but it is not possible.  Remember that the
function __() serves both as an operator for translating strings 
I<and> as a mark for translatable strings.  If the above string would
get extracted from your Perl code, the un-interpolated form would
end up in the message catalog because when parsing your code it
is unpredictable what values the variables C<$thing> and C<$color>
will have at run-time (this fact is most probably one of the reasons
you have written your program for).

However, at run-time, Perl will have interpolated the values already
I<before> __() (resp. the underlying gettext() function) has seen the
original string.  Consequently something like "This is the red car.\n"
will be looked up in the message catalog, it will not be found (because
only "This is the $color $thing.\n" is included in the database), 
and the original, untranslated string will be returned.
Honestly, because this is almost always an error, the xgettext(1)
program will bail out with a fatal error when it comes across that
string in your code.

There are two workarounds for that:

    printf __"This is the %s %s.\n", $color, $thing;

But that has several disadvantages: Your translator will only
see the isolated string, and without the surrounding code it
is almost impossible to interpret it correctly.  Of course, GNU
emacs and other software capable of editing PO translation files
will allow you to examine the context in the source code, but it
is more likely that your translator will look for a less challenging
translation project when she frequently comes across such messages.

And even if she does understand the underlying programming, what
if she has to reorder the color and the thing like in French:

    msgid "This is the red car.\n";
    msgstr "Cela est la voiture rouge.\n"

Zut alors! No way! You cannot portably reorder the arguments to 
printf() and friends in Perl (it is possible in C, but at the 
time of this writing not supported in Perl, and it would lead to
other problems anyway).

So what? The Perl backend to GNU gettext has defined an alternative
format for interpolatable strings:

    "This is the {color} {thing}.\n";

Instead of Perl variables you use place-holders (legal Perl variables
are also legal place-holders) in curly braces, and then you call

    print __x ("This is the {color} {thing}.\n", 
               thing => $thang,
               color => $color);

The function __x() will take the additional hash and replace all
occurencies of the hash keys in curly braces with the corresponding
values.  Simple, readable, understandable to translators, what else
would you want?  And if the translator forgets, misspells or otherwise
messes up some "variables", the msgfmt(1) program, that is used to
compile the textual translation file into its binary representation
will even choke on these errors and refuse to compile the translation.

=item B<__n MSGID, MSGID_PLURAL, COUNT>

Whew! That looks complicated ... It is best explained with an example.
We'll have another look at your vintage code:

    if ($files_deleted > 1) {
        print "All files have been deleted.\n";
    } else {
        print "One file has been deleted.\n";
    }

Your intent is clear, you wanted to avoid the cumbersome
"1 files deleted".  This is okay for English, but other languages
have more than one plural form.  For example in Russian it makes
a difference whether you want to say 1 file, 3 files or 6 files.
You will use three different forms of the noun 'file' in each
case.  [Note: Yep, very smart you are, the Russian word for 'file'
is in fact the English word, and it is an invariable noun, but if you
know that, you will also understand the rest despite this little
simplification ...].

That is the reason for the existance of the function ngettext(),
that __n() is a short-cut for: 

    print __n"One file has been deleted.\n", 
             "All files have been deleted.\n",
             $files_deleted;

Alternatively:

    print __n ("One file has been deleted.\n",
               "All files have been deleted.\n",
               $files_deleted);

The effect is always the same: libintl-perl will find out which
plural form to pick for your user's language, and the output string
will always look okay.

=item B<__nx MSGID, MSGID_PLURAL, COUNT, VAR1 =E<gt> VAL1, VAR2 =E<gt> VAL2, ...>

Bringing it all together:

    print __nx ("One file has been deleted.\n",
                "{count} files have been deleted.\n",
                $num_files,
                count => $num_files);

The function __nx() picks the correct plural form (also for English!)
I<and> it is capable of interpolating variables into strings.

Have a close look at the order of arguments: The first argument is the
string in the singular, the second one is the plural string. The third
one is an integer indicating the number of items.  This third argument
is I<only> used to pick the correct translation.  The optionally 
following arguments make up the hash used for interpolation.  In the
beginning it is often a little confusing that the variable holding the
number of items will usually be repeated somewhere in the interpolation
hash.

=item B<__xn MSGID, MSGID_PLURAL, COUNT, VAR1 =E<gt> VAL1, VAR2 =E<gt> VAL2, ...>

Does exactly the same thing as __nx().  In fact it is a common typo
promoted to a feature.

=item B<__p MSGCTXT, MSGID>

This is much like __. The "p" stands for "particular", and the MSGCTXT 
is used to provide context to the translator. This may be neccessary
when your string is short, and could stand for multiple things. For example:

    print __p"Verb, to view", "View";
    print __p"Noun, a view", "View";

The above may be "View" entries in a menu, where View->Source and File->View 
are different forms of "View", and likely need to be translated differently.

A typical usage are GUI programs.  Imagine a program with a main
menu and the notorious "Open" entry in the "File" menu.  Now imagine,
there is another menu entry Preferences->Advanced->Policy where you have 
a choice between the alternatives "Open" and "Closed".  In English, "Open"
is the adequate text at both places.  In other languages, it is very
likely that you need two different translations.  Therefore, you would
now write:

    __p"File|", "Open";
    __p"Preferences|Advanced|Policy", "Open";

In English, or if no translation can be found, the second argument
(MSGID) is returned.

This function was introduced in libintl-perl 1.17.

=item B<__px MSGCTXT, MSGID, VAR1 =E<gt> VAL1, VAR2 =E<gt> VAL2, ...>

Like __p(), but supports variable substitution in the string, like __x().

    print __px("Verb, to view", "View {file}", file => $filename);

See __p() and __x() for more details.

This function was introduced in libintl-perl 1.17.

=item B<__np MSGCTXT, MSGID, MSGID_PLURAL, COUNT>

This adds context to plural calls. It should not be needed very often,
if at all, due to the __nx() function. The type of variable substitution
used in other gettext libraries (using sprintf-like sybols, like %s or %1)
sometimes required context. For a (bad) example of this:

    printf (__np("[count] files have been deleted",
                "One file has been deleted.\n",
                "%s files have been deleted.\n",
                $num_files),
            $num_files);

NOTE: The above usage is discouraged. Just use the __nx() call, which 
provides inline context via the key names.

This function was introduced in libintl-perl 1.17.

=item B<__npx MSGCTXT, MSGID, MSGID_PLURAL, COUNT, VAR1 =E<gt> VAL1, VAR2 =E<gt> VAL2, ...>

This is provided for comleteness. It adds the variable interpolation
into the string to the previous method, __np().

It's usage would be like so:

    print __nx ("Files being permenantly removed",
                "One file has been deleted.\n",
                "{count} files have been deleted.\n",
                $num_files,
                count => $num_files);

I cannot think of any situations requiring this, but we can easily 
support it, so here it is.

This function was introduced in libintl-perl 1.17.

=item B<N__ (ARG1, ARG2, ...)>

A no-op function that simply echoes its arguments to the caller.  Take
the following piece of Perl:

    my @options = (
        "Open",
        "Save",
        "Save As",
    );

    ...

    my $option = $options[1];

Now say that you want to have this translatable.  You could sometimes
simply do:

    my @options = (
        __"Open",
        __"Save",
        __"Save As",
    );

    ...

    my $option = $options[1];

But often times this will not be what you want, for example when you
also need the unmodified original string.  Sometimes it may not even
work, for example, when the preferred user language is not yet
determined at the time that the list is initialized.

In these cases you would write:

    my @options = (
        N__"Open",
        N__"Save",
        N__"Save As",
    );

    ...

    my $option = __($options[1]);
    # or: my $option = dgettext ('my-domain', $options[1]);

Now all the strings in C<@options> will be left alone, since N__()
returns its arguments (one ore more) unmodified.  Nevertheless, the
string extractor will be able to recognize the strings as being 
translatable.  And you can still get the translation later by passing
the variable instead of the string to one of the above translation
functions.

=item B<N__n (MSGID, MSGID_PLURAL, COUNT)>

Does exactly the same as N__().  You will use this form if you have 
to mark the strings as having plural forms.

=item B<N__p (MSGCTXT, MSGID)>

Marks B<MSGID> as N__() does, but in the context B<MSGCTXT>.

=item B<N__np (MSGCTXT, MSGID, MSGID_PLURAL, COUNT)>

Marks B<MSGID> as N__n() does, but in the context B<MSGCTXT>.
=back

=head1 EXPORTED VARIABLES

The module exports several variables into your namespace:

=over 4

=item B<%__>

A tied hash.  Its keys are your original messages, the values are
their translations:

    my $title = "<h1>$__{'My Homepage'}</h1>";

This is much better for your translation team than

    my $title = __"<h1>My Homepage</h1>";

In the second case the HTML code will make it into the translation
database and your translators have to be aware of HTML syntax when
translating strings.

B<Warning:> Do I<not> use this hash outside of double-quoted strings!
The code in the tied hash object relies on the correct working of
the function caller() (see "perldoc -f caller"), and this function
will report incorrect results if the tied hash value is the argument
to a function from another package, for example:

  my $result = Other::Package::do_it ($__{'Some string'});

The tied hash code will see "Other::Package" as the calling package,
instead of your own package.  Consequently it will look up the message
in the wrong text domain.  There is no workaround for this bug.
Therefore:

Never use the tied hash interpolated strings!

=item B<$__>

A reference to C<%__>, in case you prefer:

     my $title = "<h1>$__->{'My Homepage'}</h1>";

=back

=head1 PERFORMANCE

Message translation can be a time-consuming task.  Take this little
example:

    1: use Locale::TextDomain ('my-domain');
    2: use POSIX (:locale_h);
    3: 
    4: setlocale (LC_ALL, '');
    5: print __"Hello world!\n";

This will usually be quite fast, but in pathological cases it may
run for several seconds.  A worst-case scenario would be a
Chinese user at a terminal that understands the codeset Big5-HKSCS.
Your translator for Chinese has however chosen to encode the translations
in the codeset EUC-TW.

What will happen at run-time?  First, the library will search and load a
(maybe large) message catalog for your textdomain 'my-domain'.  Then
it will look up the translation for "Hello world!\n", it will find that
it is encoded in EUC-TW.  Since that differs from the output codeset
Big5-HKSCS, it will first load a conversion table containing several
ten-thousands of codepoints for EUC-TW, then it does the same with
the smaller, but still very large conversion table for Big5-HKSCS,
it will convert the translation on the fly from EUC-TW into Big5-HKSCS,
and finally it will return the converted translation.

A worst-case scenario but realistic.  And for these five lines of codes,
there is not much you can do to make it any faster.  You should understand,
however, I<when> the different steps will take place, so that you can
arrange your code for it.

You have learned in the section L</DESCRIPTION> that line 1 is 
responsible for locating your message database.  However, the
use() will do nothing more than remembering your settings.  It will
not search any directories, it will not load any catalogs or 
conversion tables.

Somewhere in your code you will always have a call to 
POSIX::setlocale(), and the performance of this call may be time-consuming,
depending on the architecture of your system.  On some systems, this
will consume very little time, on others it will only consume a
considerable amount of time for the first call, and on others it may
always be time-consuming.  Since you cannot know, how setlocale() is
implemented on the target system, you should reduce the calls to
setlocale() to a minimum.

Line 5 requests the translation for your string.  Only now, the library
will actually load the message catalog, and only now will it load
eventually needed conversion tables.  And from now on, all this information
will be cached in memory.  This strategy is used throughout libintl-perl,
and you may describe it as 'load-on-first-access'.  Getting the next
translation will consume very little resources.

However, although the translation retrieval is somewhat obfuscated
by an operator-like function call, it is still a function call, and in
fact it even involves a chain of function calls.  Consequently, the
following example is probably bad practice:

    foreach (1 .. 100_000) {
        print __"Hello world!\n";
    }

This example introduces a lot of overhead into your program.  Better
do this:

    my $string = __"Hello world!\n";
    foreach (1 .. 100_000) {
        print $string;
    }

The translation will never change, there is no need to retrieve it
over and over again.  Although libintl-perl will of course cache
the translation read from the file system, you can still avoid the
overhead for the function calls.

=head1 AUTHOR

Copyright (C) 2002-2009, Guido Flohr E<lt>guido@imperia.netE<gt>, all
rights reserved.  See the source code for details.

This software is contributed to the Perl community by Imperia 
(L<http://www.imperia.net/>).

=head1 SEE ALSO

Locale::Messages(3pm), Locale::gettext_pp(3pm), perl(1),
gettext(1), gettext(3)

=cut
Local Variables:
mode: perl
perl-indent-level: 4
perl-continued-statement-offset: 4
perl-continued-brace-offset: 0
perl-brace-offset: -4
perl-brace-imaginary-offset: 0
perl-label-offset: -4
cperl-indent-level: 4
cperl-continued-statement-offset: 2
tab-width: 4
End:
=cut
