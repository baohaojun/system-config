#!/usr/bin/perl

use Module::Build;
my $class = Module::Build->subclass(
    code => do {local $/ = undef; <DATA>}
);

my $build = $class->new(
    module_name => 'Emacs::PDE',
    license => 'perl',
    add_to_cleanup => ['lisp/*.elc'],
    create_makefile_pl => 'passthrough',
);
$build->add_build_element('elc');
$build->create_build_script;

__DATA__
use File::stat;
use File::Basename;
use ExtUtils::Install;

sub process_elc_files {
    my $self = shift;
    if ( exists $self->{args}{noelc} ) {
        return;
    }
    # Files to ignore byte-compile
    my @ignore = qw(pde-load.el pde-loaddefs.el pde-patch.el);
    my @el = glob("lisp/*.el");
    foreach my $el ( @el ) {
        next if grep { basename($el) eq $_ } @ignore;
        my $elc = $el."c";
        unless ( -e $elc && stat($elc)->mtime > stat($el)->mtime ) {
            my $cmd = qq/emacs -q -batch --no-site-file --eval="(setq load-path (append load-path '(\\".\\" \\"contrib\\")))" -f batch-byte-compile $el/;
            print $cmd, "\n";
            system($cmd);
        }
    }
    unlink("lisp/tools/perldoc-cache.el");
}

sub ACTION_test {
    my ($emacs_version) = `emacs --version`;
    if ( $emacs_version =~ /GNU Emacs/ ) {
        if ( $emacs_version =~ /2[23]/ ) {
            print "Seem everything is Ok\n";
        } else {
            print "Your emacs version is too low, PDE may not work. Consider upgrade it.\n"
        }
    }
    else {
        print "No emacs found in your system.\n";
    }
    return 1;
}

sub ACTION_fakeinstall {
    my $self        = shift;
    $self->SUPER::ACTION_install(@_);
    my $home = $ENV{HOME};
    my $elispdir = $self->{args}{elispdir} || "$home/.emacs.d/pde";
    ExtUtils::Install::install( {lisp => $elispdir}, !$self->quiet, 1,
                                $self->{args}{uninst} || 0 );
}

sub ACTION_install {
    my $self        = shift;
    $self->SUPER::ACTION_install(@_);
    my $home = $ENV{HOME};
    my $elispdir = $self->{args}{elispdir} || "$home/.emacs.d/pde";
    ExtUtils::Install::install( {lisp => $elispdir}, !$self->quiet, 0,
                                $self->{args}{uninst} || 0 );
}

sub ACTION_info {
    my $self = shift;
    my $texi = "lisp/doc/pde.texi";
    my $info = "lisp/doc/pde.info";
    if ( !-e $info || stat($texi)->mtime > stat($info)->mtime ) {
        print qq/makeinfo --fill-column=70 $texi\n/;
        system("makeinfo", "--fill-column=70", "-o", $info, $texi);
    }
    else {
        print "$info is already updated to date\n";
    }
    return;
}

sub ACTION_disthtml {
    my $self = shift;
    $self->depends_on('muse');
    my $html = 'lisp/doc/pde.html';
    my $htmldir = 'lisp/doc/pde'; 
    my $texi = "lisp/doc/pde.texi";
    if ( !-e $html || stat($texi)->mtime > stat($html)->mtime ) {
        print qq/makeinfo --no-split --html -o $html $texi\n/;
        system("makeinfo", "--no-split", "--html", "-o", $html, $texi);
        print qq/makeinfo --html -o $htmldir $texi\n/;
        system("makeinfo", "--html", "-o", $htmldir, $texi);
    }
    else {
        print "html documents is updated to date.\n";
    }
    return;
}

sub ACTION_muse {
    my $self = shift;
    my @muse = ('misc/QuickStart.muse', 'misc/QuickStartEn.muse');
    my @otherlib = (
        '/home/ywb/.emacs.d/site-lisp/mycontrib/dot-emacs-helper.el',
        "/home/ywb/.emacs.d/config/muse-init.el",
        "/home/ywb/proj/darcs/pde/misc/muse-myhtml.el"
    );
    foreach my $muse ( @muse ) {
        (my $html = 'lisp/doc/'.basename($muse)) =~ s/\.muse/.html/;
        if ( !-e $html || stat($muse)->mtime > stat($muse)->mtime ) {
            system("emacs", "-q", "-batch",
                   (map { +"-l" => $_ } @otherlib),
                   "-f", "muse-project-batch-publish", "PDE");
        }
        else {
            print "$html is updated to date.\n";
        }
    }
    return;
}

sub ACTION_dist {
    my $self = shift;
    $self->depends_on('info');
    $self->depends_on('disthtml');
    return $self->SUPER::ACTION_dist(@_);
}
