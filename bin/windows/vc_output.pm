sub vc_output($)
{
    #print "In handle VC output\n" if ($debug_mode);

# #sample output from vc8, c++ files
# #1>.\findexec.cpp(31) : fatal error C1083: Cannot open include file: 'bhjdebug.h': No such file or directory

# #sample output from vc9, c# files
# #D:\profiles\Program.cs(15,29): error CS0029: Cannot implicitly convert type 'System.Diagnostics.Process[]' to 'System.Diagnostics.Process'

# how to handle this line?
#        D:\bhj\STLport-5.2.1\stlport\stl/_collate.h(39) : see declaration of 'collate'
    (my $vc_out) = @_;
    our %err_file_canonic;

    while (<$vc_out>) {
        next if (m!\(\d+\): Could not find the file!);
        s!^[0-9]+>!!; #get rid of `1>'
        s!^\s+(.*?): see declaration!$1: error C2371: see declaration!;
        s!^\s+(.*?): see refer!$1: error C2371: see refer!;
        s!\):!) :!g;
        s!\(([0-9]+),[0-9]+\)!($1)!;
        s!error (?:CS|RC)([0-9]+) ?:!error C$1:!;
        #------ Build started: Project: AlibImpl, Configuration: Release Win32 ------
        if (m!^-+Configuration: (.*?) ! or m!^------ Build started: Project: (.*?), !) {
            chdir($sln_dir);
            my $proj_dir = getProjDir($1);
            chomp($proj_dir = `cygpath -au "$proj_dir"`);
            $proj_dir or die "cygpath failed";
            die "proj_dir is empty!" if (not $proj_dir);
            my @proj_dir = canonic_files(".", $proj_dir);
            die "found more than 1 proj_dir" if (scalar @proj_dir != 1);
            chdir $proj_dir[0];
        }
        if (m/^(\s*)(.*?)(\([0-9]+(?:,[0-9]+)?\).*:.*)/) {
            
            chomp(my $key = `cygpath -au "$2"`);
            $key or die "cygpath failed";
            $key = uc($key);
            my $err_file;
            if ($err_file_canonic{$key}) {
                $err_file = $err_file_canonic{$key};
            } else {
                chomp($err_file = `cygpath -au "$2"`);
                $err_file or die "cygpath failed";
                my @err_file = canonic_files(basename($err_file), dirname($err_file));
                die "File `$2' doesn't exist!" if (scalar @err_file != 1);
                
                $err_file = $err_file[0];
                $err_file_canonic{$key} = $err_file;
            }            

            print $1, $err_file, $3, "\n";
        } else {
            print $_;
        }
    }

}

1;
