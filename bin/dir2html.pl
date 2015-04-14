#!/usr/bin/perl
use HTML::Entities;
use File::Basename;
use strict;
use File::stat;
use POSIX qw(strftime);
use String::ShellQuote;

my $file_url_prefix = "file://";
if (qx(uname) =~ m/^cygwin/i) {
  $file_url_prefix .= "/";
}

die "Usage: " . basename($0) . " [dir]\n" unless @ARGV <= 1;

my $dir = ".";
$dir = $ARGV[0] if @ARGV;

opendir(my $dir_fd, $dir) or die "Error: can't open $dir\n";
my @dir_list = grep {!/^\.$|^\.\.$/} readdir($dir_fd);

sub debug(@) {
  print STDERR "@_\n";
}

my $header = qx(cat ~/system-config/etc/dir2html.header);

my $dir_url = shell_quote($dir);
chomp (my $up_url = $file_url_prefix . qx(wlp $dir_url/..));
chomp ($dir_url = $file_url_prefix . qx(wlp $dir_url));

$header =~ s/xxx-replace-this-xxx/encode_entities $dir_url/eg;
$header =~ s/uuu-replace-this-uuu/encode_entities $up_url/eg;
print $header;

for my $de (sort @dir_list) {

# <tr class="hidden-object">
#  <td sortable-data="2.Xmodmap"><a class="file" href="file:///D:/cyg/home/bhj/.Xmodmap"><img src="1_files/a_026" alt="文件:">.Xmodmap</a></td>
#  <td sortable-data="80">1 KB</td>
#  <td sortable-data="1320394627000000">11/4/2011</td>
#  <td>4:17:07 PM</td>
# </tr>
  my $hidden = $de =~ m/^\./ ? 'class="hidden-object"' : '';
  my $name_sort = encode_entities $de;
  my $name_print = $name_sort;

  $de = "$dir/$de";
  my $size_sort = -s $de;
  chomp(my $size_pretty = qx(pretty $size_sort));
  my $date_sort = stat($de)->mtime;
  my $date_day = strftime "%m/%d/%Y", localtime $date_sort;
  my $date_hms = strftime "%I:%M:%S %p", localtime $date_sort;
  my $url = shell_quote($de);
  chomp ($url = $file_url_prefix . qx(wlp $url));

  print "<tr $hidden>\n" .
    "<td sortable-data=\"$name_sort\"><a class=\"file\" href=\"$url\">$name_print</a></td>\n" .
      "<td sortable-data=\"$size_sort\">$size_pretty</td>\n" .
        "<td sortable-data=\"$date_sort\">$date_day</td>\n" .
          "<td>$date_hms</td>\n</tr>";
}

print "</tbody></table>\n</body></html>\n"
