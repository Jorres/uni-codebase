use strict;
use warnings;

sub uniq {
  my %seen;
  return grep { !$seen{$_}++ } @_;
}

my $fl = "";

# open (FILE, "input.txt");

my @links = ();

# while (my $line = <FILE>)  {
while (my $line = <STDIN>)  { 
    $fl .= $line;
}

while (1) {
    if ($fl =~ m/<a(.*?)href="(.*?:[\/]{2})?(.*?)"(.*?)>/mg) {
        my $protocol = $2;
        my $hostResource = $3;
        my $regexStrip = qr/^(\w+)([-\w\.]*)(.*)$/;
        if ($hostResource =~ m/$regexStrip/g) {
            $hostResource =~ s/$regexStrip/$1$2/g;
            push(@links, $hostResource);
        }
    } else {
        last;
    }
}

@links = uniq @links;
@links = sort { lc($a) cmp lc($b) } @links;

print join("\n", @links);
