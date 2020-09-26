#!/usr/bin/perl
# import files from https://www.masswerk.at/vcs-tools/TinyPlayfieldEditor/ and output in separate arrays for each
# pfdata[lr]pf[012]
# Format of that data is:
# DataPF0:
#        byte pf0l[0], pf0r[0], pf0l[1], pf0r[1],... so need to split in separate arrays

@pf0l = ();
@pf1l = ();
@pf2l = ();
@pf0r = ();
@pf1r = ();
@pf2r = ();

sub importTinyPF {
  my $pf;
  while ($line = <>) {
    if ($line =~ /^DataPF([012])/) {
      $pf = $1;
    }
    if ($line =~ /^	\.byte (\%[01]{8}),(\%[01]{8}),(\%[01]{8}),(\%[01]{8}),(\%[01]{8}),(\%[01]{8}),(\%[01]{8}),(\%[01]{8})/) {
#      print "$line\n";
      push @{"pf" . $pf . "l"}, ($1, $3, $5, $7);
      push @{"pf" . $pf . "r"}, ($2, $4, $6, $8);
    }
  }
}

sub printBytes {
  while (@_) {
    printf "	.byte %s\n", pop;
  }
}

&importTinyPF();

print "\n	org \$f800\n";
print "pf0l\n";
&printBytes(@pf0l);
print "\n	org \$f900\n";
print "pf1l\n";
&printBytes(@pf1l);
print "\n	org \$fa00\n";
print "pf2l\n";
&printBytes(@pf2l);

print "\n	org \$fb00\n";
print "pf0r\n";
&printBytes(@pf0r);
print "\n	org \$fc00\n";
print "pf1r\n";
&printBytes(@pf1r);
print "\n	org \$fd00\n";
print "pf2r\n";
&printBytes(@pf2r);
