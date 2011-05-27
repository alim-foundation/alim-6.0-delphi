use strict;

my $inFile = '../../alim4/quranthemes.txt';
my $outFile = '../../idml/quranthemes.idml';
open(THEMES, $inFile) || die "Can't open themese file $inFile: $!\n";
open(IDML, ">$outFile") || die "Can't create idml file $outFile: $!\n";

print IDML "<?xml version=\"1.0\"?>\n";
print IDML "<idml>\n";
print IDML "<quran type=\"themes\" title=\"F. Malik Quran Themes\">\n";

my $lastSura = 0;
my $lineNum = 0;
sub CreateSuraElem
{
	my ($sura) = @_;
	
	if($sura != $lastSura)
	{
		my $expect = $lastSura+1;
		print STDERR "sura count mismatch, expected $expect : ", $_, " at $inFile line $lineNum.\n" if $expect != $sura;
		print IDML "\t</sura>\n" if($lastSura > 0);
		print IDML "\t<sura num=\"$sura\">\n";
		$lastSura = $sura;
	}
}

while(<THEMES>)
{
	chomp;
	$lineNum++;

	next if m/^\s*$/;
	if(m/^[^AO]\s+/)
	{
		print STDERR "What's with starting character in theme? ", $_, " at $inFile line $lineNum.\n";
	}
	if(m!(.*)\t(\d+)\:\[(\d+)\-(\d+)\]! || m!(.*)\t(\d+)\:\[(\d+)\]!)
	{
		my ($theme, $sura, $startAyah, $endAyah) = ($1, $2, $3, $4);
		CreateSuraElem($sura);
		my $attributes = '';
		if(defined $endAyah)
		{
			$attributes = "startAyah=\"$startAyah\" endAyah=\"$endAyah\"";
		}
		else
		{
			$attributes = "ayah=\"$startAyah\"";
		}
		print IDML "\t\t<theme $attributes>\n\t\t\t$theme\n\t\t</theme>\n";
	}
	else
	{
		print STDERR "Can't understand theme: ", $_, " at $inFile line $lineNum.\n";
	}
}
print IDML "\t</sura>\n";
print IDML "</quran>\n";
print IDML "</idml>\n";

close(THEMES);
close(IDML);
