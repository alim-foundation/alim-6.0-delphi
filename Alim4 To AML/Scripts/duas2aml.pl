open(DUAS, "e:\\jbsduas.txt") || die "aargh!: $!\n";

my ($category, $insideCatg, $dua) = (undef, 0, undef);
while(<DUAS>)
{
	chomp;
	($category, $dua, $ayahs) = split(/\t/);
	
	if($category)
	{
		print "</article>\n" if $insideCatg;
		print "<article title=\"$category\">\n";
		$insideCatg = 1;
	}
	print "	<section heading=\"$dua\">\n";
	my @ayahs = split(/,/, $ayahs);
	foreach (@ayahs)
	{
		my ($sura, $ayah) = split(/\./);
		print "		<ayah sura=\"$sura\" num=\"$ayah\"/>\n";
	}
	print "	</section>\n";
}
print "</article>\n" if $insideCatg;

close(DUAS);