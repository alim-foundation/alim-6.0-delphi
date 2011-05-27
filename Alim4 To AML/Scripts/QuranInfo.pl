use strict;
use QuranInfo;

my $qi = new QuranInfo('d:\projects\PerlLib\QURANDTA.INI');

open(OUTFILE, '>..\..\idml\QuranStructure.idml');
print OUTFILE "<?xml version=\"1.0\"?>\n";
print OUTFILE "<idml>\n";
	
print OUTFILE "<quran type=\"structure\">\n";
grep
{
	my $suraInfo = $_;
	
	print OUTFILE "\t<sura num=\"$suraInfo->{suraNum}\" ayahcount=\"$suraInfo->{ayatCount}\">\n";
	print OUTFILE "\t\t<name language=\"literal\">$suraInfo->{suraName}</name>\n";
	print OUTFILE "\t\t<revealed num=\"$suraInfo->{chronoOrder}\" city=\"$suraInfo->{revealedIn}\"/>\n";
	if($suraInfo->{rukuCount} > 1)
	{
		print OUTFILE "\t\t<rukus>\n";
		grep
		{
			print OUTFILE "\t\t\t<ruku num=\"$_->{rukuNum}\" startayah=\"$_->{startAyah}\" endayah=\"$_->{endAyah}\"/>\n";
			
		} @{$suraInfo->{rukuInfo}};
		print OUTFILE "\t\t</rukus>\n";
	}
	print OUTFILE "\t</sura>\n";
	
} @{$qi->{_suraInfo}};

print OUTFILE "</quran>\n";
print OUTFILE "</idml>\n";
close(OUTFILE);
