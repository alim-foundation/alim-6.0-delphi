
BEGIN
{
	$main::scriptsDir = 'e:\projects\alim\alim4 to aml\scripts';
	$main::srcDir = $main::scriptsDir . '\Hoda Arabic';
	$main::perlLib = $main::scriptsDir . '\perllib';
	$main::destDir = $main::srcDir;
	
	push(@INC, $perlLib);
}

use strict;
use QuranInfo;

sub ReadHodaArabic
{
	my ($fileName) = @_;
	open(INPUTFILE, $fileName) || die "Can't open $fileName: $!\n";

	my $statistics =
		{
			blankLines => 0,
			bismillahLines => 0,
			mergedLines => 0,
			ayahLines => 0,
		};
	my $suraAyahs = {};
	my $lineNum = 0;
	my $curAyahKey = '';

	while(<INPUTFILE>)
	{
		$lineNum++;

		if(m/^\s*$/)
		{
			$statistics->{blankLines}++;
			next;
		}

		# trim leading, trailing whitespace
		s/^\s*//g;
		s/\s*$//g;

		if(m/(.*)B\s*(\d+)\s*-\s*(\d+)\s*/)
		{
			my $sura = $2;
			my $ayah = $3;
			my $text = $1;

			# trim leading, trailing whitespace from text
			$text =~ s/^\s*//g;
			$text =~ s/\s*$//g;

			if($ayah == 0)
			{
				$statistics->{bismillahLines}++;
				next;
			}

			$curAyahKey = "$sura.$ayah";
			if(exists $suraAyahs->{$curAyahKey})
			{
				print STDERR "Key $curAyahKey shouldn't exist, but it does. Skipping text with error at $fileName line $lineNum.\n";
				next;
			}

			$suraAyahs->{$curAyahKey} = $text;		
			$statistics->{ayahLines}++;
		}
		else
		{
			if(! exists $suraAyahs->{$curAyahKey})
			{
				print STDERR "Key $curAyahKey should exist, but it doesn't. Skipping text with error at $fileName line $lineNum.\n";
				next;
			}
			
			$statistics->{mergedLines}++;
			$suraAyahs->{$curAyahKey} = $_ . ' ' . $suraAyahs->{$curAyahKey};
		}
	}

	close(INPUTFILE);
	
	return ($suraAyahs, $statistics);
}

sub ProcessSuras
{
	my ($suraAyahs) = @_;
	
	my $xmlFileName = $main::destDir . '\Arabic.aml';
	open(IDML, ">$xmlFileName") || die "Unable to create $xmlFileName: $!\n";
	print IDML "<?xml version=\"1.0\"?>\n";
	print IDML "<aml>\n";
	print IDML "<quran type=\"arabic\" title=\"Al-Qur'an in Arabic\" shortname=\"Arabic Quran\" id=\"QAT\">\n";

	my $qi = new QuranInfo('e:\projects\alim\alim4 to aml\scripts\perllib\QURANDTA.INI');	
	my $suraInfo = undef;
	foreach $suraInfo (@{$qi->{_suraInfo}})
	{
		print IDML "\t<sura num=\"$suraInfo->{suraNum}\">\n";
		my $ayahNum = 0;
		for($ayahNum=1; $ayahNum <= $suraInfo->{ayatCount}; $ayahNum++)
		{
			my $key = "$suraInfo->{suraNum}.$ayahNum";
			if(exists $suraAyahs->{$key})
			{
				print IDML "\t\t<ayah num=\"$ayahNum\">$suraAyahs->{$key}</ayah>\n";
			}
			else
			{
				print "Sura $suraInfo->{suraNum} Ayah $ayahNum text not found.\n";
			}
		}
		print IDML "\t</sura>\n";
	}
	
	print IDML "</quran>\n";
	print IDML "</aml>\n";
	close(IDML);
}

sub Main
{
	my ($suraAyahs, $statistics) = ReadHodaArabic($main::srcDir . '\hodaarab.txt');

	print "Conversion completed.\n";
	print "  Ayah Lines: $statistics->{ayahLines}\n";
	print "  Bismillah Lines (skipped lines with ayah number 0): $statistics->{bismillahLines}\n";
	print "  Merged Lines: $statistics->{mergedLines}\n";
	print "  Blanks Lines: $statistics->{blankLines}\n";
	
	ProcessSuras($suraAyahs);
}

Main;