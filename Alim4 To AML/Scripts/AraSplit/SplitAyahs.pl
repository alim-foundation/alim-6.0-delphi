use Getopt::Long;
use QuranInfo;
use GD;

my $pagesDir = "..\\..\\html\\ara\\pages";
my $ayahsDir = "..\\..\\html\\ara\\ayahs";
my $pageImageWidth = 456;
my $oneLineHeight = 45;
my $minimumLineHeight = 25; # any lines less than this will be an error 
my @potentialErrors = ();

sub DimArea
{
	my ($image, $xs, $ys, $xe, $ye) = @_;
	my ($x, $y) = (0, 0);

	my $dimColor = $image->colorAllocate(235,235,235);
	for ($x = $xs; $x <= $xe; $x++)
	{
		for ($y = $ys; $y < $ye; $y++)
		{
			my $index = $image->getPixel($x, $y);
			my ($r, $g, $b) = $image->rgb($index);
			next if $r == 255 && $g == 255 && ($b == 255 || $b == 0);
			$image->setPixel($x,$y,$dimColor);
		}
	}
}

sub ClearArea
{
	my ($image, $xs, $ys, $xe, $ye) = @_;
	my ($x, $y) = (0, 0);

	my $bgColor = $image->colorClosest(255,255,255); # find white
	for ($x = $xs; $x <= $xe; $x++)
	{
		for ($y = $ys; $y < $ye; $y++)
		{
			my $index = $image->getPixel($x, $y);
			my ($r, $g, $b) = $image->rgb($index);
			next if $r == 255 && $g == 255 && $b == 255;
			$image->setPixel($x,$y,$bgColor);
		}
	}
}

sub DimRightOfLine
{
	my ($image, $x, $y) = @_;	
	DimArea($image, $x, $y, $pageImageWidth, $y+$oneLineHeight);
}

sub DimLeftOfLine
{
	my ($image, $x, $y, $color) = @_;
	ClearArea($image, 0, $y, $x, $y+$oneLineHeight);
}

sub GetAyahNumBox
{
	my ($x, $y) = @_;
	my $x1 = $x < $pageImageWidth ? $x - 17 : $pageImageWidth;
	my $y1 = $y > 0 ? $y - 7 : 0;
	my $x2 = $x < $pageImageWidth ? $x + 17 : $pageImageWidth;
	my $y2 = $y > 0 ? $y + 37 : 0;
	
	return ($x1, $y1, $x2, $y2);
}

sub HandlePage
{
	my ($pgNum, $dataItems, $firstItem, $lastItem) = @_;
	return 1 if $pgNum < 3;
	return 1 if $firstItem->{sura} != 3;

	my $gifFile = sprintf("$pagesDir\\qpage%03d.gif", $pgNum);
	if(! open (GIF, $gifFile))
	{
		print STDERR "unable to open $gifFile: $!\n";
		return;
	}
	binmode GIF;
	my $pageImage = newFromGif GD::Image(GIF);
	my $dimColor = $pageImage->colorAllocate(204,205,204);
	my $blackColor = $pageImage->colorAllocate(200,0, 80);
	print STDERR "unable to call newFromGif for $gifFile: $!\n" if ! $pageImage;
	close GIF;
	
	foreach (@{$dataItems})
	{
		my $data = $_;
		my ($x1s, $y1s, $x2s, $y2s);
		if($data->{ayah} == 1)
		{
			# starting ayahs have the sura name block above them
			($x1s, $y1s, $x2s, $y2s) = GetAyahNumBox($pageImageWidth, $data->{ystart}+115);
		}
		else
		{
			($x1s, $y1s, $x2s, $y2s) = GetAyahNumBox($data->{xstart}, $data->{ystart});
		}		
		my ($x1e, $y1e, $x2e, $y2e) = GetAyahNumBox($data->{xend}, $data->{yend});

		# if the ayah doesn't start on the same line as it "number block", go to the next line
		if($x1s < 50 && $x1s > 0)
		{
			($x1s, $x2s) = ($pageImageWidth, $pageImageWidth);
			$y1s = $y1s+$oneLineHeight;
		}
		
		my ($width, $height) = ($pageImageWidth, $y2e-$y1s);
		if($height < $minimumLineHeight)
		{
			push(@potentialErrors, "Page $pgNum: $data->{sura}.$data->{ayah} (0,0,0,$y1s,$width,$height)");
			next;
		}
		next;
		
		$ayahImage = new GD::Image($width,$height);
		$ayahImage->copy($pageImage,0,0,0,$y1s,$width,$height);
		my $white = $ayahImage->colorClosest(255,255,255); # find white
        $ayahImage->transparent($white);
		
		DimRightOfLine($ayahImage, $x1s, 0) if $y1s > 0;
		DimLeftOfLine($ayahImage, $x1e, $y1e-$y1s);
		
		my $suraDir = sprintf("%s\\S%d", $ayahsDir, $data->{sura});
		my $ayahFile = sprintf("%s\\S%d\\A%d.gif", $ayahsDir, $data->{sura}, $data->{ayah});
		mkdir($suraDir, 0);
		open(AYAHGIF, ">$ayahFile");
		binmode AYAHGIF;
		print AYAHGIF $ayahImage->gif;
		close AYAHGIF;		
		
		print STDOUT "processed page $pgNum: $data->{sura}.$data->{ayah} (0,0,0,$y1s,$width,$height).\n";
	}

	return 1;
}

sub DoConversion
{
	my $arabicMap = new ArabicQuranMap("\\projects\\perllib\\ArabicQuranMap.txt");
	$arabicMap->ForEachPage(\&HandlePage);
	
	open(ERRORLOG, ">ayaherrors.log");
	
	my $potError;
	foreach $potError (@potentialErrors)
	{
		print ERRORLOG "$potError\n";
	}
	
	close(ERRORLOG);
}

DoConversion();