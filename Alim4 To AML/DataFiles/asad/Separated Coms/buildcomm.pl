$outFile = '..\..\asadcomm.txt';
open (FULLCOMFILE, ">$outFile") || die "Couldn't create $outFile: $!\n";

for($sNum = 1; $sNum <= 114; $sNum++)
{
	$fileName = "Surah$sNum" . "com.txt";
	open(COMFILE, $fileName) || die "Couldn't open $fileName: $!\n";
	while(<COMFILE>)
	{
		if(m/\d+\t.*/)
		{
			print FULLCOMFILE $sNum . "_" . $_;
		}
	}
	close(COMFILE);
}

close (FULLCOMFILE);