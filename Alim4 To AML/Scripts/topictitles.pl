use strict;
use Alim4Data;
use GenUtils;

my $alim4ReadParams = new Alim4ReadParams;
$alim4ReadParams->AssignDefaults();
$alim4ReadParams->srcFile('..\..\alim4\subject.txt');

my $alim4db = new Alim4File;
my $returnVal = $alim4db->ReadFile($alim4ReadParams);
if(ref $returnVal)
{
	# successfully read the file
	my $topic;
	
	$alim4db->PrintWarnings();
	foreach $topic (@{$alim4db->topicsList()})
	{
		print GenUtils::TitleCase($topic->topicTitle()), "\n";
	}
}
else
{
	print $returnVal;
}
