use strict;
use Alim4Data;
use QuranInfo;
use GenUtils;
use Text::Wrap;

GenUtils::CreateTags('idml', 'subjectindex', 'topic', 'subtopic', 'ayah', 'see', 'link');

sub GetSeeAlsoLinks
{
	my ($data) = @_;
	
	if($data =~ m/^.*\(Also see\)(.*)/) 
	{
		my $refs = $1;
		my $links = Alim4File::CaptureLinks(\$refs, 0);
		my $tags = '';
		grep
		{
			my $link = $_;
			$tags .= see_empty(2, {'topic' => $link->{linkText}});
		} @{$links};
		return $tags;
	}
	else
	{
		return '';
	}
}

sub ExportSubjectIndex
{
	my ($self, $outputFile, $fixupTitles) = @_;
	open(OUTFILE, ">$outputFile") || die "Can't create '$outputFile': $!\n";
	print OUTFILE "<?xml version=\"1.0\"?>\n";
	print OUTFILE idml_start(0);
	
	print OUTFILE subjectindex_start(0, {'title' => $self->attrs('title'), 'shortname' => $self->attrs('shortname'), 'id' => $self->attrs('bookid')});

	my $topic = undef;	
	foreach $topic (@{$self->topicsList()})
	{
		my $topicName = $fixupTitles ? GenUtils::TitleCase($topic->topicTitle()) : $topic->topicTitle();
		print OUTFILE topic_start(1, {'name' => $topicName});
		my $lineNumInFile = $topic->startLine();
		grep
		{
			if(m/^(\w+)\.\s+([^\|]*)\|(.*)/)
			{
				my $idxLine = $1;
				my $subtopic = $2;
				my $refs = $3;
				my $links = Alim4File::CaptureLinks(\$refs, 0);
				my $seeAlsoLinks = GetSeeAlsoLinks($subtopic);
				
				my $refLevel = 2;
				my $isSubtopic = (lc($subtopic) ne lc($topic->topicTitle()) && $seeAlsoLinks eq '');
				if($isSubtopic)
				{
					if($subtopic =~ m/{/)
					{
						GenUtils::Warn($self->readParams->srcFile(), $lineNumInFile, "subtopic name has link: $_");
					}
										
					$subtopic =~ s/(^\s+|\s+$)//g;
					$subtopic =~ s/(^"|"$)//g;
					print OUTFILE subtopic_start(2, {'name' => $subtopic});
					$refLevel = 3;
				}
				if($seeAlsoLinks)
				{
					print OUTFILE $seeAlsoLinks;
				}
				if((uc($subtopic) eq $subtopic) && (lc($subtopic) ne lc($topic->topicTitle())))
				{
					GenUtils::Warn($self->readParams->srcFile(), $lineNumInFile, "subtopic capitalized: $_");
				}
				
				grep
				{
					my $link = $_;
					if($link->{linkType} eq 'quranAyah')
					{
						print OUTFILE ayah_empty($refLevel, {'id' => $link->{quranLinkSura} . '.' . $link->{quranLinkAyah}});
					}
					elsif($link->{linkType} eq 'hadith' || $link->{linkType} eq '')
					{
						my $ctx = $link->{linkCtx};
						if($ctx =~ m/htB([^\s]+)/)
						{
							print OUTFILE link_empty($refLevel, {'book' => 'SHB', 'id' => $1});
						}
						elsif($ctx =~ m/fqS([^\s]+)/)
						{
							print OUTFILE link_empty($refLevel, {'book' => 'FQS', 'id' => $1});
						}
						else
						{
							GenUtils::Error($self->readParams->srcFile(), $lineNumInFile, "Invalid Hadith link '$ctx': $subtopic");
						}
					}
					else
					{
						GenUtils::Error($self->readParams->srcFile(), $lineNumInFile, "Link type '$link->{linkType}' not handled: $subtopic");
					}
				} @{$links};
				
				if($isSubtopic)
				{
					print OUTFILE subtopic_end(2);
				}
			}
			else
			{
				my $seeAlsoLinks = GetSeeAlsoLinks($_);
				if($seeAlsoLinks)
				{
					print OUTFILE $seeAlsoLinks;
				}
				else
				{
					GenUtils::Error($self->readParams->srcFile(), $lineNumInFile, "subject format error: $_");
				}
			}
			$lineNumInFile++;
		} @{$topic->topicLines()};
		print OUTFILE topic_end(1);
	}

	print OUTFILE subjectindex_end(0);
	print OUTFILE idml_end(0);
	close(OUTFILE);
}

sub Convert
{
	my ($fname, $outfName, $fixupTitles) = @_;
	my $subjectIndex = undef;
	
	my $subjReadParams = new Alim4ReadParams;
	$subjReadParams->AssignDefaults();
	$subjReadParams->createElements(0);
	$subjReadParams->srcFile($fname);

	my $subjDB = new Alim4File;
	my $returnVal = $subjDB->ReadFile($subjReadParams);
	if(ref $returnVal)
	{
		$subjDB->PrintWarnings();
		$subjectIndex = ExportSubjectIndex($subjDB, $outfName, $fixupTitles);
	}
	else
	{
		print $returnVal;
	}

	return $subjectIndex;
}

my $srcDir = '..\..\alim4';
my $destDir = '..\..\source';

Convert("$srcDir\\subject.txt", "$destDir\\subject.idml", 1);
Convert("$srcDir\\bukhsub.txt", "$destDir\\bukhsub.idml", 1);
Convert("$srcDir\\fiqhsub.txt", "$destDir\\fiqhsub.idml", 1);
