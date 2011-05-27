BEGIN
{
	$main::PerlLib = 'e:\projects\alim\alim4 to aml\scripts\perllib';
	push(@INC, $main::PerlLib);
}

use strict;
use Alim4Data;
use QuranInfo;
use GenUtils;
use Text::Wrap;

#
# TODO: yusuf ali has see n. xxxx, nn. xxxx-xx, see xvx 4, etc that need to be hyperlinked
# TODO: index entries need to be SMGL clean
#

GenUtils::CreateTags('aml', 'quran', 'sura', 'ayah', 'note', 'index:subjectindex');

sub ExportQuran
{
	my ($alimDB, $quranInfo, $outputFile, $prefixFNoteRefsWithSuraNum, $removeFNoteRefs, $subjectIndex) = @_;
	my $sura = undef;

	$Text::Wrap::columns = 90;
	open(OUTFILE, ">$outputFile") || die "Can't create '$outputFile': $!\n";
	print OUTFILE "<?xml version=\"1.0\"?>\n";
	print OUTFILE aml_start(0);
	
	print OUTFILE quran_start(0, {'title' => $alimDB->attrs('title'), 'type' => 'translation', 'shortname' => $alimDB->attrs('shortname'), 'id' => $alimDB->attrs('bookid')});
	foreach $sura (@{$alimDB->topicsList()})
	{
		#print "\rExporting " . $sura->topicTitle() . ' 'x20;
		my $suraNum = $sura->topicNum();
		my $prevAyah = 0;
		my $lineNumInFile = $sura->startLine();

		print OUTFILE sura_start(1, {'num' => $suraNum});
		my $textLine = '';
		
		grep
		{
			if(m/^(\d+)\.\s+(.*)/)
			{
				my $ayahNum = $1;
				if($ayahNum != $prevAyah+1)
				{
					print STDERR "WARNING S$suraNum: ayah order mismatch: $_\n";
				}
				my $ayahText = $2;
				
				# following line should be uncommented for Transliteration file
				# $ayahText = lc($ayahText);
				
				#see if there are any footnote problems
				my $lineCopy = $_;
				my @fnProblems = ();
				my $goodMatch = '>\s*\w+\s*</';
				$lineCopy =~ s/<fn(.*?)fn>/push(@fnProblems,$&) if $1 !~ m!$goodMatch!; ""/ge;
				grep
				{
					GenUtils::Warn($alimDB->readParams->srcFile(), $lineNumInFile, 'footnote ' . $_ . ' is invalid');
				} @fnProblems;
				
				# get any footnotes that might be referenced and remove them from text stream
				my @fnRefList = ();
				$ayahText =~ s!<fn>\s*(.*?)\s*</fn>!push(@fnRefList, $prefixFNoteRefsWithSuraNum ? "$suraNum\_$1" : $1); $removeFNoteRefs ? "" : "<fn>$1</fn>"!ge;
				my $fnotesCount = scalar @fnRefList;
				
				my $sgmlDirty = GenUtils::DirtySGML(\$ayahText);
				#GenUtils::Warn($alimDB->readParams->srcFile(), $lineNumInFile, $sgmlDirty) if $sgmlDirty;

				my $subjectTags = '';
				if(defined $subjectIndex)
				{
					my $key = "$suraNum.$ayahNum";
					if($subjectIndex->quran($key))
					{
						my $subjectsList = $subjectIndex->quran($key);						
						grep
						{
							my $subject = $_;
							my $subtopicsList = $subjectIndex->quran($key)->{$subject};
							grep
							{
								if(lc($subject) eq lc($_))
								{
									$subjectTags .= subjectindex_empty(3, {'topic'=>$subject});
								}
								else
								{
									$subjectTags .= subjectindex_empty(3, {'topic'=>$subject, 'subtopic' => $_});
								}
							} @{$subtopicsList};
						} sort keys %{$subjectsList};
					}
				}
				
				if($fnotesCount > 0)
				{
					print OUTFILE ayah_start(2, {'num' => $ayahNum});
					print OUTFILE ayah_content(2, $ayahText);
					
					my $noteRef = 0;
					foreach $noteRef (@fnRefList)
					{
						if($alimDB->footnotes($noteRef))
						{
							my $noteText = $alimDB->footnotes($noteRef);
							$sgmlDirty = GenUtils::DirtySGML(\$noteText);
							GenUtils::Warn($alimDB->readParams->srcFile(), $lineNumInFile, "$sgmlDirty in note '$noteRef'") if $sgmlDirty;
							
							# if we prefixed the note ref temporarily, take it out now
							$noteRef =~ s/^\d+\_// if($prefixFNoteRefsWithSuraNum);
							print OUTFILE note(3, {'id' => $noteRef}, $noteText);
						}
						else
						{
							GenUtils::Warn($alimDB->readParams->srcFile(), $lineNumInFile, "footnote '$noteRef' not found");
						}
					}
					print OUTFILE $subjectTags;
					print OUTFILE ayah_end(2);
				}
				else
				{
					if($subjectTags)
					{
						print OUTFILE ayah_start(2, {'num' => $ayahNum});
						print OUTFILE ayah_content(2, $ayahText);
						print OUTFILE $subjectTags;
						print OUTFILE ayah_end(2);
					}
					else
					{
						print OUTFILE ayah(2, {'num' => $ayahNum}, $ayahText);
					}
				}
				
				$prevAyah = $ayahNum;
			}
			else
			{
				GenUtils::Error($alimDB->readParams->srcFile(), $lineNumInFile, "line format error");
			}
			
			$lineNumInFile++;
		} @{$sura->topicLines()};
		
		print OUTFILE sura_end(1);
	}
	
	print OUTFILE quran_end(0);
	print OUTFILE aml_end(0);
	close(OUTFILE);
}

sub ReadSubjects
{
	my ($fname) = @_;
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
		$subjectIndex = $subjDB->CreateSubjectIndex();
	}
	else
	{
		print $returnVal;
	}

	return $subjectIndex;
}

sub Convert
{
	my ($inFileName, $subjectsFile, $outFileName, $prefixFNoteRefsWithSuraNum, $removeFNoteRefs) = @_;
	
	my $subjects = undef;
	if($subjectsFile)
	{
		$subjects = ReadSubjects($subjectsFile);
	}
	
	my $alim4ReadParams = new Alim4ReadParams;
	$alim4ReadParams->AssignDefaults();
	$alim4ReadParams->createElements(0);
	$alim4ReadParams->srcFile($inFileName);

	my $alim4db = new Alim4File;
	my $returnVal = $alim4db->ReadFile($alim4ReadParams);
	if(ref $returnVal)
	{
		$alim4db->PrintWarnings();
		ExportQuran($alim4db, 
					new QuranInfo($main::PerlLib . '\QURANDTA.INI'), 
					$outFileName, 
					defined $prefixFNoteRefsWithSuraNum ? $prefixFNoteRefsWithSuraNum : 0,
					defined $removeFNoteRefs ? $removeFNoteRefs : 0,
					defined $subjects ? $subjects : undef)
	}
	else
	{
		print $returnVal;
	}
}

my $srcDir = 'e:\projects\alim\alim4 to aml\DataFiles';
my $destDir = 'e:\projects\alim\AML Data\Quran';

#Convert("$srcDir\\yalitran.txt", "$srcDir\\yalisubj.txt", "$destDir\\Yusuf Ali Translation.aml");

# TODO: pickthall missing ayah 66 of sura 19, I replaced it with Yusuf Ali's translation instead
#Convert("$srcDir\\pickthal.txt", "", "$destDir\\Pickthall Translation.aml");

#Convert("$srcDir\\qurantlt.txt", "", "$destDir\\Roman Transliteration.aml");
Convert("$srcDir\\asad.txt", "", "$destDir\\Asad Translation.aml", 1);
#Convert("$srcDir\\malik.txt", "", "$destDir\\Malik Translation.aml");
