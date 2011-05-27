BEGIN
{
	push(@INC, 'e:\projects\alim\alim4 to aml\scripts\perllib');
}

use strict;
use Alim4Data;
use GenUtils;
use Text::Wrap;

# create functions that will automatically create XML tags
GenUtils::CreateTags('aml', 'index:subjectindex');

sub GenerateContentXML
{
	my ($alim4db, $elem, $level, $elemOptions) = @_;
	my $xml = '';
	
	if(ref $elem eq 'Alim4Heading')
	{
		$xml .= GenUtils::CallTag('h'.$elem->level(), 'B', $level, {}, $elem->text());
	}
	elsif(ref $elem eq 'Alim4List')
	{
		my $tagName = '';
		if($elem->type() eq 'bullet')
		{
			$tagName = 'ul';
			$xml .= GenUtils::CallTag($tagName, 'S', $level);
		}
		elsif($elem->type() eq 'decimal')
		{
			$tagName = 'ol';
			$xml .= GenUtils::CallTag($tagName, 'S', $level);
		}
		elsif($elem->type() eq 'alpha')
		{
			$tagName = 'ol';
			$xml .= GenUtils::CallTag($tagName, 'S', $level, { 'type'=>'A' });
		}
		else
		{
			print STDERR "unknown list type $elem->type() encountered in GenerateContentXML()\n";
		}
		foreach (@{$_->content()})
		{
			my $sgmlDirty = GenUtils::DirtySGML(\$elem);
			GenUtils::Warn($alim4db->readParams->srcFile(), 0, "$sgmlDirty [$_]") if $sgmlDirty;
			$xml .= GenUtils::CallTag('li', 'B', $level+1, $_);
		}
		$xml .= GenUtils::CallTag($tagName, 'E', $level);
	}
	else
	{
		my $sgmlDirty = GenUtils::DirtySGML(\$elem);
		GenUtils::Warn($alim4db->readParams->srcFile(), 0, "$sgmlDirty [$elem]") if $sgmlDirty;
		if($elemOptions->{addParaTags})
		{
			$xml = GenUtils::CallTag('p', 'B', $level, $elem);
		}
		else
		{
			$xml = "\t"x$level . $elem . "\n";
		}
	}
	
	return $xml;
}

sub GeneratePageXML
{
	my ($alim4db, $elem, $elemOptions, $volume) = @_;
	my $xml = '';
	
	#print "\rExporting " . $elem->topic->topicTitle() . ' 'x20;
	
	my $pageAttrs = {};
	$pageAttrs->{num}=$elem->num() if $elemOptions->{addNumAttrToElem};
	$pageAttrs->{id}=$elem->id() if $elemOptions->{addIdAttrToElem};
	$pageAttrs->{$elemOptions->{titleAttrName}}=$elem->title() if $elemOptions->{addTitleAttrToElem};
	$pageAttrs->{vol}=$volume->volNum() if defined $volume and ref $volume eq 'Alim4Volume';
	
	$xml .= GenUtils::CallTag($elemOptions->{pageTag}, 'S', 1, $pageAttrs);
	
	if($elem->topic()->attrs('hadithNarrator'))
	{
		$xml .= GenUtils::CallTag('narration', 'T', 2, {'by'=>$elem->topic->attrs('hadithNarrator')});
	}
	
	foreach (@{$elem->content()})
	{
		if(ref $_ eq 'Alim4Section')
		{
			$xml .= GenUtils::CallTag('section', 'S', 2, {'heading'=>$_->heading()});
			foreach (@{$_->content()})
			{
				$xml .= GenerateContentXML($alim4db, $_, 3, $elemOptions);
			}
			$xml .= GenUtils::CallTag('section', 'E', 2);
		}
		else
		{
			$xml .= GenerateContentXML($alim4db, $_, 2, $elemOptions);
		}
		
	}

	my $subjectTags = '';
	if(exists $elemOptions->{subjectIndex})
	{
		my $subjectIndex = $elemOptions->{subjectIndex};
		my $fixupCaps = exists $elemOptions->{fixupSubjectCapitalization} && $elemOptions->{fixupSubjectCapitalization};
		my $key = $elemOptions->{subjectIndexKeyPrefix} . (defined $volume and ref $volume eq 'Alim4Volume' ? ($volume->volNum() . '.' . $elem->num()) : $elem->num());
		if($subjectIndex->other($key))
		{
			my $subjectsList = $subjectIndex->other($key);						
			grep
			{
				my $subject = $_;
				my $subtopicsList = $subjectIndex->other($key)->{$subject};

				$subject =~ s/^\s+//;
				$subject =~ s/s+$//;
				$subject = GenUtils::TitleCase($subject) if $fixupCaps;
				
				grep
				{
					# trim leading, trailing whitespace
					s/^\s+//;
					s/\s+$//;
				
					if(lc($subject) eq lc($_))
					{
						$subjectTags .= subjectindex_empty(2, {'topic'=>$subject});
					}
					else
					{
						$subjectTags .= subjectindex_empty(2, {'topic'=>$subject, 'subtopic' => $_});
					}
				} @{$subtopicsList};
			} sort keys %{$subjectsList};			
		}
	}		
	$xml .= $subjectTags . GenUtils::CallTag($elemOptions->{pageTag}, 'E', 1);
	return $xml;
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
	my ($inFileName, $outFileName, $elemOptions) = @_;
	
	$elemOptions->{rootTag} = 'articles' if ! exists $elemOptions->{rootTag};
	$elemOptions->{pageTag} = 'article' if ! exists $elemOptions->{pageTag};
	$elemOptions->{addNumAttrToElem} = 0 if ! exists $elemOptions->{addNumAttrToElem};
	$elemOptions->{addIdAttrToElem} = 0 if ! exists $elemOptions->{addIdAttrToElem};
	$elemOptions->{addTitleAttrToElem} = 1 if ! exists $elemOptions->{addTitleAttrToElem};
	$elemOptions->{titleAttrName} = 'title' if ! exists $elemOptions->{titleAttrName};
	$elemOptions->{hasVolumeStructure} = 0 if ! exists $elemOptions->{hasVolumeStructure};
	$elemOptions->{skipBlankLines} = 0 if ! exists $elemOptions->{skipBlankLines};
	$elemOptions->{textWrapColumns} = 90 if ! exists $elemOptions->{textWrapColumns};
	$elemOptions->{subjectIndex} = ReadSubjects($elemOptions->{subjectIndexFile}) if exists $elemOptions->{subjectIndexFile};
	$elemOptions->{addParaTags} = 1 if ! exists $elemOptions->{addParaTags};
	
	my $alim4ReadParams = new Alim4ReadParams;
	$alim4ReadParams->AssignDefaults();
	$alim4ReadParams->createElements(1);
	$alim4ReadParams->hasVolumeStructure(1) if $elemOptions->{hasVolumeStructure};
	$alim4ReadParams->skipBlankLines(1) if $elemOptions->{skipBlankLines};
	$alim4ReadParams->srcFile($inFileName);

	my $alim4db = new Alim4File;
	my $returnVal = $alim4db->ReadFile($alim4ReadParams);
	if(ref $returnVal)
	{
		$alim4db->PrintWarnings();
		my $rootElem = $alim4db->elements();
		
		$Text::Wrap::columns = $elemOptions->{textWrapColumns};
		open(OUTFILE, ">$outFileName") || die "Can't create '$outFileName': $!\n";
		print OUTFILE "<?xml version=\"1.0\"?>\n";
		print OUTFILE aml_start(0);
	
		print OUTFILE GenUtils::CallTag($elemOptions->{rootTag}, 'S', 0, {'title' => $alim4db->attrs('title'), 'shortname' => $alim4db->attrs('shortname'), 'id' => $alim4db->attrs('bookid')});
		my $elem = undef;
		foreach $elem (@{$rootElem})
		{
			if(ref $elem eq 'Alim4Volume')
			{
				foreach (@{$elem->content()})
				{
					print OUTFILE GeneratePageXML($alim4db, $_, $elemOptions, $elem);
				}
			}
			else
			{
				print OUTFILE GeneratePageXML($alim4db, $elem, $elemOptions);
			}
		}
		print OUTFILE GenUtils::CallTag($elemOptions->{rootTag}, 'E', 0);
		print OUTFILE aml_end(0);
	}
	else
	{
		print $returnVal;
	}
}

sub ConvertHadithAndFiqh
{
	my ($srcDir, $destDir, $defaultExtn) = @_;
	
	$destDir = "$destDir\\Hadith";
	#Convert("$srcDir\\fiqh.txt", "$destDir\\Fiqh-us-Sunnah$defaultExtn", 
	#		{ rootTag=>'fiqa', pageTag=>'fiqh', 
	#			addNumAttrToElem=>1, addTitleAttrToElem=>0,
	#			hasVolumeStructure => 1, skipBlankLines => 1,
	#			subjectIndexKeyPrefix => 'fqS',
	#			subjectIndexFile=>"$srcDir\\fiqhsub.txt"});
	
	#Convert("$srcDir\\bukhari.txt", "$destDir\\Al-Bukhari$defaultExtn", 
	#		{ rootTag=>'ahadith', pageTag=>'hadith', 
	#			addNumAttrToElem=>1, addTitleAttrToElem=>0,
	#			hasVolumeStructure => 1, skipBlankLines => 1,
	#			subjectIndexKeyPrefix => 'htB',
	#			fixupSubjectCapitalization => 1,
	#			subjectIndexFile=>"$srcDir\\bukhsub.txt"});

	#Convert("$srcDir\\qudsi.txt", "$destDir\\Al-Qudsi$defaultExtn", 
	#		{ rootTag=>'ahadith', pageTag=>'hadith', 
	#			addNumAttrToElem=>1, addTitleAttrToElem=>0});

	#Convert("$srcDir\\muslim.txt", "$destDir\\Muslim$defaultExtn", 
	#		{ rootTag=>'ahadith', pageTag=>'hadith', 
	#			addNumAttrToElem=>1, addTitleAttrToElem=>0});

	#Convert("$srcDir\\tirmidhi.txt", "$destDir\\Al-Tirmidhi$defaultExtn", 
	#		{ rootTag=>'ahadith', pageTag=>'hadith', 
	#			addNumAttrToElem=>1, addTitleAttrToElem=>0});

	#Convert("$srcDir\\abudawud.txt", "$destDir\\Abu-Dawood$defaultExtn", 
	#		{ rootTag=>'ahadith', pageTag=>'hadith', 
	#			addNumAttrToElem=>1, addTitleAttrToElem=>0});

	Convert("$srcDir\\muwata.txt", "$destDir\\Al-Muwatta$defaultExtn", 
			{ rootTag=>'ahadith', pageTag=>'hadith', 
				addNumAttrToElem=>1, addTitleAttrToElem=>0,
				hasVolumeStructure => 1, skipBlankLines => 1,
				subjectIndexKeyPrefix => 'hdMuwatta',
				subjectIndexFile=>"$srcDir\\muwindx.txt"});

}

sub ConvertSuraIntros
{
	my ($srcDir, $destDir, $defaultExtn) = @_;
	
	$destDir = "$destDir\\Quran";
	Convert("$srcDir\\maudsinf.txt", "$destDir\\Maududi Sura Introductions$defaultExtn", 
			{ rootTag=>'suraintros', pageTag=>'sura', 
				addNumAttrToElem=>1, addTitleAttrToElem=>0});

	Convert("$srcDir\\malikintro.txt", "$destDir\\Malik Sura Introductions$defaultExtn", 
			{ rootTag=>'suraintros', pageTag=>'sura', 
				addNumAttrToElem=>1, addTitleAttrToElem=>0});
}

sub ConvertRefLibrary
{
	my ($srcDir, $destDir, $defaultExtn) = @_;
	
	$destDir = "$destDir\\Reference Library";
	
	Convert("$srcDir\\misctext.txt", "$destDir\\Miscellaneous Articles$defaultExtn");

	Convert("$srcDir\\biograph.txt", "$destDir\\Biographies of Companions$defaultExtn", 
			{ rootTag=>'biographies', pageTag=>'biography',
				addNumAttrToElem=>0, addTitleAttrToElem=>1,
				titleAttrName=>'name'});

	Convert("$srcDir\\ISLMDICT.txt", "$destDir\\Dictionary of Islamic Terms$defaultExtn", 
			{ rootTag=>'dictionary', pageTag=>'entry', 
				addNumAttrToElem=>0, addTitleAttrToElem=>1,
				titleAttrName=>'name', addParaTags=>0});

	Convert("$srcDir\\CHRONOL.txt", "$destDir\\Chronological History of Islam$defaultExtn", 
			{ rootTag=>'history', pageTag=>'chronology', 
				addNumAttrToElem=>0, addTitleAttrToElem=>1,
				titleAttrName=>'century', addParaTags=>0});
}

my $srcDir = 'e:\projects\alim\alim4 to aml\DataFiles';
my $destDir = 'e:\projects\alim\AML Data';
my $defaultExtn = '.aml';

ConvertHadithAndFiqh($srcDir, $destDir, $defaultExtn);
#ConvertSuraIntros($srcDir, $destDir, $defaultExtn);
#ConvertRefLibrary($srcDir, $destDir, $defaultExtn);

# DON'T CONVERT THESE AGAIN -- They've been modified in the AML
# images.txt
# woislam.txt
