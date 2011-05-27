#-----------------------------------------------
# Physical topic construct (created by ReadFile)
#-----------------------------------------------
package Alim4Topic;
use Class::Struct;
struct 
(
	attrs => '%',
	endLine => '$',
	maxLineLen => '$',
	minLineLen => '$',
	startLine => '$',
	topicCtx => '$',
	topicNum => '$',
	topicTitle => '$',
	topicLines => '@',
);

sub LinesCount
{
	my $self = shift;
	return scalar @{$self->topicLines()};
}

#-------------------------------------------------------
# Logical element constructs (created by CreateElements)
#-------------------------------------------------------
package Alim4Volume;
use Class::Struct;
struct 
(
	volNum => '$',
	content => '@',
);

package Alim4Page;
use Class::Struct;
struct 
(
	id => '$',
	title => '$',
	num => '$',
	topic => '$',
	content => '@',
);

package Alim4Section;
use Class::Struct;
struct 
(
	heading => '$',
	content => '@',
);

package Alim4Heading;
use Class::Struct;
struct 
(
	text => '$',
	level => '$',
);

package Alim4List;
use Class::Struct;
struct 
(
	type => '$',
	level => '$',
	content => '@',
);

#-------------------------------------------------------
# Logical element constructs (created by CreateElements)
#-------------------------------------------------------
package Alim4SubjectIndex;
use Class::Struct;
struct 
(
	quran => '%',  # indexed by sura.ayah
	#hadith => '%',
	subject => '%',
	other => '%',
);

#-----------------------------------------------
# Parameters used by ReadFile
#-----------------------------------------------
package Alim4ReadParams;
use Class::Struct;
struct 
(
	srcDir => '$',
	srcFile => '$',
	showStatusCallback => '$',
	stopAfterTopics => '$',

	createElements => '$',
	warnOnLinkRemove => '$',
	head1IsContainer => '$',
	skipBlankLines => '$',

	# if the data should be structured as a volume, give that info
	hasVolumeStructure => '$',
	volumeTitleMatch => '$',
	
	# if data items contain topic numbers in the title, use them
	topicNumsInTitle => '$',
	topicNumTitleMatch => '$',
);

sub AssignDefaults
{
	my $self = shift;
	
	$self->stopAfterTopics(0);
	$self->createElements(1);
	$self->warnOnLinkRemove(0);
	$self->skipBlankLines(0);
	$self->head1IsContainer(1);
	$self->hasVolumeStructure(0);
	$self->volumeTitleMatch('(\w+)\s+(\w+)\.(\w+)'); # match: "xyz 3.4a", $1 = type, $2 = volume, $3 = num
	$self->topicNumsInTitle(0);
	$self->topicNumTitleMatch('(\w+)\s+(\w+)');	# match: "xyz 3", $1 = type, $2 = num
}

#--------------------------------------------------------------------
# Construct used to load entire Alim4File into a single datastructure
#--------------------------------------------------------------------
package Alim4File;
use Class::Struct;
use File::Basename;
struct 
(
	attrs => '%',
	elements => '@',
	maxLineLenTopic => '$',
	minLineLenTopic => '$',
	footnotes => '%',
	footnotesFile => '$',
	readParams => '$',
	topicsCount => '$',
	topicsHash => '%',
	topicsList => '@',
	warnings => '@',
);

sub ReadFile
{
	my ($self, $readParams) = @_;

	$self->topicsCount(0);
	my $srcFile = $readParams->srcFile();
	my $srcDir = dirname($srcFile);
	my $curTopic = undef;
	my $skipBlankLines = $readParams->skipBlankLines();
	$self->readParams($readParams);
	
	$topicNum = 0;
	$lineNum = 0;
	
	open(ALIMFILE, $srcFile) || return "Can't open $srcFile: $!\n";
	while(<ALIMFILE>)
	{
		$lineNum++;
		if($lineNum % 25 == 0 && defined $readParams->showStatusCallback())
		{
			&{$readParams->showStatusCallback()}($readParams, 'topics', $lineNum);
		}
		
		# do we have a comment? if so, keep going
		next if(m/^\;/);
		
		# if we have a blank line and we want to skip blanks, then skip it
		next if m/^\s*$/ && $skipBlankLines;

		# see if we have a meta command (. at the beginning of a line followed by a command)
		if(m/^\.(\w+)(\s+)(.*)/)
		{
			# any line starting with a period with a word after it is a metacommand
			my ($cmd, $metaCmdParams) = ($1, $3);

			if("\L$cmd" eq 'topic')
			{
				# look for: .topic ctxString "title"
				# this regexp will get rid of any leading space after the first quote
				#
				if($metaCmdParams =~ m/(.+)(\s+)("\s*)(.+)(")/)
				{
					$topicNum++;
					my $topic = new Alim4Topic;
					$topic->topicNum($topicNum);
					$topic->startLine($lineNum);
					$topic->endLine($lineNum);
					$topic->topicCtx(lc $1);
					$topic->topicTitle($4);

					$self->topicsHash($topic->topicCtx(), $topic);
					push(@{$self->topicsList()}, $topic);
					$curTopic = $topic;
				}
				else
				{
					warn "unknown parameters in $cmd ($metaCmdParams)\n";
				}
			}
			elsif("\L$cmd" eq 'footnotesfile')
			{
				$self->footnotesFile($metaCmdParams);
				
				# if no file path is specified, use the source path instead
				if($self->footnotesFile() !~ m/\\/)
				{
					$self->footnotesFile("$srcDir\\" . $self->footnotesFile());
				}
			}
			elsif("\L$cmd" eq 'hadithnarrator')
			{
				$curTopic->attrs('hadithNarrator', $metaCmdParams);
			}
			elsif("\L$cmd" eq 'contentsinfo')
			{
				$curTopic->attrs('contentsInfo', $metaCmdParams);
			}			
			elsif("\L$cmd" eq 'endtopic')
			{
				$curTopic->endLine($lineNum);
				#$self->minLineLenTopic($curTopic) if ! defined $self->minLineLenTopic() || $curTopic->minLineLen() < $self->minLineLenTopic()->minLineLen();
				#$self->maxLineLenTopic($curTopic) if ! defined $self->maxLineLenTopic() || $curTopic->maxLineLen() > $self->maxLineLenTopic()->maxLineLen();
				
				if(defined $readParams->stopAfterTopics() && $readParams->stopAfterTopics() > 0)
				{
					last if $curTopic->topicNum() >= $readParams->stopAfterTopics();
				}
				$curTopic = undef;
			}
			else
			{
				if(! defined $curTopic)
				{
					# it must be a file attribute if we're outside a topic
					$self->attrs(lc $cmd, $metaCmdParams);
				}
				else
				{
					# we're inside a topic, so save it for later processing 
					push(@{$curTopic->topicLines()}, $_);
				}
			}
		}
		elsif(defined $curTopic)
		{
			# save the line in the list of lines in the current topic (do final processing if requested)
			chomp;
			push(@{$curTopic->topicLines()}, $_);
			my $len = length($_);
			$curTopic->minLineLen($len) if ! defined $curTopic->minLineLen() || $len < $curTopic->minLineLen;
			$curTopic->maxLineLen($len) if ! defined $curTopic->maxLineLen() || $len > $curTopic->maxLineLen;
		}
		else
		{
			if(m/^\s*$/)
			{
				# ignore a blank line outside the current topic scope
			}
			else
			{
				push(@{$self->warnings()}, sprintf("don't know how to handle line %d", $lineNum));
			}
		}
	}
	close(ALIMFILE);

	# see if we need to load any footnotes
	if($self->footnotesFile())
	{
		my $fnoteSrcFile = $self->footnotesFile();
		if(-e $fnoteSrcFile)
		{
			open(FOOTNOTES, $fnoteSrcFile) || return "Can't open footnotes file $fnoteSrcFile: $!\n";
			my $fnLineNum = 0;
			while(<FOOTNOTES>)
			{
				$fnLineNum++;
				if($fnLineNum % 25 == 0 && defined $readParams->showStatusCallback())
				{
					&{$readParams->showStatusCallback()}($readParams, 'fnotes', $lineNum);
				}
				if(m/(\w+)\s+(.*)/)
				{
					$self->footnotes($1, $2);
				}
				else
				{
					push(@{$self->warnings()}, "Footnotes file $fnoteSrcFile not found.");
					push(@{$self->warnings()}, sprintf("Invalid footnote format on line %s of %s", $fnLineNum, $fnoteSrcFile));
				}
			}
			close(FOOTNOTES);
		}
		else
		{
			push(@{$self->warnings()}, "Footnotes file $fnoteSrcFile not found.");
		}
	}

	# do some counting
	$self->topicsCount(scalar(@{$self->topicsList()}));
	
	if(defined $readParams->createElements() && $readParams->createElements())
	{
		$self->CreateElements($readParams);
	}

	return $self;
}

sub PrintWarnings
{
	my ($self) = @_;
	if(@{$self->warnings()})
	{
		print join("\n", @{$self->warnings()});
		print "\n";
	}
}

sub CaptureLinks
{
	my ($linksText, $addPaceHold) = @_;
	my $links = [];
	my $linkNum = 0;
	my $placeHold = '';
	
	# capture all links of the form {xxx:yyy} into a list of hashes, leaving placeholders behind
	${$linksText} =~ s/\{(.*?)\:.(.*?)\}/
					$linkNum++;
					$placeHold= defined $addPaceHold && $addPaceHold ? "!LNK$linkNum!" : $1;
					push(@{$links}, { 'linkText'=> $1, 'linkCtx' => $2, 'placeHold' => $placeHold}); $placeHold;
					/ge;

	# all the links are in the list, figure out the link type	
	my $link = undef;
	foreach $link (@{$links})
	{
		# assume we don't know what kind of link this is
		$link->{linkType} = '';
		
		# see if this is a quran ayah link
		if($link->{linkCtx} =~ m/^\s*qs(\d+)\.(\d+)/)
		{
			$link->{linkType} = 'quranAyah';
			$link->{linkText} = "Q $1:$2";
			$link->{quranLinkSura} = $1;
			$link->{quranLinkAyah} = $2;
		}
		elsif($link->{linkCtx} =~ m/^\s*sb(.*)/)
		{
			$link->{linkType} = 'subject';
		}
		#elsif($link->{linkCtx} =~ m/^\s*ht(.*)/)
		#{
		#	$link->{linkType} = 'hadith';
		#}
		else
		{
			$link->{linkType} = 'other';
		}
	}
	return $links;
}

sub CreateElements
{
	my ($self, $readParams) = @_;
	
	# the root element is just a reference to "elements"
	my $rootElem = $self->elements();

	foreach $topic (@{$self->topicsList()})
	{
		if(defined $readParams->showStatusCallback())
		{
			&{$readParams->showStatusCallback()}($readParams, 'elements', $topic);
		}

		my $pageElem = new Alim4Page;
		$pageElem->topic($topic);
		$pageElem->id($topic->topicCtx());
		$pageElem->title($topic->topicTitle());
		$pageElem->num($topic->topicNum());
		
		if(defined $readParams->hasVolumeStructure() && $readParams->hasVolumeStructure())
		{
			my $volMatch = $readParams->volumeTitleMatch();
			if($topic->topicTitle() =~ m/$volMatch/)
			{
				my ($volType, $volNum, $volItemNum) = ($1, $2, $3);
				if(! exists $volumeElems->{$volNum})
				{
					$volumeElems->{$volNum} = new Alim4Volume;
					$volumeElems->{$volNum}->volNum($volNum);
					push(@{$rootElem}, $volumeElems->{$volNum});
				}
				$pageElem->id("$volNum\.$volItemNum");
				$pageElem->num($volItemNum);
				push(@{$volumeElems->{$volNum}->content()}, $pageElem);
			}
			else
			{
				push(@{$self->warnings()}, sprintf("'%s' does not contain match volume spec '%s'", $topic->topicTitle(), $volMatch));
			}
		}
		elsif(defined $readParams->topicNumsInTitle() && $readParams->topicNumsInTitle())
		{
			if($topic->topicTitle() =~ m/$readParams->topicNumTitleMatch()/)
			{
				my ($numType, $titleNum) = ($1, $2);
				$pageElem->id($titleNum);
				$pageElem->num($titleNum);
				push(@{$rootElem}, $pageElem);
			}
			else
			{
				push(@{$self->warnings()}, sprintf("'%s' does not contain match topicnum spec '%s'", $topic->topicTitle(), $readParams->topicNumTitleMatch()));
			}
		}
		else
		{
			push(@{$rootElem}, $pageElem);
		}

		my $totalLines = scalar(@{$topic->topicLines()});
		my $topicLine = undef;
		my $lineNum = 0;
		my $insideHead1 = 0;
		my $curElem = $pageElem;
		my @listStack = ();

		foreach $topicLine (@{$topic->topicLines()})
		{
			$lineNum++;

			# as longs as this line is not a section heading, look for links
			my $links = [];
			$links = CaptureLinks(\$topicLine, 0) if $topicLine !~ m/^\@(.*)/;
			if(scalar (@{$links}) > 0 && defined $readParams->warnOnLinkRemove() && $readParams->warnOnLinkRemove())
			{
				grep
				{
					push(@{$self->warnings()}, sprintf("topic %d.%d (%s): link {%s:%s} replaced with %s", $topic->topicNum(), $lineNum, $topic->topicTitle(), $_->{'linkText'}, $_->{'linkCtx'}, $_->{'placeHold'}));
				} @{$links};
			}
			
			if($topicLine =~ m/^\@(.*)/)						# new section
			{
				$curElem = $pageElem if defined $readParams->head1IsContainer() && $readParams->head1IsContainer() && $insideHead1;

				if(defined $readParams->head1IsContainer() && $readParams->head1IsContainer())
				{
					my $sectElem = new Alim4Section;
					$sectElem->heading($1);
					push(@{$curElem->content()}, $sectElem);
					$curElem = $sectElem;
					$insideHead1 = 1;
				}
				else
				{
					my $headElem = new Alim4Heading;
					$headElem->text($1);
					$headElem->level(1);
					push(@{curElem->content()}, $headElem);					
				}				
			}
			elsif($topicLine =~ m/^\!(.*)/)						# new section (level 2)
			{
				my $headElem = new Alim4Heading;
				$headElem->text($1);
				$headElem->level(2);
				push(@{$curElem->content()}, $headElem);
			}
			elsif($topicLine =~ m/^\.(\w+)(\s+)(.*)/)			# .autoindent, etc
			{
				#my ($metaCmd, $metaCmdParams) = ($1, $3);
				#ignore...
			}
			elsif($topicLine =~ m/^(\d+)\.\s+(.*)/)				# numbered list
			{
				pop @listStack if @listStack && $listStack[$#listStack]->{'listType'} eq 'alpha';
				if($1 == 1)
				{
					my $listElem = new Alim4List;
					$listElem->type('decimal');
					$listElem->level(1);
					push(@{$curElem->content()}, $listElem);
					push(@listStack, { 'elem' => $listElem, 'listType' => 'decimal', 'listCnt' => 0 });
				}
				else
				{
					if(scalar(@listStack) < 1)
					{
						push(@{$self->warnings()}, sprintf("topic %d.%d (%s): list # %d encountered without list start", $topic->topicNum(), $lineNum, $topic->topicTitle(), $1));
						next;
					}
				}

				my $top = $listStack[$#listStack];
				if($top->{listType} eq 'decimal')
				{
					if($1 == $top->{listCnt}+1)
					{
						push(@{$top->{elem}->content()}, $2);
						$top->{listCnt}++;
					}
					else
					{
						push(@{$self->warnings()}, sprintf("topic %d.%d (%s): list # %d out of order, expected %d", $topic->topicNum(), $lineNum, $topic->topicTitle(), $1, $top->{listCnt}+1));
					}
				}
			}
			elsif($topicLine =~ m/^([A-Za-z])\.\s+(.*)/)			# lettered list
			{
				my $listId = lc $1;
				if($listId eq 'a')
				{
					my $listElem = new Alim4List;
					$listElem->type('alpha');
					$listElem->level(2);
					push(@{$curElem->content()}, $listElem);
					push(@listStack, { 'elem' => $listElem, 'listType' => 'alpha', 'listCnt' => 0 });
				}
				else
				{
					if(scalar(@listStack) < 1)
					{
						push(@{$self->warnings()}, sprintf("topic %d.%d (%s): alpha-list # %s encountered without list start", $topic->topicNum(), $lineNum, $topic->topicTitle(), $listId));
						next;
					}
				}

				my $top = $listStack[$#listStack];
				if($top->{listType} eq 'alpha')
				{
					my $alphaIdx = ord($listId) - ord('a') +1 ;
					if($alphaIdx == $top->{listCnt}+1)
					{
						push(@{$top->{elem}->content()}, $2);
						$top->{listCnt}++;
					}
					else
					{
						push(@{$self->warnings()}, sprintf("topic %d.%d (%s): list # %s out of order, expected %s", $topic->topicNum(), $lineNum, $topic->topicTitle(), $listId, chr(ord 'a' + $alphaIdx)));
					}
				}
			}
 			elsif($topicLine =~ m/^(\-)\s+(.*)/)			# bulleted text
			{
				my $top = scalar(@listStack) > 0 ? $listStack[$#listStack] : undef;
				if(! $top || $top->{'listType'} ne 'bullet')
				{
					my $listElem = new Alim4List;
					$listElem->type('bullet');
					$listElem->level(1);
					push(@{$curElem->content()}, $listElem);
					$top = { 'elem' => $listElem, 'listType' => 'bullet', 'listCnt' => 0 };
					push(@listStack, $top);
				}

				push(@{$top->{elem}->content()}, $2);
				$top->{listCnt}++;
			}
			else												# arbitrary text
			{
				@listStack = ();
				push(@{$curElem->content()}, $topicLine);
			}			
		}
	}
}

sub CreateSubjectIndex
{
	my $self = shift;
	my $index = new Alim4SubjectIndex;

	my $topic = undef;	
	foreach $topic (@{$self->topicsList()})
	{
		my $lineNumInFile = $topic->startLine();
		grep
		{
			if(m/^(\d+)\.\s+([^\|]*)\|(.*)/)
			{
				my $idxLine = $1;
				my $subtopic = $2;
				my $refs = $3;
				my $links = CaptureLinks(\$refs, 0);
				grep
				{
					my $link = $_;
					if($link->{linkType} eq 'quranAyah')
					{
						my $key = $link->{quranLinkSura} . '.' . $link->{quranLinkAyah};
						if(defined $index->quran($key) && $index->quran($key))
						{
							my $subjectsList = $index->quran($key);
							my $subtopicsList = $subjectsList->{$topic->topicTitle()};
							push(@{$subtopicsList}, $subtopic);
						}
						else
						{
							my $subtopicsList = [];
							push(@{$subtopicsList}, $subtopic);
							$index->quran($key, {});
							$index->quran($key)->{$topic->topicTitle()} = $subtopicsList;
						}
					}
					#if($link->{linkType} eq 'hadith')
					#{
					#	push(@{$index->{'hadith'}->{$link->{linkCtx}}->{$topic->topicName()}}, $subtopic);
					#}
					if($link->{linkType} eq 'other')
					{
						my $key = $link->{linkCtx};
						if(defined $index->other($key) && $index->other($key))
						{
							my $subjectsList = $index->other($key);
							my $subtopicsList = $subjectsList->{$topic->topicTitle()};
							push(@{$subtopicsList}, $subtopic);
						}
						else
						{
							my $subtopicsList = [];
							push(@{$subtopicsList}, $subtopic);
							$index->other($key, {});
							$index->other($key)->{$topic->topicTitle()} = $subtopicsList;
						}
					}
					if($link->{linkType} eq 'subject')
					{
						push(@{$index->{'subject'}->{$link->{linkCtx}}->{$topic->topicName()}}, $subtopic);
					}
				} @{$links}
			}
			else
			{
				GenUtils::Error($self->readParams->srcFile(), $lineNumInFile, "subject format error");
			}
			$lineNumInFile++;
		} @{$topic->topicLines()};
	}
	
	return $index;
}

sub IterateTopics
{
	my ($self, $callback) = @_;
	my $topic = undef;

	foreach $topic (@{$self->topicsList()})
	{
		&{$callback}($self, $topic);
	}
}

1;