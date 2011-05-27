package GenUtils;

use strict;
use Text::Wrap;

@GenUtils::dontCapitalizeInTitleCase = sort ('and', 'or', 'am', 'is', 'are', 'was', 'were', 'be', 'being', 'been', 'of', 'a', 'as', 'for');

sub PadLeft
{
	my ($str, $padLen, $padWith) = @_;
	$padWith = '0' if !defined $padWith;
	
	my $missing = $padLen - length($str);
	$str = $padWith x$missing . $str if $missing > 0;
	
	return $str;
}

sub ForceExtension
{
	my ($origFileName, $newExt) = @_;
	my $newFileName = $origFileName;
	
	$newFileName =~ s/(.+)(\.)(.*?)$/$1/;

	return $newFileName . '.' . $newExt;
}

sub PrintArray
{
	#
	# given a reference to an assoc array and a heading, print the heading
	#    on a line and then all of the array's keys and values, sorted by key
	# if heading is undefined, then heading is skipped
	#

	my ($arrayRef, $heading, $level) = @_;
	my $key;
	
	$level = 0 if !defined $level;
	
	print STDOUT ' ' x$level, "'$heading'" . "\n" if defined $heading;
	if(ref($arrayRef) eq 'HASH')
	{
		foreach $key (sort keys %{$arrayRef})
		{
			#next if(!(defined $arrayRef->{$key} && exists $arrayRef->{$key} && $arrayRef->{$key} ne ''));
			
			if(exists $arrayRef->{$key} && (ref($arrayRef->{$key}) eq 'HASH' || ref($arrayRef->{$key}) eq 'ARRAY'))
			{
				PrintArray($arrayRef->{$key}, $key, defined $level ? $level + 1 : 1);
			}
			else
			{
				print STDOUT '  ' x($level+1), "'$key'", ': ', $arrayRef->{$key}, "\n";
			}
		}
	}
	elsif(ref($arrayRef) eq 'ARRAY')
	{
		foreach $key (@{$arrayRef})
		{
			print STDOUT '  ' x$level, "'$key'", "\n" if !ref($key);
			PrintArray($key, $key, defined $level ? $level + 1 : 1);
		}
	}
}

sub ReadIniFile
{
	#
	# function slurps an entire INI-type file into assoc array (sections) of 
	# assoc arrays (keywords) that contain values of keywords
	#
	# [section 1]
	# keyword1=value1
	#
	# statement x = $array{'section 1'}{'keyword1'} would make x == value1
	#
	my ($fileName) = @_;
	my $curSectionName = '';
	my %iniFileData;
	
	open(INIFILE, $fileName) || die "Unable to open INI-type file $fileName: $!\n";
	while(<INIFILE>)
	{
		chomp;
	
		if(/^\[(.+?)\]/)
		{
			# lines like [name] are sections
			$curSectionName = $1;
		}
		elsif(/^[\;\#]/)
		{
			# ignore lines starting with ; or #
		}
		elsif(/(.+?)\=(.*)/)
		{
			# lines like x=y are keywords that belong in the current section
			$iniFileData{$curSectionName}{$1} = $2;
		}
		else
		{
			# a line by itself is ok, just put it into the current section with value
			$iniFileData{$curSectionName}{$_} = '' if($_ ne '' && $curSectionName ne '');
		}
	}
	close(INIFILE);
	
	return \%iniFileData;
}

#
# replaces all keys in replaceInfoRef with values into fmtStr
#
# e.g.:
# Given a format string like '$one hi there %two'
# and replaceInfoRef of { '$one' => 'hello', '%two' => 'more' }
# will return a string 'hello hi there more'
#

sub DoReplacements
{
	my ($fmtStr, $replaceInfoRef) = @_;
	my $replaceWhat;
	
	foreach $replaceWhat (keys %{$replaceInfoRef})
	{
		# first escape (protect) any characters not digits or alpha
		my $escapedReplaceWhat = $replaceWhat;
		$escapedReplaceWhat =~ s/([^0-9a-zA-Z])/\\$1/g;
		
		# now we've got everthing we need, so do the replacements
		$fmtStr =~ s/$escapedReplaceWhat/$replaceInfoRef->{$replaceWhat}/ge;
	}
	
	return $fmtStr;
}

#
# note: TitleCase will remove repeated whitespace between words!
#
sub TitleCase
{
	my ($origStr) = @_;
	
	$origStr =~ tr/A-Z/a-z/;               # make everthing lowercase first
	my @words = split(/\s+/, $origStr);    # get a list of the words
	
	$origStr = '';
	grep
	{
		my $dontWord;
		my $foundDontWord;
		foreach $dontWord (@GenUtils::dontCapitalizeInTitleCase)
		{
			$foundDontWord = $_ eq $dontWord;
			last if $foundDontWord != 0;
		}
		
		# if word wasn't found in dont cap list, capitalize the first alpha char
		s/^(\W*)([a-z])/$1\u$2/ if $foundDontWord != 1;
	} @words;
	
	return join(' ', @words);
}

sub Error
{
	my ($fname, $line, $error) = @_;
	print STDERR "E $error at $fname line $line.\n";
}

sub Warn
{
	my ($fname, $line, $warning) = @_;
	print STDERR "W $warning at $fname line $line.\n";
}

sub DirtySGML
{
	my ($textRef) = @_;
	
	return 'SGML error - found <' if ${$textRef} =~ m/\</;
	return 'SGML error - found >' if ${$textRef} =~ m/\>/;
	return 'SGML error - replaced & with &amp;' if ${$textRef} =~ s/\&.*[^;]/\&amp;/g;
	return '';
}

sub CallTag
{
    my $tag = lc(shift); 
    my $type = uc(shift);  # should be 'S' for start, 'E' for end, or 'B' for both
    my $level = shift || 0;
    my $attributes;
	my $attrText = '';
    if (@_ and defined $_[0] and ref($_[0]) eq "HASH") 
	{
		$attributes = shift;
		grep
		{
			my $attraVal = $attributes->{$_};
			$attraVal =~ s/"/'/g;
			$attraVal =~ s/\&/&amp;/g;
			$attrText .= ' ' . lc($_) . '="' . $attraVal . '"';
		} sort keys %{$attributes};
    }
	
	my $content = wrap("\t"x($level+1), "\t"x($level+1), @_);
	my $complete = '';
	
	$complete .= "\t"x$level . "<$tag$attrText>\n" if $type eq 'S' or $type eq 'B';
	$complete .= "\t"x$level . "<$tag$attrText/>\n" if $type eq 'T';
	$complete .= $content . "\n" if $type eq 'B' or $type eq 'C';
	$complete .= "\t"x$level . "</$tag>\n" if $type eq 'E' or $type eq 'B';
	
	return $complete;
}

sub CreateTags
{
	my (@tags) = @_;
	my @code = ();
	for (@tags) {
		my $tagName = $_;
		my $funcName = $_;
		if(m/(.*):(.*)/)
		{
			$tagName = $1;
			$funcName = $2;
		}
	    push(@code, "sub main::$funcName { CallTag('$tagName', 'B', shift, \@_); }\n");
	    push(@code, "sub main::$funcName\_empty { CallTag('$tagName', 'T', shift, \@_); }\n");
	    push(@code, "sub main::$funcName\_content { CallTag('$tagName', 'C', shift, \@_); }\n");
	    push(@code, "sub main::$funcName\_start { CallTag('$tagName', 'S', shift, \@_); }\n");
	    push(@code, "sub main::$funcName\_end { CallTag('$tagName', 'E', shift, \@_); }\n");
	}
	eval join('', @code);
	if ($@) {
	    die $@;
	}
}

1;
