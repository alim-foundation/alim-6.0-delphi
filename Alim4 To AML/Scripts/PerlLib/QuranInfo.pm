package QuranInfo;

use strict;
use GenUtils;

sub new
{
	my ($class, $dataFile) = @_;
	$dataFile ||= 'QURANDTA.INI';
	
	my $self =
		{
			_quranInfo => GenUtils::ReadIniFile($dataFile),
			_suraInfo => [],
		};
	
	bless $self, $class;
	$self->BuildSuraInfo($self);
	
	return $self;
}

sub BuildSuraInfo
{
	my $self = shift;
	my $suraNum;

	# create some convenience data structures
	foreach $suraNum (1..114)
	{
		my $suraInfo = 
			{
				suraId => $self->GenerateId('sura', $suraNum),
				suraNum => $suraNum,
				suraName => $self->{_quranInfo}->{"Sura $suraNum"}{Name},
				ayatCount => $self->{_quranInfo}->{"Sura $suraNum"}{TotalAyat},
				revealedIn => $self->{_quranInfo}->{"Sura $suraNum"}{RevealedIn},
				chronoOrder => $self->{_quranInfo}->{"Sura $suraNum"}{ChronoOrder},
				rukuAyahs => $self->{_quranInfo}->{"Sura $suraNum"}{Ruku},
				firstRukuId => $self->GenerateId('suraruku', $suraNum, 1),
			};
		push(@{$self->{_suraInfo}}, $suraInfo);
	}

	$self->ForEachSura(\&cb_BuildSuraLinks);
	$self->ForEachSura(\&cb_BuildRukuInfo);
	$self->ForEachSuraRuku(\&cb_BuildRukuLinks);
}

#
# callback used to create circular linked list of sura information
#
sub cb_BuildSuraLinks
{
	my ($self, $suraInfo) = @_;
	
	my $suraNum = $suraInfo->{suraNum};
	my $suraArrayIdx = $suraInfo->{suraNum}-1;
	
	if($suraArrayIdx > 0)
	{
		$suraInfo->{prevSuraInfo} = $self->{_suraInfo}->[$suraArrayIdx-1];
	}
	else
	{
		$suraInfo->{prevSuraInfo} = $self->{_suraInfo}->[113];
	}		
	
	if($suraArrayIdx < 113)
	{
		$suraInfo->{nextSuraInfo} = $self->{_suraInfo}->[$suraArrayIdx+1];
	}
	else
	{
		$suraInfo->{nextSuraInfo} = $self->{_suraInfo}->[0];
	}		
}

sub BuildRukuDetails
{
	my ($self, $suraInfo, $rukuList, $rukuNum) = @_;
	my $rukuCount = @{$rukuList} ? scalar(@{$rukuList}) : 1;
	my $rukuDataRef =
		{
			sura => $suraInfo,
			rukuId => $self->GenerateId('suraruku', $suraInfo->{suraNum}, $rukuNum),
			rukuNum => $rukuNum,
			startAyah => 0,
			endAyah => 0,
			ayatCount => 0,
			prevRukuInfo => {},
			nextRukuInfo => {},
		};
		
	if($rukuNum == 1 && $rukuCount == 1)
	{
		$rukuDataRef->{startAyah} = 1;
		$rukuDataRef->{endAyah} = $suraInfo->{ayatCount};
		$rukuDataRef->{ayatCount} = $suraInfo->{ayatCount};
	}
	else
	{
		my $startAyah = $rukuList->[$rukuNum-1];
		my $endAyah = $rukuNum >= $rukuCount ? 
						$suraInfo->{ayatCount} : 
						$rukuList->[$rukuNum]-1;

		$rukuDataRef->{startAyah} = $startAyah;
		$rukuDataRef->{endAyah} = $endAyah;
		$rukuDataRef->{ayatCount} = $endAyah-$startAyah+1;
	}
	
	return $rukuDataRef;
}

#
# callback used to create ruku counts and ayahs per ruku information
# --> important: cb_BuildSuraLinks should have already been called
#
sub cb_BuildRukuInfo
{
	my ($self, $suraInfo) = @_;
	
	# create list of ayahs that make up each ruku
	my @rukuList = split(/,/, $suraInfo->{rukuAyahs});
	my $rukuCount = @rukuList ? scalar(@rukuList) : 1;
	my $rukuInfo = [];

	$suraInfo->{rukuCount} = $rukuCount;
	$suraInfo->{rukuInfo} = $rukuInfo;
	
	if($rukuCount > 1)
	{
		my $rukuNum;
		foreach $rukuNum (1..$rukuCount)
		{
			my $rukuDetails = $self->BuildRukuDetails($suraInfo, \@rukuList, $rukuNum);
			push(@{$rukuInfo}, $rukuDetails);
		}
	}
	else
	{
		my $rukuDetails = $self->BuildRukuDetails($suraInfo, \@rukuList, 1);
		push(@{$rukuInfo}, $rukuDetails);
	}	
}

sub CalcPrevRuku
{
	my ($self, $rukuDetails) = @_;
	my ($sura, $ruku) = ($rukuDetails->{sura}->{suraNum}, $rukuDetails->{rukuNum});
	
	if($rukuDetails->{rukuNum} > 1)
	{
		$ruku--;
	}
	else
	{
		$sura = $rukuDetails->{sura}->{prevSuraInfo}->{suraNum};
		$ruku = $rukuDetails->{sura}->{prevSuraInfo}->{rukuCount} <= 1 ? 
		        1 : $rukuDetails->{sura}->{prevSuraInfo}->{rukuCount};
	}
	
	return ($sura, $ruku);
}

sub CalcNextRuku
{
	my ($self, $rukuDetails) = @_;
	my ($sura, $ruku) = ($rukuDetails->{sura}->{suraNum}, $rukuDetails->{rukuNum});
	
	if($rukuDetails->{rukuNum} < $rukuDetails->{sura}->{rukuCount})
	{
		$ruku++;
	}
	else
	{
		$sura = $rukuDetails->{sura}->{nextSuraInfo}->{suraNum};
		$ruku = 1;
	}
	
	return ($sura, $ruku);
}

sub cb_BuildRukuLinks
{
	my ($self, $rukuDetails) = @_;
	
	my ($sura, $ruku) = $self->CalcPrevRuku($rukuDetails);
	$rukuDetails->{prevRukuInfo} = $self->GetRukuDetails($sura, $ruku);
	
	($sura, $ruku) = $self->CalcNextRuku($rukuDetails);
	$rukuDetails->{nextRukuInfo} = $self->GetRukuDetails($sura, $ruku);
}

sub FindAyahRuku
{
	my ($self, $suraNum, $ayahNum) = @_;
	my $suraInfo = $self->GetSuraInfo($suraNum);
	
	my $rukuDetails;
	foreach $rukuDetails (@{$suraInfo->{rukuInfo}})
	{
		return $rukuDetails 
			if $ayahNum >= $rukuDetails->{startAyah} &&
				$ayahNum <= $rukuDetails->{endAyah};
	}

	return $suraInfo->{rukuInfo}->[0];
}

sub ForEachSura
{
	my ($self, $callback, $userdata) = @_;
	
	grep
	{
		&{$callback}($self, $_, $userdata);
	} @{$self->{_suraInfo}};
}

sub ForEachSuraRuku
{
	my ($self, $callback, $userdata) = @_;
	
	grep
	{
		my $suraInfo = $_;
		grep
		{
			&{$callback}($self, $_, $userdata);
		} @{$suraInfo->{rukuInfo}};
	} @{$self->{_suraInfo}};
}

sub ForEachRuku
{
	my ($self, $suraNum, $callback, $userdata) = @_;
	
	grep
	{
		&{$callback}($self, $_, $userdata);
	} @{$self->GetSuraInfo($suraNum)->{rukuInfo}};
}

sub ForEachAyah
{
	my ($self, $suraNum, $callback, $userdata) = @_;
	my $suraInfo = $self->GetSuraInfo($suraNum);
	my $ayahDetails =
		{
			sura => $suraInfo,
			ruku => {},
			ayahNum => 0,
			ayahId => '',
		};
	
	foreach (1..$suraInfo->{ayatCount})
	{
		$ayahDetails->{ayahNum} = $_;
		$ayahDetails->{ruku} = $self->FindAyahRuku($suraNum, $_);
		$ayahDetails->{ayahId} = $self->GenerateId('suraayah', $suraNum, $_);
		&{$callback}($self, $ayahDetails, $userdata);
	};
}

sub GetSuraInfo
{
	my ($self, $suraNum) = @_;
	return $self->{_suraInfo}->[$suraNum-1];
}

sub GetRukuDetails
{
	my ($self, $suraNum, $rukuNum) = @_;
	return $self->{_suraInfo}->[$suraNum-1]->{rukuInfo}->[$rukuNum-1];
}

sub GenerateId
{
	my $self = shift;
	my $idType = lc(shift);
	my $id = '';
	
	if($idType eq 'quran')
	{
		my ($bookId) = @_;
		$id = $bookId;
	}
	elsif($idType eq 'sura')
	{
		my ($suraNum) = @_;
		$id = "QS$suraNum";
	}
	elsif($idType eq 'suraruku')
	{
		my ($suraNum, $rukuNum) = @_;
		$id = "QS$suraNum\R$rukuNum";
	}
	elsif($idType eq 'suraayah')
	{
		my ($suraNum, $ayahNum) = @_;
		$id = "QS$suraNum\A$ayahNum";
	}
	else
	{
		$id = "unknown_id_type_$idType";
	}
	
	return $id;
}

sub cb_PrintRukuInfo
{
	my ($qinfo, $rukuDetails) = @_;
	
	my $info = sprintf("%03d  (%03d)  %03d  [%03d]  %03d  %03d  {%03d}  [%d,%d  %d,%d]\n",
						$rukuDetails->{sura}->{suraNum},
						$rukuDetails->{sura}->{ayatCount},
						$rukuDetails->{rukuNum},
						$rukuDetails->{sura}->{rukuCount},
						$rukuDetails->{startAyah},
						$rukuDetails->{endAyah},
						$rukuDetails->{ayatCount},
						$rukuDetails->{prevRukuSuraNum},
						$rukuDetails->{prevRukuNum},
						$rukuDetails->{nextRukuSuraNum},
						$rukuDetails->{nextRukuNum},
						);
	print $info;
}

sub UnitTest
{
	my $self = shift;
	
	print "Sura Ayat  Ruku  Rukus Start End  Ayat   PrevSura,Ruku NextSura,Ruku\n";
	$self->ForEachSuraRuku(\&cb_PrintRukuInfo);
}

package ArabicQuranMap;

sub new
{
	my ($class, $dataFile) = @_;
	$dataFile ||= 'ArabicQuranMap.txt';
	
	return undef if ! -e $dataFile;
	
	my $self =
		{
			_dataFile => $dataFile,
			_dataItems => [],
			_pages => [],
			_suras => [],
		};
	
	bless $self, $class;
	$self->ReadMapFile($self);
	
	return $self;
}

sub ReadMapFile
{
	my $self = shift;

	open(MAPFILE, $self->{_dataFile}) || die "unable to read quran map file $self->{_dataFile}: $!\n";
	my $headerLine = <MAPFILE>;
	while(<MAPFILE>)
	{
		my ($pg, $sura, $ayah, $ruku, $xstart, $ystart, $xend, $yend) = split(/\s+/);
		
		my $data =
			{
				page => $pg,
				sura => $sura,
				ayah => $ayah,
				ruku => $ruku,
				xstart => $xstart,
				ystart => $ystart,
				xend => $xend,
				yend => $yend,
			};			
			
		$self->{_firstItem} = $data if ! exists $self->{_firstItem};
		$self->{_lastItem} = $data;  # keep setting, the last one will be real
		push(@{$self->{_dataItems}}, $data);
		push(@{$self->{_pages}->[$pg-1]}, $data);
		push(@{$self->{_suras}->[$sura-1]}, $data);
	}
	close(MAPFILE);
	
	foreach (@{$self->{_dataItems}})
	{
		$_->{prevPage} = $_->{page} == 1 ? $self->{_lastItem}->{page} : $_->{page}-1;
		$_->{nextPage} = $_->{page} == $self->{_lastItem}->{page} ? 1 : $_->{page}+1;
	}
}

sub ForEachDataItem
{
	my ($self, $callback) = @_;
	
	grep
	{
		my $continue = &{$callback}($_);
		return if ! $continue;
	} @{$self->{_dataItems}}; 
}

sub ForEachPage
{
	my ($self, $callback) = @_;
	my $curPage = 0;
	
	my $total = scalar(@{$self->{_pages}});
	my $idx = undef;
	for($idx = 0; $idx < $total; $idx++)
	{
		my $dataItems = $self->{_pages}->[$idx];
		my $firstItem = $dataItems->[0];
		my $lastItem = $dataItems->[scalar(@{$dataItems})-1];
		
		my $continue = &{$callback}($idx+1, $dataItems, $firstItem, $lastItem);
		last if ! $continue;
	}
}

1;