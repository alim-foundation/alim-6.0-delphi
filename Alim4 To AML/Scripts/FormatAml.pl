use strict;
use XML::Parser;

my $parser = new XML::Parser(Style => 'Tree');
$parser->parsefile('e:\projects\alim\aml data\Quran\Introductions\Malik Sura Introductions.aml', ErrorContext => 5);

