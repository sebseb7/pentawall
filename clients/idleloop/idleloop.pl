#!/usr/bin/perl


use strict;
use ledwall_tcp;
use Time::HiRes qw(usleep time);
use List::Util qw(shuffle);


close STDOUT;
open STDOUT,'>>logfile.txt';
open STDERR,'>&STDOUT';


my $PATH = "/opt/idleloop";
$0 = 'idleloop';

while(1)
{
	eval
	{

		ledwall::init();

		ledwall::setLevel(0);

		my @files;
		
		opendir(my $dh, $PATH) || die "can't opendir: $!";
		while(my $file  = readdir($dh)) {
			next if $file !~  /\.pw$/;
			push @files,$file;
		}
		closedir $dh;
		@files = shuffle(@files);

		foreach my $file (@files)
		{
			ledwall::binFrame('020000000000');

			my $start = time*1000;

			$0 = 'idleloop - playing: '.$file;
			
			open infile,'/opt/idleloop/'.$file or next;

			while(<infile>)
			{
				if(/^(\d+) (.*)\r\n$/)
				{
					my $delay = $1 - (time*1000-$start);
					if( ($delay > 0) and ($delay < 60000))
					{
						usleep($delay*1000);
					}
					ledwall::binFrame($2);
				}
			}
			close infile;
			# black frame
			ledwall::binFrame('020000000000');
			usleep(500000);
		};
	};
}

