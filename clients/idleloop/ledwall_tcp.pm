#!/usr/bin/perl

package ledwall;

use strict;
use IO::Socket;


my $socket;

sub init()
{
	$socket = IO::Socket::INET->new(PeerAddr => '127.0.0.1',
									PeerPort => 1338,
									Proto    => "tcp",
									Type     => SOCK_STREAM)     or die "Couldn't connect : $@\n";

}


sub setPixel($$$$$)
{
	my $x = shift;
	my $y = shift;
	my $red = shift;
	my $green  =shift;
	my $blue = shift;


	print $socket '02'.sprintf("%02x",$x).sprintf("%02x",$y).sprintf("%02x",$red).sprintf("%02x",$green).sprintf("%02x",$blue)."\r\n";
}

sub setFrame($)
{
	my $frame=shift;

	my $ppp = 60;#pixel per packet
	
	print $socket '03';
		
	for(0..((720/$ppp)-1))
	{

			my $packet = $_;
			
			my $data;
			for(0..($ppp-1))
			{
				$data.=sprintf("%02x", $frame->[$packet*$ppp+$_]);
			}
			
			print $socket $data;
			

	}
	print $socket "\r\n";
}


sub binFrame($)
{
	my $frame=shift;

	print $socket $frame;

	print $socket "\r\n";
}

sub setLevel($)
{
	my $level=shift;

	print $socket '04'.sprintf("%02x", $level);
		
	print $socket "\r\n";
}

1;


