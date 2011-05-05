#!/usr/bin/perl

package ledwall;

use strict;
use Device::SerialPort;
use Time::HiRes qw(usleep);

my $port;

sub init()
{
	$port = Device::SerialPort->new("/dev/serial/by-id/usb-FTDI_FT232R_USB_UART_A400gqfB-if00-port0");
	$port->databits(8);
	$port->handshake("xoff");
	$port->baudrate(500000);
	$port->parity("none");
	$port->stopbits(1);
}

sub esc($)
{
	my $data = shift;
	
	
	$data =~ s/e/\x65\x3/go;
	$data =~ s/\x23/\x65\x1/go;
	$data =~ s/B/\x65\x2/go;
	$data =~ s/f/\x65\x4/go;
	
	return $data;
}

sub setPixel($$$$$)
{
	my $x = shift;
	my $y = shift;
	my $red = shift;
	my $green  =shift;
	my $blue = shift;

	my $module = 1+($x-1)*3+((($y-1) - ($y-1)%5)/5);
	my $led = (($y-1)%5)+1;
	
	
	setModulePixel($module,$led,$red,$green,$blue);
}


sub setModulePixel($$$$$)
{
	my $module = shift;
	my $led = shift;
	my $red = shift;
	my $green  =shift;
	my $blue = shift;

	$port->write('B'.esc(pack("C",$module).pack("C",$led).pack("C",$red).pack("C",$green).pack("C",$blue)));
	
	
}


sub setFrame($)
{
	my $frame=shift;


	$port->write('#');


	my $ppp = 60;
		
	for(0..((720/$ppp)-1))
	{

			my $packet = $_;
			
			my $data;
			for(0..($ppp-1))
			{
				$data.=pack("C",$frame->[$packet*$ppp+$_]);
			}

			$port->write(esc($data));

	}
}

1;
