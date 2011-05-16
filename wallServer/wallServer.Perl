#!/usr/bin/perl


my $ip6available;
BEGIN
{
	eval
	{
		require IO::Socket::INET6;
	};
	$ip6available = $@ if $@;
}

use strict;
use POSIX;
use IO::Socket;
use IO::Select;
use Socket;
use Fcntl;
use Device::SerialPort;
use Time::HiRes qw(usleep time);

$0='pentawall-server';




##globals

my $useIp6 = 0; # has to be set in the BEGIN Block
my $usbDevice = '/dev/serial/by-id/usb-FTDI_FT232R_USB_UART_A400eRcF-if00-port0';
my $tcpPort = 1338;
my $bufferPath = '/dev/shm/';	#where to store the buffer 
my $prioLevels = 4;				#begins with level id 0
my $defaultLevel = 1;
my $currentPrio = 0;
my $isRecording = 0;
my $recordPath = '/opt/wallRecords/';
my $currentRecordingFile;

my $frameBuffer={};  			#one buffer for each prioLevel
my %activePrios;				#show which connections is on with level

							
warn localtime(time).' started';




die $ip6available if $useIp6 and $ip6available;


close STDOUT;
open STDOUT,'>>logfile2.txt';
open STDERR,'>&STDOUT';


#{
	my %intermediaterequests; # this stores intermediate requests 
        my $bindSocket;
	if ($useIp6) {
		$bindSocket = IO::Socket::INET6->new(LocalAddr => '[::]', Proto => 'tcp',Reuse=>1,LocalPort => $tcpPort , Listen => 10) 							#	create listen socket
							|| die "Can't create Socket on host xx on port xx due to  $!\n";				
	} else {
                $bindSocket = IO::Socket::INET->new(Proto => 'tcp',Reuse=>1,LocalPort => $tcpPort , Listen => 10)                                                   #       create listen socket
                                                        || die "Can't create Socket on host xx on port xx due to  $!\n";
	}
	my	$socketflags	= fcntl($bindSocket, F_GETFL, 0)					|| die "Can't get flags on Socket due to  $!\n";				#	get the flags and 
	fcntl($bindSocket, F_SETFL, $socketflags | O_NONBLOCK) 					|| die "Can't set O_NONBLOCK flag on Socket due to $!\n";		#	set socket nonblocking
#	$bindSocket->sockopt(SO_RCVBUF, 220);		
		
	my $select = IO::Select->new($bindSocket);

	my $serial;

	serialConnect();


	foreach my $level (0..$prioLevels)
	{
		$frameBuffer->{$level} = '000000' x (15*16);
	
		if(-f $bufferPath.'pentawall_image'.$level)
		{
			open infile,$bufferPath.'pentawall_image'.$level;
			my $data = <infile>;
			chomp($data);
			$frameBuffer->{$level} = $data;
			close infile;		
		}
	}
	
	setFrame($frameBuffer->{0});



	while( 1 )
	{
		####
		#	Read in from the Sockets, that are ready
		####


		foreach my $socket ($select->can_read(1))
		{
			if( $socket == $bindSocket )								#	if the socket that is ready is the bindSocket a new Connection has occurred 
			{		
				my $client = $bindSocket->accept();						#	get the socket as seperate filedescriptor
				my $host = undef;
				my $port = undef;
				my $lport = undef;
				eval { $host	= $client->peerhost(); $port	= $client->peerport(); $lport	= $client->sockport(); };
				if( defined($host) && defined($port) && defined($lport) )
				{
					$select->add($client);									#	add the newly created socket to our select
	
#					warn localtime(time).' added'.$host.':'.$port.':'.$lport;
					$intermediaterequests{$host.':'.$port.':'.$lport}{'lastactive'}=time();
					$intermediaterequests{$host.':'.$port.':'.$lport}{'socket'}=\$client;
					$intermediaterequests{$host.':'.$port.':'.$lport}{'prio'}=$defaultLevel;
					$activePrios{$defaultLevel}->{$host.':'.$port.':'.$lport}=1;
					
					updatePrioLevel();

					my	$socketflags	= fcntl($socket, F_GETFL, 0)			|| die "Can't get flags on Socket due to  $!\n";				#	get the flags and 
					fcntl($bindSocket, F_SETFL, $socketflags | O_NONBLOCK) 		|| die "Can't set O_NONBLOCK flag on Socket due to $!\n";		#	set socket nonblocking
				}
				else
				{
					print STDERR localtime().' Got weird invitation from '.$host.':'.$port."\n";
					close($client);	
				}	
			} 
			else
			{
				my $host = undef;
				my $port = undef;
				my $lport = undef;
				eval { $host	= $socket->peerhost(); $port	= $socket->peerport();$lport	= $socket->sockport(); };
				
				if(defined($host) && defined($port) && defined($lport))
				{
				
#					warn length($intermediaterequests{$host.':'.$port}{'readbuffer'});
					#fix this !!!!
#					my $tmp = $intermediaterequests{$host.':'.$port.':'.$lport}{'readbuffer'};
					{
						my $address	= $socket->recv(my $data,3000, 0);

						if(  defined($address)  && length($data) )
						{
							$intermediaterequests{$host.':'.$port.':'.$lport}{'lastactive'}=time();
							$intermediaterequests{$host.':'.$port.':'.$lport}{'readbuffer'}.=$data;

							while($intermediaterequests{$host.':'.$port.':'.$lport}{'readbuffer'} =~  /\x0D\x0A/)
							{
								$intermediaterequests{$host.':'.$port.':'.$lport}{'writebuffer'} .= handlerequest($host,$port,$lport,$`);
								$intermediaterequests{$host.':'.$port.':'.$lport}{'readbuffer'} = $';
							}
						}
						else
						{
#							warn localtime(time).' readfail a '.$host.':'.$port.':'.$lport;
							$select->remove($socket);
							close($socket);	
							delete $activePrios{$intermediaterequests{$host.':'.$port.':'.$lport}{'prio'}}->{$host.':'.$port.':'.$lport};
							delete $intermediaterequests{$host.':'.$port.':'.$lport};
							updatePrioLevel();
						}
					}

					
				}
				else
				{
					warn localtime(time).' readfail b';
					$select->remove($socket);
					close($socket);	
					delete $activePrios{$intermediaterequests{$host.':'.$port.':'.$lport}{'prio'}}->{$host.':'.$port.':'.$lport};
					delete $intermediaterequests{$host.':'.$port.':'.$lport};
					updatePrioLevel();
				}
			}
		}

		####
		#	Write to the Sockets, that are ready
		####
		foreach my $socket ($select->can_write(1))
		{
			my $host = undef;
			my $port = undef;
			my $lport = undef;
			eval { $host	= $socket->peerhost(); $port	= $socket->peerport();$lport	= $socket->sockport() };
			
			if( defined($host) && defined($port)&& defined($lport) )
			{
				my $data	= $intermediaterequests{$host.':'.$port.':'.$lport}{'writebuffer'};
				next if !length($data);

				my $byteswritten;
				eval { $byteswritten = $socket->send($data,0) };
				warn localtime(time).' '.$@ if $@;

				if( $byteswritten )
				{
					$intermediaterequests{$host.':'.$port.':'.$lport}{'lastactive'}=time();
					$intermediaterequests{$host.':'.$port.':'.$lport}{'writebuffer'}  = substr($data, $byteswritten, length($data) - $byteswritten);
				} 
				elsif( POSIX::EWOULDBLOCK != $!)
				{
					warn localtime(time).' wouldblock';
					$select->remove($socket);
					close($socket);	
					delete $activePrios{$intermediaterequests{$host.':'.$port.':'.$lport}{'prio'}}->{$host.':'.$port.':'.$lport};
					delete $intermediaterequests{$host.':'.$port.':'.$lport};
					updatePrioLevel();
				}
			}
			else
			{
#				warn localtime(time).' closex';
				$select->remove($socket);
				close($socket);	
				delete $activePrios{$intermediaterequests{$host.':'.$port.':'.$lport}{'prio'}}->{$host.':'.$port.':'.$lport};
				delete $intermediaterequests{$host.':'.$port.':'.$lport};
				updatePrioLevel();
			}	
		}


		####
		#	cleanup sockets that haven't been active
		####
		foreach my $hostport (keys(%intermediaterequests))
		{
#			my $timeout = 300;
#			$timeout = 10 if $intermediaterequests{$hostport}{'prio'} == 3;
#			$timeout = 60 if $intermediaterequests{$hostport}{'prio'} == 2;
#			$timeout = 3600 if $intermediaterequests{$hostport}{'prio'} == 0;
			
			next if $intermediaterequests{$hostport}{'lastactive'} > (time()-300);
#			warn localtime(time).' inactive '.$hostport;
			$select->remove(${$intermediaterequests{$hostport}{'socket'}});
			close(${$intermediaterequests{$hostport}{'socket'}}) if	${$intermediaterequests{$hostport}{'socket'}};
			delete $activePrios{$intermediaterequests{$hostport}{'prio'}}->{$hostport};
			delete $intermediaterequests{$hostport};
			updatePrioLevel();
		}
	}
#}


sub handlerequest($$$$)
{
	my $host = shift;
	my $port = shift;
	my $lport = shift;
	my $data = shift;
	
	my $myPrio = $intermediaterequests{$host.':'.$port.':'.$lport}{'prio'};
	
	#keep alive
	if($data =~ /^01$/)
	{
		return "ok\r\n";
	}
	#set Pixel
	elsif($data =~ /^02(..)(..)(..)(..)(..)$/)
	{
		my $x = hex($1);
		my $y = hex($2);
		my $red_h = $3;
		my $green_h = $4;
		my $blue_h = $5;
		my $red = hex($3);
		my $green = hex($4);
		my $blue = hex($5);
		warn $data && return 'bad'."\r\n" if $x > 16;
		warn $data && return 'bad'."\r\n" if $y > 15;

		if(($x == 0)&&($y == 0))
		{
			$frameBuffer->{$myPrio} = ($red_h.$green_h.$blue_h) x (15*16);
			setModulePixel(0,0,$red,$green,$blue) if $myPrio == $currentPrio;
		}
		else
		{
			substr($frameBuffer->{$myPrio},(($y-1)*16+($x-1))*6,6,$red_h.$green_h.$blue_h);
			setPixel($x,$y,$red,$green,$blue) if $myPrio == $currentPrio;
		}

		open outfile,'>/dev/shm/pentawall_image_tmp'.$myPrio;
		print outfile $frameBuffer->{$myPrio}."\n";
		close outfile;
		rename '/dev/shm/pentawall_image_tmp'.$myPrio,'/dev/shm/pentawall_image'.$myPrio;
		
		if(($isRecording)and($myPrio == $currentPrio))
		{
			open outfile,'>>'.$recordPath.$currentRecordingFile.'.rec';
			print outfile int((time-$currentRecordingFile)*1000).' ';
			print outfile $data."\r\n";
			close outfile;

			if((time-$currentRecordingFile) > 60*5)
			{
				warn localtime(time).' autostop';
				$isRecording=0;
			}

		}
		
		return 'ok'."\r\n";
	}
	#set Frame
	elsif($data =~ /^03(.{1440})$/)
	{
		my $data2 = $1;

		$frameBuffer->{$myPrio} = $data2;		

		setFrame($data2) if $myPrio == $currentPrio;

		open outfile,'>/dev/shm/pentawall_image_tmp'.$myPrio;
		print outfile $frameBuffer->{$myPrio}."\n";
		close outfile;
		rename '/dev/shm/pentawall_image_tmp'.$myPrio,'/dev/shm/pentawall_image'.$myPrio;

		if(($isRecording)and($myPrio == $currentPrio))
		{
			open outfile,'>>'.$recordPath.$currentRecordingFile.'.rec';
			print outfile int((time-$currentRecordingFile)*1000).' ';
			print outfile $data."\r\n";
			close outfile;
			
			if((time-$currentRecordingFile) > 60*5)
			{
				warn localtime(time).' autostop';
				$isRecording=0;
			}
			
		}


		return 'ok'."\r\n";
	}
	#change prio
	elsif($data =~ /^04(\d\d)$/)
	{
		my $targetPrio = hex $1;
		return 'bad'."\r\n" if $targetPrio > $prioLevels;

		$intermediaterequests{$host.':'.$port.':'.$lport}{'prio'} = $targetPrio;

		delete $activePrios{$myPrio}->{$host.':'.$port.':'.$lport};
		$activePrios{$targetPrio}->{$host.':'.$port.':'.$lport}=1;

		updatePrioLevel();


		return 'ok'."\r\n";
	}
	#start recording
	elsif($data =~ /^05$/)
	{
		warn localtime(time).' start recording';
		$isRecording = 1;
		$currentRecordingFile = int time;
		

			open outfile,'>>'.$recordPath.$currentRecordingFile.'.rec';
			print outfile '0 03'.$frameBuffer->{$myPrio}."\r\n";
			close outfile;
		
		return 'ok'."\r\n";
	}
	#stop recording
	elsif($data =~ /^06$/)
	{
		$isRecording = 0;
		return $currentRecordingFile."\r\n";
	}
	# play recorded file
	elsif($data =~ /^07(\d+)$/)
	{
		my $fileid = $1;


		#### to implement


		return "ok\r\n";
	}
	# stop play
	elsif($data =~ /^08$/)
	{

		#### to implement

		return "ok\r\n";
	}
	else
	{
		return 'bad'."\r\n";
	}
	
}

sub updatePrioLevel()
{
	my $newPrio = 0;
	foreach my $level (0..$prioLevels)
	{
		my $count = int scalar keys %{$activePrios{$level}};
		$newPrio = $level if $count; 
	}
	if($newPrio != $currentPrio)
	{
		$currentPrio = $newPrio;
		setFrame($frameBuffer->{$currentPrio});
	}
	use Data::Dumper;
#	warn Data::Dumper->Dump([\%activePrios]);
#	warn Data::Dumper->Dump([\%intermediaterequests]);
}

sub setPixel($$$$$)
{
	my $x = shift;
	my $y = shift;
	my $red = shift;
	my $green  =shift;
	my $blue = shift;

#	0 1 2 3 4 5 6 7
#    1 1 1 1 1 2 2 2 
    
#    0 1 2 3 4 0 1 2 %5
    
#    0 0 0 0 0 5 5 5 
    

	my $module = $x + ((($y-1)-(($y-1)%5))/5)*16;
	
#	my $module = 1+($x-1)*3+((($y-1) - ($y-1)%5)/5);
#	my $led = (($y-1)%5)+1;
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



	do
	{
		eval
		{
			my $bytes;
			eval
			{
				$bytes = $serial->write('B'.esc($module).esc($led).esc($red).esc($green).esc($blue));
			};
			warn localtime(time).' connection error '.$@ if $@;
			if(! $bytes)
			{
				$serial = Device::SerialPort->new($usbDevice);
				$serial->databits(8);
				$serial->handshake("xoff");
				$serial->baudrate(500000);
				$serial->parity("none");
				$serial->stopbits(1);
				localtime(time).' reconnected';
			}
		};
	}until($serial);

}

sub esc($)
{
	my $data = shift;

	return chr(hex(65)).chr(hex(1)) if $data == hex(23);
	return chr(hex(65)).chr(hex(2)) if $data == hex(42);
	return chr(hex(65)).chr(hex(3)) if $data == hex(65);
	return chr(hex(65)).chr(hex(4)) if $data == hex(66);
	return chr($data);
}

sub esc2($)
{
	my $data = shift;
	
	
	$data =~ s/e/\x65\x3/go;
	$data =~ s/\x23/\x65\x1/go;
	$data =~ s/B/\x65\x2/go;
	$data =~ s/f/\x65\x4/go;
	
	return $data;
}

sub setFrame($)
{
	my $frame=shift;

	$serial->write('#');#

	my $ppp = 60;


#	warn $frame;		
	for(0..((720/$ppp) - 1))
	{
		my $packet = $_;
			
		my $data;
		for(0..($ppp-1))
		{
			$data.=chr(hex(substr($frame,($packet*$ppp+$_)*2,2)));
		}

		do
		{
			eval
			{
				my $bytes;
				eval
				{
					$bytes = $serial->write(esc2($data));
				};
				warn localtime(time).' connection error '.$@ if $@;
				if(! $bytes)
				{
					$serial = Device::SerialPort->new($usbDevice);
					$serial->databits(8);
					$serial->handshake("xoff");
					$serial->baudrate(500000);
					$serial->parity("none");
					$serial->stopbits(1);
					localtime(time).' reconnected';
				}
			};
		}until($serial);

	}
}

sub serialConnect()
{			
	do
	{
		eval
		{
			$serial = Device::SerialPort->new($usbDevice);
			$serial->databits(8);
			$serial->handshake("xoff");
			$serial->baudrate(500000);
			$serial->parity("none");
			$serial->stopbits(1);

			$serial->write('B'.chr(0).chr(0).chr(50).chr(50).chr(100));
			sleep(2);
			$serial->write('B'.chr(0).chr(0).chr(0).chr(0).chr(0));
		};
	}until($serial);

}
