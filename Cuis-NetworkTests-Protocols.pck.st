'From Cuis 4.1 of 12 December 2012 [latest update: #1528] on 2 January 2013 at 8:26:36 am'!
'Description Please enter a description for this package '!
!classDefinition: #HTTPSocketTest category: #'Cuis-NetworkTests-Protocols'!
TestCase subclass: #HTTPSocketTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-NetworkTests-Protocols'!
!classDefinition: 'HTTPSocketTest class' category: #'Cuis-NetworkTests-Protocols'!
HTTPSocketTest class
	instanceVariableNames: ''!

!classDefinition: #MockSocketStream category: #'Cuis-NetworkTests-Protocols'!
Stream subclass: #MockSocketStream
	instanceVariableNames: 'atEnd inStream outStream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-NetworkTests-Protocols'!
!classDefinition: 'MockSocketStream class' category: #'Cuis-NetworkTests-Protocols'!
MockSocketStream class
	instanceVariableNames: ''!

!classDefinition: #SMTPClientTest category: #'Cuis-NetworkTests-Protocols'!
TestCase subclass: #SMTPClientTest
	instanceVariableNames: 'smtp socket'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-NetworkTests-Protocols'!
!classDefinition: 'SMTPClientTest class' category: #'Cuis-NetworkTests-Protocols'!
SMTPClientTest class
	instanceVariableNames: ''!


!MockSocketStream commentStamp: 'gsa 12/7/2012 10:16' prior: 0!
This class in Pharo 1.4 is on NetworkTests-Kernel, but how is only used in tests of SMTPClient I decided to put in this package Cuis-NetworkTests-Protocols. GSA - 07/12/2012!

!HTTPSocketTest methodsFor: 'tests' stamp: 'StephaneDucasse 3/27/2010 22:24'!
expandUrl	"self debug: #expandUrl"		self assert: (HTTPSocket expandUrl: 'www.pharo-project.org' ip: #[127 0 0 1] port: 8080) = 'www.pharo-project.org'.	self assert: (HTTPSocket expandUrl: '../beam' ip: #[127 0 0 1] port: 8080) = '127.0.0.1:8080/beam'.	self assert: (HTTPSocket expandUrl: '/beam' ip: #[127 0 0 1] port: 8080) = '127.0.0.1:8080/beam'.! !

!HTTPSocketTest methodsFor: 'tests' stamp: 'StephaneDucasse 3/27/2010 22:36'!
testExpandUrl	"self debug: #testExpandUrl"		self assert: (HTTPSocket expandUrl: 'www.pharo-project.org' ip: #[127 0 0 1] port: 8080) = 'www.pharo-project.org'.	self assert: (HTTPSocket expandUrl: '../beam' ip: #[127 0 0 1] port: 8080) = '127.0.0.1:8080/beam'.	self assert: (HTTPSocket expandUrl: '/beam' ip: #[127 0 0 1] port: 8080) = '127.0.0.1:8080/beam'.! !

!HTTPSocketTest methodsFor: 'tests' stamp: 'StephaneDucasse 3/27/2010 22:51'!
testIpPortUrlPath	"self debug: #testIpPortUrlPath"		self assert: (HTTPSocket  ip: #[127 0 0 1] port: 8080 urlPath: '/beam') = '127.0.0.1:8080/beam'! !

!MockSocketStream methodsFor: 'testing' stamp: 'fbs 3/22/2004 13:08'!
atEnd	^self inStream atEnd.! !

!MockSocketStream methodsFor: 'accessing' stamp: 'fbs 3/22/2004 12:51'!
atEnd: aBoolean	atEnd := aBoolean.! !

!MockSocketStream methodsFor: 'accessing' stamp: 'fbs 3/22/2004 13:29'!
inStream	^inStream! !

!MockSocketStream methodsFor: 'initialization' stamp: 'alain.plantec 5/28/2009 10:08'!
initialize	super initialize.	self resetInStream.	self resetOutStream.! !

!MockSocketStream methodsFor: 'stream in' stamp: 'fbs 3/22/2004 13:10'!
nextLine	^self nextLineCrLf! !

!MockSocketStream methodsFor: 'stream in' stamp: 'gsa 12/7/2012 17:02'!
nextLineCrLf	^(self upToAll: String crlfString).! !

!MockSocketStream methodsFor: 'accessing' stamp: 'fbs 3/22/2004 13:08'!
outStream	^outStream! !

!MockSocketStream methodsFor: 'stream in' stamp: 'PeterHugossonMiller 9/3/2009 10:05'!
resetInStream	inStream := String new writeStream.! !

!MockSocketStream methodsFor: 'stream out' stamp: 'PeterHugossonMiller 9/3/2009 10:05'!
resetOutStream	outStream := String new writeStream.! !

!MockSocketStream methodsFor: 'stream out' stamp: 'gsa 12/7/2012 17:01'!
sendCommand: aString	self outStream		nextPutAll: aString;		nextPutAll: String crlfString.! !

!MockSocketStream methodsFor: 'stream in' stamp: 'fbs 3/22/2004 13:09'!
upToAll: delims	^self inStream upToAll: delims.! !

!MockSocketStream class methodsFor: 'instance creation' stamp: 'fbs 3/22/2004 12:46'!
on: socket	^self basicNew initialize! !

!SMTPClientTest methodsFor: 'running' stamp: 'fbs 3/22/2004 13:11'!
setUp	socket := MockSocketStream on: ''.	smtp := SMTPClient new.	smtp stream: socket.! !

!SMTPClientTest methodsFor: 'testing' stamp: 'gsa 12/7/2012 17:03'!
testMailFrom
	smtp mailFrom: 'frank@angband.za.org'.
	self assert: socket outStream contents = ('MAIL FROM: <frank@angband.za.org>', String crlfString).
	
	socket resetOutStream.
	smtp mailFrom: '<frank@angband.za.org>'.
	self assert: socket outStream contents = ('MAIL FROM: <frank@angband.za.org>', String crlfString).
	
	socket resetOutStream.
	smtp mailFrom: 'Frank <frank@angband.za.org>'.
	self assert: socket outStream contents = ('MAIL FROM: <frank@angband.za.org>', String crlfString).! !
