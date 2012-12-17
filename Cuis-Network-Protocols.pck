'From Cuis 4.1 of 12 December 2012 [latest update: #1517] on 17 December 2012 at 7:07:59 pm'!
'Description Please enter a description for this package '!
!classDefinition: #FTPConnectionException category: #'Cuis-Network-Protocols'!
Error subclass: #FTPConnectionException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Protocols'!
!classDefinition: 'FTPConnectionException class' category: #'Cuis-Network-Protocols'!
FTPConnectionException class
	instanceVariableNames: ''!

!classDefinition: #HTTPClient category: #'Cuis-Network-Protocols'!
Object subclass: #HTTPClient
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Protocols'!
!classDefinition: 'HTTPClient class' category: #'Cuis-Network-Protocols'!
HTTPClient class
	instanceVariableNames: ''!

!classDefinition: #HTTPProgress category: #'Cuis-Network-Protocols'!
Notification subclass: #HTTPProgress
	instanceVariableNames: 'total amount'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Protocols'!
!classDefinition: 'HTTPProgress class' category: #'Cuis-Network-Protocols'!
HTTPProgress class
	instanceVariableNames: ''!

!classDefinition: #HTTPSocket category: #'Cuis-Network-Protocols'!
Socket subclass: #HTTPSocket
	instanceVariableNames: 'headerTokens headers responseCode'
	classVariableNames: 'HTTPBlabEmail HTTPProxyCredentials HTTPProxyExceptions HTTPProxyPort HTTPProxyServer ParamDelimiters'
	poolDictionaries: ''
	category: 'Cuis-Network-Protocols'!
!classDefinition: 'HTTPSocket class' category: #'Cuis-Network-Protocols'!
HTTPSocket class
	instanceVariableNames: ''!

!classDefinition: #ProtocolClient category: #'Cuis-Network-Protocols'!
Object subclass: #ProtocolClient
	instanceVariableNames: 'stream connectInfo lastResponse pendingResponses progressObservers'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Protocols'!
!classDefinition: 'ProtocolClient class' category: #'Cuis-Network-Protocols'!
ProtocolClient class
	instanceVariableNames: ''!

!classDefinition: #POP3Client category: #'Cuis-Network-Protocols'!
ProtocolClient subclass: #POP3Client
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Protocols'!
!classDefinition: 'POP3Client class' category: #'Cuis-Network-Protocols'!
POP3Client class
	instanceVariableNames: ''!

!classDefinition: #ProtocolClientError category: #'Cuis-Network-Protocols'!
Error subclass: #ProtocolClientError
	instanceVariableNames: 'protocolInstance'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Protocols'!
!classDefinition: 'ProtocolClientError class' category: #'Cuis-Network-Protocols'!
ProtocolClientError class
	instanceVariableNames: ''!

!classDefinition: #LoginFailedException category: #'Cuis-Network-Protocols'!
ProtocolClientError subclass: #LoginFailedException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Protocols'!
!classDefinition: 'LoginFailedException class' category: #'Cuis-Network-Protocols'!
LoginFailedException class
	instanceVariableNames: ''!

!classDefinition: #POP3LoginError category: #'Cuis-Network-Protocols'!
ProtocolClientError subclass: #POP3LoginError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Protocols'!
!classDefinition: 'POP3LoginError class' category: #'Cuis-Network-Protocols'!
POP3LoginError class
	instanceVariableNames: ''!

!classDefinition: #TelnetProtocolClient category: #'Cuis-Network-Protocols'!
ProtocolClient subclass: #TelnetProtocolClient
	instanceVariableNames: 'responseCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Protocols'!
!classDefinition: 'TelnetProtocolClient class' category: #'Cuis-Network-Protocols'!
TelnetProtocolClient class
	instanceVariableNames: ''!

!classDefinition: #FTPClient category: #'Cuis-Network-Protocols'!
TelnetProtocolClient subclass: #FTPClient
	instanceVariableNames: 'dataSocket'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Protocols'!
!classDefinition: 'FTPClient class' category: #'Cuis-Network-Protocols'!
FTPClient class
	instanceVariableNames: ''!

!classDefinition: #SMTPClient category: #'Cuis-Network-Protocols'!
TelnetProtocolClient subclass: #SMTPClient
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Protocols'!
!classDefinition: 'SMTPClient class' category: #'Cuis-Network-Protocols'!
SMTPClient class
	instanceVariableNames: ''!

!classDefinition: #TelnetProtocolError category: #'Cuis-Network-Protocols'!
ProtocolClientError subclass: #TelnetProtocolError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Protocols'!
!classDefinition: 'TelnetProtocolError class' category: #'Cuis-Network-Protocols'!
TelnetProtocolError class
	instanceVariableNames: ''!


!FTPClient commentStamp: '<historical>' prior: 0!
A minimal FTP client program.  Could store all state in inst vars, and use an instance to represent the full state of a connection in progress.  But simpler to do all that in one method and have it be a complete transaction.Always operates in passive mode (PASV).  All connections are initiated from client in order to get through firewalls.See ServerDirectory openFTP, ServerDirectory getFileNamed:, ServerDirectory putFile:named: for examples of use.See TCP/IP, second edition, by Dr. Sidnie Feit, McGraw-Hill, 1997, Chapter 14, p311.!

!HTTPClient commentStamp: '<historical>' prior: 0!
I'm a facade for doing simple HTTP GET/POST operations like downloading / uploading some content.

For example, I get HTML content for http://www.pharo-project.org web page like this:

      HTTPClient httpGet: 'http://www.pharo-project.org'.
      HTTPClient httpGetDocument: 'http://www.pharo-project.org'.

When successful, a Stream respectively a MIMEDocument is returned, in case of error, a String is returned.

Or, I can make a application/x-www-form-urlencoded post request to http://intranet.acme.com/login and send form data to the server like this:

      (args := Dictionary new)
		at: 'username' put: #('john@acme.com');
		at: 'password' put: #('secretpassword').
	result := HTTPClient httpPostDocument: 'http://intranet.acme.com/login' args: args.

Alternatively, I can do a multipart/formdata post request to http://intranet.acme.com/files and send a file like this:

      (args := Dictionary new)
		at: 'file' put: (Array with: (MIMEDocument contents: 'This is a test' mimeType: 'text/plain' uri: 'file:///test.txt').
	result := HTTPClient httpPostDocument: 'http://intranet.acme.com/files' args: args.
	
Pay attention, my code is not really good.

<trollmode>
I'm a good example of how not to write code. Turn around, walk away,
fast. I'm fully of bugs.

Somebody at some point thought that HTTP is a simple "hacker with
telnet"-protocol. It's not.
</trollmode>

[COTDC] 3 - Laurent Laffont, Francisco Ortiz Peñaloza, Sven Van Caekenberghe, Philippe Marschall.!

!HTTPProgress commentStamp: '<historical>' prior: 0!
A notification to show progress when using HTTP.I include- total: The total size of the download (if known)- amount: The completed amount of the download (if known)!

!HTTPSocket commentStamp: '<historical>' prior: 0!
HTTPSockets support HTTP requests, either directly or via an HTTP proxy server. An HTTPSocket saves the parse of the last ASCII header it saw, to avoid having to parse it repeatedly.The real action is in httpGet:accept:.  See the examples in the class, especially httpFileInNewChangeSet: and httpShowGif:.!

!LoginFailedException commentStamp: '<historical>' prior: 0!
Exception for signaling login failures of protocol clients.!

!POP3Client commentStamp: '<historical>' prior: 0!
This class implements POP3 (Post Office Protocol 3) as specified in RFC 1939.  (see http://www.ietf.org/rfc.html)You can use it to download email from the mail server to your personal mail program.To see an example of it's use, see POPSocket class>>example.!

!POP3LoginError commentStamp: '<historical>' prior: 0!
Exception for signaling POP3 login failures.!

!ProtocolClient commentStamp: '<historical>' prior: 0!
ProtocolClient is the abstract super class for a variety of network protocol clients.It uses a stream rather than the direct network access so it could also work for streams on serial connections etc.Structure:	stream				stream representing the connection to and from the server	connectInfo			information required for opening a connection	lastResponse			remembers the last response from the server.	progressObservers 	any object understanding #show: can be registered as a progress observer (login, transfer, etc)!

!ProtocolClientError commentStamp: '<historical>' prior: 0!
Abstract super class for protocol clients	protocolInstance		reference to the protocol client throughing the exception. Exception handlers can access the client in order close, respond or whatever may be appropriate!

!SMTPClient commentStamp: '<historical>' prior: 0!
This class implements the SMTP (mail sending) protocol specified in RFC 821.HELO <SP> <domain> <CRLF>MAIL <SP> FROM:<reverse-path> <CRLF>RCPT <SP> TO:<forward-path> <CRLF>DATA <CRLF>RSET <CRLF>SEND <SP> FROM:<reverse-path> <CRLF>SOML <SP> FROM:<reverse-path> <CRLF>SAML <SP> FROM:<reverse-path> <CRLF>VRFY <SP> <string> <CRLF>EXPN <SP> <string> <CRLF>HELP [<SP> <string>] <CRLF>NOOP <CRLF>QUIT <CRLF>TURN <CRLF>!

!TelnetProtocolClient commentStamp: '<historical>' prior: 0!
Abstract super class for protocol clients based on the generic telnet protocol "<response code> <response>"Structure:	responseCode	the numerical (integer) value of the last response code!

!TelnetProtocolError commentStamp: '<historical>' prior: 0!
Abstract super class for exceptions signalled by clients based on the telnet protocol.!

!FTPClient methodsFor: 'protocol' stamp: 'mir 2/13/2002 18:05'!
abortDataConnection	self sendCommand: 'ABOR'.	self closeDataSocket! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 3/7/2002 13:36'!
ascii	self sendCommand: 'TYPE A'.	self lookForCode: 200! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 3/7/2002 13:36'!
binary	self sendCommand: 'TYPE I'.	self lookForCode: 200! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 11/13/2002 17:52'!
changeDirectoryTo: newDirName	self sendCommand: 'CWD ' , newDirName.	self checkResponse.! !

!FTPClient methodsFor: 'private' stamp: 'mir 2/19/2002 18:27'!
closeDataSocket	self dataSocket		ifNotNil: [			self dataSocket closeAndDestroy.			self dataSocket: nil]! !

!FTPClient methodsFor: 'private' stamp: 'mir 10/31/2000 16:24'!
dataSocket	^dataSocket! !

!FTPClient methodsFor: 'private' stamp: 'mir 10/31/2000 18:23'!
dataSocket: aSocket	dataSocket := aSocket! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 11/19/2002 17:11'!
deleteDirectory: dirName	self sendCommand: 'RMD ' , dirName.	self checkResponse.! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 11/19/2002 17:12'!
deleteFileNamed: fileName	self sendCommand: 'DELE ' , fileName.	self checkResponse.! !

!FTPClient methodsFor: 'private protocol' stamp: 'svp 10/28/2003 11:06'!
get: limit dataInto: dataStream	"Reel in data until the server closes the connection or the limit is reached.	At the same time, watch for errors on otherSocket."	| buf bytesRead currentlyRead |	currentlyRead := 0.	buf := String new: 4000.	[currentlyRead < limit and: 	[self dataSocket isConnected or: [self dataSocket dataAvailable]]]		whileTrue: [			self checkForPendingError.			bytesRead := self dataSocket receiveDataWithTimeoutInto: buf.			1 to: (bytesRead min: (limit - currentlyRead)) do: [:ii | dataStream nextPut: (buf at: ii)].			currentlyRead := currentlyRead + bytesRead].	dataStream reset.	"position: 0."	^ dataStream! !

!FTPClient methodsFor: 'private protocol' stamp: 'mir 2/13/2002 18:06'!
getData	| dataStream |	dataStream := RWBinaryOrTextStream on: (String new: 4000).	self getDataInto: dataStream.	self closeDataSocket.	^dataStream contents! !

!FTPClient methodsFor: 'private protocol' stamp: 'svp 10/28/2003 11:04'!
getDataInto: dataStream	"Reel in all data until the server closes the connection.  At the same time, watch for errors on otherSocket.  Don't know how much is coming.  Put the data on the stream."	| buf bytesRead |	buf := String new: 4000.	[self dataSocket isConnected or: [self dataSocket dataAvailable]]		whileTrue: [			self checkForPendingError.			bytesRead := self dataSocket receiveDataWithTimeoutInto: buf.			1 to: bytesRead do: [:ii | dataStream nextPut: (buf at: ii)]].	dataStream reset.	"position: 0."	^ dataStream! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 2/20/2002 13:53'!
getDirectory	| dirList |	self openPassiveDataConnection.	self sendCommand: 'LIST'.	dirList := self getData.	self checkResponse.	self checkResponse.	^dirList! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 11/19/2002 16:50'!
getFileList	| dirList |	self openPassiveDataConnection.	self sendCommand: 'NLST'.	dirList := self getData.	self checkResponse.	self checkResponse.	^dirList! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 11/19/2002 19:23'!
getFileNamed: remoteFileName	| data |	self openPassiveDataConnection.	self sendCommand: 'RETR ', remoteFileName.	[self checkResponse]		on: TelnetProtocolError		do: [:ex |			self closeDataSocket.			ex pass].	data := self getData.	self checkResponse.	^data! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 5/9/2003 15:50'!
getFileNamed: remoteFileName into: dataStream	self openPassiveDataConnection.	self sendCommand: 'RETR ', remoteFileName.	[self checkResponse]		on: TelnetProtocolError		do: [:ex |			self closeDataSocket.			ex pass].	self getDataInto: dataStream.	self closeDataSocket.	self checkResponse! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 10/31/2000 19:03'!
getPartial: limit fileNamed: remoteFileName into: dataStream	| data |	self openPassiveDataConnection.	self sendCommand: 'RETR ', remoteFileName.	[self checkResponse]		on: TelnetProtocolError		do: [:ex |			self closeDataSocket.			ex pass].	data := self get: limit dataInto: dataStream.	self abortDataConnection.	^data! !

!FTPClient methodsFor: 'private' stamp: 'mir 4/7/2003 17:20'!
login	self user ifNil: [^self].	["repeat both USER and PASS since some servers require it"	self sendCommand: 'USER ', self user.	"331 Password required"	self lookForCode: 331.	"will ask user, if needed"	self sendCommand: 'PASS ', self password.	"230 User logged in"	([self lookForCode: 230.]		on: TelnetProtocolError		do: [false]) == false		] whileTrue: [			(LoginFailedException protocolInstance: self) signal: self lastResponse]! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 11/12/2002 18:39'!
loginUser: userName password: passwdString	self user: userName.	self password: passwdString.	self login! !

!FTPClient methodsFor: 'private protocol' stamp: 'LucFabresse 11/2/2010 22:10'!
lookForCode: code ifDifferent: handleBlock 	"We are expecting a certain numeric code next.  	However, in the FTP protocol, multiple lines are allowed.  	If the response is multi-line, the fourth character of the first line is a  	$- and the last line repeats the numeric code but the code is followed by 	a space. So it's possible that there are more lines left of the last response that 	we need to throw away. We use peekForAll: so that we don't discard the	next response that is not a continuation line."		"check for multi-line response"	(self lastResponse size > 3			and: [(self lastResponse at: 4) = $-])		ifTrue: ["Discard continuation lines."			 | headToDiscard |			headToDiscard := self lastResponse first: 4.			[[self stream peekForAll: headToDiscard]				whileTrue: [self stream nextLine]]				on: Exception				do: [:ex | ^handleBlock value: nil]].	^ super lookForCode: code ifDifferent: handleBlock! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 11/19/2002 17:10'!
makeDirectory: newDirName	self sendCommand: 'MKD ' , newDirName.	self checkResponse.! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 11/14/2002 17:51'!
openDataSocket: remoteHostAddress port: dataPort	dataSocket := Socket new.	dataSocket connectTo: remoteHostAddress port: dataPort! !

!FTPClient methodsFor: 'private protocol' stamp: 'michael.rueger 6/16/2009 11:28'!
openPassiveDataConnection	| portInfo list dataPort remoteHostAddress remoteAddressString |	self sendCommand: 'PASV'.	self lookForCode: 227 ifDifferent: [:response | (TelnetProtocolError protocolInstance: self) signal: 'Could not enter passive mode: ' , response].	portInfo := (self lastResponse findTokens: '()') at: 2.	list := portInfo findTokens: ','.	remoteHostAddress := ByteArray		with: (list at: 1) asNumber		with: (list at: 2) asNumber		with: (list at: 3) asNumber		with: (list at: 4) asNumber.	remoteAddressString := String streamContents: [:addrStream | remoteHostAddress		do: [ :each | each printOn: addrStream ]		separatedBy: [ addrStream nextPut: $. ]]. 	dataPort := (list at: 5) asNumber * 256 + (list at: 6) asNumber.	self openDataSocket: (NetNameResolver addressForName: remoteAddressString) port: dataPort! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 11/14/2002 16:55'!
passive	self sendCommand: 'PASV'.	self lookForCode: 227! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 11/19/2002 16:54'!
putFileNamed: filePath as: fileNameOnServer	"FTP a file to the server."	| fileStream |	fileStream := FileStream readOnlyFileNamed: filePath.	fileStream		ifNil: [(FileDoesNotExistException fileName: filePath) signal].	self putFileStreamContents: fileStream as: fileNameOnServer! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 12/8/2003 16:54'!
putFileStreamContents: fileStream as: fileNameOnServer	"FTP a file to the server."	self openPassiveDataConnection.	self sendCommand: 'STOR ', fileNameOnServer.	fileStream reset.	[self sendStreamContents: fileStream]		ensure: [self closeDataSocket].	self checkResponse.	self checkResponse.! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 11/14/2002 16:43'!
pwd	| result |	self sendCommand: 'PWD'.	self lookForCode: 257.	result := self lastResponse.	^result copyFrom: (result indexOf: $")+1 to: (result lastIndexOf: $")-1! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 10/31/2000 13:12'!
quit	self sendCommand: 'QUIT'.	self close! !

!FTPClient methodsFor: 'protocol' stamp: 'mir 11/13/2002 17:50'!
removeFileNamed: remoteFileName	self sendCommand: 'DELE ', remoteFileName.	self checkResponse.! !

!FTPClient methodsFor: 'protocol' stamp: 'nk 1/26/2005 16:40'!
renameFileNamed: oldFileName to: newFileName	self sendCommand: 'RNFR ' , oldFileName.	self lookForCode: 350.	self sendCommand: 'RNTO ' , newFileName.	self lookForCode: 250! !

!FTPClient methodsFor: 'private' stamp: 'mir 11/14/2002 18:14'!
sendStreamContents: aStream	self dataSocket sendStreamContents: aStream checkBlock: [self checkForPendingError. true]! !

!FTPClient class methodsFor: 'accessing' stamp: 'mir 10/30/2000 20:10'!
defaultPortNumber	^21! !

!FTPClient class methodsFor: 'accessing' stamp: 'mir 2/25/2002 19:08'!
logFlag	^#ftp! !

!FTPClient class methodsFor: 'accessing' stamp: 'mir 2/13/2002 17:50'!
rawResponseCodes	#(200 'Command okay.'	500 'Syntax error, command unrecognized. This may include errors such as command line too long.'	501 'Syntax error in parameters or arguments.'	202 'Command not implemented, superfluous at this site.'	502 'Command not implemented.'	503 'Bad sequence of commands.'	504 'Command not implemented for that parameter.'	110 'Restart marker reply. In this case, the text is exact and not left to the particular implementation; it must read: MARK yyyy = mmmm Where yyyy is User-process data stream marker, and mmmm server''s equivalent marker (note the spaces between markers and "=").'	211 'System status, or system help reply.'	212 'Directory status.'	213 'File status.'	214 'Help message. On how to use the server or the meaning of a particular non-standard command. This reply is useful only to the human user.'	215 'NAME system type. Where NAME is an official system name from the list in the Assigned Numbers document.'	120 'Service ready in nnn minutes.'	220 'Service ready for new user.'	221 'Service closing control connection. Logged out if appropriate.'	421 'Service not available, closing control connection. This may be a reply to any command if the service knows it must shut down.'	125 'Data connection already open; transfer starting.'	225 'Data connection open; no transfer in progress.'	425 'Can''t open data connection.'	226 'Closing data connection. Requested file action successful (for example, file transfer or file abort).'	426 'Connection closed; transfer aborted.'	227 'Entering Passive Mode (h1,h2,h3,h4,p1,p2).'	230 'User logged in, proceed.'	530 'Not logged in.'	331 'User name okay, need password.'	332 'Need account for login.'	532 'Need account for storing files.'	150 'File status okay; about to open data connection.'	250 'Requested file action okay, completed.'	257 '"PATHNAME" created.'	350 'Requested file action pending further information.'	450 'Requested file action not taken. File unavailable (e.g., file busy).'	550 'Requested action not taken. File unavailable (e.g., file not found, no access).'	451 'Requested action aborted. Local error in processing.'	551 'Requested action aborted. Page type unknown.'	452 'Requested action not taken. Insufficient storage space in system.'	552 'Requested file action aborted. Exceeded storage allocation (for current directory or dataset).'	553 'Requested action not taken. File name not allowed.')! !

!FTPConnectionException methodsFor: 'as yet unclassified' stamp: 'RAA 3/9/2001 07:47'!
defaultAction	self resume! !

!FTPConnectionException methodsFor: 'as yet unclassified' stamp: 'RAA 3/14/2001 15:57'!
isResumable	^true! !

!HTTPClient class methodsFor: 'post/get' stamp: 'nk 8/30/2004 07:50'!
httpGet: url	| document |	document := self httpGetDocument: url.	^(document isString)		ifTrue: [			"strings indicate errors"			document]		ifFalse: [(RWBinaryOrTextStream with: document content) reset]! !

!HTTPClient class methodsFor: 'post/get' stamp: 'MarcusDenker 2/23/2010 16:38'!
httpGetDocument: url	^HTTPSocket httpGetDocument: url! !

!HTTPClient class methodsFor: 'post/get' stamp: 'mir 5/1/2001 15:04'!
httpPostDocument: url args: argsDict	^self httpPostDocument: url target: nil args: argsDict! !

!HTTPClient class methodsFor: 'post/get' stamp: 'MarcusDenker 2/23/2010 16:38'!
httpPostDocument: url target: target args: argsDict	^HTTPSocket httpPostDocument: url  args: argsDict! !

!HTTPClient class methodsFor: 'post/get' stamp: 'MarcusDenker 2/23/2010 16:39'!
httpPostMultipart: url args: argsDict	" do multipart/form-data encoding rather than x-www-urlencoded "	^HTTPSocket httpPostMultipart: url args: argsDict accept: nil request: ''! !

!HTTPClient class methodsFor: 'post/get' stamp: 'MarcusDenker 2/23/2010 16:40'!
requestURL: url target: target	^FileStream requestURL: url target: target! !

!HTTPClient class methodsFor: 'utilities' stamp: 'mir 2/2/2001 17:59'!
uploadFileNamed: aFilename to: baseUrl user: user passwd: passwd	| fileContents remoteFilename |	remoteFilename := (baseUrl endsWith: '/')		ifTrue: [baseUrl , '/' , aFilename]		ifFalse: [baseUrl , aFilename].	fileContents := (StandardFileStream readOnlyFileNamed: aFilename) contentsOfEntireFile.	HTTPSocket httpPut: fileContents to: remoteFilename user: user passwd: passwd! !

!HTTPProgress methodsFor: 'accessing' stamp: 'StephaneDucasse 3/28/2010 21:28'!
amount	"Answer the value of amount"	^ amount! !

!HTTPProgress methodsFor: 'accessing' stamp: 'StephaneDucasse 3/28/2010 21:28'!
amount: anObject	"Set the value of amount"	amount := anObject! !

!HTTPProgress methodsFor: 'accessing' stamp: 'StephaneDucasse 3/28/2010 21:28'!
total	"Answer the value of total"	^ total! !

!HTTPProgress methodsFor: 'accessing' stamp: 'StephaneDucasse 3/28/2010 21:28'!
total: anObject	"Set the value of total"	total := anObject! !

!HTTPSocket methodsFor: 'accessing' stamp: 'SvenVanCaekenberghe 1/8/2012 14:44'!
contentType	| type i |	type := self getHeader: 'content-type' default: nil.	type ifNil: [ ^nil ].	type := type trimBoth.	i := type indexOf: $;.	i = 0 ifTrue: [ ^type ].	^(type copyFrom: 1 to: i-1) trimBoth	! !

!HTTPSocket methodsFor: 'accessing' stamp: 'dc 10/21/2008 08:49'!
contentType: header	"extract the content type from the header.  Content-type: text/plain<cr><lf>,  User may look in headerTokens afterwards."	| this |	headerTokens ifNil: [ headerTokens := header findTokens: ParamDelimiters keep: String cr].	1 to: headerTokens size do: [:ii | 		this := headerTokens at: ii.		(this first asLowercase = $c and: [#('content-type:' 'content type') includes: this asLowercase]) ifTrue: [			^ (headerTokens at: ii+1)]].	^ nil	"not found"! !

!HTTPSocket methodsFor: 'accessing' stamp: 'dc 10/21/2008 08:49'!
contentsLength: header	"extract the data length from the header.  Content-length: 1234<cr><lf>,  User may look in headerTokens afterwards."	| this |	headerTokens := header findTokens: ParamDelimiters keep: String cr.	1 to: headerTokens size do: [:ii | 		this := headerTokens at: ii.		(this first asLowercase = $c and: [this asLowercase = 'content-length:']) ifTrue: [			^ (headerTokens at: ii+1) asNumber]].	^ nil	"not found"! !

!HTTPSocket methodsFor: 'accessing' stamp: 'adrian-lienhard 6/5/2009 22:00'!
getHeader: name 	^self getHeader: name default: nil! !

!HTTPSocket methodsFor: 'accessing' stamp: 'adrian-lienhard 6/5/2009 22:01'!
getHeader: name  default: defaultValue	^headers at: name ifAbsent: [defaultValue]! !

!HTTPSocket methodsFor: 'receiving' stamp: 'asasa 8/31/2010 20:41'!
getResponseUpTo: markerString	"Keep reading until the marker is seen.  Return three parts: header, marker, beginningOfData.  Fails if no marker in first 2000 chars." 	| buf position bytesRead tester mm tries |	buf := String new: 2000.	position := 0.	tester := 1. mm := 1.	tries := 3.	[tester := tester - markerString size + 1 max: 1.  "rewind a little, in case the marker crosses a read boundary"	tester to: position do: [:tt |		(buf at: tt) = (markerString at: mm) ifTrue: [mm := mm + 1] ifFalse: [mm := 1].			"Not totally correct for markers like xx0xx"		mm > markerString size ifTrue: ["got it"			^ Array with: (buf copyFrom: 1 to: tt+1-mm)				with: markerString				with: (buf copyFrom: tt+1 to: position)]].	 tester := 1 max: position.	"OK if mm in the middle"	 (position < buf size) & (self isConnected | self dataAvailable) 			& ((tries := tries - 1) >= 0)] whileTrue: [		self waitForDataFor: 5 ifClosed: [				Transcript show: ' <connection closed> ']			ifTimedOut: [				Transcript show: ' <response was late> '].		bytesRead := self primSocket: socketHandle receiveDataInto: buf 			startingAt: position + 1 count: buf size - position.		position := position + bytesRead].	^ Array with: (buf copyFrom: 1 to: position)		with: ''		with: ''		"Marker not found and connection closed".! !

!HTTPSocket methodsFor: 'receiving' stamp: 'PeterHugossonMiller 9/3/2009 01:58'!
getResponseUpTo: markerString ignoring: ignoreString	"Keep reading, until the marker is seen, skipping characters in ignoreString when      comparing to the marker.  Return three parts: header, marker, beginningOfData.     Fails if no marker in first 2000 chars." 	| buf position bytesRead tester mm skipped |	buf := String new: 2000.	position := 0.	tester := 1. mm := 1.	skipped := 0.	[tester := tester - markerString size + 1 max: 1.  "rewind a little, in case the marker crosses a read boundary"	tester to: position do: [:tt |		(buf at: tt) = (markerString at: mm) ifFalse:			[[ignoreString includes: (markerString at: mm)] whileTrue:				[mm := mm + 1. skipped := skipped + 1]].		(buf at: tt) = (markerString at: mm)			ifTrue: [mm := mm + 1]			ifFalse: [mm := 1. skipped := 0].			"Not totally correct for markers like xx0xx"		mm > markerString size ifTrue: ["got it"			^ Array with: (buf copyFrom: 1 to: tt+1-mm+skipped)				with: markerString				with: (buf copyFrom: tt+1 to: position)]].	 tester := 1 max: position.	"OK if mm in the middle"	 (position < buf size) & (self isConnected | self dataAvailable)] whileTrue: [		self waitForDataFor: 5 ifClosed: [				Transcript show: ' <connection closed> ']			ifTimedOut: [				Transcript show: 'data was late'].		bytesRead := self primSocket: socketHandle receiveDataInto: buf 			startingAt: position + 1 count: buf size - position.		position := position + bytesRead].	^ Array with: (buf copyFrom: 1 to: position)		with: ''		with: ''		"Marker not found and connection closed".! !

!HTTPSocket methodsFor: 'receiving' stamp: 'adrian-lienhard 6/5/2009 22:34'!
getRestOfBuffer: beginning	"We don't know the length.  Keep going until connection is closed.  Part of it has already been received.  Response is of type text, not binary."	| buf response bytesRead |	response := RWBinaryOrTextStream on: (String new: 2000).	response nextPutAll: beginning.	buf := String new: 2000.	[self isConnected | self dataAvailable] 	whileTrue: [		self waitForDataFor: 5 ifClosed: [				Transcript show: ' <connection closed> ']			ifTimedOut: [				Transcript show: 'data was slow'].		bytesRead := self primSocket: socketHandle receiveDataInto: buf 				startingAt: 1 count: buf size. 		bytesRead > 0 ifTrue: [  			response nextPutAll: (buf copyFrom: 1 to: bytesRead)] ].	response reset.	"position: 0."	^ response! !

!HTTPSocket methodsFor: 'receiving' stamp: 'StephaneDucasse 3/28/2010 21:29'!
getRestOfBuffer: beginning totalLength: length	"Reel in a string of a fixed length.  Part of it has already been received.  Close the connection after all chars are received.  We do not strip out linefeed chars.  tk 6/16/97 22:32" 	"if length is nil, read until connection close.  Response is of type text, not binary."	| buf response bytesRead |	length ifNil: [^ self getRestOfBuffer: beginning].	buf := String new: length.	response := RWBinaryOrTextStream on: buf.	response nextPutAll: beginning.	buf := String new: length.	[(response position < length) & (self isConnected | self dataAvailable)] 	whileTrue: [		(HTTPProgress new)			total: length;			amount: response position;			signal: 'Downloading...'.		self waitForDataFor: 5 ifClosed: [				Transcript show: ' <connection closed> ']			ifTimedOut: [				Transcript show: 'data was slow'].		bytesRead := self primSocket: socketHandle receiveDataInto: buf startingAt: 1 				count: (length - response position). 		bytesRead > 0 ifTrue: [  			response nextPutAll: (buf copyFrom: 1 to: bytesRead)] ].	"Transcript cr; show: 'data byte count: ', response position printString."	"Transcript cr; show: ((self isConnected) ifTrue: ['Over length by: ', bytesRead printString] 		ifFalse: ['Socket closed'])."	response position < length ifTrue: [^ 'server aborted early'].	response reset.	"position: 0."	^ response! !

!HTTPSocket methodsFor: 'accessing' stamp: 'SvenVanCaekenberghe 1/8/2012 14:45'!
header: headerText	"set the headers.  Then getHeader: can be used"	"divide into basic lines"	| lines foldedLines statusLine |	lines := headerText findTokens: String crlf.	statusLine := lines first.	lines := lines copyFrom: 2 to: lines size.	"parse the status (pretty trivial right now)"	responseCode := (statusLine findTokens: ' ') second.	"fold lines that start with spaces into the previous line"	foldedLines := OrderedCollection new.	lines do: [ :line |		line first isSeparator ifTrue: [			foldedLines at: foldedLines size  put: (foldedLines last, line) ]		ifFalse: [ foldedLines add: line ] ].	"make a dictionary mapping headers to header contents"	headers := Dictionary new.	foldedLines do: [ :line | | i |		i := line indexOf: $:.		i > 0 ifTrue: [			headers 			at: (line copyFrom: 1 to: i-1) asLowercase 			put: (line copyFrom: i+1 to: line size) trimBoth ] ].! !

!HTTPSocket methodsFor: 'receiving' stamp: 'adrian-lienhard 6/5/2009 22:02'!
redirect	"See if the header has a 'Location: url CrLf' in it.  If so, return the new URL of this page.  tk 6/24/97 18:03"	| this |	1 to: headerTokens size do: [:ii | 		this := headerTokens at: ii.		(this first asLowercase = $l and: [this asLowercase = 'location:']) ifTrue: [			^ (headerTokens at: ii+1)]].	^ nil	"not found"! !

!HTTPSocket methodsFor: 'accessing' stamp: 'ls 8/12/1998 00:41'!
responseCode	^responseCode! !

!HTTPSocket methodsFor: 'accessing' stamp: 'StephaneDucasse 3/28/2010 21:43'!
sendCommandWithProgress: commandString	"Send the given command as a single line followed by a <CR><LF> terminator."	self sendDataWithProgress: commandString, String crlf.! !

!HTTPSocket methodsFor: 'accessing' stamp: 'StephaneDucasse 3/28/2010 21:46'!
sendDataWithProgress: aStringOrByteArray	"Send all of the data in the given array, even if it requires multiple calls to send it all. Return the number of bytes sent."	"An experimental version use on slow lines: Longer timeout and smaller writes to try to avoid spurious timeouts."	| bytesSent bytesToSend count |	bytesToSend := aStringOrByteArray size.	bytesSent := 0.	[bytesSent < bytesToSend] whileTrue: [		"superclass code should probably offer a hook - sd"		(HTTPProgress new)			total: bytesToSend;			amount: bytesSent;			signal: 'Uploading...'.		(self waitForSendDoneFor: 60)			ifFalse: [ConnectionTimedOut signal: 'send data timeout; data not sent'].		count := self primSocket: socketHandle			sendData: aStringOrByteArray			startIndex: bytesSent + 1			count: (bytesToSend - bytesSent min: 5000).		bytesSent := bytesSent + count].	^ bytesSent! !

!HTTPSocket class methodsFor: 'proxy settings' stamp: 'mir 7/30/1999 16:08'!
addProxyException: domainName	"Add a (partial, wildcard) domain name to the list of proxy exceptions"	"HTTPSocket addProxyException: '*.online.disney.com'"	self httpProxyExceptions add: domainName! !

!HTTPSocket class methodsFor: 'utilities' stamp: 'PeterHugossonMiller 9/3/2009 01:44'!
argString: args	"Return the args in a long string, as encoded in a url"	| argsString first |	args isString ifTrue: ["sent in as a string, not a dictionary"		^ (args first = $? ifTrue: [''] ifFalse: ['?']), args].	argsString := String new writeStream.	argsString nextPut: $?.	first := true.	args associationsDo: [ :assoc |		assoc value do: [ :value |			first ifTrue: [ first := false ] ifFalse: [ argsString nextPut: $& ].			argsString nextPutAll: assoc key encodeForHTTP.			argsString nextPut: $=.			argsString nextPutAll: value encodeForHTTP. ] ].	^ argsString contents! !

!HTTPSocket class methodsFor: 'utilities' stamp: 'PeterHugossonMiller 9/3/2009 01:44'!
argStringUnencoded: args	"Return the args in a long string, as encoded in a url"	| argsString first |	args isString ifTrue: ["sent in as a string, not a dictionary"		^ (args first = $? ifTrue: [''] ifFalse: ['?']), args].	argsString := String new writeStream.	argsString nextPut: $?.	first := true.	args associationsDo: [ :assoc |		assoc value do: [ :value |			first ifTrue: [ first := false ] ifFalse: [ argsString nextPut: $& ].			argsString nextPutAll: assoc key.			argsString nextPut: $=.			argsString nextPutAll: value. ] ].	^ argsString contents! !

!HTTPSocket class methodsFor: 'initialization' stamp: 'AlainPlantec 12/3/2009 07:17'!
blabEmail	"Of the form 'From: me@isp.com <crlf>'"	^ HTTPBlabEmail ifNil: [HTTPBlabEmail := 'From: me@isp.com <crlf>']! !

!HTTPSocket class methodsFor: 'initialization' stamp: 'tk 9/21/1998 10:45'!
blabEmail: aRequest	"Of the form 'From: me@isp.com <crlf>'"	HTTPBlabEmail := aRequest! !

!HTTPSocket class methodsFor: 'magic numbers' stamp: 'ls 9/17/1998 07:17'!
defaultPort	"default port to connect on"	^80! !

!HTTPSocket class methodsFor: 'digest' stamp: 'gsa 12/17/2012 19:07'!
digestFor: serverText method: method url: url user: user password: password	"RFC2069"	| sock |	sock := HTTPSocket new. "header decoder is on instance side"	sock header: (serverText readStream upToAll: String crlfString, String crlfString).	^self digestFrom: sock method: method url: url user: user password: password! !

!HTTPSocket class methodsFor: 'digest' stamp: 'damiencassou 2/17/2009 09:54'!
digestFrom: sock method: method url: url user: user password: password	"RFC2069"	| auth fields realm nonce uri a1 a2 response |	sock responseCode = '401' ifFalse: [^nil].	auth := sock getHeader: 'www-authenticate'.	(auth asLowercase beginsWith: 'digest') ifFalse: [^nil].	fields := (((auth allButFirst: 6) findTokens: ', 	') collect: [:ea |		(ea copyUpTo: $=) asLowercase -> (ea copyAfter: $=) withoutQuoting]) as: Dictionary.	realm := fields at: 'realm'.	nonce := fields at: 'nonce'.	uri := url readStream upToAll: '://'; skipTo: $/; skip: -1; upTo: $#.	a1 := self md5Hash: user, ':', realm, ':', password.	a2 := self md5Hash: method, ':', uri.	a1 ifNil: [^nil "no MD5 support"].	response := self md5Hash: a1, ':', nonce, ':', a2.	^String streamContents: [:digest |		digest			nextPutAll: 'username="', user, '"';			nextPutAll: ', realm="', realm, '"';			nextPutAll: ', nonce="', nonce, '"';			nextPutAll: ', uri="', uri, '"';			nextPutAll: ', response="', response, '"'.		fields at: 'opaque' ifPresent: [:opaque |			digest nextPutAll: ', opaque="', opaque, '"'].	]! !

!HTTPSocket class methodsFor: 'utilities' stamp: 'StephaneDucasse 3/27/2010 22:52'!
expandUrl: newUrl ip: byteArrayIP port: portNum	"self expandUrl: 'www.pharo-project.org' ip: #[127 0 0 1] port: 8080"		(newUrl beginsWith: '../') ifTrue: [^self ip: byteArrayIP port: portNum urlPath: (newUrl allButFirst: 2)].	(newUrl beginsWith: '/') ifTrue: [^self ip: byteArrayIP port: portNum urlPath: newUrl].	^newUrl! !

!HTTPSocket class methodsFor: 'get the page' stamp: 'dc 10/21/2008 08:55'!
httpGetNoError: url args: args accept: mimeType	"Return the exact contents of a web file.  Do better error checking.  Asks for the given MIME type.  To fetch raw data, you can use the MIMI type 'application/octet-stream'.  If mimeType is nil, use 'text/html'.  The parsed header is saved. Use a proxy server if one has been registered.""Edited to remove a lineFeed from the source 4/4/99 - di"	| document data |	document := self httpGetDocument: url  args: args  accept: mimeType.	(document isString) ifTrue: [		"strings indicate errors"		^ document ].	data := document content.	(data beginsWith: '<HTML><HEAD>' , String lf , '<TITLE>4')		ifTrue: ["an error message  404 File not found"				^ data copyFrom: 21 to: data size-16].		^ (RWBinaryOrTextStream with: data) reset! !

!HTTPSocket class methodsFor: 'proxy settings' stamp: 'AlainPlantec 12/2/2009 07:09'!
httpProxyExceptions	^ HTTPProxyExceptions ifNil: [HTTPProxyExceptions := OrderedCollection new].! !

!HTTPSocket class methodsFor: 'proxy settings' stamp: 'AlainPlantec 12/2/2009 07:27'!
httpProxyPort	"answer the httpProxyPort"	^ HTTPProxyPort ifNil: [HTTPProxyPort := self defaultPort]! !

!HTTPSocket class methodsFor: 'proxy settings' stamp: 'AlainPlantec 12/2/2009 07:16'!
httpProxyPort: aPortNumber	HTTPProxyPort := aPortNumber! !

!HTTPSocket class methodsFor: 'proxy settings' stamp: 'AlainPlantec 12/2/2009 07:16'!
httpProxyServer	"answer the httpProxyServer. Take into account that as a Preference the Server might appear as an empty string but HTTPSocket expect it to be nil"	^ HTTPProxyServer ifNil: [HTTPProxyServer := '']! !

!HTTPSocket class methodsFor: 'proxy settings' stamp: 'SvenVanCaekenberghe 1/8/2012 14:45'!
httpProxyServer: aStringOrNil	HTTPProxyServer := (aStringOrNil ifNil: [''] ifNotNil: [aStringOrNil trimBoth ])! !

!HTTPSocket class methodsFor: 'utilities' stamp: 'nice 4/28/2009 21:42'!
initHTTPSocket: httpUrl ifError: aBlock	"Retrieve the server and port information from the URL, match it to the proxy settings and open a http socket for the request."	^self initHTTPSocket: httpUrl timeoutSecs: self standardTimeout ifError: aBlock! !

!HTTPSocket class methodsFor: 'utilities' stamp: 'nice 4/29/2009 21:31'!
initHTTPSocket: httpUrl timeoutSecs: timeout ifError: aBlock	"Retrieve the server and port information from the URL, match it to the proxy settings and open a http socket for the request."	| serverName port serverAddr s |	Socket initializeNetwork.	serverName := httpUrl authority.	port := httpUrl port ifNil: [self defaultPort].	(self shouldUseProxy: serverName) ifTrue: [ 		serverName := self httpProxyServer.		port := self httpProxyPort].  	"make the request"		serverAddr := NetNameResolver addressForName: serverName timeout: 20.	serverAddr ifNil: [		aBlock value: 'Error: Could not resolve the server named: ', serverName].	s := HTTPSocket new.	s connectTo: serverAddr port: port.	(s waitForConnectionFor: timeout ifTimedOut: [false]) ifFalse: [		Socket deadServer: httpUrl authority.		s destroy.		^aBlock value: 'Error: Server ',httpUrl authority,' is not responding'].	^s! !

!HTTPSocket class methodsFor: 'initialization' stamp: 'gsa 12/17/2012 19:04'!
initialize	"HTTPSocket initialize"	ParamDelimiters := ' ', String crlfString.	HTTPProxyPort := 80.	self httpProxyServer: nil.	HTTPBlabEmail := ''.  "	'From: somebody@no.where', CrLf	"	HTTPProxyCredentials := ''.! !

!HTTPSocket class methodsFor: 'utilities' stamp: 'StephaneDucasse 3/27/2010 22:51'!
ip: byteArrayIP port: portNum urlPath: urlPathString 	"HTTPSocket  ip: #[127 0 0 1] port: 8080 urlPath: '/beam'"		^String streamContents: [:stream | 		byteArrayIP			do: [:each | each printOn: stream]			separatedBy: [stream nextPut: $.].		stream nextPut: $:.		portNum printOn: stream.		stream nextPutAll: urlPathString]! !

!HTTPSocket class methodsFor: 'digest' stamp: 'lr 3/14/2010 21:13'!
md5Hash: aString	"Answer hash of aString as lowercase 32 digit hex String.	There are several providers of MD5 hash ..."	"(self md5Hash: 'user:realm:passwd') =  '007e68e539ed680c24f6d9a370f3bcb1'"	| hash |	hash := Smalltalk globals at: #CMD5Hasher ifPresent: [ :cls | cls hashMessage: aString ].	hash		ifNil: [ hash := Smalltalk globals at: #TCryptoRandom ifPresent: [ :cls | (cls basicNew md5HashMessage: aString) asInteger ] ].	hash		ifNotNil: [ 			hash := hash hex asLowercase.			(hash beginsWith: '16r')				ifTrue: [ hash := hash allButFirst: 3 ].			hash := hash padded: #left to: 32 with: $0 ].	^ hash! !

!HTTPSocket class methodsFor: 'proxy settings' stamp: 'VeronicaUquillas 6/11/2010 14:46'!
proxyTestingComment	"Test Kevin's SmartCache on this machine"	"	HTTPSocket useProxyServerNamed: '127.0.0.1' port: 8080.		HTTPSocket httpShowPage: 'http://www.disneyblast.com/default.html'.		HTTPSocket stopUsingProxyServer.	"	"Test getting to outside world from DOL"	"	HTTPSocket useProxyServerNamed: 'web-proxy.online.disney.com' port: 8080.		HTTPSocket httpShowPage: 'http://www.apple.com/default.html'.		HTTPSocket stopUsingProxyServer.	"	"Test Windows Machine in our cubicle at DOL"	"	HTTPSocket useProxyServerNamed: '206.18.67.150' port: 8080.		HTTPSocket httpShowPage: 'http://....'.		HTTPSocket stopUsingProxyServer.	"	"	HTTPSocket httpShowPage: 'kids.online.disney.com/'	"	"	HTTPSocket httpShowGif: 'kids.online.disney.com/~kevin/images/dlogo.gif'	"! !

!HTTPSocket class methodsFor: 'proxy settings' stamp: 'gsa 12/17/2012 19:06'!
proxyUser: userName password: password  "Store  HTTP 1.0 basic authentication credentials  Note: this is an ugly hack that stores your password   in your image.  It's just enought to get you going  if you use a firewall that requires authentication"  | stream encodedStream |  stream := (userName ,':' , password) readStream.  encodedStream := Base64MimeConverter mimeEncode: stream.  HTTPProxyCredentials := 'Proxy-Authorization: Basic ' , (encodedStream contents) , String crlfString! !

!HTTPSocket class methodsFor: 'proxy settings' stamp: 'mir 7/30/1999 15:03'!
removeProxyException: domainName	"Remove a (partial, wildcard) domain name from the list of proxy exceptions"	self httpProxyExceptions remove: domainName ifAbsent: []! !

!HTTPSocket class methodsFor: 'utilities' stamp: 'hpt 12/10/2004 23:21'!
shouldUseProxy: serverName	"Retrieve the server and port information from the URL, match it to the proxy settings and open a http socket for the request."	self httpProxyServer ifNotEmpty: [		self httpProxyExceptions			detect: [:domainName | domainName match: serverName]			ifNone: [^true]].	^false! !

!HTTPSocket class methodsFor: 'proxy settings' stamp: 'hpt 12/9/2004 22:55'!
stopUsingProxyServer	"Stop directing HTTP request through a proxy server."	self httpProxyServer: nil.	self httpProxyPort: 80.	HTTPProxyCredentials := ''! !

!HTTPSocket class methodsFor: 'proxy settings' stamp: 'AlainPlantec 12/3/2009 14:13'!
useProxyServerNamed: proxyServerName port: portNum	"Direct all HTTP requests to the HTTP proxy server with the given name and port number."	proxyServerName ifNil: [  "clear proxy settings"		self httpProxyServer: nil.		self httpProxyPort: 80.		^ self].	proxyServerName isString		ifFalse: [self error: 'Server name must be a String or nil'].	self httpProxyServer: proxyServerName.	self httpProxyPort: portNum.	self httpProxyPort class == String ifTrue: [HTTPProxyPort := portNum asNumber].	self httpProxyPort ifNil: [self httpProxyPort: self defaultPort].! !

!HTTPSocket class methodsFor: 'proxy settings' stamp: 'al 1/8/2004 12:54'!
useProxyServerNamed: proxyServerName port: portNum proxyUser: aString password: anotherString	self useProxyServerNamed: proxyServerName port: portNum.	self proxyUser: aString password: anotherString! !

!HTTPSocket class methodsFor: 'utilities' stamp: 'tak 9/25/2008 15:09'!
userAgentString 	"self userAgentString."	^'User-Agent: ',		SystemVersion current version, '-', 		SystemVersion current highestUpdate printString! !

!LoginFailedException methodsFor: 'exceptiondescription' stamp: 'mir 2/15/2002 13:10'!
isResumable	"Resumable so we can give the user another chance to login"	^true! !

!POP3Client methodsFor: 'private protocol' stamp: 'lr 3/14/2010 21:13'!
apopLogin	"Attempt to authenticate ourselves to the server without sending the password as cleartext."	"For secure authentication, we look for a timestamp in the initial response string we get from the server, and then try the APOP command as specified in RFC 1939.  If the initial response from the server is	+OK POP3 server ready <1896.697170952@dbc.mtview.ca.us>we extract the timestamp	<1896.697170952@dbc.mtview.ca.us>then form a string of the form	<1896.697170952@dbc.mtview.ca.us>USERPASSWORDand then send only the MD5 hash of that to the server.  Thus the password never hits the wire"	[ 	| hash timestamp |	"Look for a timestamp in the response we received from the server"	timestamp := self lastResponse findTokens: '<>' includes: '@'.	timestamp ifNil: [ (POP3LoginError protocolInstance: self) signal: 'APOP not supported.' ].	(Smalltalk globals includesKey: #MD5)		ifTrue: [ 			hash := ((Smalltalk globals at: #MD5) hashMessage: '<' , timestamp , '>' , self password) storeStringHex asLowercase.	"trim starting 16r and zero pad it to 32 characters if needed"			hash := hash padded: #left to: 32 with: $0 ]		ifFalse: [ (POP3LoginError protocolInstance: self) signal: 'APOP (MD5) not supported.' ].	self sendCommand: 'APOP ' , self user , ' ' , hash.	self checkResponse.	self logProgress: self lastResponse ]		on: ProtocolClientError		do: [ :ex | 			self close.			(LoginFailedException protocolInstance: self) signal: 'Login failed.' ]! !

!POP3Client methodsFor: 'public protocol' stamp: 'mir 3/7/2002 14:58'!
apopLoginUser: userName password: password	self loginUser: userName password: password loginMethod: #APOP! !

!POP3Client methodsFor: 'private protocol' stamp: 'mir 4/7/2003 17:38'!
clearTextLogin	[self sendCommand: 'USER ', self user.	self checkResponse.	self logProgress: self lastResponse.	self sendCommand: 'PASS ', self password.	self checkResponse.	self logProgress: self lastResponse]		on: TelnetProtocolError		do: [:ex |			"Neither authentication worked.  Indicate an error and close up"			self close.			ex resignalAs: ((LoginFailedException protocolInstance: self) signal: 'Login failed.')]! !

!POP3Client methodsFor: 'public protocol' stamp: 'mir 3/7/2002 14:35'!
deleteMessage: num	"delete the numbered message"	self ensureConnection.	self sendCommand: 'DELE ', num printString.	self checkResponse.	self logProgress: self lastResponse! !

!POP3Client methodsFor: 'private protocol' stamp: 'PeterHugossonMiller 9/3/2009 10:12'!
getMultilineResponse	"Get a multiple line response to the last command, filtering out LF characters. A multiple line response ends with a line containing only a single period (.) character."	| response done chunk |	response := String new writeStream.	done := false.	[done] whileFalse: [		chunk := self stream nextLine.		(chunk beginsWith: '.')			ifTrue: [response nextPutAll: (chunk copyFrom: 2 to: chunk size); cr ]			ifFalse: [response nextPutAll: chunk; cr ].		done := (chunk = '.') ].	^ response contents! !

!POP3Client methodsFor: 'private protocol' stamp: 'mir 4/7/2003 17:39'!
login	self loginMethod		ifNil: [^self].	self loginMethod == #clearText		ifTrue: [^self clearTextLogin].	self loginMethod == #APOP		ifTrue: [^self apopLogin].	(POP3LoginError protocolInstance: self) signal: 'Unsupported login procedure.'! !

!POP3Client methodsFor: 'private' stamp: 'mir 11/11/2002 16:20'!
loginMethod	^self connectionInfo at: #loginMethod ifAbsent: [nil]! !

!POP3Client methodsFor: 'private' stamp: 'mir 3/8/2002 11:41'!
loginMethod: aSymbol	^self connectionInfo at: #loginMethod put: aSymbol! !

!POP3Client methodsFor: 'public protocol' stamp: 'mir 3/7/2002 14:57'!
loginUser: userName password: password	self loginUser: userName password: password loginMethod: #clearText! !

!POP3Client methodsFor: 'public protocol' stamp: 'mir 3/8/2002 11:40'!
loginUser: userName password: password loginMethod: aLoginMethod	self user: userName.	self password: password.	self loginMethod: aLoginMethod.	self login! !

!POP3Client methodsFor: 'public protocol' stamp: 'nice 1/5/2010 15:59'!
messageCount	"Query the server and answer the number of messages that are in the user's mailbox."	| numMessages |	self ensureConnection.	self sendCommand: 'STAT'.	self checkResponse.	self logProgress: self lastResponse.	[ | answerString |answerString := (self lastResponse findTokens: Character separators) second.	numMessages := answerString asNumber asInteger]		on: Error		do: [:ex | (ProtocolClientError protocolInstance: self) signal: 'Invalid STAT response.'].	^numMessages! !

!POP3Client methodsFor: 'public protocol' stamp: 'len 12/14/2002 17:50'!
quit	"QUIT <CRLF>"	self sendCommand: 'QUIT'.	self checkResponse.! !

!POP3Client methodsFor: 'private testing' stamp: 'mir 3/7/2002 13:43'!
responseIsError	^self lastResponse beginsWith: '-'! !

!POP3Client methodsFor: 'private testing' stamp: 'mir 11/11/2002 15:44'!
responseIsWarning	^self lastResponse beginsWith: '-'! !

!POP3Client methodsFor: 'public protocol' stamp: 'mir 3/7/2002 14:35'!
retrieveMessage: number	"retrieve the numbered message"	self ensureConnection.	self sendCommand: 'RETR ', number printString.	self checkResponse.	self logProgress: self lastResponse.	^self getMultilineResponse! !

!POP3Client class methodsFor: 'accessing' stamp: 'mir 3/7/2002 12:51'!
defaultPortNumber	^110! !

!POP3Client class methodsFor: 'example' stamp: 'rbb 3/1/2005 11:05'!
example	"POP3Client example"	"download a user's messages into an OrderedCollection and inspect the OrderedCollection"	| ps messages userName password |	userName := (UIManager default request: 'POP username').	password := (UIManager default request: 'POP password').	ps := POP3Client openOnHostNamed: (UIManager default request: 'POP server').	[	ps loginUser: userName password: password.	ps logProgressToTranscript.	messages := OrderedCollection new.	1 to: ps messageCount do: [ :messageNr |		messages add: (ps retrieveMessage: messageNr) ]]		ensure: [ps close].	messages inspect.! !

!POP3Client class methodsFor: 'accessing' stamp: 'mir 3/7/2002 12:52'!
logFlag	^#pop! !

!ProtocolClient methodsFor: 'private protocol' stamp: 'mir 7/23/2003 16:52'!
checkForPendingError	"If data is waiting, check it to catch any error reports.	In case the response is not an error, push it back."	self stream isDataAvailable		ifFalse: [^self].	self fetchNextResponse.	self		checkResponse: self lastResponse		onError: [:response | (TelnetProtocolError protocolInstance: self) signal]		onWarning: [:response | (TelnetProtocolError protocolInstance: self) signal].	"if we get here, it wasn't an error"	self pushResponse: self lastResponse! !

!ProtocolClient methodsFor: 'private protocol' stamp: 'mir 5/9/2003 18:47'!
checkResponse	"Get the response from the server and check for errors."	self		checkResponseOnError: [:response | (TelnetProtocolError protocolInstance: self) signal]		onWarning: [:response | (TelnetProtocolError protocolInstance: self) signal].! !

!ProtocolClient methodsFor: 'private protocol' stamp: 'mir 7/23/2003 16:51'!
checkResponse: aResponse onError: errorBlock onWarning: warningBlock	"Get the response from the server and check for errors. Invoke one of the blocks if an error or warning is encountered. See class comment for classification of error codes."	self responseIsError		ifTrue: [errorBlock value: aResponse].	self responseIsWarning		ifTrue: [warningBlock value: aResponse].! !

!ProtocolClient methodsFor: 'private protocol' stamp: 'mir 7/23/2003 16:54'!
checkResponseOnError: errorBlock onWarning: warningBlock	"Get the response from the server and check for errors. Invoke one of the blocks if an error or warning is encountered. See class comment for classification of error codes."	self fetchPendingResponse.	self checkResponse: self lastResponse onError: errorBlock onWarning: warningBlock! !

!ProtocolClient methodsFor: 'actions' stamp: 'mir 3/7/2002 13:10'!
close	self stream		ifNotNil: [			self stream close.			stream := nil]! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 3/8/2002 11:35'!
connectionInfo	connectInfo ifNil: [connectInfo := Dictionary new].	^connectInfo! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 2/25/2002 19:34'!
defaultPortNumber	^self class defaultPortNumber! !

!ProtocolClient methodsFor: 'private' stamp: 'md 8/14/2005 18:27'!
ensureConnection	self isConnected		ifTrue: [^self].	self stream		ifNotNil: [self stream close].	self stream: (SocketStream openConnectionToHost: self host port: self port).	self checkResponse.	self login! !

!ProtocolClient methodsFor: 'private protocol' stamp: 'mir 3/7/2002 13:16'!
fetchNextResponse	self lastResponse: self stream nextLine! !

!ProtocolClient methodsFor: 'private protocol' stamp: 'mir 7/23/2003 16:50'!
fetchPendingResponse	^pendingResponses		ifNil: [self fetchNextResponse; lastResponse]		ifNotNil: [self popResponse]! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 4/7/2003 16:56'!
host	^self connectionInfo at: #host! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 3/8/2002 11:37'!
host: hostId	^self connectionInfo at: #host put: hostId! !

!ProtocolClient methodsFor: 'testing' stamp: 'mir 3/7/2002 14:33'!
isConnected	^stream notNil		and: [stream isConnected]! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 3/7/2002 13:35'!
lastResponse	^lastResponse! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 3/7/2002 13:35'!
lastResponse: aString	lastResponse := aString.! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 2/25/2002 19:07'!
logFlag	^self class logFlag! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 5/12/2003 18:10'!
logProgress: aString	self progressObservers do: [:each | each show: aString].! !

!ProtocolClient methodsFor: 'accessing' stamp: 'mir 3/7/2002 14:55'!
logProgressToTranscript	self progressObservers add: Transcript! !

!ProtocolClient methodsFor: 'accessing' stamp: 'mir 5/9/2003 15:52'!
messageText	^super messageText		ifNil: [self response]! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 3/8/2002 11:40'!
openOnHost: hostIP port: portNumber	self host: hostIP.	self port: portNumber.	self ensureConnection! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 4/7/2003 16:56'!
password	^self connectionInfo at: #password! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 3/8/2002 11:37'!
password: aString	^self connectionInfo at: #password put: aString! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 7/23/2003 16:45'!
pendingResponses	pendingResponses ifNil: [pendingResponses := OrderedCollection new].	^pendingResponses! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 7/23/2003 16:55'!
popResponse	| pendingResponse |	pendingResponse := self pendingResponses removeFirst.	pendingResponses isEmpty		ifTrue: [pendingResponses := nil].	^pendingResponse! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 4/7/2003 16:57'!
port	^self connectionInfo at: #port! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 3/8/2002 11:38'!
port: aPortNumber	^self connectionInfo at: #port put: aPortNumber! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 3/7/2002 14:54'!
progressObservers	progressObservers ifNil: [progressObservers := OrderedCollection new].	^progressObservers! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 7/23/2003 16:45'!
pushResponse: aResponse	self pendingResponses add: aResponse! !

!ProtocolClient methodsFor: 'actions' stamp: 'mir 3/7/2002 13:11'!
reopen	self ensureConnection! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 3/8/2002 11:35'!
resetConnectionInfo	connectInfo := nil! !

!ProtocolClient methodsFor: 'accessing' stamp: 'mir 5/9/2003 15:52'!
response	^self protocolInstance lastResponse! !

!ProtocolClient methodsFor: 'private testing' stamp: 'mir 3/7/2002 13:42'!
responseIsError	self subclassResponsibility! !

!ProtocolClient methodsFor: 'private testing' stamp: 'mir 3/7/2002 13:42'!
responseIsWarning	self subclassResponsibility! !

!ProtocolClient methodsFor: 'private protocol' stamp: 'mir 5/12/2003 18:10'!
sendCommand: aString	self stream sendCommand: aString.! !

!ProtocolClient methodsFor: 'private protocol' stamp: 'mir 3/5/2002 14:31'!
sendStreamContents: aStream	self stream sendStreamContents: aStream! !

!ProtocolClient methodsFor: 'accessing' stamp: 'mir 2/22/2002 17:33'!
stream	^stream! !

!ProtocolClient methodsFor: 'accessing' stamp: 'mir 2/22/2002 17:33'!
stream: aStream	stream := aStream! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 11/11/2002 16:19'!
user	^self connectionInfo at: #user ifAbsent: [nil]! !

!ProtocolClient methodsFor: 'private' stamp: 'mir 3/8/2002 11:39'!
user: aString	^self connectionInfo at: #user put: aString! !

!ProtocolClient class methodsFor: 'accessing' stamp: 'mir 2/25/2002 16:00'!
defaultPortNumber	self subclassResponsibility! !

!ProtocolClient class methodsFor: 'accessing' stamp: 'mir 2/25/2002 19:07'!
logFlag	self subclassResponsibility! !

!ProtocolClient class methodsFor: 'instance creation' stamp: 'mir 2/25/2002 15:59'!
openOnHost: hostIP port: portNumber	^self new openOnHost: hostIP port: portNumber! !

!ProtocolClient class methodsFor: 'instance creation' stamp: 'gk 3/2/2004 11:10'!
openOnHostNamed: hostName	"If the hostname uses the colon syntax to express a certain portnumber	we use that instead of the default port number."	| i |	i := hostName indexOf: $:.	i = 0 ifTrue: [			^self openOnHostNamed: hostName port: self defaultPortNumber]		ifFalse: [			| s p | 			s := hostName truncateTo: i - 1.			p := (hostName copyFrom: i + 1 to: hostName size) asInteger.			^self openOnHostNamed: s port: p]	! !

!ProtocolClient class methodsFor: 'instance creation' stamp: 'mir 2/25/2002 15:58'!
openOnHostNamed: hostName port: portNumber	| serverIP |	serverIP := NetNameResolver addressForName: hostName timeout: 20.	^self openOnHost: serverIP port: portNumber! !

!ProtocolClient class methodsFor: 'retrieval' stamp: 'mir 3/5/2002 16:21'!
retrieveMIMEDocument: aURI	self subclassResponsibility! !

!ProtocolClientError methodsFor: 'accessing' stamp: 'mir 5/16/2003 11:17'!
messageText	^super messageText		ifNil: [self response]! !

!ProtocolClientError methodsFor: 'accessing' stamp: 'mir 10/30/2000 13:48'!
protocolInstance	^protocolInstance! !

!ProtocolClientError methodsFor: 'accessing' stamp: 'mir 10/30/2000 13:48'!
protocolInstance: aProtocolInstance	protocolInstance := aProtocolInstance! !

!ProtocolClientError methodsFor: 'accessing' stamp: 'mir 5/16/2003 11:18'!
response	^self protocolInstance lastResponse! !

!ProtocolClientError class methodsFor: 'instance creation' stamp: 'mir 10/30/2000 16:15'!
protocolInstance: aProtocolInstance	^self new protocolInstance: aProtocolInstance! !

!SMTPClient methodsFor: 'private protocol' stamp: 'nice 1/5/2010 15:59'!
data: messageData	"send the data of a message"	"DATA <CRLF>"		"inform the server we are sending the message data"	self sendCommand: 'DATA'.	self checkResponse.	"process the data one line at a time"	messageData linesDo:  [ :messageLine | | cookedLine |		cookedLine := messageLine.		(cookedLine beginsWith: '.') ifTrue: [ 			"lines beginning with a dot must have the dot doubled"			cookedLine := '.', cookedLine ].		self sendCommand: cookedLine ].	"inform the server the entire message text has arrived"	self sendCommand: '.'.	self checkResponse.! !

!SMTPClient methodsFor: 'utility' stamp: 'StephaneDucasse 1/3/2010 21:31'!
encodeString: aString 	| str dec out |	str := String new: (aString size * 4 / 3 + 3) ceiling.	dec := Base64MimeConverter new.	dec		mimeStream: (out := str writeStream) ;		dataStream: aString readStream;		mimeEncode.	^out contents! !

!SMTPClient methodsFor: 'private protocol' stamp: 'gk 8/4/2006 15:15'!
initiateSession	"EHLO <SP> <domain> <CRLF>"	self sendCommand: (self useHelo ifTrue:['HELO '] ifFalse: ['EHLO ']) , NetNameResolver localHostName.	self checkResponse.! !

!SMTPClient methodsFor: 'public protocol' stamp: 'StephaneDucasse 1/3/2010 21:34'!
localHostName	"The local host name for purposes of identifying the the server. 	If nil, NetNameResolver localHostName will be used."  	^self connectionInfo at: #localHostName ifAbsent: [NetNameResolver localHostName]! !

!SMTPClient methodsFor: 'public protocol' stamp: 'StephaneDucasse 1/3/2010 21:34'!
localHostName: aString	"The local host name for purposes of identifying the the server. 	If nil, NetNameResolver localHostName will be used."  	^self connectionInfo at: #localHostName put: aString! !

!SMTPClient methodsFor: 'private protocol' stamp: 'MarcusDenker 2/14/2010 20:16'!
login	self initiateSession. "send EHLO first"	self user ifNil: [^self].	self sendCommand: 'AUTH LOGIN ' , (self encodeString: self user).	[self checkResponse]		on: TelnetProtocolError		do: [ :ex | ex isCommandUnrecognized ifTrue: [^ self] ifFalse: [ex pass]].	self sendCommand: (self encodeString: self password).	self checkResponse! !

!SMTPClient methodsFor: 'private protocol' stamp: 'fbs 3/23/2004 17:16'!
mailFrom: fromAddress	" MAIL <SP> FROM:<reverse-path> <CRLF>"	| address |	address := (MailAddressParser addressesIn: fromAddress) first.	self sendCommand: 'MAIL FROM: <', address, '>'.	self checkResponse.! !

!SMTPClient methodsFor: 'public protocol' stamp: 'mir 2/21/2002 15:43'!
mailFrom: sender to: recipientList text: messageText	"deliver this mail to a list of users.  NOTE: the recipient list should be a collection of simple internet style addresses -- no '<>' or '()' stuff"	self mailFrom: sender.	recipientList do: [ :recipient |		self recipient: recipient ].	self data: messageText.! !

!SMTPClient methodsFor: 'private protocol' stamp: 'mir 2/21/2002 17:52'!
quit	"send a QUIT command.  This is polite to do, and indeed some servers might drop messages that don't have an associated QUIT"	"QUIT <CRLF>"	self sendCommand: 'QUIT'.	self checkResponse.! !

!SMTPClient methodsFor: 'private protocol' stamp: 'mir 2/21/2002 17:52'!
recipient: aRecipient	"specify a recipient for the message.  aRecipient should be a bare email address"	"RCPT <SP> TO:<forward-path> <CRLF>"	self sendCommand: 'RCPT TO: <', aRecipient, '>'.	self checkResponse.! !

!SMTPClient methodsFor: 'public protocol' stamp: 'gk 8/4/2006 15:14'!
useHelo	"If client use HELO instead of EHLO. HELO is the old protocol and	an old server may require it instead of EHLO."	^self connectionInfo at: #useHelo ifAbsent: [false]! !

!SMTPClient methodsFor: 'public protocol' stamp: 'gk 8/4/2006 15:14'!
useHelo: aBoolean	"Tell client to use HELO instead of EHLO. HELO is the old protocol and	an old server may require it instead of EHLO."	^self connectionInfo at: #useHelo put: aBoolean! !

!SMTPClient class methodsFor: 'accessing' stamp: 'mir 2/21/2002 17:22'!
defaultPortNumber	^25! !

!SMTPClient class methodsFor: 'sending mail' stamp: 'SeanDeNigris 12/6/2011 14:31'!
deliver: aMailMessage usingServer: aString	"See comment for #deliverMailFrom:to:text:usingServer:"		self deliverMailFrom: aMailMessage from to: aMailMessage recipientList text: aMailMessage text usingServer: aString.! !

!SMTPClient class methodsFor: 'sending mail' stamp: 'SeanDeNigris 12/6/2011 14:32'!
deliverMailFrom: fromAddress to: recipientList text: messageText usingServer: aString	"Deliver a single email to a list of users and then close the connection.  For delivering multiple messages, it is best to create a single connection and send all mail over it	recipientList - a collection of simple internet style addresses -- no '<>' or '()' stuff	aString - can be		- server name and port number e.g. mail.mydomain.com:26		- server name only e.g. mail.mydomain.com, default port is used"	| smtpClient |	smtpClient := self openOnHostNamed: aString.	[smtpClient initiateSession.	smtpClient mailFrom: fromAddress to: recipientList text: messageText.	smtpClient quit]		ensure: [smtpClient close]! !

!SMTPClient class methodsFor: 'example' stamp: 'adrian_lienhard 7/18/2009 16:00'!
example	"SMTPClient example"	self deliverMailFrom: 'm.rueger@acm.org' to: #('m.rueger@acm.org') text:'From: testTo: "not listed"Subject: this is a testHello from Pharo!!'	usingServer: 'smtp.concentric.net'! !

!SMTPClient class methodsFor: 'example' stamp: 'adrian_lienhard 7/18/2009 16:00'!
example2	"SMTPClient example2"	self deliverMailFrom: 'm.rueger@acm.org' to: #('m.rueger@acm.org') text:'Subject: this is a testHello from Pharo!!'	usingServer: 'smtp.concentric.net'! !

!SMTPClient class methodsFor: 'accessing' stamp: 'mir 2/25/2002 19:07'!
logFlag	^#smtp! !

!TelnetProtocolClient methodsFor: 'private' stamp: 'mir 2/22/2002 17:34'!
determineResponseCode	self lastResponse size >= 3		ifFalse: [^0].	^[SmallInteger readFromString: (self lastResponse copyFrom: 1 to: 3)]		on: Error		do: [:ex | ex return: 0]! !

!TelnetProtocolClient methodsFor: 'private protocol' stamp: 'nk 2/24/2005 18:21'!
fetchNextResponse	"The FTP and similar protocols allow multi-line responses.	If the response is multi-line, the fourth character of the first line is a  	$- and the last line repeats the numeric code but the code is followed by 	a space."	| response result firstLine |	result := '' writeStream.	firstLine := self stream nextLine.	result nextPutAll: firstLine.	(self responseIsContinuation: firstLine) 		ifTrue: 			["continued over multiple lines. Discard continuation lines."						[response := self stream nextLine.			response ifNil: [^nil].			response size > 3 and: 					[(response copyFrom: 1 to: 3) = (firstLine copyFrom: 1 to: 3) 						and: [(response at: 4) = Character space]]] 					whileFalse: 						[result							cr;							nextPutAll: response]].	self lastResponse: result contents! !

!TelnetProtocolClient methodsFor: 'private' stamp: 'mir 11/14/2002 18:27'!
lastResponse: aString	super lastResponse: aString.	responseCode := self determineResponseCode! !

!TelnetProtocolClient methodsFor: 'private protocol' stamp: 'mir 4/7/2003 15:46'!
lookForCode: code	"We are expecting a certain code next."	self		lookForCode: code		ifDifferent: [:response | (TelnetProtocolError protocolInstance: self) signal: response]! !

!TelnetProtocolClient methodsFor: 'private protocol' stamp: 'mir 11/14/2002 16:21'!
lookForCode: code ifDifferent: handleBlock	"We are expecting a certain code next."	self fetchNextResponse.	self responseCode == code		ifFalse: [handleBlock value: self lastResponse]! !

!TelnetProtocolClient methodsFor: 'accessing' stamp: 'mir 2/22/2002 17:33'!
responseCode	^responseCode! !

!TelnetProtocolClient methodsFor: 'private testing' stamp: 'mir 2/22/2002 17:35'!
responseIsContinuation	^(self lastResponse size > 3		and: [(self lastResponse at: 4) == $-])! !

!TelnetProtocolClient methodsFor: 'private testing' stamp: 'mir 11/14/2002 16:18'!
responseIsContinuation: response	^(response size > 3		and: [(response at: 4) == $-])! !

!TelnetProtocolClient methodsFor: 'private testing' stamp: 'mir 2/22/2002 17:35'!
responseIsError	^self responseCode between: 500 and: 599! !

!TelnetProtocolClient methodsFor: 'private testing' stamp: 'mir 2/22/2002 17:35'!
responseIsWarning	^self responseCode between: 400 and: 499! !

!TelnetProtocolClient class methodsFor: 'accessing' stamp: 'mir 2/21/2002 17:21'!
rawResponseCodes	self subclassResponsibility! !

!TelnetProtocolError methodsFor: 'accessing' stamp: 'mir 4/7/2003 16:47'!
code	^self protocolInstance responseCode! !

!TelnetProtocolError methodsFor: 'private' stamp: 'len 12/14/2002 14:15'!
isCommandUnrecognized	^ self code = 500! !
HTTPSocket initialize!
