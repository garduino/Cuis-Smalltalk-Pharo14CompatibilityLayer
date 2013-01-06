'From Cuis 4.1 of 12 December 2012 [latest update: #1549] on 6 January 2013 at 6:18:14 pm'!
'Description Please enter a description for this package '!
!classDefinition: #RemoteFileStream category: #'Cuis-Network-RemoteDirectory'!
RWBinaryOrTextStream subclass: #RemoteFileStream
	instanceVariableNames: 'remoteFile localDataValid'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-RemoteDirectory'!
!classDefinition: 'RemoteFileStream class' category: #'Cuis-Network-RemoteDirectory'!
RemoteFileStream class
	instanceVariableNames: ''!

!classDefinition: #ServerDirectory category: #'Cuis-Network-RemoteDirectory'!
Object subclass: #ServerDirectory
	instanceVariableNames: 'server directory type user passwordHolder group moniker altURL urlObject client loaderUrl keepAlive encodingName'
	classVariableNames: 'Servers'
	poolDictionaries: ''
	category: 'Cuis-Network-RemoteDirectory'!
!classDefinition: 'ServerDirectory class' category: #'Cuis-Network-RemoteDirectory'!
ServerDirectory class
	instanceVariableNames: ''!

!classDefinition: #ServerFile category: #'Cuis-Network-RemoteDirectory'!
ServerDirectory subclass: #ServerFile
	instanceVariableNames: 'fileName rwmode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-RemoteDirectory'!
!classDefinition: 'ServerFile class' category: #'Cuis-Network-RemoteDirectory'!
ServerFile class
	instanceVariableNames: ''!


!RemoteFileStream commentStamp: '<historical>' prior: 0!
An in-memory stream that can be used to fileIn code from the network.  Knows its ServerFile, and thus its name, path, etc.

localDataValid -- false when have never read the file from the server.  Set to true after reading, when my contents has the true data.  When creating a remote file, set localDataValid to true so it will write to server.!

!ServerDirectory commentStamp: '<historical>' prior: 0!
Holds all the information needed to read or write on a directory of an internet server.  I am used for FTP and HTTP (and STMP?  NNTP?).  The password policy is: unless it is a public password (like annomyous), clear all passwords before any snapshot.  There is a way to store passwords on the disk.

server 		'www.disney.com'  or '123.34.56.08' or the ServerDirectory above me 
			(if I am a subdirectory sharing the info in a master directory)
directory 	'ftp/pubs/'  name of my directory within the server or superdirectory.
			(for file://, directory is converted to local delimiters.)
type 		#ftp	what you can do in this directory
user 		'Jones45'
password 	an instance of Password.  
group 		an Association ('group name' -> an array of ServerDirectorys)
			If this first one is down, try the next one.  Store on all of them.  I am in the list.
moniker 	'Main Squeak Directory'  Description of this directory.
altURL		When a FTP server holds some web pages, the altURL of those pages is often
			different from the FTP directory.  Put the altURL here.  If the directory is 
			'public_html/Squeak/', the altURL might be 'www.webPage.com/~kaehler2/
			Squeak/'.
urlObject	An instance of a subclass of Url.  It is very good at parsing complex urls.
			Relative references.  file:// uses this.  Use this in the future instead of 
			server and directory inst vars.
socket		nil or an FTPSocket.  Only non-nil if the connection is being kept open
			for multiple stores or retrievals.  
loaderUrl	a partial url that is ised to invoke squeak in a browser and load a project.

A normal call on some command like (aServer getFileNamed: 'foo') does not set 'socket'.  Socket being nil tells it to close the connection and destroy the socket after this one transcation.  If the caller stores into 'socket', then the same command does NOT close the 
connection.  
	Call 'openKeepFTP' or 'openGroup' to store into socket and keep the connection open.  It is up to the user to call 'quit' or 'closeGroup' later.

DD openKeepFTP.
Transcript cr; show: ((DD getFileNamed: '1198misc-tkKG.cs') next: 100).
Transcript cr; show: ((DD getFileNamed: '1192multFinder-tkKF.cs') next: 100).
DD quit.!

!ServerFile commentStamp: '<historical>' prior: 0!
Represents the non-data part of a file on a server on the internet.  I am owned by a RemoteFileStream, who has the data.

Since FileStream is a Stream and I am not, use this to always get a stream:
	xxx isStream ifFalse: [^ xxx asStream].

!

!RemoteFileStream methodsFor: 'as yet unclassified' stamp: 'tk 12/3/2003 21:04'!
close
	"Write if we have data to write.  FTP files are always binary to preserve the data exactly.  The binary/text (ascii) flag is just for tell how the bits are delivered from a read."

	remoteFile writable ifTrue: [
			remoteFile putFile: (self as: RWBinaryOrTextStream) reset named: remoteFile fileName]! !

!RemoteFileStream methodsFor: 'as yet unclassified' stamp: 'tk 1/13/2000 21:47'!
contentsOfEntireFile
	"Fetch the data off the server and store it in me.  But not if I already have it."

	readLimit := readLimit max: position.
	localDataValid ifTrue: [^ super contentsOfEntireFile].
	collection size = 0 ifTrue: [self on: (String new: 2000)].
	remoteFile getFileNamed: remoteFile fileName into: self.	"sets localDataValid := true"
	^ super contentsOfEntireFile! !

!RemoteFileStream methodsFor: 'as yet unclassified' stamp: 'tk 1/13/2000 21:45'!
dataIsValid

	localDataValid := true.! !

!RemoteFileStream methodsFor: 'as yet unclassified' stamp: 'tk 5/19/1998 09:19'!
directory
	^ remoteFile! !

!RemoteFileStream methodsFor: 'as yet unclassified' stamp: 'tk 3/13/2000 16:51'!
directoryUrl
	^ remoteFile directoryUrl! !

!RemoteFileStream methodsFor: 'as yet unclassified' stamp: 'tk 5/19/1998 14:30'!
localName
	^ remoteFile fileName! !

!RemoteFileStream methodsFor: 'as yet unclassified' stamp: 'tk 1/13/2000 21:48'!
openReadOnly
	"If we have data, don't reread."

	self readOnly.
	readLimit := readLimit max: position.
	localDataValid ifFalse: [remoteFile getFileNamed: remoteFile fileName into: self].
		"sets localDataValid := true"! !

!RemoteFileStream methodsFor: 'as yet unclassified' stamp: 'tk 11/24/1998 22:43'!
padToEndWith: aChar
	"On the Mac, files do not truncate, so pad it with a harmless character.  But Remote FTP files always replace, so no need to pad."

	self atEnd ifFalse: [self inform: 'Why is this stream not at its end?'].! !

!RemoteFileStream methodsFor: 'as yet unclassified' stamp: 'tk 5/19/1998 14:32'!
readOnly
	^ remoteFile readOnly! !

!RemoteFileStream methodsFor: 'as yet unclassified' stamp: 'tk 1/13/2000 21:39'!
remoteFile: aServerFile
	remoteFile := aServerFile.
	localDataValid := false.	"need to read from the server"! !

!RemoteFileStream methodsFor: 'as yet unclassified' stamp: 'RAA 9/11/2000 19:13'!
sleep
	"If we are done, then let the server know"

	self close.
	remoteFile sleep.! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 1/14/1999 21:01'!
altUrl
	"When a ftp server also has http access, use this to store the http url"
	^ altURL! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 1/14/1999 20:56'!
altUrl: aString
	altURL := aString! !

!ServerDirectory methodsFor: 'file directory' stamp: 'RAA 7/4/2000 10:08'!
asServerFileNamed: aName

	| rFile |
	rFile := self as: ServerFile.
	(aName includes: self pathNameDelimiter)
		ifTrue: [rFile fullPath: aName]
			"sets server, directory(path), fileName.  If relative, merge with self."
		ifFalse: [rFile fileName: aName].	"JUST a single NAME, already have the rest"
			"Mac files that include / in name, must encode it as %2F "
	^rFile
! !

!ServerDirectory methodsFor: 'file directory' stamp: 'tpr 4/28/2004 17:32'!
assureExistence
	"Make sure the current directory exists. If necessary, create all parts inbetween"
	
	self exists ifFalse: [
		self isRoot ifFalse: [
			self containingDirectory assureExistenceOfPath: self localName]]! !

!ServerDirectory methodsFor: 'file directory' stamp: 'tpr 4/28/2004 17:31'!
assureExistenceOfPath: localPath
	"Make sure the local directory exists. If necessary, create all parts inbetween"

	localPath = (String with: self pathNameDelimiter) ifTrue: [^self].
	self assureExistence.
	(self localPathExists: localPath) ifFalse: [
		self createDirectory: localPath].! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 9/18/1998 23:24'!
bareDirectory

	^ directory first == $/ 
		ifTrue: [directory copyFrom: 2 to: directory size]
		ifFalse: [directory]! !

!ServerDirectory methodsFor: 'updates' stamp: 'tk 1/7/2001 11:58'!
checkNames: list
	"Look at these names for update and see if they are OK"

list do: [:local |
	(local count: [:char | char == $.]) > 1 ifTrue: [
		self inform: 'File name ',local,'
may not have more than one period'.
	^ false].
	local size > 26 ifTrue: ["allows for 5 digit update numbers"
		self inform: 'File name ',local,'
is too long.  Please rename it.'.
	^ false].
	(local at: 1) isDigit ifTrue: [
		self inform: 'File name ',local,'
may not begin with a number'.
	^ false].
	(local findDelimiters: '%/* ' startingAt: 1) <= local size ifTrue: [
		self inform: 'File name ',local,'
may not contain % / * or space'.
	^ false]].
^ true
! !

!ServerDirectory methodsFor: 'updates' stamp: 'gsa 1/6/2013 18:15'!
checkServersWithPrefix: prefix andParseListInto: listBlock

"gsa 06/01/2013 replaced UIManager with PopUpMenu"

	"Check that all servers are up and have the latest Updates.list.
	Warn user when can't write to a server that can still be read.
	The contents of updates.list is parsed into {{vers. {fileNames*}}*},
	and returned via the listBlock."

	|  serverList updateLists listContents maxSize outOfDateServers |
	serverList := self serversInGroup.
	serverList isEmpty
		ifTrue: [^Array new].

	updateLists := Dictionary new.
	serverList do: [:updateServer |
		[listContents := updateServer getFileNamed: prefix , 'updates.list'.
		updateLists at: updateServer put: listContents]
			on: Error
			do: [:ex | PopUpMenu withCaption:  'Server ', updateServer moniker,
					' is unavailable.\Please consider phoning the administator.\' withCRs, listContents
					chooseFrom:  #('Cancel entire update').		
				"
				UIManager default chooseFrom: #('Cancel entire update')
					title: 'Server ', updateServer moniker,
					' is unavailable.\Please consider phoning the administator.\' withCRs, listContents."
				^Array new]].

	maxSize := (updateLists collect: [:each | each size]) max.
	outOfDateServers := updateLists keys select: [:updateServer |
		(updateLists at: updateServer) size < maxSize].

	outOfDateServers do: [:updateServer |
		(self outOfDate: updateServer) ifTrue: [^Array new]].

	listBlock value: (self parseListContents: listContents).

	serverList removeAll: outOfDateServers.
	^serverList

! !

!ServerDirectory methodsFor: 'server groups' stamp: 'mir 6/26/2001 11:56'!
closeGroup
	"Close connection with all servers in the group."

	self serversInGroup do: [:aDir | aDir quit].
! !

!ServerDirectory methodsFor: 'file directory' stamp: 'yo 8/28/2008 00:29'!
containingDirectory

	^ self copy directory: (self dirPathFor: directory).

! !

!ServerDirectory methodsFor: 'file directory' stamp: 'mir 12/8/2003 14:16'!
createDirectory: localName
	"Create a new sub directory within the current one"

	self isTypeFile ifTrue: [
		^FileDirectory createDirectory: localName
	].

	client := self openFTPClient.
	[client makeDirectory: localName]
		ensure: [self quit].
! !

!ServerDirectory methodsFor: 'file directory' stamp: 'mir 12/8/2003 14:17'!
deleteDirectory: localName
	"Delete the sub directory within the current one.  Call needs to ask user to confirm."

	self isTypeFile ifTrue: [
		^FileDirectory deleteFileNamed: localName
	].
		"Is this the right command???"

	client := self openFTPClient.
	[client deleteDirectory: localName]
		ensure: [self quit].
! !

!ServerDirectory methodsFor: 'file directory' stamp: 'mir 12/8/2003 14:17'!
deleteFileNamed: fullName
	"Detete a remote file.  fullName is directory path, and does include name of the server.  Or it can just be a fileName."
	| file |
	file := self asServerFileNamed: fullName.
	file isTypeFile ifTrue: [
		^ (FileDirectory forFileName: (file fileNameRelativeTo: self)) 
			deleteFileNamed: file fileName
	].
	
	client := self openFTPClient.
	[client deleteFileNamed: fullName]
		ensure: [self quit].
! !

!ServerDirectory methodsFor: 'accessing' stamp: 'yo 8/28/2008 00:29'!
dirPathFor: fullName 
	"Return the directory part the given name."
	self
		splitName: fullName
		to: [:dirPath :localName | ^ dirPath]! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 11/24/1998 15:00'!
directory
	"String of part of url that is the directory. Has slashes as separators"

	urlObject ifNotNil: [^ urlObject pathDirString].
	^ directory! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 1/3/98 21:36'!
directory: anObject
	directory := anObject! !

!ServerDirectory methodsFor: 'file directory' stamp: 'tak 9/25/2008 15:13'!
directoryNamed: localFileName 
	"Return a copy of me pointing at this directory below me"
	| new newPath newAltUrl |
	new := self copy.
	urlObject ifNotNil: [
		new urlObject path: new urlObject path copy.
		new urlObject path removeLast; addLast: localFileName; addLast: ''.
		^ new].
	"sbw.  When working from an FTP server, the first time we access
	a subdirectory the <directory> variable is empty.  In that case we
	cannot begin with a leading path delimiter since that leads us to
	the wrong place."
	newPath := directory isEmpty
				ifTrue: [localFileName]
				ifFalse: [directory , self pathNameDelimiter asString , localFileName].
	self altUrl ifNotNil: [
		newAltUrl := self altUrl, self pathNameDelimiter asString , localFileName].
	new directory: newPath; altUrl: newAltUrl.
	^ new! !

!ServerDirectory methodsFor: 'file directory' stamp: 'TorstenBergmann 7/5/2010 16:54'!
directoryNames
	"Return a collection of names for the subdirectories of this directory."
	"(ServerDirectory serverNamed: 'UIUCArchive') directoryNames"

	^ (self entries select: [:entry | entry isDirectory])
		collect: [:entry | entry name]
! !

!ServerDirectory methodsFor: 'accessing' stamp: 'RAA 2/2/2001 08:29'!
directoryWrapperClass

	^FileDirectoryWrapper! !

!ServerDirectory methodsFor: 'accessing' stamp: 'mir 6/25/2001 10:45'!
downloadUrl
	"The url under which files will be accessible."
	^(self altUrl
		ifNil: [self realUrl]
		ifNotNil: [self altUrl]) , '/'! !

!ServerDirectory methodsFor: 'file directory' stamp: 'mir 5/13/2003 10:44'!
entries 
	"Return a collection of directory entries for the files and directories in this directory. Each entry is a five-element array: (<name> <creationTime> <modificationTime> <dirFlag> <fileSize>)."
	| dir ftpEntries |
	"We start with ftp directory entries of the form...
d---------   1 owner    group               0 Apr 27 22:01 blasttest
----------   1 owner    group           93812 Jul 21  1997 COMMAND.COM
    1        2   3           4                 5    6  7    8       9   -- token index"
	self isTypeFile ifTrue: [
		urlObject isAbsolute ifFalse: [urlObject default].
		^ (FileDirectory on: urlObject pathForDirectory) entries
	].

	dir := self getDirectory.
	(dir respondsTo: #contentsOfEntireFile) ifFalse: [^ #()].
	ftpEntries := dir contentsOfEntireFile findTokens: String crlf.
"ftpEntries inspect."
	^ ftpEntries 
		collect:[:ftpEntry | self class parseFTPEntry: ftpEntry]
		thenSelect: [:entry | entry notNil]! !

!ServerDirectory methodsFor: 'file directory' stamp: 'MarcusDenker 11/7/2009 19:08'!
exists
	"It is difficult to tell if a directory exists.  This is ugly, but it works for writable directories.  http: will fall back on ftp for this"

	| probe success |
	success := false.
	self isTypeFile ifTrue: [
		self entries size > 0 ifTrue: [^ true].
		probe := self newFileNamed: 'withNoName23'. 
		probe ifNotNil: [
			probe close.
			probe directory deleteFileNamed: probe localName].
		^probe notNil].
	[client := self openFTPClient.
	[client pwd]
		ensure: [self quit].
		success := true]
		on: Error
		do: [:ex | ].
	^success! !

!ServerDirectory methodsFor: 'file directory' stamp: 'StephaneDucasse 8/21/2010 22:31'!
fileAndDirectoryNames
	"FileDirectory default fileAndDirectoryNames"

	^ self entries collect: [:entry | entry name]
! !

!ServerDirectory methodsFor: 'up/download' stamp: 'mir 9/25/2008 15:11'!
fileExists: fileName
	"Does the file exist on this server directory?  fileName must be simple with no / or references to other directories."

	| stream |
	self isTypeFile ifTrue: [^ self fileNames includes: fileName].
	self isTypeHTTP ifTrue: [
		"http"
		stream := self readOnlyFileNamed: fileName.
		^stream contents notEmpty].
	"ftp"
	^ self entries anySatisfy: [:entry | entry name = fileName]! !

!ServerDirectory methodsFor: 'file directory' stamp: 'RAA 7/4/2000 10:31'!
fileNamed: fullName
	"Create a RemoteFileStream for writing.  If the file exists, do not complain.  fullName is directory path, and does include name of the server.  Or it can just be a fileName.  Only write the data upon close."

	| file remoteStrm |
	file := self asServerFileNamed: fullName.
	file readWrite.
	file isTypeFile ifTrue: [
		^ FileStream fileNamed: (file fileNameRelativeTo: self)
	].

	remoteStrm := RemoteFileStream on: (String new: 2000).
	remoteStrm remoteFile: file.
	^ remoteStrm	"no actual writing till close"
! !

!ServerDirectory methodsFor: 'file directory' stamp: 'StephaneDucasse 8/21/2010 22:30'!
fileNames
	"Return a collection of names for the files (but not directories) in this directory."
	"(ServerDirectory serverNamed: 'UIUCArchive') fileNames"

	self isTypeFTP | self isTypeFile ifFalse: [
		^ self error: 'To see a directory, use file:// or ftp://'
	].
	^ (self entries select: [:entry | entry isDirectory not])
		collect: [:entry | entry name]
! !

!ServerDirectory methodsFor: 'initialize' stamp: 'tk 11/26/1998 09:50'!
fromUser
	"Ask the user for all data on a new server.  Save it in a named server."  ! !

!ServerDirectory methodsFor: 'file directory' stamp: 'tak 9/25/2008 15:12'!
fullNameFor: aFileName
	"Convention: 
	If it is an absolute path, directory stored with a leading slash, and url has no user@.
	If relative path, directory stored with no leading slash, and url begins user@.
	Should we include ftp:// on the front?"

	urlObject ifNotNil: [^ urlObject pathString, aFileName].
	(aFileName includes: self pathNameDelimiter)
		ifTrue: [^ aFileName].
	self isTypeHTTP ifTrue: [^ self downloadUrl, aFileName].
	directory isEmpty ifTrue: [^ server,  self pathNameDelimiter asString, aFileName].
	^ (directory first == $/ ifTrue: [''] ifFalse: [user,'@']), 
		server, self slashDirectory, 
		self pathNameDelimiter asString, aFileName! !

!ServerDirectory methodsFor: 'accessing' stamp: 'nice 1/5/2010 15:59'!
fullPath: serverAndDirectory
	"Parse and save a full path.  Convention:  if ftp://user@server/dir, then dir is relative to user's directory.  dir has no slash at beginning.  If ftp://server/dir, then dir is absolute to top of machine, give dir a slash at the beginning."

	| start bare sz userAndServer both slash score best |
	bare := serverAndDirectory.
	sz := serverAndDirectory size.
	bare size > 0 ifTrue: [ 
		start := (bare copyFrom: 1 to: (8 min: sz)) asLowercase.
		((start beginsWith: 'ftp:') or: [start beginsWith: 'nil:']) "fix bad urls"
			ifTrue: [type := #ftp.
				bare := bare copyFrom: (7 min: sz) to: bare size].
		(start beginsWith: 'http:') 
			ifTrue: [type := #http.
				bare := bare copyFrom: (8 min: sz) to: serverAndDirectory size].
		((start beginsWith: 'file:') or: [type == #file])
			ifTrue: [type := #file.
				urlObject := FileUrl absoluteFromText: serverAndDirectory.
				^ self]].
	userAndServer := bare copyUpTo: self pathNameDelimiter.
	both := userAndServer findTokens: '@'.
	slash := both size.	"absolute = 1, relative = 2"
	server := both last.
	both size > 1 ifTrue: [user := both at: 1].
	bare size > (userAndServer size + 1) 
		ifTrue: [directory := bare copyFrom: userAndServer size + slash to: bare size]
		ifFalse: [directory := ''].

	"If this server is already known, copy in its userName and password"
	type == #ftp ifFalse: [^ self].
	score := -1.
	ServerDirectory serverNames do: [:name | | match sd |
		sd := ServerDirectory serverNamed: name.
		server = sd server ifTrue: [
			match := directory asLowercase charactersExactlyMatching: sd directory asLowercase.
			match > score ifTrue: [score := match.  best := sd]]].
	best ifNil: [
		self fromUser
	] ifNotNil: [
		user := best user.
		altURL := best altUrl.
		loaderUrl := best loaderUrl.
		self password: best password
	].
! !

!ServerDirectory methodsFor: 'up/download' stamp: 'nice 1/5/2010 20:45'!
getDirectory
	"Return a stream with a listing of the current server directory.  (Later -- Use a proxy server if one has been registered.)"
	| listing |
	client := self openFTPClient.
	listing := [ client getDirectory ] ensure: [ self quit ].
	^ listing readStream! !

!ServerDirectory methodsFor: 'up/download' stamp: 'nice 1/5/2010 20:45'!
getFileList
	"Return a stream with a list of files in the current server directory.  (Later -- Use a proxy server if one has been registered.)"
	| listing |
	client := self openFTPClient.
	listing := [ client getFileList ] ensure: [ self quit ].
	^ listing readStream! !

!ServerDirectory methodsFor: 'up/download' stamp: 'nice 1/5/2010 20:45'!
getFileNamed: fileNameOnServer
	"Just FTP a file from a server.  Return contents.
	(Later -- Use a proxy server if one has been registered.)"

	| result |
	client := self openFTPClient.
	result := [client getFileNamed: fileNameOnServer]
		ensure: [self quit].
	^result! !

!ServerDirectory methodsFor: 'up/download' stamp: 'tak 9/25/2008 15:12'!
getFileNamed: fileNameOnServer into: dataStream
	
	^ self getFileNamed: fileNameOnServer into: dataStream
		httpRequest: 'Pragma: no-cache', String crlf! !

!ServerDirectory methodsFor: 'up/download' stamp: 'ar 4/10/2005 18:52'!
getFileNamed: fileNameOnServer into: dataStream httpRequest: requestString
	"Just FTP a file from a server.  Return a stream.  (Later -- Use a proxy server if one has been registered.)"

	| resp |
	self isTypeFile ifTrue: [
		dataStream nextPutAll: 
			(resp := FileStream oldFileNamed: server,(self serverDelimiter asString), 
				self bareDirectory, (self serverDelimiter asString),
				fileNameOnServer) contentsOfEntireFile.
		dataStream dataIsValid.
		^ resp].
	self isTypeHTTP ifTrue: [
		resp := HTTPSocket httpGet: (self fullNameFor: fileNameOnServer) 
				args: nil accept: 'application/octet-stream' request: requestString.
		resp isString ifTrue: [^ dataStream].	"error, no data"
		dataStream copyFrom: resp.
		dataStream dataIsValid.
		^ dataStream].

	client := self openFTPClient.	"Open passive.  Do everything up to RETR or STOR"
	[client getFileNamed: fileNameOnServer into: dataStream]
		ensure: [self quit].

	dataStream dataIsValid.
! !

!ServerDirectory methodsFor: 'file directory' stamp: 'RAA 6/23/2000 09:47'!
getOnly: nnn from: fileNameOnServer
	| file ff resp |
	"Use FTP to just capture the first nnn characters of the file.  Break the connection after that.  Goes faster for long files.  Return the contents, not a stream."

	self isTypeFile ifTrue: [
		file := self as: ServerFile.
		file fileName: fileNameOnServer.
		ff := FileStream oldFileOrNoneNamed: (file fileNameRelativeTo: self).
		^ ff next: nnn].
	self isTypeHTTP ifTrue: [
		resp := HTTPSocket httpGet: (self fullNameFor: fileNameOnServer) 
				accept: 'application/octet-stream'.
			"For now, get the whole file.  This branch not used often."
		^ resp truncateTo: nnn].
	
	^ self getOnlyBuffer: (String new: nnn) from: fileNameOnServer! !

!ServerDirectory methodsFor: 'up/download' stamp: 'PeterHugossonMiller 9/3/2009 11:20'!
getOnlyBuffer: buffer from: fileNameOnServer
	"Open ftp, fill the buffer, and close the connection.  Only first part of a very long file."

	| dataStream |
	client := self openFTPClient.
	dataStream := buffer writeStream.
	[client getPartial: buffer size fileNamed: fileNameOnServer into: dataStream]
		ensure: [self quit].
	^buffer! !

!ServerDirectory methodsFor: 'server groups' stamp: 'nk 8/30/2004 08:00'!
groupName

	^group
		ifNil: [self moniker]
		ifNotNil: [
			(group isString)
				ifTrue: [group]
				ifFalse: [group key]]! !

!ServerDirectory methodsFor: 'file directory' stamp: 'tk 11/20/1998 12:28'!
includesKey: localName
	"Answer true if this directory includes a file or directory of the given name. Note that the name should be a local file name, in contrast with fileExists:, which takes either local or full-qualified file names."

	^ self fileAndDirectoryNames includes: localName
! !

!ServerDirectory methodsFor: 'testing' stamp: 'dgd 12/27/2003 10:47'!
isRemoteDirectory
	"answer whatever the receiver is a remote directory"
	^ true! !

!ServerDirectory methodsFor: 'testing' stamp: 'yo 8/28/2008 00:22'!
isRoot
	^ directory = '/'! !

!ServerDirectory methodsFor: 'accessing' stamp: 'RAA 6/23/2000 09:40'!
isTypeFTP

	^self typeWithDefault == #ftp! !

!ServerDirectory methodsFor: 'accessing' stamp: 'RAA 6/23/2000 09:41'!
isTypeFile

	^self typeWithDefault == #file! !

!ServerDirectory methodsFor: 'accessing' stamp: 'RAA 6/23/2000 09:46'!
isTypeHTTP

	^self typeWithDefault == #http! !

!ServerDirectory methodsFor: 'testing' stamp: 'mir 12/8/2003 12:28'!
keepAlive
	keepAlive ifNil: [keepAlive := false].
	^keepAlive! !

!ServerDirectory methodsFor: 'accessing' stamp: 'mir 12/8/2003 14:18'!
keepAlive: aBoolean
	keepAlive := aBoolean! !

!ServerDirectory methodsFor: 'accessing' stamp: 'RAA 9/14/2000 13:22'!
loaderUrl

	^loaderUrl! !

!ServerDirectory methodsFor: 'file directory' stamp: 'bf 9/25/2008 15:11'!
localName

	directory isEmpty ifTrue: [self error: 'no directory'].
	^ self localNameFor: directory.! !

!ServerDirectory methodsFor: 'file directory' stamp: 'RAA 7/28/2000 13:46'!
localNameFor: fullName
	"Return the local part the given name."

	self
		splitName: fullName
		to: [:dirPath :localName | ^ localName]
! !

!ServerDirectory methodsFor: 'file directory' stamp: 'mir 9/25/2008 15:11'!
localPathExists: localPath

	^ self directoryNames includes: localPath! !

!ServerDirectory methodsFor: 'file directory' stamp: 'mir 8/24/2001 12:01'!
matchingEntries: criteria
	"Ignore the filter criteria for now"
	^self entries! !

!ServerDirectory methodsFor: 'accessing' stamp: 'fbs 2/2/2005 13:24'!
moniker
	"a plain language name for this directory"

	moniker ifNotNil: [^ moniker].
	directory ifNotNil: [^ self server].
	urlObject ifNotNil: [^ urlObject asString].
	^ ''! !

!ServerDirectory methodsFor: 'file directory' stamp: 'rbb 3/1/2005 11:12'!
newFileNamed: fullName
	"Create a RemoteFileStream.  If the file exists, and complain.  fullName is directory path, and does include name of the server.  Or it can just be a fileName.  Only write the data upon close."

	| file remoteStrm selection |

	file := self asServerFileNamed: fullName.
	file readWrite.
	file isTypeFile ifTrue: [
		^ FileStream newFileNamed: (file fileNameRelativeTo: self)].
	file exists 
		ifTrue: [
			selection := UIManager default 
				chooseFrom: #('overwrite that file' 'choose another name' 'cancel')
				title: (file fullNameFor: file fileName) , '
already exists.']
		ifFalse: [selection := 1].

	selection = 1 ifTrue:
		[remoteStrm := RemoteFileStream on: (String new: 2000).
		remoteStrm remoteFile: file.
		remoteStrm dataIsValid.	"empty stream is the real contents!!"
		^ remoteStrm].	"no actual writing till close"
	selection = 2 ifTrue: [
		^ self newFileNamed:
			(UIManager default request: 'Enter a new file name'
				initialAnswer: file fileName)].
	^ nil	"cancel"! !

!ServerDirectory methodsFor: 'file directory' stamp: 'RAA 7/4/2000 10:10'!
oldFileNamed: aName
	"If the file exists, answer a read-only RemoteFileStream on it.  aName is directory path, and does include name of the server.  Or it can just be a fileName.  For now, pre-read the file."

	| rFile |

	rFile := self asServerFileNamed: aName.
	rFile readOnly.
	rFile isTypeFile ifTrue: [
		^ FileStream oldFileNamed: (rFile fileNameRelativeTo: self)].

	^self streamOnBeginningOf: rFile
! !

!ServerDirectory methodsFor: 'file directory' stamp: 'StephaneDucasse 8/21/2010 22:50'!
oldFileOrNoneNamed: fullName
	"If the file exists, answer a read-only RemoteFileStream on it. If it doesn't, answer nil.  fullName is directory path, and does include name of the server.  Or just a simple fileName.  Do prefetch the data."
 
	
	^ Cursor wait showWhile:
		[ | file |
		file := self asServerFileNamed: fullName.
		file readOnly.
		file isTypeFile
			ifTrue: [FileStream oldFileOrNoneNamed: (file fileNameRelativeTo: self)]
			ifFalse: [self streamOnBeginningOf: file]]! !

!ServerDirectory methodsFor: 'file directory' stamp: 'tk 9/19/1998 18:59'!
on: fullName
	"Answer another ServerDirectory on the partial path name.  fullName is directory path, and does include the name of the server."

	| new |
	new := self copy.
	new fullPath: fullName.		"sets server, directory(path)"
	^ new! !

!ServerDirectory methodsFor: 'dis/connect' stamp: 'rbb 2/18/2005 14:41'!
openFTPClient

	| loginSuccessful what |
	client
		ifNotNil: [client isConnected
			ifTrue: [^client]
			ifFalse: [client := nil]].
	client := FTPClient openOnHostNamed: server.
	loginSuccessful := false.
	[loginSuccessful]
		whileFalse: [
			[loginSuccessful := true.
			client loginUser: self user password: self password]
				on: LoginFailedException
				do: [:ex | 
					passwordHolder := nil.
					what := UIManager default 
						chooseFrom: #('enter password' 'give up') 
						title: 'Would you like to try another password?'.
					what = 1 ifFalse: [self error: 'Login failed.'. ^nil].
					loginSuccessful := false]].
	client changeDirectoryTo: directory.
	^client! !

!ServerDirectory methodsFor: 'server groups' stamp: 'mir 6/26/2001 11:57'!
openGroup
	"Open all servers in the group.  Don't forget to close later."

	self serversInGroup do: [:aDir | aDir wakeUp].
! !

!ServerDirectory methodsFor: 'updates' stamp: 'rbb 2/18/2005 14:39'!
outOfDate: aServer
	"Inform the user that this server does not have a current version of 'Updates.list'  Return true if the user does not want any updates to happen."

| response |
response := UIManager default chooseFrom: #('Install on others' 'Cancel entire update')
		title: 'The server ', aServer moniker, ' is not up to date.
Please store the missing updates maually.'.
^ response ~= 1! !

!ServerDirectory methodsFor: 'updates' stamp: 'Jb 10/23/2010 11:55'!
parseListContents: listContents 
	"Parse the contents of updates.list into {{vers. {fileNames*}}*}, and return it."
	| sections vers strm line fileNames |
	sections := OrderedCollection new.
	fileNames := OrderedCollection new: 1000.
	vers := nil.
	strm := listContents readStream.
	[ strm atEnd ] whileFalse: 
		[ line := strm nextLine.
		line size > 0 ifTrue: 
			[ line first = $# 
				ifTrue: 
					[ vers ifNotNil: [ sections addLast: {  vers. (fileNames asArray)  } ].
					"Start a new section"
					vers := line allButFirst.
					fileNames resetTo: 1 ]
				ifFalse: [ line first = $* ifFalse: [ fileNames addLast: line ] ] ] ].
	vers ifNotNil: [ sections addLast: {  vers. (fileNames asArray)  } ].
	^ sections asArray
	" TEST:
 | list |
list := Utilities parseListContents: (FileStream oldFileNamed: 'updates.list') contentsOfEntireFile.
list = (Utilities parseListContents: (String streamContents: [:s | Utilities writeList: list toStream: s]))
	ifFalse: [self error: 'test failed']
	ifTrue: [self inform: 'test OK']
"! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 2/14/1999 20:44'!
password

	passwordHolder ifNil: [passwordHolder := Password new].
	^ passwordHolder passwordFor: self	"may ask the user"! !

!ServerDirectory methodsFor: 'accessing' stamp: 'ar 4/10/2005 18:52'!
password: pp

	passwordHolder := Password new.
	pp isString 
		ifTrue: [passwordHolder cache: pp. ^ self].
	pp isInteger 
		ifTrue: [passwordHolder sequence: pp]
		ifFalse: [passwordHolder := pp].! !

!ServerDirectory methodsFor: 'file directory' stamp: 'tk 1/14/1999 20:54'!
pathName
	"Path name as used in reading the file.  with slashes for ftp, with local file delimiter (:) for a file: url"

	urlObject ifNotNil: [^ urlObject pathForFile].
	directory size = 0 ifTrue: [^ server].
	^ (directory at: 1) = self pathNameDelimiter
		ifTrue: [server, directory]
		ifFalse: [user, '@', server, self pathNameDelimiter asString, directory]! !

!ServerDirectory methodsFor: 'file directory' stamp: 'tk 9/3/1998 12:34'!
pathNameDelimiter
	"the separator that is used in URLs"

	^ $/! !

!ServerDirectory methodsFor: 'file directory' stamp: 'tk 9/8/1998 12:12'!
pathParts
	"Return the path from the root of the file system to this directory as an array of directory names.  On a remote server."

	urlObject ifNotNil: [^ (urlObject path copy) removeLast; yourself].
	^ (OrderedCollection with: server) addAll: 
		(directory findTokens: self pathNameDelimiter asString);
			yourself.
! !

!ServerDirectory methodsFor: 'accessing' stamp: 'nice 1/13/2010 21:48'!
postCopy
	super postCopy.
	urlObject := urlObject copy! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 5/23/1998 09:41'!
printOn: aStrm
	aStrm nextPutAll: self class name; nextPut: $<.
	aStrm nextPutAll: self moniker.
	aStrm nextPut: $>.
! !

!ServerDirectory methodsFor: 'up/download' stamp: 'mir 12/8/2003 16:39'!
putFile: fileStream named: fileNameOnServer
	"Just FTP a local fileStream to the server.  (Later -- Use a proxy server if one has been registered.)"

	client := self openFTPClient.
	client binary.
	[client putFileStreamContents: fileStream as: fileNameOnServer]
		ensure: [self quit]! !

!ServerDirectory methodsFor: 'up/download' stamp: 'HenrikSperreJohansen 6/28/2010 12:26'!
putFile: fileStream named: fileNameOnServer retry: aBool
	"ar 11/24/1998 Do the usual putFile:named: operation but retry if some error occurs and aBool is set. Added due to having severe transmission problems on shell.webpage.com."
	| resp |
	self isTypeFile ifTrue: [
		^ (FileDirectory on: urlObject pathForDirectory)
			putFile: fileStream named: fileNameOnServer].

	[[resp := self putFile: fileStream named: fileNameOnServer] 
		ifError:[:err | resp := '5xx ',err]. "Report as error"
	aBool and:[((resp isString) and: [resp size > 0]) and:[resp first ~= $2]]] whileTrue:[
		(self confirm:('Error storing ',fileNameOnServer,' on the server.\(',resp,',)\Retry operation?') withCRs) ifFalse:[^resp].
	].
	^resp! !

!ServerDirectory methodsFor: 'updates' stamp: 'StephaneDucasse 1/29/2011 11:59'!
putUpdate: fileStrm 
	"Put this file out as an Update on the servers of my group.  Each version of the system may have its own set of update files, or they may all share the same files.  'updates.list' holds the master list.  Each update is a fileIn whose name begins with a number.  See Utilities class readServerUpdatesThrough:saveLocally:updateImage:.
	When two sets of updates are stored on the same directory, one of them has a * in its 
serverUrls description.  When that is true, the first word of the description is put on
the front of 'updates.list', and that index file is used."
	| myServers updateStrm newName response localName seq indexPrefix listContents version versIndex lastNum stripped |
	localName := fileStrm localName.
	fileStrm size = 0 ifTrue: [ ^ self inform: 'That file has zero bytes!!  May have a new name.' ].
	(fileStrm contentsOfEntireFile includes: Character linefeed) ifTrue: 
		[ (self confirm: 'That file contains linefeeds.  Proceed if...
you know that this is okay (e.g. the file contains raw binary data).') ifFalse: [^ nil] ].
	fileStrm reset.
	(self checkNames: {  localName  }) ifFalse: [ ^ nil ].	"illegal characters"
	response := UIManager default 
		chooseFrom: #('Install update' 'Cancel update' )
		title: 'Do you really want to broadcast the file ' , localName , '\to every Squeak user who updates from ' withCRs , self groupName , '?'.
	response = 1 ifFalse: [ ^ nil ].	"abort"
	self openGroup.
	indexPrefix := (self groupName includes: $*) 
		ifTrue: 
			[ (self groupName findTokens: ' ') first	"special for internal updates" ]
		ifFalse: [ '' ].	"normal"
	myServers := self 
		checkServersWithPrefix: indexPrefix
		andParseListInto: [ :x | listContents := x ].
	myServers size = 0 ifTrue: 
		[ self closeGroup.
		^ self ].
	version := SystemVersion current version.
	versIndex := (listContents collect: [ :pair | pair first ]) indexOf: version.
	versIndex = 0 ifTrue: 
		[ self inform: 'There is no section in updates.list for your version'.
		self closeGroup.
		^ nil ].	"abort"

	"A few affirmations..."
	versIndex < listContents size ifTrue: 
		[ (self confirm: 'This system, ' , version , ' is not the latest version.\Make update for an older version?' withCRs) ifFalse: 
			[ self closeGroup.
			^ nil ] ].	"abort"
	(listContents at: versIndex) last isEmpty ifTrue: 
		[ (self confirm: 'Please confirm that you mean to issue the first update for ' , version , '\(otherwise something is wrong).' withCRs) ifFalse: 
			[ self closeGroup.
			^ nil ] ].

	"We now determine next update number to be max of entire index"
	lastNum := listContents 
		inject: 0
		into: 
			[ :max :pair | 
			pair last isEmpty 
				ifTrue: [ max ]
				ifFalse: [ max max: pair last last initialIntegerOrNil ] ].

	"Save old copy of updates.list on local disk"
	FileDirectory default deleteFileNamed: indexPrefix , 'updates.list.bk'.
	UpdateStreamer new 
		writeList: listContents
		toStream: (FileStream fileNamed: indexPrefix , 'updates.list.bk').

	"append name to updates with new sequence number"
	seq := (lastNum + 1) printString 
		padded: #left
		to: 4
		with: $0.
	"strip off any old seq number"
	stripped := localName 
		copyFrom: (localName findFirst: [ :c | c isDigit not ])
		to: localName size.
	newName := seq , stripped.
	listContents 
		at: versIndex
		put: { 
				version.
				((listContents at: versIndex) last copyWith: newName)
			 }.

	"Write a new copy on all servers..."
	updateStrm := (String streamContents: 
		[ :s | 
		UpdateStreamer new 
			writeList: listContents
			toStream: s ]) readStream.
	myServers do: 
		[ :aServer | 
		fileStrm reset.	"reopen"
		aServer 
			putFile: fileStrm
			named: newName
			retry: true.
		updateStrm reset.
		aServer 
			putFile: updateStrm
			named: indexPrefix , 'updates.list'
			retry: true.
		Transcript
			show: 'Update succeeded on server ' , aServer moniker;
			cr ].
	self closeGroup.
	Transcript
		cr;
		show: 'Be sure to test your new update!!';
		cr.
	"rename the file locally (may fail)"
	fileStrm directory 
		rename: localName
		toBe: newName! !

!ServerDirectory methodsFor: 'dis/connect' stamp: 'mir 12/8/2003 12:53'!
quit
	"break the connection"

	self keepAlive
		ifFalse: [self quitClient]! !

!ServerDirectory methodsFor: 'dis/connect' stamp: 'mir 12/8/2003 12:53'!
quitClient
	"break the connection"

	client ifNotNil: [client quit].
	client := nil! !

!ServerDirectory methodsFor: 'file directory' stamp: 'RAA 7/4/2000 10:08'!
readOnlyFileNamed: aName
	"If the file exists, answer a read-only RemoteFileStream on it.  aName is directory path, and does include name of the server.  Or it can just be a fileName.  For now, pre-read the file."

	| rFile |

	rFile := self asServerFileNamed: aName.
	rFile readOnly.
	rFile isTypeFile ifTrue: [
		^ FileStream oldFileNamed: (rFile fileNameRelativeTo: self)].

	^self streamOnBeginningOf: rFile! !

!ServerDirectory methodsFor: 'accessing' stamp: 'fbs 2/2/2005 13:24'!
realUrl
	"a fully expanded version of the url we represent.  Prefix the path with http: or ftp: or file:"

	self isTypeFile ifTrue: [
		self fileNameRelativeTo: self.
		^ urlObject asString
	].
	^ self typeWithDefault asString, '://', self pathName
	! !

!ServerDirectory methodsFor: 'file directory' stamp: 'mir 12/8/2003 14:17'!
rename: fullName toBe: newName
	"Rename a remote file.  fullName is just be a fileName, or can be directory path that includes name of the server.  newName is just a fileName"
	| file |

	file := self asServerFileNamed: fullName.
	file isTypeFile ifTrue: [
		(FileDirectory forFileName: (file fileNameRelativeTo: self)) 
			rename: file fileName toBe: newName
	].
	
	client := self openFTPClient.
	[client renameFileNamed: fullName to: newName]
		ensure: [self quit].
	! !

!ServerDirectory methodsFor: 'multi-action sessions' stamp: 'mir 11/19/2002 17:51'!
reset
	! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 11/24/1998 18:18'!
server
	^ server! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 1/3/98 21:36'!
server: anObject
	server := anObject! !

!ServerDirectory methodsFor: 'file directory' stamp: 'RAA 6/23/2000 09:45'!
serverDelimiter
	"the separator that is used in the place where the file actually is.  ftp server or local disk."

	^ self isTypeFile 
		ifTrue: [FileDirectory default pathNameDelimiter]
		ifFalse: [$/]	"for ftp, http"! !

!ServerDirectory methodsFor: 'server groups' stamp: 'mir 6/26/2001 11:56'!
serversInGroup
	^self groupName
		ifNil: [Array with: self]
		ifNotNil: [self class serversInGroupNamed: self groupName]! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 9/18/1998 23:22'!
slashDirectory

	^ directory first == $/ 
		ifTrue: [directory]
		ifFalse: ['/', directory]! !

!ServerDirectory methodsFor: 'multi-action sessions' stamp: 'mir 12/8/2003 12:54'!
sleep
	"If still connected, break the connection"

	self quitClient.
	self keepAlive: false! !

!ServerDirectory methodsFor: 'file directory' stamp: 'RAA 7/28/2000 13:46'!
splitName: fullName to: pathAndNameBlock
	"Take the file name and convert it to the path name of a directory and a local file name within that directory. FileName must be of the form: <dirPath><delimiter><localName>, where <dirPath><delimiter> is optional. The <dirPath> part may contain delimiters."

	| delimiter i dirName localName |
	delimiter := self pathNameDelimiter.
	(i := fullName findLast: [:c | c = delimiter]) = 0
		ifTrue:
			[dirName := String new.
			localName := fullName]
		ifFalse:
			[dirName := fullName copyFrom: 1 to: (i - 1 max: 1).
			localName := fullName copyFrom: i + 1 to: fullName size].

	^ pathAndNameBlock value: dirName value: localName! !

!ServerDirectory methodsFor: 'file directory' stamp: 'RAA 7/4/2000 10:03'!
streamOnBeginningOf: serverFile

	| remoteStrm |
	remoteStrm := RemoteFileStream on: (String new: 2000).
	remoteStrm remoteFile: serverFile.
	serverFile getFileNamed: serverFile fileName into: remoteStrm.	"prefetch data"
	^ remoteStrm! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 2/14/1999 21:44'!
type: aSymbol
	type := aSymbol! !

!ServerDirectory methodsFor: 'accessing' stamp: 'RAA 6/23/2000 09:30'!
typeWithDefault

	^ type ifNil: [type := #ftp]! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 1/25/1999 15:12'!
url
	"This was mis-named at the beginning.  Eventually convert over to altUrl and use this for the real url."
	^ self realUrl! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 9/5/1998 17:20'!
url: aString
	altURL := aString! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 9/6/1998 00:44'!
urlObject
	^ urlObject! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 9/8/1998 11:56'!
urlObject: aUrl

	urlObject := aUrl! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 11/24/1998 22:16'!
user
	^ user! !

!ServerDirectory methodsFor: 'accessing' stamp: 'tk 1/3/98 21:36'!
user: anObject
	user := anObject! !

!ServerDirectory methodsFor: 'multi-action sessions' stamp: 'mir 12/8/2003 12:55'!
wakeUp
	"Start a multi-action session: Open for FTP and keep the connection open"

	self isTypeFTP
		ifTrue: [client := self openFTPClient].
	self keepAlive: true
! !

!ServerDirectory class methodsFor: 'initialization' stamp: 'StephaneDucasse 8/21/2010 22:57'!
cleanUp: aggressive
	"Clean out servers when doing aggressive cleanup"

	aggressive ifTrue: [self  resetServers].! !

!ServerDirectory class methodsFor: 'server groups' stamp: 'MarcusDenker 3/5/2010 14:33'!
groupNames
	"Return the names of all registered groups of servers, including individual servers not in any group."
	"ServerDirectory groupNames"
	| names |
	names := Set new.
	self servers do: [:server |
		names add: server groupName].
	^names asArray sort
! !

!ServerDirectory class methodsFor: 'misc' stamp: 'tk 9/8/1998 11:57'!
newFrom: aSimilarObject
	"Must copy the urlObject, so they won't be shared"

	| inst |
	inst := super newFrom: aSimilarObject.
	inst urlObject: aSimilarObject urlObject copy.
	^ inst! !

!ServerDirectory class methodsFor: 'misc' stamp: 'tak 9/25/2008 15:10'!
on: pathString

	^ self new on: pathString! !

!ServerDirectory class methodsFor: 'misc' stamp: 'kfr 7/18/2003 11:18'!
parseFTPEntry: ftpEntry
	| tokens longy dateInSeconds thisYear thisMonth |
	thisYear := Date today year.
	thisMonth := Date today monthIndex.
	tokens := ftpEntry findTokens: ' '. 

	tokens size = 8 ifTrue:
		[((tokens at: 6) size ~= 3 and: [(tokens at: 5) size = 3]) ifTrue:
			["Fix for case that group is blank (relies on month being 3 chars)"
			tokens := tokens copyReplaceFrom: 4 to: 3 with: {'blank'}]].
	tokens size >= 9 ifFalse:[^nil].

	((tokens at: 6) size ~= 3 and: [(tokens at: 5) size = 3]) ifTrue:
		["Fix for case that group is blank (relies on month being 3 chars)"
		tokens := tokens copyReplaceFrom: 4 to: 3 with: {'blank'}].

	tokens size > 9 ifTrue:
		[longy := tokens at: 9.
		10 to: tokens size do: [:i | longy := longy , ' ' , (tokens at: i)].
		tokens at: 9 put: longy].
	dateInSeconds := self
		secondsForDay: (tokens at: 7) 
		month: (tokens at: 6) 
		yearOrTime: (tokens at: 8) 
		thisMonth: thisMonth 
		thisYear: thisYear. 

	^DirectoryEntry name: (tokens last)  "file name"
		creationTime: dateInSeconds "creation date"
		modificationTime: dateInSeconds "modification time"
		isDirectory:( (tokens first first) = $d or: [tokens first first =$l]) "is-a-directory flag"
		fileSize: tokens fifth asNumber "file size"
! !

!ServerDirectory class methodsFor: 'available servers' stamp: 'mir 6/26/2001 09:47'!
removeServerNamed: nameString
	self
		removeServerNamed: nameString
		ifAbsent: [self error: 'Server "' , nameString asString , '" not found']! !

!ServerDirectory class methodsFor: 'available servers' stamp: 'mir 6/26/2001 09:46'!
removeServerNamed: nameString ifAbsent: aBlock
	self servers removeKey: nameString ifAbsent: [aBlock value]! !

!ServerDirectory class methodsFor: 'available servers' stamp: 'StephaneDucasse 7/10/2010 18:21'!
resetServers
	Servers := nil! !

!ServerDirectory class methodsFor: 'misc' stamp: 'damiencassou 5/30/2008 15:52'!
secondsForDay: dayToken month: monthToken yearOrTime: ytToken thisMonth: thisMonth thisYear: thisYear 
	| ftpDay ftpMonth pickAYear jDateToday trialJulianDate |
	ftpDay := dayToken asNumber.
	ftpMonth := Date indexOfMonth: monthToken.
	(ytToken includes: $:) ifFalse: 
		[ ^ (Date 
			newDay: ftpDay
			month: ftpMonth
			year: ytToken asNumber) asSeconds ].
	jDateToday := Date today dayOfYear.
	trialJulianDate := (Date 
		newDay: ftpDay
		month: ftpMonth
		year: thisYear) dayOfYear.

	"Date has no year if within six months (do we need to check the day, too?)"

	"Well it appear to be pickier than that... it isn't just 6 months or 6 months and the day of the month, put perhaps the julian date AND the time as well. I don't know what the precise standard is, but this seems to produce better results"
	pickAYear := jDateToday - trialJulianDate > 182 
		ifTrue: [ thisYear + 1	"his clock could be ahead of ours??" ]
		ifFalse: 
			[ pickAYear := trialJulianDate - jDateToday > 182 
				ifTrue: [ thisYear - 1 ]
				ifFalse: [ thisYear ] ].
	^ (Date 
		newDay: ftpDay
		month: ftpMonth
		year: pickAYear) asSeconds + (Time readFrom: ytToken readStream) asSeconds! !

!ServerDirectory class methodsFor: 'server groups' stamp: 'mir 6/26/2001 12:06'!
serverInGroupNamed: groupName
	"Return the first (available) server in the group of this name."

	| servers |
	servers := self serversInGroupNamed: groupName.
	servers isEmpty
		ifTrue: [self error: 'No server found in group "' , groupName asString , '".'].
	^servers first! !

!ServerDirectory class methodsFor: 'available servers' stamp: 'mir 6/26/2001 14:06'!
serverNamed: nameString
	^self serverNamed: nameString ifAbsent: [self error: 'Server name not found']! !

!ServerDirectory class methodsFor: 'available servers' stamp: 'mir 6/26/2001 09:44'!
serverNamed: nameString ifAbsent: aBlock
	^self servers at: nameString asString ifAbsent: [aBlock value]! !

!ServerDirectory class methodsFor: 'available servers' stamp: 'MarcusDenker 3/5/2010 14:34'!
serverNames
	^self servers keys sort! !

!ServerDirectory class methodsFor: 'available servers' stamp: 'mir 6/26/2001 09:45'!
servers
	Servers ifNil: [Servers := Dictionary new].
	^Servers! !

!ServerDirectory class methodsFor: 'server groups' stamp: 'mir 6/26/2001 11:55'!
serversInGroupNamed: nameString
	"Return the servers in the group of this name."
	"ServerDirectory serversInGroupNamed: 'Squeak Public Updates' "

	^self servers values select: [:server |
		nameString = server groupName].
! !

!ServerFile methodsFor: 'as yet unclassified' stamp: 'tk 3/13/2000 16:53'!
directoryUrl
	| ru |
	"A url to the directory this file is in"

	ru := self realUrl.
	^ ru copyFrom: 1 to: (ru size - fileName size)! !

!ServerFile methodsFor: 'as yet unclassified' stamp: 'tk 5/19/1998 13:12'!
exists
	"Return true if the file exists on the server already"

	^ self fileExists: fileName! !

!ServerFile methodsFor: 'as yet unclassified' stamp: 'tk 11/24/1998 15:01'!
fileName
	"should this be local or as in a url?"

	urlObject ifNotNil: [^ urlObject path last].	"path last encodeForHTTP ?"
	^ fileName! !

!ServerFile methodsFor: 'as yet unclassified' stamp: 'tk 11/24/1998 14:45'!
fileName: aString

urlObject ~~ nil  "type == #file" 
	ifTrue: [urlObject path at: urlObject path size put: aString]
	ifFalse: [fileName := aString]! !

!ServerFile methodsFor: 'as yet unclassified' stamp: 'VeronicaUquillas 6/11/2010 16:08'!
fileNameRelativeTo: aServerDir
	"Produce an absolute fileName from me and an absolute directory"
	urlObject isAbsolute ifFalse: [
		(aServerDir urlObject ~~ nil and: [aServerDir urlObject isAbsolute]) 
			ifTrue: [urlObject 
				privateInitializeFromText: urlObject pathString 
				relativeTo: aServerDir urlObject]
			ifFalse: [urlObject default]].	"relative to the system directory"
	^ urlObject pathForDirectory, self fileName! !

!ServerFile methodsFor: 'as yet unclassified' stamp: 'RAA 6/23/2000 09:45'!
fullPath: serverAndDirectory
	"Parse and save a full path.  Separate out fileName at the end."

	| delim ii |
	super fullPath: serverAndDirectory.		"set server and directory"
	self isTypeFile ifTrue: [
		fileName :=  ''.
		^ self
	].
	delim := self pathNameDelimiter.
	ii := directory findLast: [:c | c = delim].
	ii = 0
		ifTrue: [self error: 'expecting directory and fileName']
		ifFalse: [fileName := directory copyFrom: ii+1 to: directory size.
			directory := (directory copyFrom: 1 to: directory size - fileName size - 1)].! !

!ServerFile methodsFor: 'as yet unclassified' stamp: 'tk 4/13/2000 17:12'!
localName

	^ self fileName! !

!ServerFile methodsFor: 'as yet unclassified' stamp: 'tk 5/18/1998 16:42'!
readOnly
	"Set the receiver to be read-only"

	rwmode := false! !

!ServerFile methodsFor: 'as yet unclassified' stamp: 'tk 5/19/1998 13:35'!
readWrite
	"Set the receiver to be writable"

	rwmode := true! !

!ServerFile methodsFor: 'as yet unclassified' stamp: 'fbs 2/2/2005 13:27'!
realUrl
	"a fully expanded version of the url we represent.  Prefix the path with http: or ftp: or file:"

	self isTypeFile ifTrue: [
		self fileNameRelativeTo: self.
		^ urlObject asString
	].
	^ self typeWithDefault asString, '://', self pathName, '/', fileName	"note difference!!"
	! !

!ServerFile methodsFor: 'as yet unclassified' stamp: 'tk 5/19/1998 13:35'!
writable
	^ rwmode! !
