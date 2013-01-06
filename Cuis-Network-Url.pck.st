'From Cuis 4.1 of 12 December 2012 [latest update: #1528] on 2 January 2013 at 8:24:03 am'!
'Description Please enter a description for this package '!
!classDefinition: #Authorizer category: #'Cuis-Network-Url'!
Object subclass: #Authorizer
	instanceVariableNames: 'users realm'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Url'!
!classDefinition: 'Authorizer class' category: #'Cuis-Network-Url'!
Authorizer class
	instanceVariableNames: ''!

!classDefinition: #Url category: #'Cuis-Network-Url'!
Object subclass: #Url
	instanceVariableNames: 'fragment'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Url'!
!classDefinition: 'Url class' category: #'Cuis-Network-Url'!
Url class
	instanceVariableNames: ''!

!classDefinition: #FileUrl category: #'Cuis-Network-Url'!
Url subclass: #FileUrl
	instanceVariableNames: 'host path isAbsolute'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Url'!
!classDefinition: 'FileUrl class' category: #'Cuis-Network-Url'!
FileUrl class
	instanceVariableNames: ''!

!classDefinition: #GenericUrl category: #'Cuis-Network-Url'!
Url subclass: #GenericUrl
	instanceVariableNames: 'schemeName locator'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Url'!
!classDefinition: 'GenericUrl class' category: #'Cuis-Network-Url'!
GenericUrl class
	instanceVariableNames: ''!

!classDefinition: #BrowserUrl category: #'Cuis-Network-Url'!
GenericUrl subclass: #BrowserUrl
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Url'!
!classDefinition: 'BrowserUrl class' category: #'Cuis-Network-Url'!
BrowserUrl class
	instanceVariableNames: ''!

!classDefinition: #HierarchicalUrl category: #'Cuis-Network-Url'!
Url subclass: #HierarchicalUrl
	instanceVariableNames: 'schemeName authority path query port username password'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Url'!
!classDefinition: 'HierarchicalUrl class' category: #'Cuis-Network-Url'!
HierarchicalUrl class
	instanceVariableNames: ''!

!classDefinition: #FtpUrl category: #'Cuis-Network-Url'!
HierarchicalUrl subclass: #FtpUrl
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Url'!
!classDefinition: 'FtpUrl class' category: #'Cuis-Network-Url'!
FtpUrl class
	instanceVariableNames: ''!

!classDefinition: #HttpUrl category: #'Cuis-Network-Url'!
HierarchicalUrl subclass: #HttpUrl
	instanceVariableNames: 'realm'
	classVariableNames: 'Passwords'
	poolDictionaries: ''
	category: 'Cuis-Network-Url'!
!classDefinition: 'HttpUrl class' category: #'Cuis-Network-Url'!
HttpUrl class
	instanceVariableNames: ''!

!classDefinition: #HttpsUrl category: #'Cuis-Network-Url'!
HttpUrl subclass: #HttpsUrl
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Url'!
!classDefinition: 'HttpsUrl class' category: #'Cuis-Network-Url'!
HttpsUrl class
	instanceVariableNames: ''!

!classDefinition: #MailtoUrl category: #'Cuis-Network-Url'!
GenericUrl subclass: #MailtoUrl
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Url'!
!classDefinition: 'MailtoUrl class' category: #'Cuis-Network-Url'!
MailtoUrl class
	instanceVariableNames: ''!


!Authorizer commentStamp: '<historical>' prior: 0!
The Authorizer does user authorization checking. Each instance of authorizer keeps track of the realm that it is authorizing for, and the table of authorized users. An authorizer can be asked to return the user name/symbol associated with a userID (which concatenates the username and password from the HTTP request) with the user: method.
!

!BrowserUrl commentStamp: '<historical>' prior: 0!
URLs that instruct a browser to do something.!

!FileUrl commentStamp: 'gk 10/21/2005 10:58' prior: 0!
This class models a file URL according to (somewhat) RFC1738, see http://www.w3.org/Addressing/rfc1738.txt

Here is the relevant part of the RFC:

3.10 FILES

   The file URL scheme is used to designate files accessible on a
   particular host computer. This scheme, unlike most other URL schemes,
   does not designate a resource that is universally accessible over the
   Internet.

   A file URL takes the form:

       file://<host>/<path>

   where <host> is the fully qualified domain name of the system on
   which the <path> is accessible, and <path> is a hierarchical
   directory path of the form <directory>/<directory>/.../<name>.

   For example, a VMS file

     DISK$USER:[MY.NOTES]NOTE123456.TXT

   might become

     <URL:file://vms.host.edu/disk$user/my/notes/note12345.txt>

   As a special case, <host> can be the string "localhost" or the empty
   string; this is interpreted as `the machine from which the URL is
   being interpreted'.

   The file URL scheme is unusual in that it does not specify an
   Internet protocol or access method for such files; as such, its
   utility in network protocols between hosts is limited.

From the above we can conclude that the RFC says that the <path> part never starts or ends with a slash and is always absolute. If the last name can be a directory instead of a file is not specified clearly.

The path is stored as a SequenceableCollection of path parts.

Notes regarding non RFC features in this class:

- If the last path part is the empty string, then the FileUrl is referring to a directory. This is also shown with a trailing slash when converted to a String.

- The FileUrl has an attribute isAbsolute which signals if the path should be considered absolute or relative to the current directory. This distinction is not visible in the String representation of FileUrl, since the RFC does not have that.

- Fragment is supported (kept for historical reasons)

!

!FtpUrl commentStamp: 'ls 6/15/2003 13:44' prior: 0!
a reference to a file which may be downloaded by anonymous ftp .



TODO: use the username and password, if specified
!

!GenericUrl commentStamp: '<historical>' prior: 0!
a URL type that can't be broken down in any systematic way.  For example, mailto: and telnet: URLs.  The part after the scheme name is stored available via the #locator message.!

!HierarchicalUrl commentStamp: '<historical>' prior: 0!
A URL which has a hierarchical encoding.  For instance, http and ftp URLs are hierarchical.!

!HttpUrl commentStamp: 'ls 6/15/2003 13:44' prior: 0!
A URL that can be accessed via the Hypertext Transfer Protocol (HTTP), ie, a standard Web URL

realm = the name of the security realm that has been discovered for this URL.   Look it up in Passwords.

Passwords = a Dictionary of (realm -> encoded user&password)


TODO: use the username and password, if specified
!

!MailtoUrl commentStamp: '<historical>' prior: 0!
a URL specifying a mailing address; activating it triggers a mail-sender to start up, if one is present.!

!Url commentStamp: '<historical>' prior: 0!
A Uniform Resource Locator.  It specifies the location of a document on the Internet.  The base class is abstract; child classes break different types of URLs down in ways appropriate for that type.!

!Authorizer methodsFor: 'authentication' stamp: 'PeterHugossonMiller 9/3/2009 00:12'!
encode: nameString password: pwdString
	"Encode per RFC1421 of the username:password combination."

	| clear code clearSize idx map |
	clear := (nameString, ':', pwdString) asByteArray.
	clearSize := clear size.
	[ clear size \\ 3 ~= 0 ] whileTrue: [ clear := clear, #(0) ].
	idx := 1.
	map := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'.
	code := String new writeStream.
	[ idx < clear size ] whileTrue: [ code 
		nextPut: (map at: (clear at: idx) // 4 + 1);
		nextPut: (map at: (clear at: idx) \\ 4 * 16 + ((clear at: idx + 1) // 16) + 1);
   		nextPut: (map at: (clear at: idx + 1) \\ 16 * 4 + ((clear at: idx + 2) // 64) + 1);
   		nextPut: (map at: (clear at: idx + 2) \\ 64 + 1).
		idx := idx + 3 ].
	code := code contents.
	idx := code size.
	clear size - clearSize timesRepeat: [ code at: idx put: $=. idx := idx - 1].
	^code! !

!Authorizer methodsFor: 'authentication' stamp: 'mjg 11/3/97 12:31'!
mapFrom: aKey to: aPerson
	"Establish a mapping from a RFC 1421 key to a user."

	users isNil ifTrue: [ users := Dictionary new ].
	aPerson
	 isNil ifTrue: [ users removeKey: aKey ]
	 ifFalse: [
		users removeKey: (users keyAtValue: aPerson ifAbsent: []) ifAbsent: [].
		users at: aKey put: aPerson ]
! !

!Authorizer methodsFor: 'authentication' stamp: 'tk 5/21/1998 16:32'!
mapName: nameString password: pwdString to: aPerson
	"Insert/remove the encoding per RFC1421 of the username:password combination into/from the UserMap.  DO NOT call this directly, use mapName:password:to: in your ServerAction class.  Only it knows how to record the change on the disk!!"

	self mapFrom: (self encode: nameString password: pwdString) to: aPerson
! !

!Authorizer methodsFor: 'realms' stamp: 'mjg 11/3/97 12:33'!
realm
	^realm! !

!Authorizer methodsFor: 'realms' stamp: 'mjg 11/3/97 12:33'!
realm: aString
	realm := aString
! !

!Authorizer methodsFor: 'authentication' stamp: 'ar 8/17/2001 18:19'!
user: userId
	"Return the requesting user."
	^users at: userId ifAbsent: [ self error: (self class unauthorizedFor: realm) ]! !

!Authorizer class methodsFor: 'as yet unclassified' stamp: 'adrian_lienhard 7/18/2009 15:52'!
unauthorizedFor: realm
	^'HTTP/1.0 401 Unauthorized', self crlf, 'WWW-Authenticate: Basic realm="Pharo/',realm,'"',
	String crlfcrlf, '<html><title>Unauthorized</title><body><h2>Unauthorized for ',realm, '</h2></body></html>'

! !

!BrowserUrl methodsFor: 'downloading' stamp: 'ls 8/4/1998 20:42'!
hasContents
	^true! !

!BrowserUrl class methodsFor: 'constants' stamp: 'SeanDeNigris 1/29/2011 19:33'!
schemeName

	^ 'browser'! !

!FileUrl methodsFor: 'downloading' stamp: 'VeronicaUquillas 6/11/2010 14:01'!
default
	"Use the default local Pharo file directory."
	
	| local |
	local := self class pathParts: (FileDirectory default pathParts), #('') isAbsolute: true.
	self privateInitializeFromText: self pathString relativeTo: local.
		"sets absolute also"! !

!FileUrl methodsFor: 'access' stamp: 'gk 10/21/2005 10:21'!
directoryUrl
	"The path always has at least one element so this works."

	^self copy path: (path copyFrom: 1 to: path size - 1)! !

!FileUrl methodsFor: 'access' stamp: 'gk 10/21/2005 11:14'!
fileName
	"Return the last part of the path,
	most often a filename but can also be a directory."

	^self path last! !

!FileUrl methodsFor: 'testing' stamp: 'gk 2/9/2004 20:32'!
firstPartIsDriveLetter
	"Return true if the first part of the path is a letter
	followed by a $: like 'C:' "
	
	| firstPart |
	path isEmpty ifTrue: [^false].
	firstPart := path first.
	^firstPart size = 2 and: [
		firstPart first isLetter
			and: [firstPart last = $:]]! !

!FileUrl methodsFor: 'downloading' stamp: 'ls 8/4/1998 20:42'!
hasContents
	^true! !

!FileUrl methodsFor: 'accessing' stamp: 'gk 2/10/2004 10:16'!
host
	"Return the host name, either 'localhost', '', or a fully qualified domain name."
	
	^host ifNil: ['']! !

!FileUrl methodsFor: 'accessing' stamp: 'gk 2/12/2004 16:22'!
host: hostName
	"Set the host name, either 'localhost', '', or a fully qualified domain name."
	
	host := hostName! !

!FileUrl methodsFor: 'private-initialization' stamp: 'gk 2/10/2004 13:05'!
host: aHostString pathParts: aCollection isAbsolute: aBoolean

	host := aHostString.
	path := aCollection.
	isAbsolute := aBoolean! !

!FileUrl methodsFor: 'private-initialization' stamp: 'gk 2/12/2004 16:01'!
initializeFromPathString: aPathString
	"<aPathString> is a file path as a String.
	We construct a path collection using various heuristics."

	| pathString hasDriveLetter |
	pathString := aPathString.
	pathString isEmpty ifTrue: [pathString := '/'].
	path := (pathString findTokens: '/') collect: [:token | token unescapePercents].

	"A path like 'C:' refers in practice to 'c:/'"
	((pathString endsWith: '/') or:
		[(hasDriveLetter := self firstPartIsDriveLetter) and: [path size = 1]])
			ifTrue: [path add: ''].

	"Decide if we are absolute by checking for leading $/ or
	beginning with drive letter. Smarts for other OSes?"
	self isAbsolute: ((pathString beginsWith: '/')
						or: [hasDriveLetter ifNil: [self firstPartIsDriveLetter]])! !

!FileUrl methodsFor: 'accessing' stamp: 'gk 10/21/2005 11:12'!
isAbsolute
	"Should the path be considered absolute to
	the filesystem instead of relative to the default directory?"
 
	^isAbsolute! !

!FileUrl methodsFor: 'accessing' stamp: 'gk 10/21/2005 11:13'!
isAbsolute: aBoolean
	"Set if the path should be considered absolute to
	the filesystem instead of relative to the default directory."

	isAbsolute := aBoolean! !

!FileUrl methodsFor: 'accessing' stamp: 'gk 2/10/2004 00:15'!
path
	"Return an ordered collection of the path elements."
	
	^path! !

!FileUrl methodsFor: 'accessing' stamp: 'gk 10/21/2005 11:11'!
path: aCollection
	"Set the collection of path elements."

	path := aCollection! !

!FileUrl methodsFor: 'paths' stamp: 'gk 2/10/2004 00:19'!
pathDirString
	"Path to directory as url, using slash as delimiter.
	Filename is left out."

	^String streamContents: [ :s |
		isAbsolute ifTrue: [ s nextPut: $/ ].
		1 to: self path size - 1 do: [ :ii |
			s nextPutAll: (path at: ii); nextPut: $/]]! !

!FileUrl methodsFor: 'paths' stamp: 'gk 10/21/2005 10:01'!
pathForDirectory
	"Path using local file system's pathname delimiter.
	DOS paths with drive letters should not
	be prepended with a delimiter even though
	they are absolute. Filename is left out."

	| delimiter |
	delimiter :=  FileDirectory default pathNameDelimiter.
	^String streamContents: [ :s |
		(self isAbsolute and: [self firstPartIsDriveLetter not])
			ifTrue: [ s nextPut: delimiter ].
		1 to: self path size - 1 do: [ :ii |
			s nextPutAll: (path at: ii); nextPut: delimiter]]! !

!FileUrl methodsFor: 'access' stamp: 'ar 10/13/2004 17:54'!
pathForFile
	"Path using local file system's delimiter.  $\ or $:"
	^FileDirectory default pathFromUrl: self! !

!FileUrl methodsFor: 'private-initialization' stamp: 'gk 2/10/2004 13:04'!
pathParts: aCollection isAbsolute: aBoolean

	^self host: nil pathParts: aCollection isAbsolute: aBoolean! !

!FileUrl methodsFor: 'paths' stamp: 'nice 1/5/2010 15:59'!
pathString
	"Path as it appears in a URL with $/ as delimiter."
	
	
	^String streamContents: [ :s | | first |
		"isAbsolute ifTrue:[ s nextPut: $/ ]."
		first := true.
		self path do: [ :p |
			first ifFalse: [ s nextPut: $/ ].
			first := false.
			s nextPutAll: p encodeForHTTP ] ]! !

!FileUrl methodsFor: 'copying' stamp: 'nice 1/13/2010 21:57'!
postCopy
	"Be sure not to share the path with the copy."

	super postCopy.
	path := path copy! !

!FileUrl methodsFor: 'printing' stamp: 'fbs 2/2/2005 13:09'!
printOn: aStream
	"Return the FileUrl according to RFC1738 plus supporting fragments:
		'file://<host>/<path>#<fragment>'
	Note that <host> being '' is equivalent to 'localhost'.
	Note: The pathString can not start with a leading $/
	to indicate an 'absolute' file path.
	This is not according to RFC1738 where the path should have
	no leading or trailing slashes, and always
	be considered absolute relative to the filesystem."

	aStream nextPutAll: self schemeName, '://'.

	host ifNotNil: [aStream nextPutAll: host].

	aStream
		nextPut: $/;
		nextPutAll: self pathString.

	fragment ifNotNil:
		[aStream
			nextPut: $#;
			nextPutAll: fragment encodeForHTTP].! !

!FileUrl methodsFor: 'private-initialization' stamp: 'SvenVanCaekenberghe 1/8/2012 14:44'!
privateInitializeFromText: aString
	"Calculate host and path from a file URL in String format.
	Some malformed formats are allowed and interpreted by guessing."

	| schemeName pathString bare hasDriveLetter stream char i |
	bare := aString trimBoth.
	schemeName := Url schemeNameForString: bare.
	(schemeName isNil or: [schemeName ~= self schemeName])
		ifTrue: [
			host := ''.
			pathString := bare]
		ifFalse: [
			"First remove schemeName and colon"
			bare := bare copyFrom: (schemeName size + 2) to: bare size.
			"A proper file URL then has two slashes before host,
			A malformed URL is interpreted as using syntax file:<path>."
			(bare beginsWith: '//')
				ifTrue: [i := bare indexOf: $/ startingAt: 3.
						i=0 ifTrue: [
								host := bare copyFrom: 3 to: bare size.
								pathString := '']
							ifFalse: [
								host := bare copyFrom: 3 to: i-1.
								pathString := bare copyFrom: host size + 3 to: bare size]]
				ifFalse: [host := ''.
						pathString := bare]].
	self initializeFromPathString: pathString
! !

!FileUrl methodsFor: 'private-initialization' stamp: 'gk 2/12/2004 16:29'!
privateInitializeFromText: pathString relativeTo: aUrl
	"<pathString> should be a filesystem path.
	This url is adjusted to be aUrl + the path."

	| bare newPath |
	self host: aUrl host.
	self initializeFromPathString: pathString.
	self isAbsolute: aUrl isAbsolute.

	newPath := aUrl path copy.
	newPath removeLast.	"empty string that says its a directory"
	path do: [ :token |
		((token ~= '..') and: [token ~= '.']) ifTrue: [ 
			newPath addLast: token unescapePercents ].
		token = '..' ifTrue: [ 
			newPath isEmpty ifFalse: [ 
				newPath last = '..' ifFalse: [ newPath removeLast ] ] ].
		"token = '.' do nothing" ].
	path := newPath

	! !

!FileUrl methodsFor: 'downloading' stamp: 'nice 1/5/2010 15:59'!
retrieveContents
	| s entries pathString |
	pathString := self pathForFile.
	
	"We pursue the execution even if the file is not found"
	[ | type file |file := FileStream readOnlyFileNamed: pathString.
	  	type := file mimeTypes.
		type ifNotNil: [type := type first].
		type ifNil: [type := MIMEDocument guessTypeFromName: self path last].
		^MIMELocalFileDocument 
			contentStream: file
			mimeType: type]  on: FileDoesNotExistException do:[:ex| ].

	"see if it's a directory... If not, then nil is returned"
	entries := [(FileDirectory on: pathString) entries] 
				on: InvalidDirectoryError do: [:ex| ^ nil].

	s := String new writeStream.
	(pathString endsWith: '/') ifFalse: [ pathString := pathString, '/' ].
	s nextPutAll: '<title>Directory Listing for ', pathString, '</title>'.
	s nextPutAll: '<h1>Directory Listing for ', pathString, '</h1>'.
	s nextPutAll: '<ul>'.
	s cr.
	entries do: [ :entry |
		s nextPutAll: '<li><a href="'.
		s nextPutAll: entry name.
		s nextPutAll: '">'.
		s nextPutAll: entry name.
		s nextPutAll: '</a>'.
		s cr. ].
	s nextPutAll: '</ul>'.
	^MIMEDocument  contentType: 'text/html'  content: s contents  url: ('file://', pathString)! !

!FileUrl methodsFor: 'classification' stamp: 'gk 2/10/2004 10:34'!
scheme
	^self class schemeName! !

!FileUrl methodsFor: 'classification' stamp: 'gk 2/10/2004 10:34'!
schemeName
	^self class schemeName! !

!FileUrl class methodsFor: 'instance creation' stamp: 'gk 2/10/2004 12:16'!
absoluteFromText: aString
	"Method that can be called explicitly to create a FileUrl."

	^self new privateInitializeFromText: aString! !

!FileUrl class methodsFor: 'instance creation' stamp: 'gk 2/10/2004 13:04'!
host: aHost pathParts: aCollectionOfPathParts isAbsolute: aBoolean
	"Create a FileUrl."

	^self new host: aHost pathParts: aCollectionOfPathParts isAbsolute: aBoolean! !

!FileUrl class methodsFor: 'instance creation' stamp: 'gk 2/10/2004 13:10'!
pathParts: aCollectionOfPathParts
	"Create a FileUrl."

	^self host: nil pathParts: aCollectionOfPathParts isAbsolute: true! !

!FileUrl class methodsFor: 'instance creation' stamp: 'gk 2/10/2004 13:06'!
pathParts: aCollectionOfPathParts isAbsolute: aBoolean
	"Create a FileUrl."

	^self host: nil pathParts: aCollectionOfPathParts isAbsolute: aBoolean! !

!FileUrl class methodsFor: 'constants' stamp: 'gk 2/10/2004 10:33'!
schemeName
	^'file'! !

!FtpUrl methodsFor: 'downloading' stamp: 'PeterHugossonMiller 9/3/2009 01:34'!
downloadUrl
	"Returns a http download url for the location defined by this url."
	| ans |
	ans := String new writeStream.
	ans nextPutAll: self schemeName.
	ans nextPutAll: '://'.
	ans nextPutAll: self authority.
	port ifNotNil: [ans nextPut: $:; print: port].
	path do: [ :pathElem |
		ans nextPut: $/.
		ans nextPutAll: pathElem encodeForHTTP. ].
	self query isNil ifFalse: [ 
		ans nextPut: $?.
		ans nextPutAll: self query. ].
	self fragment isNil ifFalse: [
		ans nextPut: $#.
		ans nextPutAll: self fragment encodeForHTTP. ].
	
	^ans contents! !

!FtpUrl methodsFor: 'access' stamp: 'ls 7/24/1998 00:18'!
pathString
	self path isEmpty ifTrue: [ ^'/' copy ].

	^String streamContents: [ :s |
		self path do: [ :p |
		 	s nextPut: $/.
			s nextPutAll: p ] ]! !

!FtpUrl methodsFor: 'downloading' stamp: 'adrian_lienhard 7/18/2009 15:56'!
retrieveContents
	"currently assumes directories end in /, and things that don't end in / are files.  Also, doesn't handle errors real well...."
	| server contents pathString listing auth idx fileName serverName userName password |
	pathString := self pathString.
	pathString := pathString copyFrom: 2 to: pathString size. "remove the leading /"
	pathString last = $/ ifTrue:["directory?!!"
		fileName := nil.
	] ifFalse:[
		fileName := pathString copyFrom: (pathString lastIndexOf: $/)+1 to: pathString size.
		pathString := pathString copyFrom: 1 to: (pathString lastIndexOf: $/) - 1.
	].
	auth := self authority.
	idx := auth indexOf: $@.
	idx > 0 ifTrue:[
		serverName := (auth copyFrom: idx+1 to: auth size).
		userName := (auth copyFrom: 1 to: idx-1).
		password := nil.
	] ifFalse:[
		serverName := auth.
		userName := 'anonymous'.
		password := 'user'.
	].
	server := ServerDirectory servers 
		detect:[:s| s isTypeFTP and:[s server asLowercase = serverName asLowercase]]
		ifNone:[nil].
	server ifNil:[
		server := ServerDirectory new.
		server server: serverName.
	] ifNotNil:[server := server copy reset].
	server user: userName.
	password ifNotNil:[server password: password].
	server directory: pathString.

	fileName == nil ifFalse:[
		"a file"
		contents := (server getFileNamed: fileName).
		server sleep.
		^MIMEDocument contentType: (MIMEDocument guessTypeFromName: self path last) content: contents].

	"a directory?"
	listing := String streamContents: [ :stream |
		stream nextPutAll: '<title>', self pathString, '</title>'; cr.
		stream nextPutAll: '<h1>Listing for ', self pathString, '</h1>'; cr.
		stream nextPutAll: '<ul>'; cr.
		server entries do: [ :entry |
			stream nextPutAll: '<li>';
				nextPutAll: '<a href="', entry name encodeForHTTP.
			entry isDirectory ifTrue: [ stream nextPut: $/ ].
			stream nextPutAll: '">';
				nextPutAll: entry name;
				nextPutAll: '</a>';
				cr ] ].
	server sleep.
	^MIMEDocument contentType: 'text/html' content: listing! !

!FtpUrl class methodsFor: 'constants' stamp: 'SeanDeNigris 1/29/2011 19:34'!
schemeName

	^ 'ftp'.! !

!GenericUrl methodsFor: 'access' stamp: 'ls 6/20/1998 19:46'!
locator
	^locator! !

!GenericUrl methodsFor: 'printing' stamp: 'CamilloBruni 12/16/2011 11:27'!
printOn: aStream
	
	self schemeName ifNotNil: [
		aStream nextPutAll: self schemeName; nextPut: $:].
	
	aStream nextPutAll: self locator.

	self fragment ifNotNil: [
		aStream nextPut: $#; nextPutAll: self fragment].! !

!GenericUrl methodsFor: 'parsing' stamp: 'CamilloBruni 12/16/2011 11:24'!
privateInitializeFromText: aString
	schemeName := Url schemeNameForString: aString.
	locator :=  schemeName 
		ifNil: [ aString ]
		ifNotNil: [ aString copyFrom: (schemeName size+2) to: aString size ].! !

!GenericUrl methodsFor: 'parsing' stamp: 'ls 8/4/1998 01:28'!
privateInitializeFromText: aString relativeTo: aUrl
	schemeName := aUrl schemeName.
	locator := aString.! !

!GenericUrl methodsFor: 'classification' stamp: 'FBS 11/20/2003 13:39'!
scheme
	^ self schemeName.! !

!GenericUrl methodsFor: 'access' stamp: 'ls 6/20/1998 19:46'!
schemeName
	^schemeName! !

!GenericUrl methodsFor: 'private' stamp: 'ls 6/20/1998 19:46'!
schemeName: schemeName0  locator: locator0
	schemeName := schemeName0.
	locator := locator0.! !

!GenericUrl class methodsFor: 'parsing' stamp: 'ls 7/26/1998 21:24'!
absoluteFromText: aString
	| schemeName locator |
	schemeName := Url schemeNameForString: aString.
	schemeName ifNil: [ ^self schemeName: 'xnoscheme' locator: aString ].
	locator := aString copyFrom: (schemeName size + 2) to: aString size.
	^self schemeName: schemeName locator: locator! !

!GenericUrl class methodsFor: 'instance creation' stamp: 'ls 6/20/1998 19:46'!
schemeName: schemeName  locator: locator
	^self new schemeName: schemeName  locator: locator! !

!HierarchicalUrl methodsFor: 'access' stamp: 'ls 6/20/1998 19:58'!
authority
	^authority! !

!HierarchicalUrl methodsFor: 'access' stamp: 'gk 10/21/2005 10:21'!
directoryUrl
	"The path always has at least one element so this works."

	^self copy path: (path copyFrom: 1 to: path size - 1)! !

!HierarchicalUrl methodsFor: 'access' stamp: 'gk 10/21/2005 11:06'!
fileName
	"Return the last part of the path,
	most often a filename but does not need to be."

	^self path last! !

!HierarchicalUrl methodsFor: 'printing' stamp: 'PeterHugossonMiller 9/3/2009 02:00'!
fullPath
	| ans |
	ans := String new writeStream.
	path do: [ :pathElem |
		ans nextPut: $/.
		ans nextPutAll: pathElem encodeForHTTP. ].
	self query isNil ifFalse: [ 
		ans nextPut: $?.
		ans nextPutAll: self query. ].
	self fragment isNil ifFalse: [
		ans nextPut: $#.
		ans nextPutAll: self fragment encodeForHTTP. ].
	
	^ans contents! !

!HierarchicalUrl methodsFor: 'downloading' stamp: 'ls 8/4/1998 20:44'!
hasContents
	"most of these do...."
	^true! !

!HierarchicalUrl methodsFor: 'access' stamp: 'tk 9/6/1998 12:45'!
isAbsolute
	
	path size > 0 ifFalse: [^ false].
	(path at: 1) size > 0 ifFalse: [^ false].
	^ ((path at: 1) at: 1) ~~ $.! !

!HierarchicalUrl methodsFor: 'access' stamp: 'ls 6/15/2003 13:13'!
password
	"http://user:pword@foo.com' asUrl password"
	^password! !

!HierarchicalUrl methodsFor: 'access' stamp: 'KLC 4/3/2006 10:05'!
path
	"return a collection of the decoded path elements, as strings"
	^path! !

!HierarchicalUrl methodsFor: 'access' stamp: 'gk 10/21/2005 11:15'!
path: aCollection
	"Set the collection of path elements."

	path := aCollection! !

!HierarchicalUrl methodsFor: 'access' stamp: 'mir 7/30/1999 13:05'!
port
	^port! !

!HierarchicalUrl methodsFor: 'copying' stamp: 'nice 1/13/2010 21:58'!
postCopy
	"Be sure not to share the path with the copy"

	super postCopy.
	path := path copy! !

!HierarchicalUrl methodsFor: 'printing' stamp: 'StephaneDucasse 5/9/2010 20:19'!
printOn: aStream

	aStream nextPutAll: self schemeName.
	aStream nextPutAll: '://'.
	self username ifNotNil: [
		aStream nextPutAll: self username encodeForHTTP.
		self password ifNotNil: [
			aStream nextPutAll: ':'.
			aStream nextPutAll: self password encodeForHTTP].
		aStream nextPutAll: '@' ].
	aStream nextPutAll: self authority.
	port ifNotNil: [aStream nextPut: $:; print: port].
	path do: [ :pathElem |
		aStream nextPut: $/.
		aStream nextPutAll: pathElem encodeForHTTP. ].
	self query isNil ifFalse: [ 
		aStream nextPut: $?.
		aStream nextPutAll: self query. ].
	self fragment isNil ifFalse: [
		aStream nextPut: $#.
		aStream nextPutAll: self fragment encodeForHTTP. ].! !

!HierarchicalUrl methodsFor: 'parsing' stamp: 'StephaneDucasse 5/9/2010 20:26'!
privateInitializeFromText: aString 
	| remainder ind specifiedSchemeName |
	remainder := aString.
	schemeName ifNil: 
			[specifiedSchemeName := Url schemeNameForString: remainder.
			specifiedSchemeName ifNotNil: 
					[schemeName := specifiedSchemeName.
					remainder := remainder copyFrom: schemeName size + 2 to: remainder size].
			schemeName ifNil: 
					["assume HTTP"
					schemeName := 'http']].

	"remove leading // if it's there"
	(remainder beginsWith: '//') 
		ifTrue: [remainder := remainder copyFrom: 3 to: remainder size].

	"get the query"
	ind := remainder indexOf: $?.
	ind > 0 
		ifTrue: 
			[query := remainder copyFrom: ind + 1 to: remainder size.
			remainder := remainder copyFrom: 1 to: ind - 1].

	"get the authority"
	ind := remainder indexOf: $/.
	ind > 0 
		ifTrue: 
			[ind = 1 
				ifTrue: [authority := '']
				ifFalse: 
					[authority := remainder copyFrom: 1 to: ind - 1.
					remainder := remainder copyFrom: ind + 1 to: remainder size]]
		ifFalse: 
			[authority := remainder.
			remainder := ''].

	"extract the username+password"
	(authority includes: $@) 
		ifTrue: 
			[username := authority copyUpTo: $@.
			authority := authority copyFrom: (authority indexOf: $@) + 1
						to: authority size.
			(username includes: $:) 
				ifTrue: 
					[password := (username copyFrom: (username indexOf: $:) + 1 to: username size) unescapePercents.
					username := (username copyUpTo: $:) unescapePercents]].

	"Extract the port"
	(authority includes: $:) 
		ifTrue: 
			[| lastColonIndex portString |
			lastColonIndex := authority findLast: [:c | c = $:].
			portString := authority copyFrom: lastColonIndex + 1 to: authority size.
			portString isAllDigits 
				ifTrue: 
					[port := Integer readFromString: portString.
					(port > 65535) ifTrue: [self error: 'Invalid port number'].
					 authority := authority copyFrom: 1 to: lastColonIndex - 1]
				ifFalse:[self error: 'Invalid port number']].

	"get the path"
	path := self privateParsePath: remainder relativeTo: #() .! !

!HierarchicalUrl methodsFor: 'parsing' stamp: 'ls 6/15/2003 13:40'!
privateInitializeFromText: aString relativeTo: aUrl 
	| remainder ind basePath |
	remainder := aString.
	"set the scheme"
	schemeName := aUrl schemeName.

	"a leading // means the authority is specified, meaning it is absolute"
	(remainder beginsWith: '//')
		ifTrue: [^ self privateInitializeFromText: aString].

	"otherwise, use the same authority"
	authority := aUrl authority.
	port := aUrl port.
	username := aUrl username.
	password := aUrl password.

	"get the query"
	ind := remainder indexOf: $?.
	ind > 0
		ifTrue: [query := remainder copyFrom: ind + 1 to: remainder size.
			remainder := remainder copyFrom: 1 to: ind - 1].

	"get the path"
	(remainder beginsWith: '/')
		ifTrue: [ basePath := #() ]
		ifFalse: [ basePath := aUrl path ].
	path := self privateParsePath: remainder  relativeTo: basePath.

! !

!HierarchicalUrl methodsFor: 'parsing' stamp: 'PeterHugossonMiller 9/3/2009 02:00'!
privateParsePath: remainder relativeTo: basePath 
	| nextTok s parsedPath |
	s := remainder readStream.
	parsedPath := OrderedCollection new.
	parsedPath addAll: basePath.
	parsedPath isEmpty ifFalse: [ parsedPath removeLast ].
	
	[ s peek = $/ ifTrue: [ s next ].
	nextTok := String new writeStream.
	[ s atEnd or: [ s peek = $/ ] ] whileFalse: [ nextTok nextPut: s next ].
	nextTok := nextTok contents unescapePercents.
	nextTok = '..' 
		ifTrue: [ parsedPath size > 0 ifTrue: [ parsedPath removeLast ] ]
		ifFalse: [ nextTok ~= '.' ifTrue: [ parsedPath add: nextTok ] ].
	s atEnd ] whileFalse.
	parsedPath isEmpty ifTrue: [ parsedPath add: '' ].
	^ parsedPath! !

!HierarchicalUrl methodsFor: 'access' stamp: 'ls 6/20/1998 19:59'!
query
	"return the query, the part after any ?.  Any %XY's have already been decoded.  If there wasno query part, nil is returned (it is possible to also have an empty query"
	^query ! !

!HierarchicalUrl methodsFor: 'classification' stamp: 'FBS 11/20/2003 13:07'!
scheme
	^ self schemeName.! !

!HierarchicalUrl methodsFor: 'access' stamp: 'ls 6/20/1998 19:58'!
schemeName
	^schemeName! !

!HierarchicalUrl methodsFor: 'private' stamp: 'ls 6/20/1998 19:41'!
schemeName: schemeName0  authority: authority0  path: path0  query: query0
	"initialize a new instance"
	schemeName := schemeName0.
	authority := authority0.
	path := path0.
	query := query0.
! !

!HierarchicalUrl methodsFor: 'access' stamp: 'ls 6/15/2003 13:13'!
username
	"http://user:pword@foo.com' asUrl username"
	^username! !

!HierarchicalUrl class methodsFor: 'instance creation' stamp: 'ls 6/20/1998 19:41'!
schemeName: schemeName  authority: authority  path: path  query: query
	^self new schemeName: schemeName  authority: authority  path: path  query: query! !

!HttpUrl methodsFor: 'downloading' stamp: 'StephaneDucasse 9/7/2011 21:53'!
askNamePassword
	"Authorization is required by the host site.  Ask the user for a userName and password.  Encode them and store under this realm.  Return false if the user wants to give up."

	| user pass |
	(self confirm: 'Host ', self asText, '
wants a different user and password.  Type them now?' orCancel: [false])
		ifFalse: [^ false].
	user := UIManager default request: 'User account name?' initialAnswer: ''.
	pass := UIManager default requestPassword: 'Password?'.
	Passwords at: realm put: (Authorizer new encode: user password: pass).
	^ true! !

!HttpUrl methodsFor: 'downloading' stamp: 'nk 8/30/2004 07:50'!
checkAuthorization: webDocument retry: retryBlock
	"authorization failed if webDocument is a String"
	| oldRealm i end encoded |
	((webDocument isString)
		and: [(webDocument beginsWith: 'HTTP/1.0 401')
			or: [webDocument beginsWith: 'HTTP/1.1 401']])
	ifFalse: [^self].

	oldRealm := realm.
	i := webDocument findString: 'realm="'.
	i = 0 ifTrue: [^self].
	end := webDocument indexOf: $" startingAt: i.
	realm := webDocument copyFrom: i+7 to: end.
	"realm := (webDocument findTokens: '""') at: 2."
	Passwords ifNil: [Passwords := Dictionary new].
	encoded := Passwords at: realm ifAbsent: [nil].
	(oldRealm ~= realm) & (encoded ~~ nil) 
		ifTrue: [^ retryBlock value]
		ifFalse: ["ask the user"
			self askNamePassword ifTrue: [^ retryBlock value]]! !

!HttpUrl methodsFor: 'downloading' stamp: 'noha 5/28/2009 22:14'!
normalizeContents: webDocument
	(webDocument isString) ifTrue: [
		^MIMEDocument
			contentType: 'text/plain'
			content: 'error occured retrieving ', self asString, ': ', webDocument
			url: (Url absoluteFromText: '')].
	webDocument contentType = MIMEDocument defaultContentType ifTrue: [
		^MIMEDocument 
			contents: webDocument content 
			mimeType: (MIMEDocument guessTypeFromName: self path last) 
			uri: webDocument url ].

	^webDocument! !

!HttpUrl methodsFor: 'downloading' stamp: 'fbs 2/2/2005 13:24'!
postFormArgs: args
	| contents request |
	request := realm ifNotNil: [Passwords at: realm ifAbsent: ['']]
		ifNil: [''].
	request = '' ifFalse: [request := 'Authorization: Basic ', request, String crlf].
		"Why doesn't Netscape send the name of the realm instead of Basic?"
	contents := (HTTPSocket httpPostDocument: self asString args: args
				accept: 'application/octet-stream' request: request).

	self checkAuthorization: contents retry: [^ self postFormArgs: args].

	^self normalizeContents: contents! !

!HttpUrl methodsFor: 'downloading' stamp: 'fbs 2/2/2005 13:24'!
postMultipartFormArgs: args
	| contents request |
	request := realm ifNotNil: [Passwords at: realm ifAbsent: ['']]
		ifNil: [''].
	request = '' ifFalse: [request := 'Authorization: Basic ', request, String crlf].
		"Why doesn't Netscape send the name of the realm instead of Basic?"
	contents := (HTTPSocket httpPostMultipart: self asString args: args
				accept: 'application/octet-stream' request: request).

	self checkAuthorization: contents retry: [^ self postMultipartFormArgs: args].

	^self normalizeContents: contents! !

!HttpUrl methodsFor: 'downloading' stamp: 'tk 9/22/1998 19:49'!
privateInitializeFromText: aString relativeTo: aUrl

	super privateInitializeFromText: aString relativeTo: aUrl.
	realm := aUrl realm.! !

!HttpUrl methodsFor: 'downloading' stamp: 'tk 9/22/1998 19:47'!
realm
	^ realm! !

!HttpUrl methodsFor: 'downloading' stamp: 'tk 9/22/1998 20:21'!
retrieveContents
	^ self retrieveContentsArgs: nil! !

!HttpUrl methodsFor: 'downloading'!
retrieveContentsAccept: mimeType
	^ self retrieveContentsArgs: nil accept: mimeType! !

!HttpUrl methodsFor: 'downloading' stamp: 'GabrielOmarCotelli 6/12/2009 21:25'!
retrieveContentsArgs: args 

	"From http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
	 The Accept request-header field can be used to specify certain media types which are acceptable for the response. 
       Accept         = 'Accept' ':'
                        #( media-range [ accept-params ] )
       media-range    = ( '*/*'
                        | ( type '/' '*' )
                        | ( type '/' subtype )
                        ) *( ';' parameter )
       accept-params  = ';' 'q' '=' qvalue *( accept-extension )
       accept-extension = ';' token [ '=' ( token | quoted-string ) ]
	The asterisk *' character is used to group media types into ranges, with '*/*' indicating all media types and 'type/*' indicating 
	all subtypes of that type. Each media-range MAY be followed by one or more accept-params, beginning with the 'q' parameter for 
	indicating a relative quality factor. Quality factors allow the user or user agent to indicate the relative degree of preference for 
	that media-range, using the qvalue scale from 0 to 1"

	| allMimeTypes |
	allMimeTypes := '*/*; q=1'.
	^ self 
		retrieveContentsArgs: args
		accept: allMimeTypes! !

!HttpUrl methodsFor: 'downloading' stamp: 'fbs 2/2/2005 13:24'!
retrieveContentsArgs: args accept: mimeType
	| contents request |
	request := realm ifNotNil: [Passwords at: realm ifAbsent: ['']] ifNil: [''].
	request = '' ifFalse: [request := 'Authorization: Basic ' , request , String crlf].
		"Why doesn't Netscape send the name of the realm instead of Basic?"

	contents := (HTTPSocket
		httpGetDocument: self withoutFragment asString
		args: args
		accept: mimeType
		request: request).

	self checkAuthorization: contents retry: [^ self retrieveContentsArgs: args].

	^ self normalizeContents: contents! !

!HttpUrl class methodsFor: 'constants' stamp: 'SeanDeNigris 1/30/2011 11:32'!
schemeName

	^ 'http'.! !

!HttpUrl class methodsFor: 'system startup' stamp: 'tk 9/22/1998 23:13'!
shutDown
	"Forget all cached passwords, so they won't stay in the image"

	Passwords := nil.! !

!HttpsUrl class methodsFor: 'constants' stamp: 'CamilloBruni 12/16/2011 11:17'!
schemeName

	^ 'https'.! !

!MailtoUrl methodsFor: 'downloading' stamp: 'dvf 5/11/2002 00:47'!
activate
	"Activate a Celeste window for the receiver"

	MailSender sendMessage: (MailMessage from: self composeText)! !

!MailtoUrl methodsFor: 'downloading' stamp: 'dvf 5/11/2002 01:00'!
composeText
	"Answer the template for a new message."

	^ String streamContents: [:str |
		str nextPutAll: 'From: '.
		str nextPutAll: MailSender userName; cr.
		str nextPutAll: 'To: '.
		str nextPutAll: locator asString; cr.

		str nextPutAll: 'Subject: '; cr.

		str cr].! !

!MailtoUrl class methodsFor: 'constants' stamp: 'SeanDeNigris 1/29/2011 19:33'!
schemeName

	^ 'mailto'! !

!String methodsFor: '*Cuis-Network-Url' stamp: 'gsa 12/7/2012 18:08'!
asUrl
	"convert to a Url"
	"'http://www.cc.gatech.edu/' asUrl"
	"msw://chaos.resnet.gatech.edu:9000/' asUrl"
	^Url absoluteFromText: self! !

!String methodsFor: '*Cuis-Network-Url' stamp: 'gsa 12/7/2012 18:09'!
asUrlRelativeTo: aUrl
	^aUrl newFromRelativeText: self! !

!Url methodsFor: 'downloading' stamp: 'ls 8/4/1998 20:41'!
activate
	"spawn an external handler for this URL"
	! !

!Url methodsFor: 'converting' stamp: 'ls 7/14/1998 03:17'!
asText
	^self asString asText! !

!Url methodsFor: 'converting' stamp: 'tb 5/24/2006 22:05'!
asURI
	^self asString asURI! !

!Url methodsFor: 'converting' stamp: 'ls 6/29/1998 00:36'!
asUrl
	^self! !

!Url methodsFor: 'converting' stamp: 'ls 7/3/1998 21:11'!
asUrlRelativeTo: aUrl
	^self! !

!Url methodsFor: 'accessing' stamp: 'mir 2/22/2000 18:05'!
authority
	^''! !

!Url methodsFor: 'converting' stamp: 'fbs 2/2/2005 13:24'!
downloadUrl
	^self asString! !

!Url methodsFor: 'fragment' stamp: 'ls 8/4/1998 01:41'!
fragment
	^fragment! !

!Url methodsFor: 'downloading' stamp: 'ls 8/4/1998 20:40'!
hasContents
	"whether this URL can download contents to be displayed; if not, it fundamentally requires an outside application to deal with it.  For example, mailto: and telnet: urls"
	^false! !

!Url methodsFor: 'parsing' stamp: 'SvenVanCaekenberghe 1/8/2012 14:50'!
newFromRelativeText: aString
	"return a URL relative to the current one, given by aString.  For instance, if self is 'http://host/dir/file', and aString is '/dir2/file2', then the return will be a Url for 'http://host/dir2/file2'"

	"if the scheme is the same, or not specified, then use the same class"

	| newSchemeName remainder fragmentStart newFragment newUrl bare |

	bare := aString trimBoth.
	newSchemeName := Url schemeNameForString: bare.
	(newSchemeName notNil and: [ newSchemeName ~= self schemeName ]) ifTrue: [
		"different scheme -- start from scratch"
		^Url absoluteFromText: aString ].

	remainder := bare.

	"remove the fragment, if any"
	fragmentStart := remainder indexOf: $#.
	fragmentStart > 0 ifTrue: [
		newFragment := remainder copyFrom: fragmentStart+1 to: remainder size. 
		remainder := remainder copyFrom: 1 to: fragmentStart-1].

	"remove the scheme name"
	newSchemeName ifNotNil: [
		remainder := remainder copyFrom: (newSchemeName size + 2) to: remainder size ].

	"create and initialize the new url"
	newUrl := self class new privateInitializeFromText: remainder  relativeTo: self.


	"set the fragment"
	newUrl privateFragment: newFragment.


	^newUrl! !

!Url methodsFor: 'printing' stamp: 'fbs 2/2/2005 13:26'!
printOn: aStream
	^self subclassResponsibility! !

!Url methodsFor: 'fragment' stamp: 'ls 8/4/1998 01:02'!
privateFragment: aString
	fragment := aString! !

!Url methodsFor: 'parsing' stamp: 'ls 8/4/1998 00:50'!
privateInitializeFromText: aString
	^self subclassResponsibility! !

!Url methodsFor: 'parsing' stamp: 'ls 8/4/1998 00:55'!
privateInitializeFromText: aString relativeTo: aUrl
	"initialize from the given string, as a relative URL.  aString will have had the scheme name removed, if it was present to begin with.  If it was, then the scheme name was the same as the receiver's scheme name"

	"by default, just do regular initialization"
	^self privateInitializeFromText: aString! !

!Url methodsFor: 'downloading' stamp: 'ls 7/23/1998 20:14'!
retrieveContents
	"return a MIMEObject with the object's contents, or nil if the object could not be retrieved"
	^nil! !

!Url methodsFor: 'downloading' stamp: 'ls 7/23/1998 20:14'!
retrieveContentsForBrowser: aBrowser
	"return a MIMEObject with the object's contents, or nil if the object could not be retrieved.  Since aBrowser is specified, this could do browser specific things"
	^self retrieveContents! !

!Url methodsFor: 'classification' stamp: 'ls 6/16/1998 16:22'!
scheme
	"return a string with the scheme of this URL.  For instance, HTTP"
	^self subclassResponsibility! !

!Url methodsFor: 'classification' stamp: 'ls 7/3/1998 21:11'!
schemeName
	"return a lowercase string with the scheme of this URL.  For instance, 'http'"
	^self subclassResponsibility! !

!Url methodsFor: 'fragment' stamp: 'ls 6/1/2000 16:19'!
withFragment: newFragment
	"return a URL which is the same except that it has a different fragment"
	^self copy privateFragment: newFragment; yourself! !

!Url methodsFor: 'fragment' stamp: 'ls 6/1/2000 16:26'!
withoutFragment
	"return a URL which is identical to the receiver except that it has no fragment associated with it"
	^self withFragment: nil! !

!Url class methodsFor: 'instance creation' stamp: 'gk 10/21/2005 10:45'!
absoluteFromFileNameOrUrlString: aString
	"Return a URL from and handle Strings without schemes
	as local relative FileUrls instead of defaulting to a HttpUrl
	as absoluteFromText: does."

	^(Url schemeNameForString: aString)
		ifNil: [aString asUrlRelativeTo: FileDirectory default asUrl]
		ifNotNil: [Url absoluteFromText: aString]! !

!Url class methodsFor: 'instance creation' stamp: 'SvenVanCaekenberghe 1/8/2012 14:50'!
absoluteFromText: aString
	"Return a URL from a string and handle
	a String without a scheme as a HttpUrl."

	"Url absoluteFromText: 'http://chaos.resnet.gatech.edu:8000/docs/java/index.html?A%20query%20#part'" 
	"Url absoluteFromText: 'msw://chaos.resnet.gatech.edu:9000/testbook?top'"
	"Url absoluteFromText: 'telnet:chaos.resnet.gatech.edu'"
	"Url absoluteFromText: 'file:/etc/passwd'"

	| remainder index scheme fragment newUrl |
	"trim surrounding whitespace"
	remainder := aString trimBoth.	

	"extract the fragment, if any"
	index := remainder indexOf: $#.
	index > 0 ifTrue: [
		fragment := remainder copyFrom: index + 1 to: remainder size.
		remainder := remainder copyFrom: 1 to: index - 1].

	"choose class based on the scheme name, and let that class do the bulk of the parsing"
	scheme := self schemeNameForString: remainder.
	newUrl := (self urlClassForScheme: scheme) new privateInitializeFromText: remainder.
	newUrl privateFragment: fragment.
	^newUrl! !

!Url class methodsFor: 'parsing' stamp: 'st 9/27/2004 15:47'!
combine: baseURL withRelative: relURL 
	"Take two URL as string form, combine them and return the corresponding URL in string form"

	^((self absoluteFromText: baseURL) newFromRelativeText: relURL) asString! !

!Url class methodsFor: 'constants' stamp: 'SeanDeNigris 1/30/2011 12:00'!
schemeName
	"When searching for a class to handle a particular scheme, make sure that Url classes never match by default. This is so that abstract Url classes e.g. HierarchicalUrl can be iterated over, but will not be selected"

	^ nil.! !

!Url class methodsFor: 'parsing' stamp: 'gk 10/21/2005 10:41'!
schemeNameForString: aString
	"Get the scheme name from a string, or return nil if it's not specified. 
	Used in internal parsing routines - an outsider may as well use asUrl. 
	Return scheme in lowercases."
	
	"Url schemeNameForString: 'http://www.yahoo.com'"
	"Url schemeNameForString: '/etc/passwed'"
	"Url schemeNameForString: '/etc/testing:1.2.3'"

	| index schemeName |
	index := aString indexOf: $: ifAbsent: [^ nil].
	schemeName := aString first: index - 1.
	(schemeName allSatisfy: [:each | each isLetter]) ifFalse: [^ nil].
	^ schemeName asLowercase! !

!Url class methodsFor: 'parsing' stamp: 'CamilloBruni 12/16/2011 11:18'!
urlClassForScheme: scheme

	^ self allSubclasses
				detect: [ :urlClass | urlClass schemeName = scheme ]
				ifNone: [ GenericUrl ].! !
