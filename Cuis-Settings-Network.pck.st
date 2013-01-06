'From Cuis 4.1 of 12 December 2012 [latest update: #1528] on 2 January 2013 at 8:29 am'!
'Description Please enter a description for this package '!
!classDefinition: #NetworkSystemSettings category: #'Cuis-Settings-Network'!
Object subclass: #NetworkSystemSettings
	instanceVariableNames: ''
	classVariableNames: 'BlabEmail LastHTTPProxyPort LastHTTPProxyServer ProxyPassword ProxyUser UseHTTPProxy UseNetworkAuthentification'
	poolDictionaries: ''
	category: 'Cuis-Settings-Network'!
!classDefinition: 'NetworkSystemSettings class' category: #'Cuis-Settings-Network'!
NetworkSystemSettings class
	instanceVariableNames: ''!


!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 12/3/2009 07:19'!
blabEmail
	^ BlabEmail ifNil: [BlabEmail := '']! !

!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 12/18/2009 07:37'!
blabEmail: aBlabEmailString
	BlabEmail := aBlabEmailString.
	self blabEmail 
		ifEmpty: [HTTPSocket blabEmail: '']
		ifNotEmpty: [HTTPSocket blabEmail: 'From: ', BlabEmail, ' <crlf>']! !

!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 1/31/2010 17:07'!
httpProxyPort  
	^ HTTPSocket httpProxyPort! !

!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 12/2/2009 07:34'!
httpProxyPort: aPort
	HTTPSocket httpProxyPort: aPort.
	LastHTTPProxyPort := HTTPSocket httpProxyPort.
! !

!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 12/2/2009 07:33'!
httpProxyServer
	^ HTTPSocket httpProxyServer! !

!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 12/2/2009 07:36'!
httpProxyServer: aServerName
	HTTPSocket httpProxyServer: aServerName.
	LastHTTPProxyServer := HTTPSocket httpProxyServer! !

!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 12/9/2009 08:39'!
lastHTTPProxyPort
	^ LastHTTPProxyPort ifNil: [LastHTTPProxyPort := HTTPSocket httpProxyPort]. 
! !

!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 12/2/2009 07:36'!
lastHTTPProxyServer
	^ LastHTTPProxyServer ifNil: [LastHTTPProxyServer := HTTPSocket httpProxyServer].
! !

!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 12/28/2010 22:39'!
networkSettingsOn: aBuilder 
	<systemsettings> 
	(aBuilder group: #network) 
		label: 'Network' translated;
		description: 'All network related settings' translated;
		noOrdering;
		with: [ 
			(aBuilder setting: #useHTTPProxy)      
				label: 'Use HTTP proxy' translated;   
				noOrdering;
		 		description: 'If checked then the you will be able to set a port number and a server name. If unchecked, then no http proxy is used.' translated;
				with: [
					(aBuilder setting: #httpProxyPort)
						label: 'Port' translated;
						target: HTTPSocket;
						description: 'The HTTP proxy port'.
					(aBuilder setting: #httpProxyServer)
						label: 'Server' translated;
						target: HTTPSocket;
						description: 'The HTTP proxy server (i.e. proxy.univ-brest.fr)'].
			(aBuilder setting: #useNetworkAuthentification) 
				label: 'Proxy authentication' translated;
				noOrdering;
		 		description: 'If checked then you will be able to enter a user name and a password for network authentification. Store  HTTP 1.0 basic authentication credentials. Note: this is an ugly hack that stores your password in your image.  It''s just enought to get you going if you use a firewall that requires authentication' translated;
				with: [
					(aBuilder setting: #proxyUser)
						label: 'User name' translated;
						description: 'The proxy user name' translated.
					(aBuilder setting: #proxyPassword)
						type: #Password;
						label: 'Password' translated;
						description: 'The user password' translated].
			(aBuilder setting: #blabEmail)
				label: 'Blab email' translated;
				ghostHelp: 'somebody@some.where';
				description: 'Enter blab email of the form ''somebody@some.where''. It can be empty' translated]! !

!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 12/2/2009 23:03'!
proxyPassword
	| encoded stream |
	encoded := ProxyPassword ifNil: [ProxyPassword := ''].
	stream := ReadWriteStream on: (String new: 16).
	stream nextPutAll: encoded.
	^ (Base64MimeConverter mimeDecodeToChars: stream) contents.
! !

!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 5/7/2010 19:41'!
proxyPassword: aPassword
	| stream |
	stream := (aPassword ifNil: ['']) readStream.
	ProxyPassword := (Base64MimeConverter mimeEncode: stream) contents.
	HTTPSocket proxyUser: self proxyUser password: self proxyPassword
! !

!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 1/10/2010 09:36'!
proxyUser
	| encoded stream |
	encoded := ProxyUser ifNil: [ProxyUser := ''].
	stream := ReadWriteStream on: (String new: 16). 
	stream nextPutAll: encoded.
	^ (Base64MimeConverter mimeDecodeToChars: stream) contents
! !

!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 5/7/2010 19:48'!
proxyUser: aUser
	| stream |
	stream := (aUser ifNil: ['']) readStream.
	ProxyUser := (Base64MimeConverter mimeEncode: stream) contents.
	HTTPSocket proxyUser: self proxyUser password: self proxyPassword

! !

!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 12/2/2009 07:23'!
useHTTPProxy
	^ UseHTTPProxy ifNil: [UseHTTPProxy := false]! !

!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 12/18/2009 07:33'!
useHTTPProxy: aBoolean
	UseHTTPProxy = aBoolean ifTrue: [^self].
	UseHTTPProxy := aBoolean.
	self  useHTTPProxy
		ifTrue: [self httpProxyPort: self lastHTTPProxyPort .
			self httpProxyServer: self lastHTTPProxyServer]
		ifFalse: [HTTPSocket stopUsingProxyServer]
! !

!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 12/3/2009 06:46'!
useNetworkAuthentification
	^ UseNetworkAuthentification ifNil: [UseNetworkAuthentification := false]! !

!NetworkSystemSettings class methodsFor: 'settings' stamp: 'AlainPlantec 12/18/2009 07:34'!
useNetworkAuthentification: aBoolean
	UseNetworkAuthentification = aBoolean ifTrue: [^ self].
	UseNetworkAuthentification := aBoolean.
	self useNetworkAuthentification 
		ifTrue: [HTTPSocket proxyUser: self proxyUser password: self proxyPassword]
		ifFalse: [HTTPSocket proxyUser: '' password: '']! !
