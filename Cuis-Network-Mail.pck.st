'From Cuis 4.1 of 12 December 2012 [latest update: #1549] on 6 January 2013 at 7:32:25 pm'!
'Description Please enter a description for this package '!
!classDefinition: #MIMEHeaderValue category: #'Cuis-Network-Mail'!
Object subclass: #MIMEHeaderValue
	instanceVariableNames: 'mainValue parameters'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Mail'!
!classDefinition: 'MIMEHeaderValue class' category: #'Cuis-Network-Mail'!
MIMEHeaderValue class
	instanceVariableNames: ''!

!classDefinition: #MailComposition category: #'Cuis-Network-Mail'!
ProtoObject subclass: #MailComposition
	instanceVariableNames: 'messageText textEditor morphicWindow'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Mail'!
!classDefinition: 'MailComposition class' category: #'Cuis-Network-Mail'!
MailComposition class
	instanceVariableNames: ''!

!classDefinition: #MailMessage category: #'Cuis-Network-Mail'!
Object subclass: #MailMessage
	instanceVariableNames: 'text body fields parts'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Mail'!
!classDefinition: 'MailMessage class' category: #'Cuis-Network-Mail'!
MailMessage class
	instanceVariableNames: ''!

!classDefinition: #MailSender category: #'Cuis-Network-Mail'!
AppRegistry subclass: #MailSender
	instanceVariableNames: ''
	classVariableNames: 'SmtpServer UserName'
	poolDictionaries: ''
	category: 'Cuis-Network-Mail'!
!classDefinition: 'MailSender class' category: #'Cuis-Network-Mail'!
MailSender class
	instanceVariableNames: ''!

!classDefinition: #TextMessageLink category: #'Cuis-Network-Mail'!
TextAttribute subclass: #TextMessageLink
	instanceVariableNames: 'message'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-Mail'!
!classDefinition: 'TextMessageLink class' category: #'Cuis-Network-Mail'!
TextMessageLink class
	instanceVariableNames: ''!


!MIMEHeaderValue commentStamp: '<historical>' prior: 0!
I contain the value portion of a MIME-compatible header.

I must be only initialized with the value and not the field name.  E.g. in processing
	Subject: This is the subject
the MIMEHeaderValue should be given only 'This is the subject'

For traditional non-MIME headers, the complete value returned for mainValue and paramaters returns an empty collection.

For MIME headers, both mainValue and parameters are used.!

!MailComposition commentStamp: '<historical>' prior: 0!
a message being composed.  When finished, it will be submitted via a Celeste.!

!MailMessage commentStamp: '<historical>' prior: 0!
I represent an Internet mail or news message.

	text - the raw text of my message
	body - the body of my message, as a MIMEDocument
	fields - a dictionary mapping lowercased field names into collections of MIMEHeaderValue's
	parts - if I am a multipart message, then this is a cache of my parts!

!TextMessageLink commentStamp: '<historical>' prior: 0!
A link to a hidden mail message.  Clicking on it allows the message to be viewed or saved to disk.!

!MIMEHeaderValue methodsFor: 'printing' stamp: 'PeterHugossonMiller 9/3/2009 10:01'!
asHeaderValue
	| strm |
	strm := (String new: 20) writeStream.
	strm nextPutAll: mainValue.
	parameters associationsDo: [:e | strm nextPut: $; ; nextPutAll: e key;
		 nextPutAll: '="';
		 nextPutAll: e value , '"'].
	^ strm contents! !

!MIMEHeaderValue methodsFor: 'accessing' stamp: 'dvf 4/27/2000 18:55'!
mainValue
	^mainValue! !

!MIMEHeaderValue methodsFor: 'accessing' stamp: 'dvf 4/27/2000 18:13'!
mainValue: anObject
	mainValue := anObject! !

!MIMEHeaderValue methodsFor: 'accessing' stamp: 'ls 2/10/2001 13:06'!
parameterAt: aParameter put: value
	parameters at: aParameter put: value! !

!MIMEHeaderValue methodsFor: 'accessing' stamp: 'dvf 4/27/2000 18:18'!
parameters
	^parameters! !

!MIMEHeaderValue methodsFor: 'accessing' stamp: 'dvf 4/27/2000 18:11'!
parameters: anObject
	parameters := anObject! !

!MIMEHeaderValue methodsFor: 'printing' stamp: 'ls 2/10/2001 12:37'!
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ': '.
	aStream nextPutAll: self asHeaderValue! !

!MIMEHeaderValue class methodsFor: 'instance creation' stamp: 'mdr 4/11/2001 12:19'!
forField: aFName fromString: aString
	"Create a MIMEHeaderValue from aString.  How it is parsed depends on whether it is a MIME specific field or a generic header field."

	(aFName beginsWith: 'content-') 
		ifTrue: [^self fromMIMEHeader: aString]
		ifFalse: [^self fromTraditionalHeader: aString]
! !

!MIMEHeaderValue class methodsFor: 'instance creation' stamp: 'SvenVanCaekenberghe 1/8/2012 14:45'!
fromMIMEHeader: aString 
	"This is the value of a MIME header field and so is parsed to extract the various parts"
	| parts newValue parms |
	newValue := self new.
	parts := (aString findTokens: ';') readStream.
	newValue mainValue: parts next.
	parms := Dictionary new.
	parts do: 
		[ :e | | separatorPos parmValue parmName | 
		separatorPos := e 
			findAnySubStr: '='
			startingAt: 1.
		separatorPos <= e size ifTrue: 
			[ parmName := (e 
				copyFrom: 1
				to: separatorPos - 1) trimBoth asLowercase.
			parmValue := (e 
				copyFrom: separatorPos + 1
				to: e size) trimBoth withoutQuoting.
			parms 
				at: parmName
				put: parmValue ] ].
	newValue parameters: parms.
	^ newValue! !

!MIMEHeaderValue class methodsFor: 'instance creation' stamp: 'mdr 4/11/2001 12:02'!
fromTraditionalHeader: aString
	"This is a traditional non-MIME header (like Subject:) and so should be stored whole"

	| newValue |

	newValue := self new.
	newValue mainValue: aString.
	newValue parameters: #().
	^newValue.
! !

!MailComposition methodsFor: 'interface' stamp: 'janniklaval 6/11/2010 13:22'!
addAttachment
	| file fileResult fileName fileSelected|
	textEditor
		ifNotNil: [self hasUnacceptedEdits ifTrue: [textEditor accept]].
	
	fileSelected := UIManager default chooseFileMatching: nil.
	fileSelected
		ifNil: [^ self].
		
	(fileResult := FileStream fileNamed: fileSelected)
		ifNotNil: 
			[fileName := fileResult directory fullNameFor: fileResult name.
			file := FileStream readOnlyFileNamed: fileName.
			file ifNotNil:
				[file binary.
				self messageText:
						((MailMessage from: self messageText asString)
							addAttachmentFrom: file withName: fileResult name; text).
				file close]] ! !

!MailComposition methodsFor: 'private' stamp: 'nice 1/5/2010 15:59'!
breakLines: aString  atWidth: width
	"break lines in the given string into shorter lines"
	| result atAttachment |

	result := (String new: (aString size * 50 // 49)) writeStream.

	atAttachment := false.
	aString asString linesDo: [ :line | | start end | 
		(line beginsWith: '====') ifTrue: [ atAttachment := true ].
		atAttachment ifTrue: [
			"at or after an attachment line; no more wrapping for the rest of the message"
			result nextPutAll: line.  result cr ]
		ifFalse: [
			(line beginsWith: '>') ifTrue: [
				"it's quoted text; don't wrap it"
				result nextPutAll: line. result cr. ]
			ifFalse: [
				"regular old line.  Wrap it to multiple lines"
				start := 1.
					"output one shorter line each time through this loop"
				[ start + width <= line size ] whileTrue: [
	
					"find the end of the line"
					end := start + width - 1.
					[end >= start and: [ (line at: (end+1)) isSeparator not ]] whileTrue: [
						end := end - 1 ].
					end < start ifTrue: [
						"a word spans the entire width!!"
						end := start + width - 1 ].

					"copy the line to the output"
					result nextPutAll: (line copyFrom: start to: end).
					result cr.

					"get ready for next iteration"
					start := end+1.
					(line at: start) isSeparator ifTrue: [ start := start + 1 ].
				].

				"write out the final part of the line"
				result nextPutAll: (line copyFrom: start to: line size).
				result cr.
			].
		].
	].

	^result contents! !

!MailComposition methodsFor: 'private' stamp: 'ls 2/10/2001 14:08'!
breakLinesInMessage: message
	"reformat long lines in the specified message into shorter ones"
	message body  mainType = 'text' ifTrue: [
		"it's a single-part text message.  reformat the text"
		| newBodyText |
		newBodyText := self breakLines: message bodyText  atWidth: 72.
		message body: (MIMEDocument contentType: message body contentType content: newBodyText).

		^self ].

	message body isMultipart ifTrue: [
		"multipart message; process the top-level parts.  HACK: the parts are modified in place"
		message parts do: [ :part |
			part body mainType = 'text' ifTrue: [
				| newBodyText |
				newBodyText := self breakLines: part bodyText atWidth: 72.
				part body: (MIMEDocument contentType: part body contentType content: newBodyText) ] ].
		message regenerateBodyFromParts. ].! !

!MailComposition methodsFor: 'interface' stamp: 'tbn 7/29/2010 22:08'!
menuGet: aMenu shifted: shifted
	
	aMenu addList: {
		{'Find...(f)' translated.		#find}.
		{'Find again (g)' translated.		#findAgain}.
		{'Set search string (h)' translated.	#setSearchString}.
			#-.
		{'Accept (s)' translated. #accept}.
		{'Send message' translated.  #submit}}.

	^aMenu.! !

!MailComposition methodsFor: 'access' stamp: 'yo 7/26/2004 22:06'!
messageText
	"return the current text"
	^messageText.
! !

!MailComposition methodsFor: 'access' stamp: 'yo 7/26/2004 22:47'!
messageText: aText
	"change the current text"
	messageText := aText.
	self changed: #messageText.
	^true! !

!MailComposition methodsFor: 'interface' stamp: 'alain.plantec 5/30/2008 13:43'!
open
	"open an interface"
	self openInMorphic ! !

!MailComposition methodsFor: 'interface' stamp: 'alain.plantec 6/10/2008 22:30'!
openInMorphic
	"open an interface for sending a mail message with the given initial 
	text"
	| textMorph buttonsList sendButton attachmentButton |
	morphicWindow := SystemWindow labelled: 'Mister Postman'.
	morphicWindow model: self.
	textEditor := textMorph := PluggableTextMorph
						on: self
						text: #messageText
						accept: #messageText:
						readSelection: nil
						menu: #menuGet:shifted:.
	morphicWindow
		addMorph: textMorph
		frame: (0 @ 0.1 corner: 1 @ 1).
	buttonsList := AlignmentMorph newRow.
	sendButton := PluggableButtonMorph
				on: self
				getState: nil
				action: #submit.
	sendButton hResizing: #spaceFill;
		 vResizing: #spaceFill;
		 label: 'send message';
		 setBalloonText: 'Accept any unaccepted edits and add this to the queue of messages to be sent';
		 onColor: Color white offColor: Color white.
	buttonsList addMorphBack: sendButton.
	attachmentButton := PluggableButtonMorph
				on: self
				getState: nil
				action: #addAttachment.
	attachmentButton hResizing: #spaceFill;
		 vResizing: #spaceFill;
		 label: 'add attachment';
		 setBalloonText: 'Send a file with the message';
		 onColor: Color white offColor: Color white.
	buttonsList addMorphBack: attachmentButton.
	morphicWindow
		addMorph: buttonsList
		frame: (0 @ 0 extent: 1 @ 0.1).
	morphicWindow openInWorld! !

!MailComposition methodsFor: 'private' stamp: 'fc 1/19/2005 20:53'!
perform: selector orSendTo: otherTarget

	(self respondsTo: selector)
		ifTrue: [^self perform: selector]
		ifFalse: [^otherTarget perform: selector]

	! !

!MailComposition methodsFor: 'interface' stamp: 'dvf 5/11/2002 01:23'!
sendMailMessage: aMailMessage
	self messageText: aMailMessage text! !

!MailComposition methodsFor: 'access' stamp: 'dvf 5/11/2002 00:24'!
smtpServer
	^MailSender smtpServer! !

!MailComposition methodsFor: 'access' stamp: 'alain.plantec 6/19/2008 09:45'!
submit
	| message |
	"submit the message"
	textEditor
		ifNotNil: [self hasUnacceptedEdits ifTrue: [textEditor accept]].
	message := MailMessage from: messageText asString.
	self breakLinesInMessage: message.
	SMTPClient deliverMailFrom: message from to: (Array with: message to) text: message text usingServer: self smtpServer.

	morphicWindow ifNotNil: [morphicWindow delete].
! !

!MailComposition class methodsFor: 'instance creation' stamp: 'dvf 5/11/2002 00:40'!
initialize
	super initialize.
	MailSender register: self.! !

!MailComposition class methodsFor: 'instance creation' stamp: 'dvf 5/11/2002 01:25'!
sendMailMessage: aMailMessage
	| newComposition |
	newComposition := self new.
	newComposition messageText: aMailMessage text; open! !

!MailComposition class methodsFor: 'instance creation' stamp: 'dvf 5/11/2002 00:40'!
unload

	MailSender unregister: self ! !

!MailMessage methodsFor: 'multipart' stamp: 'TonyFleig 11/28/2010 12:54'!
addAlternativePart: newPart
	self makeMultipart: 'alternative' with: newPart.
! !

!MailMessage methodsFor: 'multipart' stamp: 'TonyFleig 11/28/2010 12:54'!
addAlternativePart: bodyString contentType: aContentTypeString

	| newPart |
	newPart := MailMessage empty.
	newPart setField: 'content-type' toString: aContentTypeString.
	newPart body: (MIMEDocument contentType: aContentTypeString  content: bodyString).
	self addAlternativePart: newPart.
! !

!MailMessage methodsFor: 'multipart' stamp: 'mdr 4/11/2001 12:04'!
addAttachmentFrom: aStream withName: aName
	"add an attachment, encoding with base64.  aName is the option filename to encode"
	| newPart |
	self makeMultipart.
	self parts.  "make sure parts have been parsed"

	"create the attachment as a MailMessage"
	newPart := MailMessage empty.
	newPart setField: 'content-type' toString: 'application/octet-stream'.
	newPart setField: 'content-transfer-encoding' toString: 'base64'.
	aName ifNotNil: [
		| dispositionField |
		dispositionField := MIMEHeaderValue fromMIMEHeader: 'attachment'.
		dispositionField parameterAt: 'filename' put: aName.
		newPart setField: 'content-disposition' to: dispositionField ].
	newPart body: (MIMEDocument contentType: 'application/octet-stream' content: aStream upToEnd).


	"regenerate our text"
	parts := parts copyWith: newPart.
	self regenerateBodyFromParts.
	text := nil.! !

!MailMessage methodsFor: 'multipart' stamp: 'TonyFleig 11/28/2010 12:55'!
addMixedPart: newPart
	self makeMultipart: 'mixed' with: newPart.
! !

!MailMessage methodsFor: 'multipart' stamp: 'TonyFleig 11/28/2010 12:55'!
addMixedPart: bodyString contentType: aContentTypeString

	| newPart |
	newPart := MailMessage empty.
	newPart setField: 'content-type' toString: aContentTypeString.
	newPart body: (MIMEDocument contentType: aContentTypeString  content: bodyString).
	self addMixedPart: newPart.
! !

!MailMessage methodsFor: 'multipart' stamp: 'TonyFleig 11/28/2010 12:55'!
addPart: bodyString contentType: aContentTypeString

	| newPart |
	newPart := MailMessage empty.
	newPart setField: 'content-type' toString: aContentTypeString.
	newPart body: (MIMEDocument contentType: aContentTypeString  content: bodyString).
	self addPart: newPart.
! !

!MailMessage methodsFor: 'printing/formatting' stamp: 'nice 1/5/2010 15:59'!
asSendableText
	"break lines in the given string into shorter lines"
	| result atAttachment width aString pastHeader |
	width := 72.
	aString := self text.
	result := (String new: aString size * 50 // 49) writeStream.
	pastHeader := false.
	atAttachment := false.
	aString asString
		linesDo: 
			[:line | | end start | 
			line isEmpty ifTrue: [pastHeader := true].
			pastHeader
				ifTrue: 
					["(line beginsWith: '--==')
						ifTrue: [atAttachment := true]."
					atAttachment
						ifTrue: 
							["at or after an attachment line; no more 
							wrapping for the rest of the message"
							result nextPutAll: line.
							result cr]
						ifFalse: [(line beginsWith: '>')
								ifTrue: 
									["it's quoted text; don't wrap it"
									result nextPutAll: line.
									result cr]
								ifFalse: 
									["regular old line.  Wrap it to multiple 
									lines "
									start := 1.
									"output one shorter line each time 
									through this loop"
									[start + width <= line size]
										whileTrue: 
											["find the end of the line"
											end := start + width - 1.
											[end >= start and: [(line at: end + 1) isSeparator not]]
												whileTrue: [end := end - 1].
											end < start ifTrue: ["a word spans the entire 
												width!! "
												end := start + width - 1].
											"copy the line to the output"
											result nextPutAll: (line copyFrom: start to: end).
											result cr.
											"get ready for next iteration"
											start := end + 1.
											(line at: start) isSeparator ifTrue: [start := start + 1]].
									"write out the final part of the line"
									result nextPutAll: (line copyFrom: start to: line size).
									result cr]]]
				ifFalse: 
					[result nextPutAll: line.
					result cr]].
	^ result contents! !

!MailMessage methodsFor: 'multipart' stamp: 'mdr 5/7/2001 11:22'!
atomicParts
	"Answer all of the leaf parts of this message, including those of multipart included messages"

	self body isMultipart ifFalse: [^ OrderedCollection with: self].
	^ self parts inject: OrderedCollection new into: [:col :part | col , part atomicParts]! !

!MailMessage methodsFor: 'multipart' stamp: 'mdr 3/22/2001 09:06'!
attachmentSeparator
	^(self fieldNamed: 'content-type' ifAbsent: [^nil]) parameters
		at: 'boundary' ifAbsent: [^nil]! !

!MailMessage methodsFor: 'accessing' stamp: 'ls 1/3/1999 15:48'!
body
	"return just the body of the message"
	^body! !

!MailMessage methodsFor: 'initialization' stamp: 'ls 2/10/2001 12:48'!
body: newBody
	"change the body"
	body := newBody.
	text := nil.! !

!MailMessage methodsFor: 'accessing' stamp: 'ls 1/3/1999 15:52'!
bodyText
	"return the text of the body of the message"
	^body content! !

!MailMessage methodsFor: 'printing/formatting' stamp: 'lr 3/14/2010 21:13'!
bodyTextFormatted
	"Answer a version of the text in my body suitable for display.  This will parse multipart forms, decode HTML, and other such things"

	"check for multipart"

	self body isMultipart
		ifTrue: [ 
			"check for alternative forms"
			self body isMultipartAlternative
				ifTrue: [ 
					"it's multipart/alternative.  search for a part that we can display, biasing towards nicer formats"
					#('text/html' 'text/plain')
						do: [ :format | 
							self parts
								do: [ :part | 
									part body contentType = format
										ifTrue: [ ^ part bodyTextFormatted ] ] ].	"couldn't find a desirable part to display; just display the first part"
					^ self parts first bodyTextFormatted ].	"not alternative parts.  put something for each part"
			^ Text
				streamContents: [ :str | 
					self parts
						do: [ :part | 
							((#('text' 'multipart') includes: part body mainType) or: [ part body contentType = 'message/rfc822' ])
								ifTrue: [ 
									"try to inline the message part"
									str nextPutAll: part bodyTextFormatted ]
								ifFalse: [ 
									| descript |
									str cr.
									descript := part name ifNil: [ 'attachment' ].
									str nextPutAll: (Text string: '[' , descript , ']' attribute: (TextMessageLink message: part)) ] ] ] ].	"check for HTML"
	self body contentType = 'text/html'
		ifTrue: [ 
			Smalltalk globals
				at: #HtmlParser
				ifPresent: [ :htmlParser | ^ (htmlParser parse: body content readStream) formattedText ] ].	"check for an embedded message"
	self body contentType = 'message/rfc822'
		ifTrue: [ ^ (MailMessage from: self body content) formattedText ].	"nothing special--just return the text"
	^ body content! !

!MailMessage methodsFor: 'fields' stamp: 'bf 3/10/2000 15:22'!
canonicalFields
	"Break long header fields and escape those containing high-ascii characters according to RFC2047"

	self rewriteFields:
		[ :fName :fValue | 
			(fName size + fValue size < 72 and: [fValue allSatisfy: [:c | c asciiValue <= 128]])
				ifFalse: [RFC2047MimeConverter mimeEncode: fName, ': ', fValue]]
		append: [].

! !

!MailMessage methodsFor: 'accessing' stamp: 'ls 3/18/2001 16:34'!
cc

	^self fieldsNamed: 'cc' separatedBy: ', '! !

!MailMessage methodsFor: 'printing/formatting' stamp: 'PeterHugossonMiller 9/3/2009 10:03'!
cleanedHeader
	"Reply with a cleaned up version email header.  First show fields people would normally want to see (in a regular order for easy browsing), and then any other fields not explictly excluded"
	| new priorityFields omittedFields |
	new := (String new: text size) writeStream.
	priorityFields := #('Date' 'From' 'Subject' 'To' 'Cc' ).
	omittedFields := MailMessage omittedHeaderFields.

	"Show the priority fields first, in the order given in priorityFields"
	priorityFields do: 
		[ :pField | 
		"We don't check whether the priority field is in the omitted list!!"
		self 
			headerFieldsNamed: pField
			do: 
				[ :fValue | 
				new
					nextPutAll: pField , ': ' , fValue decodeMimeHeader;
					cr ] ].

	"Show the rest of the fields, omitting the uninteresting ones and ones we have already shown"
	omittedFields := omittedFields , priorityFields.
	self 
		fieldsFrom: text readStream
		do: 
			[ :fName :fValue | 
			((fName beginsWith: 'x-') or: [ omittedFields anySatisfy: [ :omitted | fName sameAs: omitted ] ]) ifFalse: 
				[ new
					nextPutAll: fName , ': ' , fValue;
					cr ] ].
	^ new contents! !

!MailMessage methodsFor: 'testing' stamp: 'kfr 11/5/2004 17:32'!
containsViewableImage
	^self body isJpeg | self body isGif | self body isPng! !

!MailMessage methodsFor: 'accessing' stamp: 'ls 2/10/2001 12:19'!
date
	"Answer a date string for this message."

	^(Date fromSeconds: self time + (Date newDay: 1 year: 1980) asSeconds) 
		printFormat: #(2 1 3 47 1 2)! !

!MailMessage methodsFor: 'multipart' stamp: 'ls 3/18/2001 16:26'!
decoderClass
	| encoding |
	encoding := self fieldNamed: 'content-transfer-encoding' ifAbsent: [^ nil].
	encoding := encoding mainValue.
	encoding asLowercase = 'base64' ifTrue: [^ Base64MimeConverter].
	encoding asLowercase = 'quoted-printable' ifTrue: [^ QuotedPrintableMimeConverter].
	^ nil! !

!MailMessage methodsFor: 'printing/formatting' stamp: 'mdr 5/7/2001 11:07'!
excerpt
	"Return a short excerpt of the text of the message"

	^ self bodyText withSeparatorsCompacted truncateWithElipsisTo: 60! !

!MailMessage methodsFor: 'fields' stamp: 'ls 3/18/2001 16:32'!
fieldNamed: aString ifAbsent: aBlock
	| matchingFields |
	"return the value of the field with the specified name.  If there is more than one field, then return the first one"
	matchingFields := fields at: aString asLowercase ifAbsent: [ ^aBlock value ].
	^matchingFields first! !

!MailMessage methodsFor: 'accessing' stamp: 'ls 3/18/2001 16:27'!
fields
	"return the internal fields structure.  This is private and subject to change!!"
	^ fields! !

!MailMessage methodsFor: 'parsing' stamp: 'SvenVanCaekenberghe 1/8/2012 14:45'!
fieldsFrom: aStream do: aBlock 
	"Invoke the given block with each of the header fields from the given stream. The block arguments are the field name and value. The streams position is left right after the empty line separating header and body."
	| savedLine line s |
	savedLine := self readStringLineFrom: aStream.
	[ aStream atEnd ] whileFalse: 
		[ line := savedLine.
		line isEmpty ifTrue: [ ^ self ].	"quit when we hit a blank line"
		
		[ savedLine := self readStringLineFrom: aStream.
		savedLine size > 0 and: [ savedLine first isSeparator ] ] whileTrue: 
			[ "lines starting with white space are continuation lines"
			s := savedLine readStream.
			s skipSeparators.
			line := line , ' ' , s upToEnd ].
		self 
			reportField: line trimBoth
			to: aBlock ].

	"process final header line of a body-less message"
	savedLine isEmpty ifFalse: 
		[ self 
			reportField: savedLine trimBoth
			to: aBlock ]! !

!MailMessage methodsFor: 'fields' stamp: 'ls 3/18/2001 16:21'!
fieldsNamed: aString ifAbsent: aBlock
	"return a list of all fields with the given name"
	^fields at: aString asLowercase ifAbsent: aBlock! !

!MailMessage methodsFor: 'fields' stamp: 'ls 3/18/2001 16:36'!
fieldsNamed: aString  separatedBy: separationString
	"return all fields with the specified name, concatenated together with separationString between each element.  Return an empty string if no fields with the specified name are present"
	| matchingFields |
	matchingFields := self fieldsNamed: aString ifAbsent: [ ^'' ].
	^String streamContents: [ :str |
		matchingFields
			do: [ :field | str nextPutAll: field mainValue ]
			separatedBy: [ str nextPutAll: separationString ]].
! !

!MailMessage methodsFor: 'printing/formatting'!
format
	"Replace the text of this message with a formatted version."
	"NOTE: This operation discards extra header fields."

	text := self formattedText.! !

!MailMessage methodsFor: 'printing/formatting' stamp: 'ls 4/30/2000 18:52'!
formattedText
	"Answer a version of my text suitable for display.  This cleans up the header, decodes HTML, and things like that"

	
	^ self cleanedHeader asText, String cr , self bodyTextFormatted! !

!MailMessage methodsFor: 'accessing' stamp: 'mdr 3/21/2001 15:28'!
from

	^(self fieldNamed: 'from' ifAbsent: [ ^'' ]) mainValue! !

!MailMessage methodsFor: 'initialization' stamp: 'SvenVanCaekenberghe 1/8/2012 15:24'!
from: aString 
	"Parse aString to initialize myself."
	| parseStream contentType bodyText contentTransferEncoding |
	text := aString trimRight, String cr.
	parseStream := text readStream.
	contentType := 'text/plain'.
	contentTransferEncoding := nil.
	fields := Dictionary new.

	"Extract information out of the header fields"
	self 
		fieldsFrom: parseStream
		do: 
			[ :fName :fValue | 
			"NB: fName is all lowercase"
			fName = 'content-type' ifTrue: [ contentType := (fValue copyUpTo: $;) asLowercase ].
			fName = 'content-transfer-encoding' ifTrue: [ contentTransferEncoding := fValue asLowercase ].
			(fields 
				at: fName
				ifAbsentPut: [ OrderedCollection new: 1 ]) add: (MIMEHeaderValue 
					forField: fName
					fromString: fValue) ].

	"Extract the body of the message"
	bodyText := parseStream upToEnd.
	contentTransferEncoding = 'base64' ifTrue: 
		[ bodyText := Base64MimeConverter mimeDecodeToChars: bodyText readStream.
		bodyText := bodyText contents ].
	contentTransferEncoding = 'quoted-printable' ifTrue: [ bodyText := bodyText decodeQuotedPrintable ].
	body := MIMEDocument 
		contentType: contentType
		content: bodyText! !

!MailMessage methodsFor: 'fields' stamp: 'ls 3/18/2001 16:28'!
hasFieldNamed: aString
	^fields includesKey: aString asLowercase! !

!MailMessage methodsFor: 'parsing' stamp: 'damiencassou 5/30/2008 15:52'!
headerFieldsNamed: fieldName do: aBlock 
	"Evalue aBlock once for each header field which matches fieldName.  The block is valued with one parameter, the value of the field"
	self 
		fieldsFrom: text readStream
		do: [ :fName :fValue | (fieldName sameAs: fName) ifTrue: [ aBlock value: fValue ] ]! !

!MailMessage methodsFor: 'initialization' stamp: 'alain.plantec 5/28/2009 10:06'!
initialize
	"initialize as an empty message"

	super initialize.
	text := String cr.
	fields := Dictionary new.
	body := MIMEDocument contentType: 'text/plain' content: String cr! !

!MailMessage methodsFor: 'multipart' stamp: 'mdr 4/11/2001 12:06'!
makeMultipart
	"if I am not multipart already, then become a multipart message with one part"

	| part multipartHeader |

	body isMultipart ifTrue: [ ^self ].

	"set up the new message part"
	part := MailMessage empty.
	part body: body.
	(self hasFieldNamed: 'content-type') ifTrue: [
		part setField: 'content-type' to: (self fieldNamed: 'content-type' ifAbsent: ['']) ].
	parts := Array with: part.

	"fix up our header"
	multipartHeader := MIMEHeaderValue fromMIMEHeader: 'multipart/mixed'.
	multipartHeader parameterAt: 'boundary' put: self class generateSeparator .
	self setField: 'content-type' to: multipartHeader.

	self setField: 'mime-version' to: (MIMEHeaderValue fromMIMEHeader: '1.0').
	self removeFieldNamed: 'content-transfer-encoding'.

	"regenerate everything"
	self regenerateBodyFromParts.
	text := nil.! !

!MailMessage methodsFor: 'multipart' stamp: 'TonyFleig 11/28/2010 12:55'!
makeMultipart: subType with: newPart
	"if I am not multipart already, then become a multipart message with one part"

	| multipartHeader |

	body isMultipart
		ifFalse: [
			parts := Array with: newPart.

			"fix up our header"
			multipartHeader := MIMEHeaderValue fromMIMEHeader: 'multipart/',subType.
			multipartHeader parameterAt: 'boundary' put: self class generateSeparator .
			self setField: 'content-type' to: multipartHeader.

			self setField: 'mime-version' to: (MIMEHeaderValue fromMIMEHeader: '1.0').
			self removeFieldNamed: 'content-transfer-encoding']
		ifTrue: [
			self parts.
			parts := parts copyWith: newPart.
			].

	"regenerate everything"
	self regenerateBodyFromParts.
	text := nil.! !

!MailMessage methodsFor: 'accessing' stamp: 'ls 3/18/2001 16:26'!
name
	"return a default name for this part, if any was specified.  If not, return nil"
	| type nameField disposition |

	"try in the content-type: header"
	type := self fieldNamed: 'content-type' ifAbsent: [nil].
	(type notNil and: [(nameField := type parameters at: 'name' ifAbsent: [nil]) notNil])
		ifTrue: [^ nameField].

	"try in content-disposition:"
	disposition := self fieldNamed: 'content-disposition' ifAbsent: [nil].
	(disposition notNil and: [(nameField := disposition parameters at: 'filename' ifAbsent: [nil]) notNil])
		ifTrue: [^ nameField].

	"give up"
	^ nil! !

!MailMessage methodsFor: 'multipart' stamp: 'SvenVanCaekenberghe 1/8/2012 15:24'!
parseParts
	"private -- parse the parts of the message and store them into a collection"
	"If this is not multipart, store an empty collection"
	| parseStream msgStream messages separator |
	self body isMultipart ifFalse: 
		[ parts := #().
		^ self ].

	"If we can't find a valid separator, handle it as if the message is not multipart"
	separator := self attachmentSeparator.
	separator ifNil: 
		[ Transcript
			show: 'Ignoring bad attachment separater';
			cr.
		parts := #().
		^ self ].
	separator := '--' , separator trimRight.
	parseStream := self bodyText readStream.
	msgStream := LimitingLineStreamWrapper 
		on: parseStream
		delimiter: separator.
	msgStream limitingBlock: 
		[ :aLine | 
		aLine trimRight = separator or: 
			[ "Match the separator"
			aLine trimRight = (separator , '--') ] ].	"or the final separator with --"

	"Throw away everything up to and including the first separator"
	msgStream upToEnd.
	msgStream skipThisLine.

	"Extract each of the multi-parts as strings"
	messages := OrderedCollection new.
	[ parseStream atEnd ] whileFalse: 
		[ messages add: msgStream upToEnd.
		msgStream skipThisLine ].
	parts := messages collect: [ :e | MailMessage from: e ]! !

!MailMessage methodsFor: 'multipart' stamp: 'ls 4/30/2000 18:22'!
parts
	parts ifNil: [self parseParts].
	^ parts! !

!MailMessage methodsFor: 'printing/formatting' stamp: 'ls 11/11/2001 13:27'!
printOn: aStream 
	"For text parts with no filename show: 'text/plain: first line of text...'    
	for attachments/filenamed parts show: 'attachment: filename.ext'"

	| name |

	aStream nextPutAll: ((name := self name) ifNil: ['Text: ' , self excerpt]
			ifNotNil: ['File: ' , name])! !

!MailMessage methodsFor: 'parsing' stamp: 'PeterHugossonMiller 9/3/2009 10:03'!
readDateFrom: aStream
	"Parse a date from the given stream and answer nil if the date can't be parsed. The date may be in any of the following forms:
		<day> <monthName> <year>		(5 April 1982; 5-APR-82)
		<monthName> <day> <year>		(April 5, 1982)
		<monthNumber> <day> <year>		(4/5/82)
	In addition, the date may be preceded by the day of the week and an optional comma, such as:
		Tue, November 14, 1989"

	| day month year |
	self skipWeekdayName: aStream.
	aStream peek isDigit ifTrue: [day := Integer readFrom: aStream].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
	aStream peek isLetter
		ifTrue:		"month name or weekday name"
			[month := (String new: 10) writeStream.
			 [aStream peek isLetter] whileTrue: [month nextPut: aStream next].
			 month := month contents.
			 day isNil ifTrue:		"name/number..."
				[[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
				 (aStream peek isDigit) ifFalse: [^nil].
				 day := Integer readFrom: aStream]]
		ifFalse:		"number/number..."
			[month := Date nameOfMonth: day.
			 day := Integer readFrom: aStream].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
	(aStream peek isDigit) ifFalse: [^nil].
	year := Integer readFrom: aStream.
	^Date newDay: day month: month year: year! !

!MailMessage methodsFor: 'parsing' stamp: 'HenrikSperreJohansen 6/12/2010 02:38'!
readStringLineFrom: aStream 
	"Read and answer the next line from the given stream. Consume the carriage return but do not append it to the string."

	^aStream nextLine! !

!MailMessage methodsFor: 'accessing' stamp: 'SvenVanCaekenberghe 1/8/2012 15:17'!
recipientList

	^ (self to findTokens: $,) collect: [ :e | e trimLeft ]! !

!MailMessage methodsFor: 'printing/formatting' stamp: 'bkv 6/23/2003 14:17'!
regenerateBodyFromParts
	"regenerate the message body from the multiple parts"
	| bodyText |

	bodyText := String streamContents: [ :str |
		str cr.
		parts do: [ :part |
			str
				cr;
				nextPutAll: '--';
				nextPutAll: self attachmentSeparator;
				cr;
				nextPutAll: part text ].
	
		str
			cr;
			nextPutAll: '--';
			nextPutAll: self attachmentSeparator;
			nextPutAll: '--';
			cr ].

	body := MIMEDocument contentType: 'multipart/mixed' content: bodyText.
	text := nil.  "text needs to be reformatted"! !

!MailMessage methodsFor: 'printing/formatting' stamp: 'nice 1/5/2010 15:59'!
regenerateText
	"regenerate the full text from the body and headers"
	
	text := String streamContents: 
		[ :str | | encodedBodyText | 
		"first put the header"
		fields keysAndValuesDo: 
			[ :fieldName :fieldValues | 
			fieldValues do: 
				[ :fieldValue | 
				str
					nextPutAll: fieldName capitalized;
					nextPutAll: ': ';
					nextPutAll: fieldValue asHeaderValue;
					cr ] ].

		"skip a line between header and body"
		str cr.

		"put the body, being sure to encode it according to the header"
		encodedBodyText := body content.
		self decoderClass ifNotNil: 
			[ encodedBodyText := (self decoderClass mimeEncode: encodedBodyText readStream) upToEnd ].
		str nextPutAll: encodedBodyText ]! !

!MailMessage methodsFor: 'fields' stamp: 'ls 3/18/2001 16:30'!
removeFieldNamed: name
	"remove all fields with the specified name"
	fields removeKey: name ifAbsent: []! !

!MailMessage methodsFor: 'parsing' stamp: 'SvenVanCaekenberghe 1/8/2012 14:45'!
reportField: aString to: aBlock 
	"Evaluate the given block with the field name a value in the given field. Do nothing if the field is malformed."
	| s fieldName fieldValue |
	(aString includes: $:) ifFalse: [ ^ self ].
	s := aString readStream.
	fieldName := (s upTo: $:) asLowercase.	"fieldname must be lowercase"
	fieldValue := s upToEnd trimBoth.
	fieldValue isEmpty ifFalse: 
		[ aBlock 
			value: fieldName
			value: fieldValue ]! !

!MailMessage methodsFor: 'fields' stamp: 'StephaneDucasse 10/20/2011 15:44'!
rewriteFields: aBlock append: appendBlock 
	"Rewrite header fields. The body is not modified.
	Each field's key and value is reported to aBlock. The block's return value is the replacement for the entire header line. Nil means don't change the line, empty means delete it. After all fields are processed, evaluate appendBlock and append the result to the header."
	| old new appendString |
	Halt halt: 'this method is out of date.  it needs to update body, at the very least.  do we really need this now that we have setField:to: and setField:toString: ?!!'.
	old := text readStream.
	new := (String new: text size) writeStream.
	self 
		fieldsFrom: old
		do: 
			[ :fName :fValue | | result | 
			result := aBlock 
				value: fName
				value: fValue.
			result 
				ifNil: 
					[ new
						nextPutAll: fName , ': ' , fValue;
						cr ]
				ifNotNil: 
					[ result isEmpty ifFalse: 
						[ new nextPutAll: result.
						result last = Character cr ifFalse: [ new cr ] ] ] ].
	appendString := appendBlock value.
	appendString isEmptyOrNil ifFalse: 
		[ new nextPutAll: appendString.
		appendString last = Character cr ifFalse: [ new cr ] ].
	new cr.	"End of header"
	text := new contents , old upToEnd! !

!MailMessage methodsFor: 'multipart' stamp: 'JB 2/12/2010 15:27'!
save
	"save the part to a file"
	| fileName file |
	fileName := self name
				ifNil: ['attachment' , MailMessage dateTimeSuffix].
	(fileName includes: $.) ifFalse: [
		#(isJpeg 'jpg' isGif 'gif' isPng 'png' isPnm 'pnm') pairsDo: [ :s :e |
			(self body perform: s) ifTrue: [fileName := fileName, '.', e]
		]
	].
	fileName := UIManager default request: 'File name for save?' initialAnswer: fileName.
	fileName isEmptyOrNil
		ifTrue: [^ nil].
	file := FileStream newFileNamed: fileName.
	file nextPutAll: self bodyText.
	file close! !

!MailMessage methodsFor: 'testing' stamp: 'HenrikSperreJohansen 6/28/2010 12:21'!
selfTest
	"For testing only: Check that this instance is well formed and makes sense"

	self formattedText.

	[MailAddressParser addressesIn: self from] ifError:
		[ :err | Transcript show: 'Error parsing From: (', self from, ') ', err].
	[MailAddressParser addressesIn: self to] ifError:
		[ :err | Transcript show: 'Error parsing To: (', self to, ') ', err].
	[MailAddressParser addressesIn: self cc] ifError:
		[ :err | Transcript show: 'Error parsing CC: (', self cc, ') ', err].
! !

!MailMessage methodsFor: 'sending' stamp: 'SeanDeNigris 12/2/2011 12:02'!
sendOn: serverString 

	SMTPClient deliver: self usingServer: serverString.! !

!MailMessage methodsFor: 'initialization' stamp: 'ls 3/18/2001 16:20'!
setField: fieldName to: aFieldValue
	"set a field.  If any field of the specified name exists, it will be overwritten"
	fields at: fieldName asLowercase put: (OrderedCollection with: aFieldValue).
	text := nil.! !

!MailMessage methodsFor: 'initialization' stamp: 'mdr 4/11/2001 11:59'!
setField: fieldName toString: fieldValue
	^self setField: fieldName to: (MIMEHeaderValue forField: fieldName fromString: fieldValue)! !

!MailMessage methodsFor: 'parsing' stamp: 'PeterHugossonMiller 9/3/2009 10:03'!
skipWeekdayName: aStream
	"If the given stream starts with a weekday name or its abbreviation, advance the stream to the first alphaNumeric character following the weekday name."

	| position name abbrev |
	aStream skipSeparators.
	(aStream peek isDigit) ifTrue: [^self].
	(aStream peek isLetter) ifTrue:
		[position := aStream position.
		 name := (String new: 10) writeStream.
		 [aStream peek isLetter] whileTrue: [name nextPut: aStream next].
		 abbrev := (name contents copyFrom: 1 to: (3 min: name position)).
		 abbrev := abbrev asLowercase.
		 (#('sun' 'mon' 'tue' 'wed' 'thu' 'fri' 'sat') includes: abbrev asLowercase)
			ifTrue:
				["found a weekday; skip to the next alphanumeric character"
				 [aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1]]
			ifFalse:
				["didn't find a weekday so restore stream position"
				 aStream position: position]].! !

!MailMessage methodsFor: 'accessing' stamp: 'ls 3/18/2001 16:24'!
subject

		^(self fieldNamed: 'subject' ifAbsent: [ ^'' ])  mainValue! !

!MailMessage methodsFor: 'accessing' stamp: 'ls 2/10/2001 12:49'!
text
	"the full, unprocessed text of the message"
	text ifNil: [ self regenerateText ].
	^text! !

!MailMessage methodsFor: 'accessing' stamp: 'HenrikSperreJohansen 6/28/2010 12:22'!
time
	| dateField |
	dateField := (self fieldNamed: 'date' ifAbsent: [ ^0 ]) mainValue.
	^ [self timeFrom: dateField] ifError: [Date today asSeconds].
! !

!MailMessage methodsFor: 'parsing' stamp: 'damiencassou 5/30/2008 15:52'!
timeFrom: aString 
	"Parse the date and time (rfc822) and answer the result as the number of seconds 
	since the start of 1980."
	| s t rawDelta delta plusOrMinus |
	s := aString readStream.

	"date part"
	t := ((self readDateFrom: s) ifNil: [ Date today ]) asSeconds.
	[ s atEnd or: [ s peek isAlphaNumeric ] ] whileFalse: [ s next ].

	"time part"
	s atEnd ifFalse: 
		[ "read time part (interpreted as local, regardless of sender's timezone)"
		s peek isDigit ifTrue: [ t := t + (Time readFrom: s) asSeconds ] ].
	s skipSeparators.

	"Check for a numeric time zone offset"
	('+-' includes: s peek) ifTrue: 
		[ plusOrMinus := s next.
		rawDelta := s peek isDigit 
			ifTrue: [ Integer readFrom: s ]
			ifFalse: [ 0 ].
		delta := (rawDelta // 100 * 60 + (rawDelta \\ 100)) * 60.
		t := plusOrMinus = $+ 
			ifTrue: [ t - delta ]
			ifFalse: [ t + delta ] ].

	"We ignore text time zone offsets like EST, GMT, etc..."
	^ t - (Date 
			newDay: 1
			year: 1980) asSeconds

	"MailMessage new timeFrom: 'Thu, 22 Jun 2000 14:17:47 -500'"
	"MailMessage new timeFrom: 'Thu, 22 Jun 2000 14:17:47 --500'"
	"MailMessage new timeFrom: 'on, 04 apr 2001 14:57:32'"! !

!MailMessage methodsFor: 'accessing' stamp: 'ls 3/18/2001 16:35'!
to
	^self fieldsNamed: 'to' separatedBy: ', '! !

!MailMessage methodsFor: 'printing/formatting' stamp: 'sbw 1/21/2001 19:47'!
viewBody
	"open a viewer on the body of this message"
	self containsViewableImage
		ifTrue: [^ self viewImageInBody].
	(StringHolder new contents: self bodyTextFormatted;
		 yourself)
		openLabel: (self name
				ifNil: ['(a message part)'])! !

!MailMessage methodsFor: 'printing/formatting' stamp: 'nk 6/12/2004 09:36'!
viewImageInBody
	| stream image |
	stream := self body contentStream.
	image := Form fromBinaryStream: stream.
	(World drawingClass withForm: image) openInWorld! !

!MailMessage class methodsFor: 'utilities' stamp: 'SeanDeNigris 12/3/2011 13:36'!
dateStampNow
	"Return the current date and time formatted per RFC5322  e.g.  'Thu, 18 Feb 1999 20:38:51 -0500'"

	^	DateAndTime now asEmailString.! !

!MailMessage class methodsFor: 'utilities' stamp: 'JB 2/12/2010 15:27'!
dateTimeSuffix
	"Answer a string which indicates the date and time, intended for use in building fileout filenames, etc."

	^self monthDayTime24StringFrom: Time primSecondsClock! !

!MailMessage class methodsFor: 'instance creation' stamp: 'nk 7/30/2004 18:08'!
empty
	"return a message with no text and no header"

	^self new! !

!MailMessage class methodsFor: 'instance creation' stamp: 'SeanDeNigris 12/2/2011 11:22'!
from: aString
	"Initialize a new instance from the given string."

	^ self new from: aString! !

!MailMessage class methodsFor: 'instance creation' stamp: 'SeanDeNigris 12/2/2011 11:53'!
from: senderString to: recipients about: subjectString asFollows: bodyString 

	| recipientsString |
	recipientsString := String streamContents: [ :str |
		recipients do: [ :e | str nextPutAll: e ] separatedBy: [ str nextPutAll: ', ' ] ].
	^ (self from: Character lf asString, bodyString)
			setField: 'from' toString: senderString;
			setField: 'date' toString: self dateStampNow;
			setField: 'subject' toString: subjectString;
			setField: 'to' toString: recipientsString.! !

!MailMessage class methodsFor: 'utilities' stamp: 'ls 4/30/2000 22:58'!
generateSeparator
	"generate a separator usable for making MIME multipart documents.  A leading -- will *not* be included"
	^'==CelesteAttachment' , (10000 to: 99999) atRandom asString , '=='.! !

!MailMessage class methodsFor: 'utilities' stamp: 'JB 2/12/2010 15:27'!
monthDayTime24StringFrom: aSecondCount
	| aDate aTime |
	"From the date/time represented by aSecondCount, produce a string which indicates the date and time in the compact form
             ddMMMhhmm		where dd is a two-digit day-of-month, MMM is the alpha month abbreviation and hhmm is the time on a 24-hr clock.

          Utilities monthDayTime24StringFrom: Time primSecondsClock
"

	aDate := Date fromSeconds: aSecondCount.
	aTime := Time fromSeconds: aSecondCount \\ 86400.

	^ (aDate dayOfMonth asTwoCharacterString), 
		(aDate monthName copyFrom: 1 to: 3), 
		(aTime hhmm24)! !

!MailMessage class methodsFor: 'preferences' stamp: 'mdr 7/9/2001 13:23'!
omittedHeaderFields
	"Reply a list of fields to omit when displaying a nice simple message"

	"Note that heads of the form
		X-something: value
	are filtered programatically.  This is done since we don't want any of them
	and it is impossible to predict them in advance."

	^ #(
			'comments'
			'priority'
			'disposition-notification-to'
			'content-id'
			'received'
			'return-path'
			'newsgroups'
			'message-id'
			'path'
			'in-reply-to'
			'sender'
			'fonts'
			'mime-version'
			'status'
			'content-type'
			'content-transfer-encoding'
			'errors-to'
			'keywords'
			'references'
			'nntp-posting-host'
			'lines'
			'return-receipt-to'
			'precedence'
			'originator'
			'distribution'
			'content-disposition'
			'importance'
			'resent-to'
			'resent-cc'
			'resent-message-id'
			'resent-date'
			'resent-sender'
			'resent-from'
			'delivered-to'
			'user-agent'
			'content-class'
			'thread-topic'
			'thread-index'
			'list-help',
			'list-post',
			'list-subscribe',
			'list-id',
			'list-unsubscribe',
			'list-archive'
		)
! !

!MailSender class methodsFor: 'testing' stamp: 'dvf 5/11/2002 01:31'!
isSmtpServerSet
	^ SmtpServer notNil and: [SmtpServer notEmpty]
! !

!MailSender class methodsFor: 'communication' stamp: 'ads 5/11/2003 21:11'!
sendMessage: aMailMessage

	self default ifNotNil: [self default sendMailMessage: aMailMessage]! !

!MailSender class methodsFor: 'settings' stamp: 'rbb 3/1/2005 10:59'!
setSmtpServer
	"Set the SMTP server used to send outgoing messages via"
	SmtpServer ifNil: [SmtpServer := ''].
	SmtpServer := UIManager default
		request: 'What is your mail server for outgoing mail?'
		initialAnswer: SmtpServer.
! !

!MailSender class methodsFor: 'settings' stamp: 'md 9/29/2010 17:54'!
setSmtpServer: aString
	SmtpServer := aString! !

!MailSender class methodsFor: 'settings' stamp: 'rbb 3/1/2005 11:00'!
setUserName
	"Change the user's email name for use in composing messages."

	(UserName isNil) ifTrue: [UserName := ''].
	UserName := UIManager default
		request: 'What is your email address?\(This is the address other people will reply to you)' withCRs
		initialAnswer: UserName.
	UserName ifNotNil: [UserName := UserName]! !

!MailSender class methodsFor: 'accessing' stamp: 'dvf 5/11/2002 01:29'!
smtpServer
	"Answer the server for sending email"

	self isSmtpServerSet
		ifFalse: [self setSmtpServer].
	SmtpServer isEmpty ifTrue: [
		self error: 'no SMTP server specified' ].

	^SmtpServer! !

!MailSender class methodsFor: 'accessing' stamp: 'dvf 5/11/2002 00:49'!
userName
	"Answer the user name to be used in composing messages."

	(UserName isNil or: [UserName isEmpty])
		ifTrue: [self setUserName].

	UserName isEmpty ifTrue: [ self error: 'no user name specified' ].

	^UserName! !

!TextMessageLink methodsFor: 'appearance' stamp: 'ls 4/30/2000 20:34'!
emphasizeScanner: scanner
	scanner textColor: Color brown! !

!TextMessageLink methodsFor: 'initialization' stamp: 'ls 4/30/2000 18:54'!
initialize: message0
	message := message0! !

!TextMessageLink methodsFor: 'acting' stamp: 'CamilloBruni 2/2/2012 00:18'!
mayActOnClick: anEvent
	^true! !

!TextMessageLink class methodsFor: 'instance creation' stamp: 'ls 4/30/2000 19:00'!
message: aMessage
	^super new initialize: aMessage! !
MailComposition initialize!
