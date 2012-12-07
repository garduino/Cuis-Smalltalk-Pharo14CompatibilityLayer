'From Cuis 4.0 of 21 April 2012 [latest update: #1308] on 7 December 2012 at 11:03:46 am'!
'Description Please enter a description for this package '!
!classDefinition: #MailAddressParser category: #'Cuis-Network-RFC822'!
Object subclass: #MailAddressParser
	instanceVariableNames: 'tokens addresses curAddrTokens'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-RFC822'!
!classDefinition: 'MailAddressParser class' category: #'Cuis-Network-RFC822'!
MailAddressParser class
	instanceVariableNames: ''!

!classDefinition: #MailAddressToken category: #'Cuis-Network-RFC822'!
Object subclass: #MailAddressToken
	instanceVariableNames: 'type text'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-RFC822'!
!classDefinition: 'MailAddressToken class' category: #'Cuis-Network-RFC822'!
MailAddressToken class
	instanceVariableNames: ''!

!classDefinition: #MailAddressTokenizer category: #'Cuis-Network-RFC822'!
Stream subclass: #MailAddressTokenizer
	instanceVariableNames: 'cachedToken text pos'
	classVariableNames: 'CSNonAtom CSNonSeparators CSParens CSSpecials'
	poolDictionaries: ''
	category: 'Cuis-Network-RFC822'!
!classDefinition: 'MailAddressTokenizer class' category: #'Cuis-Network-RFC822'!
MailAddressTokenizer class
	instanceVariableNames: ''!


!MailAddressParser commentStamp: '<historical>' prior: 0!
Parse mail addresses.  The basic syntax is:	addressList := MailAddressParser addressesIn: aStringThis currently only returns the bare addresses, but it could also return a list of the address "source codes".  For example, if you give it "Joe <joe@foo>, <jane>", it will currently return a list ('joe@foo' 'jane').  It would be nice to also get a list ('Joe <joe@foo>'  '<jane>').!

!MailAddressToken commentStamp: '<historical>' prior: 0!
a single token from an RFC822 mail address.  Used internally in MailAddressParser!

!MailAddressTokenizer commentStamp: '<historical>' prior: 0!
Divides an address into tokens, as specified in RFC 822.  Used by MailAddressParser.!

!CharacterSet class methodsFor: '*Cuis-Network-RFC822' stamp: 'gsa 12/7/2012 10:48'!
empty
 	"return an empty set of characters"
	^self new! !

!MailAddressParser methodsFor: 'building address list' stamp: 'ls 9/13/1998 01:31'!
addToAddress	"add the last token to the address.  removes the token from the collection"	curAddrTokens addFirst: (tokens removeLast)! !

!MailAddressParser methodsFor: 'building address list' stamp: 'ls 9/13/1998 01:30'!
finishAddress	"we've finished one address.  Bundle it up and add it to the list of addresses"	| address |	address := String streamContents: [ :str |		curAddrTokens do: [ :tok | str nextPutAll: tok text ] ].	addresses addFirst: address.	curAddrTokens := nil.! !

!MailAddressParser methodsFor: 'parsing' stamp: 'ls 9/13/1998 02:08'!
grabAddressWithRoute	"grad an address of the form 'Descriptive Text <real.address@c.d.e>"		self startNewAddress.	tokens removeLast.	"remove the >"	"grab until we see a $<"	[ 		tokens isEmpty ifTrue: [			self error: '<> are not matched' ].		tokens last type = $<	] whileFalse: [ self addToAddress ].	tokens removeLast.  "remove the <"	self removePhrase.	self finishAddress! !

!MailAddressParser methodsFor: 'parsing' stamp: 'bf 3/12/2000 20:06'!
grabAddresses	"grab all the addresses in the string"	| token |	"remove comments"	tokens removeAllSuchThat: [:t | t type == #Comment].	"grab one address or address group each time through this loop"	[ 		"remove commas"		[			tokens isEmpty not and: [ tokens last type = $, ]		] whileTrue: [ tokens removeLast ].		"check whether any tokens are left"		tokens isEmpty 	] whileFalse: [		token := tokens last.		"delegate, depending on what form the address is in"		"the from can be determined from the last token"		token type = $> ifTrue: [			self grabAddressWithRoute ]		ifFalse: [ 			(#(Atom DomainLiteral QuotedString) includes: token type)  ifTrue: [				self grabBasicAddress ]		ifFalse: [			token type = $; ifTrue: [				self grabGroupAddress ]		ifFalse: [			^self error: 'un-recognized address format' ] ] ]	].	^addresses! !

!MailAddressParser methodsFor: 'parsing' stamp: 'ls 10/23/1998 13:39'!
grabBasicAddress	"grad an address of the form a.b@c.d.e"	self startNewAddress.	"grab either the domain if specified, or the domain if not"	self addToAddress.	[tokens isEmpty not and: [ tokens last type = $.] ] 		whileTrue: 			["add name-dot pairs of tokens"			self addToAddress.			(#(Atom QuotedString ) includes: tokens last type)				ifFalse: [self error: 'bad token in address: ' , tokens last text].			self addToAddress].	(tokens isEmpty or: [tokens last type ~= $@])		ifTrue: ["no domain specified"			self finishAddress]		ifFalse: 			["that was the domain.  check that no QuotedString's slipped in"			curAddrTokens do: [:tok | tok type = #QuotedString ifTrue: [self error: 'quote marks are not allowed within a domain name (' , tok text , ')']].			"add the @ sign"			self addToAddress.			"add the local part"			(#(Atom QuotedString ) includes: tokens last type)				ifFalse: [self error: 'invalid local part for address: ' , tokens last text].			self addToAddress.			"add word-dot pairs if there are any"			[tokens isEmpty not and: [tokens last type = $.]]				whileTrue: 					[self addToAddress.					(tokens isEmpty not and: [#(Atom QuotedString ) includes: tokens last type])						ifTrue: [self addToAddress]].			self finishAddress]! !

!MailAddressParser methodsFor: 'parsing' stamp: 'ls 9/13/1998 02:07'!
grabGroupAddress	"grab an address of the form 'phrase : address, address, ..., address;'"	"I'm not 100% sure what this format means, so I'm just returningthe list of addresses between the : and ;   -ls  (if this sounds right to someone, feel free to remove this comment :)"	"remove the $; "	tokens removeLast.	"grab one address each time through this loop"	[ 		"remove commas"		[			tokens isEmpty not and: [ tokens last type = $, ]		] whileTrue: [ tokens removeLast ].		tokens isEmpty ifTrue: [			"no matching :"			^self error: 'stray ; in address list'. ].		tokens last type = $:	] whileFalse: [		"delegate to either grabAddressWithRoute, or grabBasicAddress.  nested groups are not allowed"		tokens last type = $> ifTrue: [			self grabAddressWithRoute ]		ifFalse: [ 			(#(Atom DomainLiteral QuotedString) includes: tokens last type)  ifTrue: [				self grabBasicAddress ]		ifFalse: [			^self error: 'un-recognized address format' ] ]	].	tokens removeLast.   "remove the :"	self removePhrase.! !

!MailAddressParser methodsFor: 'private-initialization' stamp: 'ls 9/13/1998 01:25'!
initialize: tokenList	tokens := tokenList asOrderedCollection copy.	addresses := OrderedCollection new.! !

!MailAddressParser methodsFor: 'parsing' stamp: 'ls 9/13/1998 02:08'!
removePhrase	"skip most characters to the left of this"	[		tokens isEmpty not and: [			#(Atom QuotedString $. $@) includes: (tokens last type) ]	] whileTrue: [ tokens removeLast ].! !

!MailAddressParser methodsFor: 'building address list' stamp: 'ls 9/13/1998 01:30'!
startNewAddress	"set up data structures to begin a new address"	(curAddrTokens ~~ nil) ifTrue: [		self error: 'starting new address before finishing the last one!!' ].	curAddrTokens := OrderedCollection new.	! !

!MailAddressParser class methodsFor: 'parsing' stamp: 'ls 9/13/1998 01:34'!
addressesIn: aString	"return a collection of the bare addresses listed in aString"	| tokens |	tokens := MailAddressTokenizer tokensIn: aString.	^(self new initialize: tokens) grabAddresses! !

!MailAddressToken methodsFor: 'printing' stamp: 'ls 9/12/1998 20:40'!
printOn: aStream	aStream nextPut: $[.	aStream nextPutAll: self type asString.	aStream nextPut: $|.	aStream nextPutAll: self text.	aStream nextPut: $].! !

!MailAddressToken methodsFor: 'access' stamp: 'ls 9/12/1998 20:42'!
text	^text! !

!MailAddressToken methodsFor: 'access' stamp: 'ls 9/12/1998 20:42'!
type	^type! !

!MailAddressToken methodsFor: 'private' stamp: 'ls 9/12/1998 20:24'!
type: type0  text: text0	type := type0.	text := text0.! !

!MailAddressToken class methodsFor: 'instance creation' stamp: 'ls 9/12/1998 20:31'!
type: type  text: text	^self new type: type text: text! !

!MailAddressTokenizer methodsFor: 'stream protocol' stamp: 'ls 9/12/1998 20:53'!
atEnd	^self peek == nil! !

!MailAddressTokenizer methodsFor: 'tokenizing' stamp: 'ls 9/12/1998 20:51'!
atEndOfChars	^pos > text size! !

!MailAddressTokenizer methodsFor: 'initialization' stamp: 'ls 9/12/1998 20:13'!
initialize: aString	text := aString.	pos := 1.! !

!MailAddressTokenizer methodsFor: 'stream protocol' stamp: 'ls 9/12/1998 20:51'!
next	| ans |	cachedToken ifNil: [ ^self nextToken ].	ans := cachedToken.	cachedToken := nil.	^ans! !

!MailAddressTokenizer methodsFor: 'tokenizing' stamp: 'ls 9/12/1998 20:44'!
nextAtom	| start end |	start := pos.	pos := text indexOfAnyOf: CSNonAtom startingAt: start ifAbsent: [ text size + 1].	end := pos - 1.	^MailAddressToken		type: #Atom		text: (text copyFrom: start to: end)! !

!MailAddressTokenizer methodsFor: 'tokenizing' stamp: 'ls 9/12/1998 20:52'!
nextChar	self atEndOfChars ifTrue: [ ^nil ].	pos := pos + 1.	^text at: (pos-1)! !

!MailAddressTokenizer methodsFor: 'tokenizing' stamp: 'mas 2/8/2001 11:36'!
nextComment	| start nestLevel paren |	start := pos.	pos := pos + 1.	nestLevel := 1.	[ nestLevel > 0 ] whileTrue: [		pos := text indexOfAnyOf: CSParens startingAt: pos  ifAbsent: [ 0 ].		pos = 0 ifTrue: [ 			self error: 'unterminated comment.  ie, more (''s than )''s' ].		paren := self nextChar.		paren = $( ifTrue: [ nestLevel := nestLevel + 1 ] ifFalse: [ nestLevel := nestLevel - 1 ]].	^ MailAddressToken type: #Comment		text: (text copyFrom: start to: pos - 1)! !

!MailAddressTokenizer methodsFor: 'tokenizing' stamp: 'ls 9/13/1998 01:39'!
nextDomainLiteral	| start end |	start := pos.	end := text indexOf: $] startingAt: start ifAbsent: [ 0 ].	end = 0 ifTrue: [		"not specified"		self error: 'saw [ without a matching ]' ].	pos := end+1.	^MailAddressToken		type: #DomainLiteral		text: (text copyFrom: start to: end)! !

!MailAddressTokenizer methodsFor: 'tokenizing' stamp: 'PeterHugossonMiller 9/3/2009 10:02'!
nextQuotedString	| res c |	res := String new writeStream.	res nextPut: self nextChar.   "record the starting quote"	[ self atEndOfChars ] whileFalse: [		c := self nextChar.		c = $\ ifTrue: [			res nextPut: c.			res nextPut: self nextChar ]		ifFalse: [			c = $" ifTrue: [				res nextPut: c.				^MailAddressToken type: #QuotedString  text: res contents ]			ifFalse: [				res nextPut: c ] ] ].	"hmm, never saw the final quote mark"	^MailAddressToken type: #QuotedString  text: (res contents, '"')! !

!MailAddressTokenizer methodsFor: 'tokenizing' stamp: 'ls 9/12/1998 20:44'!
nextSpecial	| c |	c := self nextChar.	^MailAddressToken type: c  text: c asString.! !

!MailAddressTokenizer methodsFor: 'tokenizing' stamp: 'bf 3/12/2000 19:53'!
nextToken	| c |	self skipSeparators.	c := self peekChar.	c ifNil: [ ^nil ].	c = $( ifTrue: [ ^self nextComment ].	c = $" ifTrue: [ ^self nextQuotedString ].	c = $[ ifTrue: [ ^self nextDomainLiteral ].	(CSSpecials includes: c) ifTrue: [ ^self nextSpecial ].	^self nextAtom! !

!MailAddressTokenizer methodsFor: 'stream protocol' stamp: 'ls 9/12/1998 20:53'!
peek	cachedToken ifNil: [ cachedToken := self nextToken. ].		^cachedToken	! !

!MailAddressTokenizer methodsFor: 'tokenizing' stamp: 'ls 9/12/1998 20:15'!
peekChar	^text at: pos ifAbsent: [ nil ]! !

!MailAddressTokenizer methodsFor: 'tokenizing' stamp: 'ls 9/12/1998 20:14'!
skipSeparators	pos := text indexOfAnyOf: CSNonSeparators  startingAt: pos  ifAbsent: [ text size + 1 ].! !

!MailAddressTokenizer class methodsFor: 'instance creation' stamp: 'ls 9/12/1998 20:54'!
forString: aString	^super basicNew initialize: aString! !

!MailAddressTokenizer class methodsFor: 'initialization' stamp: 'gsa 12/7/2012 11:01'!
initialize
	"Initalize class variables using   MailAddressTokenizer initialize"

	| atomChars |

	CSParens := CharacterSet empty.
	CSParens addAll: '()'.

	CSSpecials := CharacterSet empty.
	CSSpecials addAll: '()<>@,;:\".[]'.

	CSNonSeparators := CharacterSet separators complement.


	"(from RFC 2822)"
	atomChars := CharacterSet empty.
	atomChars addAll: ($A to: $Z).
	atomChars addAll: ($a to: $z).
	atomChars addAll: ($0 to: $9).
	atomChars addAll: '!!#$%^''*+-/=?^_`{|}~'.

	CSNonAtom :=  atomChars complement.! !

!MailAddressTokenizer class methodsFor: 'instance creation' stamp: 'ls 9/13/1998 01:34'!
tokensIn: aString	"return a collection of the tokens in aString"	^(self forString: aString) upToEnd! !
MailAddressTokenizer initialize!
