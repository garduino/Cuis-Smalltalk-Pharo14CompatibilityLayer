'From Cuis 4.1 of 12 December 2012 [latest update: #1528] on 2 January 2013 at 8:36:33 am'!
'Description Please enter a description for this package '!
!classDefinition: #UUID category: #'Cuis-Network-UUID'!
ByteArray variableByteSubclass: #UUID
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-Network-UUID'!
!classDefinition: 'UUID class' category: #'Cuis-Network-UUID'!
UUID class
	instanceVariableNames: ''!

!classDefinition: #UUIDGenerator category: #'Cuis-Network-UUID'!
Object subclass: #UUIDGenerator
	instanceVariableNames: 'timeLow timeMid timeHiAndVersion clockSeqHiAndReserved clockSeqLow node randomCounter randomGenerator semaphoreForGenerator'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'Cuis-Network-UUID'!
!classDefinition: 'UUIDGenerator class' category: #'Cuis-Network-UUID'!
UUIDGenerator class
	instanceVariableNames: ''!


!UUID commentStamp: '<historical>' prior: 0!
A class to generate UUID
by John M McIntosh johnmci@smalltalkconsulting.com

See http://www.webdav.org/specs/draft-leach-uuids-guids-01.txt

If a plugin does not exist then we generate a UUID version 4 type GUUID!

!UUIDGenerator commentStamp: '<historical>' prior: 0!
This class generates a pseudo-random UUID
by John M McIntosh johnmci@smalltalkconsulting.com

See http://www.webdav.org/specs/draft-leach-uuids-guids-01.txt!

!UUID methodsFor: 'comparing' stamp: 'EstebanLorenzano 11/18/2011 13:56'!
< aMagnitude 
	"Answer whether the receiver is less than the argument."

	(self size = aMagnitude size) ifFalse: [ 
		^ self size < aMagnitude size ].
	1 to: self size do: [:i |
		(self at: i) = (aMagnitude at: i) ifFalse: [
			^ (self at: i) < (aMagnitude at: i) ]].			
	^false.! !

!UUID methodsFor: 'comparing' stamp: 'JMM 11/22/2001 17:30'!
<= aMagnitude 
	"Answer whether the receiver is less than or equal to the argument."

	^(self > aMagnitude) not! !

!UUID methodsFor: 'comparing' stamp: 'JMM 11/22/2001 17:30'!
> aMagnitude 
	"Answer whether the receiver is greater than the argument."

	^aMagnitude < self! !

!UUID methodsFor: 'comparing' stamp: 'JMM 11/22/2001 17:30'!
>= aMagnitude 
	"Answer whether the receiver is greater than or equal to the argument."

	^(self < aMagnitude) not! !

!UUID methodsFor: 'converting' stamp: 'PeterHugossonMiller 9/3/2009 11:46'!
asString
	| result data |
	data := String new: 36.
	result := data writeStream.
	1 to: 4 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	5 to: 6 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	7 to: 8 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	9 to: 10 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	11 to: 16 do:[:i| self printHexAt: i to: result].
	^data.
	! !

!UUID methodsFor: 'converting' stamp: 'gk 4/18/2006 22:15'!
asString36
	"Encode the UUID as a base 36 string using 0-9 and lowercase a-z.
	This is the shortest representation still being able to work as
	filenames etc since it does not depend on case nor characters
	that might cause problems, and it fits into short filenames like on
	the old MacOS HFS filesystem. The check for 36r is to make this code
	work in versions before Squeak 3.8."

	| num candidate |
	num := 0.
	1 to: self size do: [:i | num := num + ((256 raisedTo: i - 1) * (self at: i))].
	candidate := num printStringBase: 36.
	^((candidate beginsWith: '36r')
			ifTrue: [candidate copyFrom: 4 to: candidate size]
			ifFalse: [candidate]) asLowercase! !

!UUID methodsFor: 'converting' stamp: 'damiencassou 5/30/2008 15:52'!
asUUID: aString 
	| stream token byte |
	stream := (aString 
		copyReplaceAll: '-'
		with: '') asUppercase readStream.
	1 
		to: stream size / 2
		do: 
			[ :i | 
			token := stream next: 2.
			byte := Integer 
				readFrom: token readStream
				base: 16.
			self 
				at: i
				put: byte ].
	^ self! !

!UUID methodsFor: 'converting' stamp: 'PeterHugossonMiller 9/3/2009 11:46'!
createStringStartingAt: index for: bytes

	| results candidate data |
	data := String new: bytes*2.
	results := data writeStream.
	index to: index+bytes -1 do: 
		[:i |
		candidate := ((self at: i) printStringBase: 16) last: 2.
		candidate first = $r ifTrue: [candidate := String with: $0 with: candidate last].
		results nextPutAll: candidate].
	^data asLowercase! !

!UUID methodsFor: 'initalize-release' stamp: 'alain.plantec 5/28/2009 11:15'!
initialize
	super initialize.
	self primMakeUUID.! !

!UUID methodsFor: 'testing' stamp: 'JMM 10/9/2001 14:17'!
isNilUUID
	1 to: self size do: [:i | (self at: i) ~= 0 ifTrue: [^false]].
	^true.! !

!UUID methodsFor: 'system primitives' stamp: 'ar 2/3/2002 19:42'!
primMakeUUID
	<primitive: 'primitiveMakeUUID' module: 'UUIDPlugin'>
	UUIDGenerator default generateBytes: self forVersion: 4.! !

!UUID methodsFor: 'converting' stamp: 'ar 2/8/2004 12:16'!
printHexAt: index to: aStream
	| map v |
	map := '0123456789abcdef'.
	v := self at: index.
	aStream nextPut: (map at: (v bitShift: -4) + 1). 
	aStream nextPut: (map at: (v bitAnd: 15) + 1).
! !

!UUID methodsFor: 'printing' stamp: 'JMM 10/9/2001 14:46'!
printOn: aStream
	aStream nextPutAll: 'an UUID('.
	self asString printOn: aStream.
	aStream nextPutAll: ')'! !

!UUID methodsFor: 'printing' stamp: 'ar 2/8/2004 12:16'!
printString
	^self asString! !

!UUID class methodsFor: 'instance creation' stamp: 'StephaneDucasse 8/21/2010 22:01'!
fromString36: aString
	"Decode the UUID from a base 36 string using 0-9 and lowercase a-z.
	This is the shortest representation still being able to work as
	filenames etc since it does not depend on case nor characters
	that might cause problems."

	| object num |
	object := self nilUUID.
	num := Integer readFrom: aString asUppercase readStream base: 36.
	1 to: 16 do: [:i | object at: i put: (num digitAt: i)].
	^object! !

!UUID class methodsFor: 'instance creation' stamp: 'dvf 9/10/2004 23:10'!
fromString: aString
	| object |
	aString size ~= 36 ifTrue: [Error signal].
	object := self nilUUID. 
	object asUUID: aString.
	^object! !

!UUID class methodsFor: 'instance creation' stamp: 'nk 7/30/2004 21:49'!
new
	^(self new: 16)! !

!UUID class methodsFor: 'instance creation' stamp: 'nk 6/28/2004 16:10'!
nilUUID
	"Must call basicNew: here because I have a non-trivial initialize method."

	^self basicNew: 16! !

!UUIDGenerator methodsFor: 'instance creation' stamp: 'JMM 11/22/2001 13:45'!
generateBytes: aPlaceHolder forVersion: aVersion
	aVersion = 4 ifTrue: [self generateFieldsVersion4]
		ifFalse: [self error: 'Unsupported version'].
	self placeFields: aPlaceHolder.! !

!UUIDGenerator methodsFor: 'instance creation' stamp: 'JMM 11/22/2001 23:13'!
generateFieldsVersion4

	timeLow := self generateRandomBitsOfLength: 32.
	timeMid := self generateRandomBitsOfLength: 16.
	timeHiAndVersion := 16r4000 bitOr: (self generateRandomBitsOfLength: 12).
	clockSeqHiAndReserved := 16r80 bitOr: (self generateRandomBitsOfLength: 6).
	clockSeqLow := self generateRandomBitsOfLength: 8.
	node := self generateRandomBitsOfLength: 48.
	! !

!UUIDGenerator methodsFor: 'generator' stamp: 'nice 1/5/2010 20:55'!
generateOneOrZero
	^self semaphoreForGenerator
		critical: [
			| value | 
			value := self randomGenerator next.
			self randomCounter: self randomCounter + 1.
			self randomCounter > 100000
				ifTrue: [self setupRandom].
			value < 0.5
					ifTrue: [0]
					ifFalse: [1]]! !

!UUIDGenerator methodsFor: 'generator' stamp: 'JMM 11/21/2001 15:12'!
generateRandomBitsOfLength: aNumberOfBits
| target |
	target := 0.
	aNumberOfBits isZero ifTrue: [^target].
	target := self generateOneOrZero.
	(aNumberOfBits - 1)  timesRepeat:
		[target := (target bitShift: 1)  bitOr: self generateOneOrZero].
	^target! !

!UUIDGenerator methodsFor: 'instance creation' stamp: 'alain.plantec 5/28/2009 11:15'!
initialize
	super initialize.
	self setupRandom.
	semaphoreForGenerator := Semaphore forMutualExclusion.
	! !

!UUIDGenerator methodsFor: 'random seed' stamp: 'dew 2/8/2003 00:28'!
makeSeed
	"Try various methods of getting good seeds"
	| seed |
	seed := self makeUnixSeed.
	seed ifNotNil: [^seed].

	"not sure if this is reliably random... commented out for now. -dew"
	"seed := self makeSeedFromSound.
	seed ifNotNil: [^seed]."
	
	"default"
	[seed := (Time millisecondClockValue bitAnd: 16r3FFFFFFF) bitXor: self hash.
	seed := seed bitXor: (Time totalSeconds bitAnd: 16r3FFFFFFF).
	seed = 0] whileTrue: ["Try again if ever get a seed = 0"].

	^seed
! !

!UUIDGenerator methodsFor: 'random seed' stamp: 'gk 2/23/2004 21:09'!
makeSeedFromSound
 	| answer |
 	[answer := SoundService default randomBitsFromSoundInput: 32
 	] ifError: [answer := nil].
 	^answer! !

!UUIDGenerator methodsFor: 'random seed' stamp: 'StephaneDucasse 2/2/2010 22:18'!
makeUnixSeed
	
	^ [FileStream readOnlyFileNamed: '/dev/urandom' do: [ :strm | 
			strm binary.
			strm converter: Latin1TextConverter new.
			Integer
				byte1: strm next
				byte2: strm next
				byte3: strm next
				byte4: strm next. ] ] on: FileStreamException do: [ nil ].
! !

!UUIDGenerator methodsFor: 'instance creation' stamp: 'JMM 11/22/2001 23:12'!
placeFields: aByteArray

	aByteArray at: 1 put: ((timeLow bitShift: -24) bitAnd: 16rFF).
	aByteArray at: 2 put: ((timeLow bitShift: -16) bitAnd: 16rFF).
	aByteArray at: 3 put: ((timeLow bitShift: -8) bitAnd: 16rFF).
	aByteArray at: 4 put: (timeLow bitAnd: 16rFF).
	aByteArray at: 5 put: ((timeMid bitShift: -8) bitAnd: 16rFF).
	aByteArray at: 6 put: (timeMid bitAnd: 16rFF).
	aByteArray at: 7 put: ((timeHiAndVersion bitShift: -8) bitAnd: 16rFF).
	aByteArray at: 8 put: (timeHiAndVersion bitAnd: 16rFF).
	aByteArray at: 9 put: clockSeqHiAndReserved.
	aByteArray at: 10 put: clockSeqLow.
	0 to: 5 do: [:i |
		aByteArray at: 11 + i put: ((node bitShift: (-8*i)) bitAnd: 16rFF)]
! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 11/21/2001 14:28'!
randomCounter
	^randomCounter! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 11/21/2001 14:29'!
randomCounter: aNumber
	randomCounter := aNumber
! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 11/21/2001 14:27'!
randomGenerator
	^randomGenerator
! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 11/21/2001 14:27'!
randomGenerator: aGenerator
	randomGenerator := aGenerator
! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 11/21/2001 14:29'!
semaphoreForGenerator
	^semaphoreForGenerator! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 11/21/2001 14:29'!
semaphoreForGenerator: aSema
	semaphoreForGenerator := aSema
! !

!UUIDGenerator methodsFor: 'instance creation' stamp: 'CdG 11/19/2002 21:30'!
setupRandom
	randomCounter := 0.
	randomGenerator := Random seed: self makeSeed.! !

!UUIDGenerator class methodsFor: 'instance creation' stamp: 'JMM 11/22/2001 13:41'!
default
	Default ifNil: [self generateDefault].
	^Default! !

!UUIDGenerator class methodsFor: 'instance creation' stamp: 'nk 7/30/2004 21:51'!
generateDefault
	Default := self new! !

!UUIDGenerator class methodsFor: 'initialization' stamp: 'PavelKrivanek 6/10/2011 17:00'!
initialize
  Smalltalk addToStartUpList: self ! !

!UUIDGenerator class methodsFor: 'initialization' stamp: 'CdG 11/19/2002 21:07'!
startUp
	Default := nil! !
UUIDGenerator initialize!
