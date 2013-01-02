'From Cuis 4.1 of 12 December 2012 [latest update: #1528] on 2 January 2013 at 8:39:06 am'!
'Description Please enter a description for this package '!
!classDefinition: #UUIDPrimitivesTest category: #'Cuis-NetworkTests-UUID'!
TestCase subclass: #UUIDPrimitivesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-NetworkTests-UUID'!
!classDefinition: 'UUIDPrimitivesTest class' category: #'Cuis-NetworkTests-UUID'!
UUIDPrimitivesTest class
	instanceVariableNames: ''!

!classDefinition: #UUIDTest category: #'Cuis-NetworkTests-UUID'!
TestCase subclass: #UUIDTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-NetworkTests-UUID'!
!classDefinition: 'UUIDTest class' category: #'Cuis-NetworkTests-UUID'!
UUIDTest class
	instanceVariableNames: ''!


!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'StephaneDucasse 11/5/2011 15:35'!
testComparison
	"self debug: #testComparison"
	|a b|
	a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
	b := UUID fromString: 'e85ae7ba-3ca3-4bae-9f62-cc2ce51c525e'.
	self assert: a < b.
	self deny: a> b.
	
	a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
	b := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df642'.
	self assert: a < b.
	
	a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
	b := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df640'.
	self assert: a > b.
	! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'JMM 11/22/2001 17:14'!
testCreation
	| uuid |
	uuid := UUID new.
	self should: [uuid size = 16].
	self shouldnt: [uuid isNilUUID].
	self should: [uuid asString size = 36].
! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'JMM 11/22/2001 17:27'!
testCreationEquality
	| uuid1 uuid2 |
	uuid1 := UUID new.
	uuid2 := UUID new.
	self should: [uuid1 = uuid1].
	self should: [uuid2 = uuid2].
	self shouldnt: [uuid1 = uuid2].
	self shouldnt: [uuid1 hash = uuid2 hash].
! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'JMM 11/22/2001 17:17'!
testCreationFromString
	| uuid string |
	string := UUID nilUUID asString.
	uuid := UUID fromString: string.
	self should: [uuid size = 16].
	self should: [uuid = UUID nilUUID].
	self should: [uuid isNilUUID].
	self should: [uuid asString size = 36].
	self should: [uuid asArray asSet size = 1].
	self should: [(uuid asArray asSet asArray at: 1) = 0].
! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'StephaneDucasse 8/21/2010 20:54'!
testCreationFromString36NotNil
	| uuid uuid36 |
	uuid := UUID fromString: 'fedcba98-7654-3210-0123-456789abcdef'.
	uuid36 := UUID fromString36: 'e738i8a5the1f87mrh14o6vi6'.
	self should: [uuid = uuid36]! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'StephaneDucasse 8/21/2010 20:58'!
testCreationFromString36With0
	| uuid |
	uuid := UUID fromString36: '0'.
	self assert: (uuid isNilUUID).
	
! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'StephaneDucasse 8/21/2010 20:59'!
testCreationFromString36WithNillUUID
	
	self shouldnt: [UUID fromString36: (UUID nilUUID asString36)] 
    		raise: Error.! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'JMM 11/22/2001 17:18'!
testCreationFromStringNotNil
	| uuid string |
	string := UUID new asString.
	uuid := UUID fromString: string.
	self should: [uuid size = 16].
	self should: [uuid asString size = 36].

! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'JMM 11/22/2001 17:16'!
testCreationNil
	| uuid |
	uuid := UUID nilUUID.
	self should: [uuid size = 16].
	self should: [uuid isNilUUID].
	self should: [uuid asString size = 36].
	self should: [uuid asArray asSet size = 1].
	self should: [(uuid asArray asSet asArray at: 1) = 0].
! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'nice 1/5/2010 15:59'!
testCreationNodeBased
	

	(UUID new asString last: 12) = (UUID new asString last: 12) ifFalse: [^self].
	1000 timesRepeat:
		[ | uuid |uuid := UUID new.
		self should: [((uuid at: 7) bitAnd: 16rF0) = 16r10].
		self should: [((uuid at: 9) bitAnd: 16rC0) = 16r80]]
! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'nice 1/5/2010 15:59'!
testDuplicationsKinda
	| check size |

	size := 5000.
	check := Set new: size.
	size timesRepeat: 
		[ | uuid |uuid := UUID new.
		self shouldnt: [check includes: uuid].
		check add: uuid].
		! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'nice 1/5/2010 15:59'!
testOrder
	
	100 timesRepeat:
		[ | uuid1 uuid2 |uuid1 := UUID new.
		uuid2 := UUID new.
		(uuid1 asString last: 12) = (uuid2 asString last: 12) ifTrue:
			[self should: [uuid1 < uuid2].
			self should: [uuid2 > uuid1].
			self shouldnt: [uuid1 = uuid2]]]
! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'StephaneDucasse 7/22/2011 18:09'!
testUniqueness 
	"Test uniqueness for a given number of generated values"
	
	|maxTestValue|
	maxTestValue := 1000.
	self assert: ((1 to: maxTestValue) collect: [ :i | UUID new asString ]) asSet size = maxTestValue! !

!UUIDTest methodsFor: 'testing' stamp: 'EstebanLorenzano 11/18/2011 13:58'!
testComparison
	|a b|
	
	a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
	b := UUID fromString: 'e85ae7ba-3ca3-4bae-9f62-cc2ce51c525e'.
	
	self assert: a = a copy.
	self assert: b = b copy.
	self assert: a < b.
	self assert: b > a.
	
	self deny: (a > b) = (b > a)
	
	! !
