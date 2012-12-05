'From Cuis 4.0 of 21 April 2012 [latest update: #1308] on 27 November 2012 at 9:13:07 pm'!
'Description Please enter a description for this package '!
!classDefinition: #MatrixPharo14CLayer category: #'CuisSandstoneDB-Pharo14CLayer'!
Collection subclass: #MatrixPharo14CLayer
	instanceVariableNames: 'nrows ncols contents'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CuisSandstoneDB-Pharo14CLayer'!
!classDefinition: 'MatrixPharo14CLayer class' category: #'CuisSandstoneDB-Pharo14CLayer'!
MatrixPharo14CLayer class
	instanceVariableNames: ''!


!Collection methodsFor: '*CuisSandstoneDB-Pharo14CLayer' stamp: 'gsa 11/23/2012 11:08'!
contains: aBlock
	"VW compatibility"
	^self anySatisfy: aBlock! !

!Collection methodsFor: '*CuisSandstoneDB-Pharo14CLayer' stamp: 'gsa 11/27/2012 18:37'!
select: selectBlock thenDo: doBlock
    "Utility method to improve readability.
	Do not create the intermediate collection."

    self do: [: each |
        ( selectBlock value: each ) 
			ifTrue: [ doBlock value: each ]
    ].! !

!Dictionary methodsFor: '*CuisSandstoneDB-Pharo14CLayer' stamp: 'gsa 11/23/2012 16:15'!
associationsSelect: aBlock 
	"Evaluate aBlock with each of my associations as the argument. Collect
	into a new dictionary, only those associations for which aBlock evaluates
	to true."

	| newCollection |
	newCollection := self species new.
	self associationsDo: 
		[:each | 
		(aBlock value: each) ifTrue: [newCollection add: each]].
	^newCollection! !

!MatrixPharo14CLayer methodsFor: 'copying' stamp: 'GermanArduino 11/27/2012 16:33'!
, aMatrix

!MatrixPharo14CLayer methodsFor: 'copying' stamp: 'GermanArduino 11/27/2012 16:33'!
,, aMatrix

!MatrixPharo14CLayer methodsFor: 'comparing' stamp: 'GermanArduino 11/27/2012 16:33'!
= aMatrix

!MatrixPharo14CLayer methodsFor: 'adding' stamp: 'GermanArduino 11/27/2012 16:33'!
add: newObject

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
anyOne

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asArray

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asBag

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asByteArray

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asCharacterSet

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asFloatArray

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asIdentitySet

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asIntegerArray

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asOrderedCollection

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asSet

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asSortedCollection

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asSortedCollection: aBlock

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asWordArray

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
at: row at: column

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
at: r at: c ifInvalid: v

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
at: row at: column incrementBy: value

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
at: row at: column put: value

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
atAllPut: value

!MatrixPharo14CLayer methodsFor: 'accessing rows/columns' stamp: 'GermanArduino 11/27/2012 16:33'!
atColumn: column

!MatrixPharo14CLayer methodsFor: 'accessing rows/columns' stamp: 'GermanArduino 11/27/2012 16:33'!
atColumn: column put: aCollection

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
atRandom

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
atRandom: aGenerator

!MatrixPharo14CLayer methodsFor: 'accessing rows/columns' stamp: 'GermanArduino 11/27/2012 16:33'!
atRow: row

!MatrixPharo14CLayer methodsFor: 'accessing rows/columns' stamp: 'GermanArduino 11/27/2012 16:33'!
atRow: row put: aCollection

!MatrixPharo14CLayer methodsFor: 'accessing submatrices' stamp: 'GermanArduino 11/27/2012 16:33'!
atRows: rs columns: cs

!MatrixPharo14CLayer methodsFor: 'accessing submatrices' stamp: 'GermanArduino 11/27/2012 16:33'!
atRows: r1 to: r2 columns: c1 to: c2

!MatrixPharo14CLayer methodsFor: 'accessing submatrices' stamp: 'GermanArduino 11/27/2012 16:33'!
atRows: r1 to: r2 columns: c1 to: c2 ifInvalid: element

!MatrixPharo14CLayer methodsFor: 'accessing submatrices' stamp: 'GermanArduino 11/27/2012 16:33'!
atRows: r1 to: r2 columns: c1 to: c2 put: aMatrix

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
collect: aBlock

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
columnCount

!MatrixPharo14CLayer methodsFor: 'accessing rows/columns' stamp: 'GermanArduino 11/27/2012 16:33'!
diagonal

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
difference: aCollection

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
do: aBlock

!MatrixPharo14CLayer methodsFor: 'comparing' stamp: 'GermanArduino 11/27/2012 16:33'!
hash

!MatrixPharo14CLayer methodsFor: 'testing' stamp: 'GermanArduino 11/27/2012 16:33'!
identityIncludes: anObject

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
identityIndexOf: anElement

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
identityIndexOf: anElement ifAbsent: anExceptionBlock

!MatrixPharo14CLayer methodsFor: 'testing' stamp: 'GermanArduino 11/27/2012 16:33'!
includes: anObject

!MatrixPharo14CLayer methodsFor: 'testing' stamp: 'GermanArduino 11/27/2012 16:33'!
includesAll: aCollection

!MatrixPharo14CLayer methodsFor: 'testing' stamp: 'GermanArduino 11/27/2012 16:33'!
includesAllOf: aCollection

!MatrixPharo14CLayer methodsFor: 'testing' stamp: 'GermanArduino 11/27/2012 16:33'!
includesAny: aCollection

!MatrixPharo14CLayer methodsFor: 'testing' stamp: 'GermanArduino 11/27/2012 16:33'!
includesAnyOf: aCollection

!MatrixPharo14CLayer methodsFor: 'private' stamp: 'GermanArduino 11/27/2012 16:33'!
indexForRow: row andColumn: column

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
indexOf: anElement

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
indexOf: anElement ifAbsent: anExceptionBlock

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
indicesCollect: aBlock

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
indicesDo: aBlock

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
indicesInject: start into: aBlock

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
intersection: aCollection

!MatrixPharo14CLayer methodsFor: 'testing' stamp: 'GermanArduino 11/27/2012 16:33'!
isSequenceable

!MatrixPharo14CLayer methodsFor: 'testing' stamp: 'GermanArduino 11/27/2012 16:33'!
occurrencesOf: anObject

!MatrixPharo14CLayer methodsFor: 'copying' stamp: 'GermanArduino 11/27/2012 16:33'!
postCopy

!MatrixPharo14CLayer methodsFor: 'private' stamp: 'GermanArduino 11/27/2012 16:33'!
privateContents

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
readStream

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
reject: aBlock

!MatrixPharo14CLayer methodsFor: 'removing' stamp: 'GermanArduino 11/27/2012 16:33'!
remove: anObject ifAbsent: anExceptionBlock

!MatrixPharo14CLayer methodsFor: 'removing' stamp: 'GermanArduino 11/27/2012 16:33'!
removeAll

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
replaceAll: oldObject with: newObject

!MatrixPharo14CLayer methodsFor: 'private' stamp: 'GermanArduino 11/27/2012 16:33'!
rowAndColumnForIndex: index

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
rowCount

!MatrixPharo14CLayer methodsFor: 'private' stamp: 'GermanArduino 11/27/2012 16:33'!
rows: rows columns: columns contents: anArray

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
select: aBlock

!MatrixPharo14CLayer methodsFor: 'copying' stamp: 'GermanArduino 11/27/2012 16:33'!
shuffled

!MatrixPharo14CLayer methodsFor: 'copying' stamp: 'GermanArduino 11/27/2012 16:33'!
shuffledBy: aRandom

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
size

!MatrixPharo14CLayer methodsFor: 'printing' stamp: 'GermanArduino 11/27/2012 16:33'!
storeOn: aStream

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
swap: r1 at: c1 with: r2 at: c2

!MatrixPharo14CLayer methodsFor: 'accessing rows/columns' stamp: 'GermanArduino 11/27/2012 16:33'!
swapColumn: anIndex withColumn: anotherIndex

!MatrixPharo14CLayer methodsFor: 'accessing rows/columns' stamp: 'GermanArduino 11/27/2012 16:33'!
swapRow: anIndex withRow: anotherIndex

!MatrixPharo14CLayer methodsFor: 'accessing rows/columns' stamp: 'GermanArduino 11/27/2012 16:33'!
transposed

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
with: aCollection collect: aBlock

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
with: aCollection do: aBlock

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
with: aCollection inject: startingValue into: aBlock

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
withIndicesCollect: aBlock

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
withIndicesDo: aBlock

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
withIndicesInject: start into: aBlock

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
column: aCollection

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
diagonal: aCollection

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
identity: n

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
new: dim

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
new: dim element: element

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
new: dim tabulate: aBlock

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
ones: n

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
row: aCollection

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
rows: rows columns: columns

!MatrixPharo14CLayer class methodsFor: 'private' stamp: 'GermanArduino 11/27/2012 16:33'!
rows: rows columns: columns contents: contents

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
rows: rows columns: columns element: element

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
rows: rows columns: columns tabulate: aBlock

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
zeros: n

!Symbol methodsFor: '*CuisSandstoneDB-Pharo14CLayer' stamp: 'gsa 11/23/2012 10:51'!
value: anObject 
	^anObject perform: self.! !