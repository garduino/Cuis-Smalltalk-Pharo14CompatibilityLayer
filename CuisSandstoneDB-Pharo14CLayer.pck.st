'From Cuis 4.1 of 12 December 2012 [latest update: #1578] on 25 January 2013 at 8:31:37 am'!
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
, aMatrix	"Answer a new matrix having the same number of rows as the receiver and aMatrix,	 its columns being the columns of the receiver followed by the columns of aMatrix."	|newCont newCols anArray oldCols a b c|	[nrows = aMatrix rowCount] assert.	newCont := Array new: self size + aMatrix size.	anArray := aMatrix privateContents.	oldCols := aMatrix columnCount.	newCols := ncols + oldCols.	a := b := c := 1.	1 to: nrows do: [:r |		newCont replaceFrom: a to: a+ncols-1 with: contents startingAt: b.		newCont replaceFrom: a+ncols to: a+newCols-1 with: anArray startingAt: c.		a := a + newCols.		b := b + ncols.		c := c + oldCols].	^self class rows: nrows columns: newCols contents: newCont		! !

!MatrixPharo14CLayer methodsFor: 'copying' stamp: 'GermanArduino 11/27/2012 16:33'!
,, aMatrix	"Answer a new matrix having the same number of columns as the receiver and aMatrix,	 its rows being the rows of the receiver followed by the rows of aMatrix."	[ncols = aMatrix columnCount] assert.	^self class rows: nrows + aMatrix rowCount columns: ncols		contents: contents , aMatrix privateContents! !

!MatrixPharo14CLayer methodsFor: 'comparing' stamp: 'GermanArduino 11/27/2012 16:33'!
= aMatrix	^aMatrix class == self class and: [	 aMatrix rowCount = nrows and: [	 aMatrix columnCount = ncols and: [	 aMatrix privateContents = contents]]]! !

!MatrixPharo14CLayer methodsFor: 'adding' stamp: 'GermanArduino 11/27/2012 16:33'!
add: newObject	self shouldNotImplement! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
anyOne	^contents anyOne! !

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asArray	^contents shallowCopy! !

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asBag	^contents asBag! !

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asByteArray	^contents asByteArray! !

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asCharacterSet	^contents asCharacterSet! !

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asFloatArray	^contents asFloatArray! !

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asIdentitySet	^contents asIdentitySet! !

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asIntegerArray	^contents asIntegerArray! !

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asOrderedCollection	^contents asOrderedCollection! !

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asSet	^contents asSet! !

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asSortedCollection	^contents asSortedCollection! !

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asSortedCollection: aBlock	^contents asSortedCollection: aBlock! !

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
asWordArray	^contents asWordArray! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
at: row at: column	^contents at: (self indexForRow: row andColumn: column)! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
at: r at: c ifInvalid: v	"If r,c is a valid index for this matrix, answer the corresponding element.	 Otherwise, answer v."	(r between: 1 and: nrows) ifFalse: [^v].	(c between: 1 and: ncols) ifFalse: [^v].	^contents at: (r-1)*ncols + c! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
at: row at: column incrementBy: value	"Array2D>>at:at:add: was the origin of this method, but in Smalltalk add:	 generally suggests adding an element to a collection, not doing a sum.	 This method, and SequenceableCollection>>at:incrementBy: that supports	 it, have been renamed to reveal their intention more clearly."	^contents at: (self indexForRow: row andColumn: column) incrementBy: value! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
at: row at: column put: value	^contents at: (self indexForRow: row andColumn: column) put: value! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
atAllPut: value	contents atAllPut: value! !

!MatrixPharo14CLayer methodsFor: 'accessing rows/columns' stamp: 'GermanArduino 11/27/2012 16:33'!
atColumn: column	|p|	p := (self indexForRow: 1 andColumn: column)-ncols.	^(1 to: nrows) collect: [:row | contents at: (p := p+ncols)]! !

!MatrixPharo14CLayer methodsFor: 'accessing rows/columns' stamp: 'GermanArduino 11/27/2012 16:33'!
atColumn: column put: aCollection	|p|	aCollection size = nrows ifFalse: [self error: 'wrong column size'].	p := (self indexForRow: 1 andColumn: column)-ncols.	aCollection do: [:each | contents at: (p := p+ncols) put: each].	^aCollection! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
atRandom	^contents atRandom! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
atRandom: aGenerator	^contents atRandom: aGenerator! !

!MatrixPharo14CLayer methodsFor: 'accessing rows/columns' stamp: 'GermanArduino 11/27/2012 16:33'!
atRow: row	(row between: 1 and: nrows)		ifFalse: [self error: '1st subscript out of range'].	^contents copyFrom: (row-1)*ncols+1 to: row*ncols! !

!MatrixPharo14CLayer methodsFor: 'accessing rows/columns' stamp: 'GermanArduino 11/27/2012 16:33'!
atRow: row put: aCollection	|p|	aCollection size = ncols ifFalse: [self error: 'wrong row size'].	p := (self indexForRow: row andColumn: 1)-1.	aCollection do: [:each | contents at: (p := p+1) put: each].	^aCollection! !

!MatrixPharo14CLayer methodsFor: 'accessing submatrices' stamp: 'GermanArduino 11/27/2012 16:33'!
atRows: rs columns: cs	"Answer a Matrix obtained by slicing the receiver.	 rs and cs should be sequenceable collections of positive integers."	^self class rows: rs size columns: cs size tabulate: [:r :c |		self at: (rs at: r) at: (cs at: c)]! !

!MatrixPharo14CLayer methodsFor: 'accessing submatrices' stamp: 'GermanArduino 11/27/2012 16:33'!
atRows: r1 to: r2 columns: c1 to: c2	"Answer a submatrix [r1..r2][c1..c2] of the receiver."	|rd cd|	rd := r1 - 1.	cd := c1 - 1.	^self class rows: r2-rd columns: c2-cd tabulate: [:r :c| self at: r+rd at: c+cd]! !

!MatrixPharo14CLayer methodsFor: 'accessing submatrices' stamp: 'GermanArduino 11/27/2012 16:33'!
atRows: r1 to: r2 columns: c1 to: c2 ifInvalid: element	"Answer a submatrix [r1..r2][c1..c2] of the receiver.	 Portions of the result outside the bounds of the original matrix	 are filled in with element."	|rd cd|	rd := r1 - 1.	cd := c1 - 1.	^self class rows: r2-rd columns: c2-cd tabulate: [:r :c| self at: r+rd at: c+cd ifInvalid: element]! !

!MatrixPharo14CLayer methodsFor: 'accessing submatrices' stamp: 'GermanArduino 11/27/2012 16:33'!
atRows: r1 to: r2 columns: c1 to: c2 put: aMatrix	"Set the [r1..r2][c1..c2] submatrix of the receiver	 from the [1..r2-r1+1][1..c2-c1+1] submatrix of aMatrix.	 As long as aMatrix responds to at:at: and accepts arguments in the range shown,	 we don't care if it is bigger or even if it is a Matrix at all."	|rd cd|	rd := r1 - 1.	cd := c1 - 1.	r1 to: r2 do: [:r |		c1 to: c2 do: [:c |			self at: r at: c put: (aMatrix at: r-rd at: c-cd)]].	^aMatrix! !

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
collect: aBlock	"Answer a new matrix with transformed elements; transformations should be independent."	^self class rows: nrows columns: ncols contents: (contents collect: aBlock)! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
columnCount	^ncols! !

!MatrixPharo14CLayer methodsFor: 'accessing rows/columns' stamp: 'GermanArduino 11/27/2012 16:33'!
diagonal	"Answer (1 to: (nrows min: ncols)) collect: [:i | self at: i at: i]"	|i|	i := ncols negated.	^(1 to: (nrows min: ncols)) collect: [:j | contents at: (i := i + ncols + 1)]! !

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
difference: aCollection	"Union is in because the result is always a Set.	 Difference and intersection are out because the result is like the receiver,	 and with irregular seleection that cannot be."	self shouldNotImplement! !

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
do: aBlock	"Pass elements to aBlock one at a time in row-major order."	contents do: aBlock! !

!MatrixPharo14CLayer methodsFor: 'comparing' stamp: 'GermanArduino 11/27/2012 16:33'!
hash	"I'm really not sure what would be a good hash function here.	 The essential thing is that it must be compatible with #=, and	 this satisfies that requirement."	^contents hash! !

!MatrixPharo14CLayer methodsFor: 'testing' stamp: 'GermanArduino 11/27/2012 16:33'!
identityIncludes: anObject	^contents identityIncludes: anObject! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
identityIndexOf: anElement	^self identityIndexOf: anElement ifAbsent: [0@0]! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
identityIndexOf: anElement ifAbsent: anExceptionBlock	^self rowAndColumnForIndex:		 (contents identityIndexOf: anElement ifAbsent: [^anExceptionBlock value])! !

!MatrixPharo14CLayer methodsFor: 'testing' stamp: 'GermanArduino 11/27/2012 16:33'!
includes: anObject	^contents includes: anObject! !

!MatrixPharo14CLayer methodsFor: 'testing' stamp: 'GermanArduino 11/27/2012 16:33'!
includesAll: aCollection	^contents includesAll: aCollection! !

!MatrixPharo14CLayer methodsFor: 'testing' stamp: 'GermanArduino 11/27/2012 16:33'!
includesAllOf: aCollection		self flag: 'use includesAll: instead'.		^ self includesAll: aCollection! !

!MatrixPharo14CLayer methodsFor: 'testing' stamp: 'GermanArduino 11/27/2012 16:33'!
includesAny: aCollection	^contents includesAny: aCollection! !

!MatrixPharo14CLayer methodsFor: 'testing' stamp: 'GermanArduino 11/27/2012 16:33'!
includesAnyOf: aCollection		self flag: 'use includesAny: instead'.		^ self includesAny: aCollection! !

!MatrixPharo14CLayer methodsFor: 'private' stamp: 'GermanArduino 11/27/2012 16:33'!
indexForRow: row andColumn: column	(row between: 1 and: nrows)		ifFalse: [self error: '1st subscript out of range'].	(column between: 1 and: ncols)		ifFalse: [self error: '2nd subscript out of range'].	^(row-1) * ncols + column! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
indexOf: anElement	"If there are integers r, c such that (self at: r at: c) = anElement,	 answer some such r@c, otherwise answer 0@0.  This kind of perverse	 result is provided by analogy with SequenceableCollection>>indexOf:.	 The order in which the receiver are searched is UNSPECIFIED except	 that it is the same as the order used by #indexOf:ifAbsent: and #readStream."	^self indexOf: anElement ifAbsent: [0@0]! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
indexOf: anElement ifAbsent: anExceptionBlock	"If there are integers r, c such that (self at: r at: c) = anElement,	 answer some such r@c, otherwise answer the result of anExceptionBlock."	^self rowAndColumnForIndex:		 (contents indexOf: anElement ifAbsent: [^anExceptionBlock value])! !

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
indicesCollect: aBlock	|r i|	r := Array new: nrows * ncols.	i := 0.	1 to: nrows do: [:row |		1 to: ncols do: [:column |			r at: (i := i+1) put: (aBlock value: row value: column)]].	^self class rows: nrows columns: ncols contents: r! !

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
indicesDo: aBlock	1 to: nrows do: [:row |		1 to: ncols do: [:column |			aBlock value: row value: column]].! !

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
indicesInject: start into: aBlock	|current|	current := start.	1 to: nrows do: [:row |		1 to: ncols do: [:column |			current := aBlock value: current value: row value: column]].	^current! !

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
intersection: aCollection	"Union is in because the result is always a Set.	 Difference and intersection are out because the result is like the receiver,	 and with irregular seleection that cannot be."	self shouldNotImplement! !

!MatrixPharo14CLayer methodsFor: 'testing' stamp: 'GermanArduino 11/27/2012 16:33'!
isSequenceable	"LIE so that arithmetic on matrices will work.	 What matters for arithmetic is not that there should be random indexing	 but that the structure should be stable and independent of the values of	 the elements.  #isSequenceable is simply the wrong question to ask."	^true! !

!MatrixPharo14CLayer methodsFor: 'testing' stamp: 'GermanArduino 11/27/2012 16:33'!
occurrencesOf: anObject	^contents occurrencesOf: anObject! !

!MatrixPharo14CLayer methodsFor: 'copying' stamp: 'GermanArduino 11/27/2012 16:33'!
postCopy	super postCopy.	contents := contents copy! !

!MatrixPharo14CLayer methodsFor: 'private' stamp: 'GermanArduino 11/27/2012 16:33'!
privateContents	"Only used in #, #,, and #= so far.	 It used to be called #contents, but that clashes with Collection>>contents."	^contents! !

!MatrixPharo14CLayer methodsFor: 'converting' stamp: 'GermanArduino 11/27/2012 16:33'!
readStream	"Answer a ReadStream that returns all the elements of the receiver	 in some UNSPECIFIED order."	^ contents readStream! !

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
reject: aBlock	self shouldNotImplement! !

!MatrixPharo14CLayer methodsFor: 'removing' stamp: 'GermanArduino 11/27/2012 16:33'!
remove: anObject ifAbsent: anExceptionBlock	self shouldNotImplement! !

!MatrixPharo14CLayer methodsFor: 'removing' stamp: 'GermanArduino 11/27/2012 16:33'!
removeAll	self shouldNotImplement! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
replaceAll: oldObject with: newObject	contents replaceAll: oldObject with: newObject! !

!MatrixPharo14CLayer methodsFor: 'private' stamp: 'GermanArduino 11/27/2012 16:33'!
rowAndColumnForIndex: index	|t|	t := index - 1.	^(t // ncols + 1)@(t \\ ncols + 1)! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
rowCount	^nrows! !

!MatrixPharo14CLayer methodsFor: 'private' stamp: 'GermanArduino 11/27/2012 16:33'!
rows: rows columns: columns contents: anArray	[rows isInteger and: [rows >= 0]] assert.	[columns isInteger and: [columns >= 0]] assert.	[rows * columns = anArray size] assert.	nrows := rows.	ncols := columns.	contents := anArray.	^self! !

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
select: aBlock	self shouldNotImplement! !

!MatrixPharo14CLayer methodsFor: 'copying' stamp: 'GermanArduino 11/27/2012 16:33'!
shuffled	^self class rows: nrows columns: ncols contents: (contents shuffled)! !

!MatrixPharo14CLayer methodsFor: 'copying' stamp: 'GermanArduino 11/27/2012 16:33'!
shuffledBy: aRandom	^self class rows: nrows columns: ncols contents: (contents shuffledBy: aRandom)! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
size	^contents size! !

!MatrixPharo14CLayer methodsFor: 'printing' stamp: 'GermanArduino 11/27/2012 16:33'!
storeOn: aStream	aStream nextPut: $(; nextPutAll: self class name;		nextPutAll: ' rows: '; store: nrows;		nextPutAll: ' columns: '; store: ncols;		nextPutAll: ' contents: '; store: contents;		nextPut: $)! !

!MatrixPharo14CLayer methodsFor: 'accessing' stamp: 'GermanArduino 11/27/2012 16:33'!
swap: r1 at: c1 with: r2 at: c2	contents swap: (self indexForRow: r1 andColumn: c1)			 with: (self indexForRow: r2 andColumn: c2)! !

!MatrixPharo14CLayer methodsFor: 'accessing rows/columns' stamp: 'GermanArduino 11/27/2012 16:33'!
swapColumn: anIndex withColumn: anotherIndex	|a b|	a := self indexForRow: 1 andColumn: anIndex.	b := self indexForRow: 1 andColumn: anotherIndex.	nrows timesRepeat: [		contents swap: a with: b.		a := a + ncols.		b := b + ncols].! !

!MatrixPharo14CLayer methodsFor: 'accessing rows/columns' stamp: 'GermanArduino 11/27/2012 16:33'!
swapRow: anIndex withRow: anotherIndex	|a b|	a := self indexForRow: anIndex andColumn: 1.	b := self indexForRow: anotherIndex andColumn: 1.	ncols timesRepeat: [		contents swap: a with: b.		a := a + 1.		b := b + 1].! !

!MatrixPharo14CLayer methodsFor: 'accessing rows/columns' stamp: 'GermanArduino 11/27/2012 16:33'!
transposed	[nrows = ncols] assert.	^self indicesCollect: [:row :column | self at: column at: row]! !

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
with: aCollection collect: aBlock	"aCollection must support #at:at: and be at least as large as the receiver."	^self withIndicesCollect: [:each :row :column |		aBlock value: each value: (aCollection at: row at: column)]! !

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
with: aCollection do: aBlock	"aCollection must support #at:at: and be at least as large as the receiver."	self withIndicesDo: [:each :row :column |		aBlock value: each value: (aCollection at: row at: column)].! !

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
with: aCollection inject: startingValue into: aBlock	"aCollection must support #at:at: and be at least as large as the receiver."	^self withIndicesInject: startingValue into: [:value :each :row :column |		aBlock value: value value: each value: (aCollection at: row at: column)]! !

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
withIndicesCollect: aBlock	|i r|	i := 0.	r := contents shallowCopy.	1 to: nrows do: [:row |		1 to: ncols do: [:column |			i := i+1.			r at: i put: (aBlock value: (r at: i) value: row value: column)]].	^self class rows: nrows columns: ncols contents: r! !

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
withIndicesDo: aBlock	|i|	i := 0.	1 to: nrows do: [:row |		1 to: ncols do: [:column |			aBlock value: (contents at: (i := i+1)) value: row value: column]].! !

!MatrixPharo14CLayer methodsFor: 'enumerating' stamp: 'GermanArduino 11/27/2012 16:33'!
withIndicesInject: start into: aBlock	|i current|	i := 0.	current := start.	1 to: nrows do: [:row |		1 to: ncols do: [:column |			current := aBlock value: current value: (contents at: (i := i+1)) 							  value: row value: column]].	^current! !

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
column: aCollection	"Should this be called #fromColumn:?"	^self rows: aCollection size columns: 1 contents: aCollection asArray shallowCopy! !

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
diagonal: aCollection	|r i|	r := self zeros: aCollection size.	i := 0.	aCollection do: [:each | i := i+1. r at: i at: i put: each].	^r! !

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
identity: n	|r|	r := self zeros: n.	1 to: n do: [:i | r at: i at: i put: 1].	^r! !

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
new: dim	"Answer a dim*dim matrix.  Is this an abuse of #new:?  The argument is NOT a size."	^self rows: dim columns: dim! !

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
new: dim element: element	"Answer a dim*dim matrix with all elements set to element.	 Is this an abuse of #new:?  The argument is NOT a size."	^self rows: dim columns: dim element: element! !

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
new: dim tabulate: aBlock	"Answer a dim*dim matrix where it at: i at: j is aBlock value: i value: j."	^self rows: dim columns: dim tabulate: aBlock! !

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
ones: n	^self new: n element: 1! !

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
row: aCollection	"Should this be called #fromRow:?"	^self rows: 1 columns: aCollection size contents: aCollection asArray shallowCopy! !

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
rows: rows columns: columns	^self rows: rows columns: columns contents: (Array new: rows*columns)! !

!MatrixPharo14CLayer class methodsFor: 'private' stamp: 'GermanArduino 11/27/2012 16:33'!
rows: rows columns: columns contents: contents	^self new rows: rows columns: columns contents: contents! !

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
rows: rows columns: columns element: element	^self rows: rows columns: columns		contents: ((Array new: rows*columns) atAllPut: element; yourself)! !

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
rows: rows columns: columns tabulate: aBlock	"Answer a new Matrix of the given dimensions where	 result at: i at: j     is   aBlock value: i value: j"	|a i|	a := Array new: rows*columns.	i := 0.	1 to: rows do: [:row |		1 to: columns do: [:column |			a at: (i := i+1) put: (aBlock value: row value: column)]].	^self rows: rows columns: columns contents: a! !

!MatrixPharo14CLayer class methodsFor: 'instance creation' stamp: 'GermanArduino 11/27/2012 16:33'!
zeros: n	^self new: n element: 0! !

!Symbol methodsFor: '*CuisSandstoneDB-Pharo14CLayer' stamp: 'gsa 11/23/2012 10:51'!
value: anObject 
	^anObject perform: self.! !
