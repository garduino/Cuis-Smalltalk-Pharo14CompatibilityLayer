'From Cuis 4.1 of 12 December 2012 [latest update: #1578] on 25 January 2013 at 8:36:44 am'!
'Description Please enter a description for this package '!

!Character methodsFor: '*Cuis-Pharo14CLayer' stamp: 'gsa 5/13/2012 10:42'!
charCode

	^ (value bitAnd: 16r3FFFFF).
! !

!Character class methodsFor: '*Cuis-Pharo14CLayer' stamp: 'gsa 1/25/2013 08:35'!
codePoint: anInteger 
	"Just for ANSI Compliance"	
	^self value: anInteger
	! !

!ClassDescription methodsFor: '*Cuis-Pharo14CLayer' stamp: 'gsa 5/13/2012 10:26'!
classSide
	^self theMetaClass! !

!Matrix methodsFor: '*Cuis-Pharo14CLayer' stamp: 'gsa 11/23/2012 12:04'!
at: row at: column
	^contents at: (self indexForRow: row andColumn: column)! !

!Matrix methodsFor: '*Cuis-Pharo14CLayer' stamp: 'gsa 11/23/2012 12:03'!
at: row at: column put: value
	^contents at: (self indexForRow: row andColumn: column) put: value! !

!Matrix methodsFor: '*Cuis-Pharo14CLayer' stamp: 'gsa 11/23/2012 12:02'!
indexForRow: row andColumn: column
	(row between: 1 and: nrows)
		ifFalse: [self error: '1st subscript out of range'].
	(column between: 1 and: ncols)
		ifFalse: [self error: '2nd subscript out of range'].
	^(row-1) * ncols + column! !

!Matrix methodsFor: '*Cuis-Pharo14CLayer' stamp: 'gsa 11/23/2012 11:58'!
rows: rows columns: columns contents: anArray
	[rows isInteger and: [rows >= 0]] assert.
	[columns isInteger and: [columns >= 0]] assert.
	[rows * columns = anArray size] assert.
	nrows := rows.
	ncols := columns.
	contents := anArray.
	^self! !

!Matrix class methodsFor: '*Cuis-Pharo14CLayer' stamp: 'gsa 11/23/2012 11:21'!
rows: rows columns: columns
	^self rows: rows columns: columns contents: (Array new: rows*columns)! !

!Matrix class methodsFor: '*Cuis-Pharo14CLayer' stamp: 'gsa 11/23/2012 11:20'!
rows: rows columns: columns contents: contents
	^self new rows: rows columns: columns contents: contents! !
