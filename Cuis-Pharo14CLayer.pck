'From Cuis 4.0 of 21 April 2012 [latest update: #1271] on 13 May 2012 at 10:47:35 am'!
'Description Please enter a description for this package '!

!Character methodsFor: '*Cuis-Pharo14CLayer' stamp: 'gsa 5/13/2012 10:42'!
charCode

	^ (value bitAnd: 16r3FFFFF).
! !

!Character class methodsFor: '*Cuis-Pharo14CLayer' stamp: 'gsa 5/13/2012 10:44'!
codePoint: anInteger 
	"Just for ANSI Compliance"	
	^self value: anInteger
	! !

!ClassDescription methodsFor: '*Cuis-Pharo14CLayer' stamp: 'gsa 5/13/2012 10:26'!
classSide
	^self theMetaClass! !
