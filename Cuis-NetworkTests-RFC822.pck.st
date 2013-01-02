'From Cuis 4.1 of 12 December 2012 [latest update: #1528] on 2 January 2013 at 8:27:14 am'!
'Description Please enter a description for this package '!
!classDefinition: #MailAddressParserTest category: #'Cuis-NetworkTests-RFC822'!
TestCase subclass: #MailAddressParserTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-NetworkTests-RFC822'!
!classDefinition: 'MailAddressParserTest class' category: #'Cuis-NetworkTests-RFC822'!
MailAddressParserTest class
	instanceVariableNames: ''!


!MailAddressParserTest commentStamp: '<historical>' prior: 0!
This is the unit test for the class MailAddressParser. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 	- http://www.c2.com/cgi/wiki?UnitTest	- there is a chapter in the PharoByExample book (http://pharobyexample.org)	- the sunit class category!

!MailAddressParserTest methodsFor: 'tests' stamp: 'sd 6/5/2005 10:31'!
testAddressesIn	| testString correctAnswer |	testString := 'joe@lama.com, joe2@lama.com joe3@lama.com joe4 , Not an Address <joe5@address>, joe.(annoying (nested) comment)literal@[1.2.3.4], "an annoying" group : joe1@groupie, joe2@groupie, "Joey" joe3@groupy, "joe6"."joe8"@group.com;,  Lex''s email account <lex>'.correctAnswer := #('joe@lama.com' 'joe2@lama.com' 'joe3@lama.com' 'joe4' 'joe5@address' 'joe.literal@[1.2.3.4]' 'joe1@groupie' 'joe2@groupie' '"Joey"' 'joe3@groupy' '"joe6"."joe8"@group.com' 'lex') asOrderedCollection.	self assert: ((MailAddressParser addressesIn: testString) =  correctAnswer).! !
