'From Cuis 4.0 of 21 April 2012 [latest update: #1270] on 20 May 2012 at 5:42:16 pm'!
'Description Please enter a description for this package '!
!classDefinition: #LanguageEnvironment category: #'Cuis-Multilingual-Languages'!
Object subclass: #LanguageEnvironment
	instanceVariableNames: 'id'
	classVariableNames: 'ClipboardInterpreterClass Current FileNameConverterClass InputInterpreterClass KnownEnvironments SystemConverterClass'
	poolDictionaries: ''
	category: 'Cuis-Multilingual-Languages'!
!classDefinition: 'LanguageEnvironment class' category: #'Cuis-Multilingual-Languages'!
LanguageEnvironment class
	instanceVariableNames: ''!


!LanguageEnvironment commentStamp: '<historical>' prior: 0!
The name multilingualized Squeak suggests that you can use multiple language at one time.  This is true, of course, but the system still how to manage the primary language; that provides the interpretation of data going out or coming in from outside world. It also provides how to render strings, as there rendering rule could be different in one language to another, even if the code points in a string is the same.  Originally, LanguageEnvironment and its subclasses only has class side methods.  After merged with Diego's Babel work, it now has instance side methods.  Since this historical reason, the class side and instance side are not related well.  When we talk about the interface with the outside of the Squeak world, there are three different "channels"; the keyboard input, clipboard output and input, and filename.  On a not-to-uncommon system such as a Unix system localized to Japan, all of these three can have (and does have) different encodings.  So we need to manage them separately.  Note that the encoding in a file can be anything.  While it is nice to provide a suggested guess for this 'default system file content encoding', it is not critical.  Rendering support is limited basic L-to-R rendering so far.  But you can provide different line-wrap rule, at least.!

!LanguageEnvironment methodsFor: 'utilities' stamp: 'mir 7/21/2004 18:05'!
checkPhrase: phrase translation: translation	"check the translation.	Answer a string with a comment or meaning no-comments"	^nil! !

!LanguageEnvironment methodsFor: 'accessing' stamp: 'tak 8/4/2005 11:02'!
fontEncodingName	^ #Font , self class name! !

!LanguageEnvironment methodsFor: 'fonts support' stamp: 'tak 8/5/2005 20:46'!
fontFileName	"(Locale isoLanguage: 'ja') languageEnvironment fontFileName"	^ self fontEncodingName , '.sar'! !

!LanguageEnvironment methodsFor: 'fonts support' stamp: 'StephaneDucasse 3/27/2010 21:59'!
fontFullName	"(Locale isoLanguage: 'ja') languageEnvironment fontFullName"	^ FileDirectory default fullNameFor: self fontFileName! !

!LanguageEnvironment methodsFor: 'fonts support' stamp: 'adrian_lienhard 7/18/2009 15:28'!
isFontAvailable	| encoding f |	encoding := self leadingChar + 1.	f := TextStyle defaultFont.	f isFontSet ifTrue: [		f fontArray			at: encoding			ifAbsent: [^ false].		^ true	].	^ encoding = 1! !

!LanguageEnvironment methodsFor: 'accessing' stamp: 'mir 7/15/2004 15:32'!
isoCountry	^self localeID isoCountry! !

!LanguageEnvironment methodsFor: 'accessing' stamp: 'mir 7/15/2004 15:32'!
isoLanguage	^self localeID isoLanguage! !

!LanguageEnvironment methodsFor: 'accessing' stamp: 'mir 7/15/2004 18:55'!
leadingChar	^self class leadingChar! !

!LanguageEnvironment methodsFor: 'accessing' stamp: 'mir 7/15/2004 15:31'!
localeID	^id! !

!LanguageEnvironment methodsFor: 'initialization' stamp: 'mir 7/15/2004 15:31'!
localeID: anID	id := anID! !

!LanguageEnvironment methodsFor: 'fonts support' stamp: 'tak 8/4/2005 11:08'!
removeFonts	"(Locale isoLanguage: 'ja') languageEnvironment removeFonts"	StrikeFontSet removeFontsForEncoding: self leadingChar encodingName: self fontEncodingName! !

!LanguageEnvironment class methodsFor: 'accessing' stamp: 'yo 3/17/2004 15:24'!
canBeGlobalVarInitial: char	^ Unicode canBeGlobalVarInitial: char.! !

!LanguageEnvironment class methodsFor: 'accessing' stamp: 'yo 3/17/2004 15:24'!
canBeNonGlobalVarInitial: char	^ Unicode canBeNonGlobalVarInitial: char.! !

!LanguageEnvironment class methodsFor: 'initialization' stamp: 'yo 3/15/2004 21:15'!
clearDefault	ClipboardInterpreterClass := nil.	InputInterpreterClass := nil.	SystemConverterClass := nil.	FileNameConverterClass := nil.! !

!LanguageEnvironment class methodsFor: 'accessing' stamp: 'yo 7/28/2004 21:34'!
currentPlatform	^ Locale currentPlatform languageEnvironment.! !

!LanguageEnvironment class methodsFor: 'public query' stamp: 'StephaneDucasse 8/3/2010 22:48'!
defaultEncodingName	^ 'mac-roman' copy! !

!LanguageEnvironment class methodsFor: 'public query' stamp: 'yo 7/28/2004 21:56'!
defaultFileNameConverter	FileNameConverterClass		ifNil: [FileNameConverterClass := self currentPlatform class fileNameConverterClass].	^ FileNameConverterClass new! !

!LanguageEnvironment class methodsFor: 'public query' stamp: 'yo 7/28/2004 21:56'!
defaultSystemConverter	SystemConverterClass ifNil: [SystemConverterClass := self currentPlatform class systemConverterClass].	^ SystemConverterClass new.! !

!LanguageEnvironment class methodsFor: 'accessing' stamp: 'StephaneDucasse 2/13/2010 12:14'!
digitValueOf: char	"Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and < 0	otherwise. This is used to parse literal numbers of radix 2-36."	^ Unicode digitValueOf: char.! !

!LanguageEnvironment class methodsFor: 'subclass responsibilities' stamp: 'michael.rueger 2/5/2009 17:23'!
fileNameConverterClass	^UTF8TextConverter! !

!LanguageEnvironment class methodsFor: 'private' stamp: 'nice 1/5/2010 15:59'!
initKnownEnvironments	"LanguageEnvironment initKnownEnvironments"	| known |	known := Dictionary new.	self allSubclassesDo: [:subClass | 		subClass supportedLanguages do: [:language | | env id | 			env := subClass new.			id := LocaleID isoString: language.			env localeID: id.			known at: id put: env]].	^known! !

!LanguageEnvironment class methodsFor: 'initialization' stamp: 'mir 7/15/2004 15:54'!
initialize	"LanguageEnvironment initialize"	Smalltalk addToStartUpList: LanguageEnvironment after: FileDirectory.	Smalltalk addToStartUpList: FileDirectory after: LanguageEnvironment.! !

!LanguageEnvironment class methodsFor: 'rendering support' stamp: 'yo 3/17/2004 21:54'!
isBreakableAt: index in: text	| char |	char := text at: index.	char = Character space ifTrue: [^ true].	char = Character cr ifTrue: [^ true].	^ false.! !

!LanguageEnvironment class methodsFor: 'accessing' stamp: 'yo 12/2/2004 16:13'!
isCharset	^ false.! !

!LanguageEnvironment class methodsFor: 'accessing' stamp: 'yo 3/17/2004 15:24'!
isDigit: char	^ Unicode isDigit: char.! !

!LanguageEnvironment class methodsFor: 'accessing' stamp: 'yo 3/17/2004 15:25'!
isLetter: char	^ Unicode isLetter: char.! !

!LanguageEnvironment class methodsFor: 'accessing' stamp: 'yo 3/17/2004 15:25'!
isLowercase: char	^ Unicode isLowercase: char.! !

!LanguageEnvironment class methodsFor: 'accessing' stamp: 'yo 3/17/2004 15:25'!
isUppercase: char	^ Unicode isUppercase: char.! !

!LanguageEnvironment class methodsFor: 'private' stamp: 'mir 7/15/2004 15:45'!
knownEnvironments	"LanguageEnvironment knownEnvironments"	"KnownEnvironments := nil"	^KnownEnvironments ifNil: [KnownEnvironments := self initKnownEnvironments]! !

!LanguageEnvironment class methodsFor: 'subclass responsibilities' stamp: 'yo 3/17/2004 15:11'!
leadingChar	self subclassResponsibility.	^ 0.! !

!LanguageEnvironment class methodsFor: 'class initialization' stamp: 'mir 7/15/2004 16:13'!
localeChanged	self startUp! !

!LanguageEnvironment class methodsFor: 'accessing' stamp: 'michael.rueger 3/15/2009 11:45'!
localeID: localeID	self knownEnvironments at: localeID ifPresent: [:value | ^value].	^self knownEnvironments		at: (LocaleID isoLanguage: localeID isoLanguage)		ifAbsent: [self localeID: (LocaleID isoLanguage: 'en')]! !

!LanguageEnvironment class methodsFor: 'initialization' stamp: 'mir 7/21/2004 19:10'!
resetKnownEnvironments	"LanguageEnvironment resetKnownEnvironments"	KnownEnvironments := nil! !

!LanguageEnvironment class methodsFor: 'language methods' stamp: 'yo 1/18/2005 15:56'!
scanSelector	^ #scanMultiCharactersFrom:to:in:rightX:stopConditions:kern:! !

!LanguageEnvironment class methodsFor: 'private'!
setUsePangoFlag	^ self! !

!LanguageEnvironment class methodsFor: 'class initialization' stamp: 'michael.rueger 3/2/2009 11:06'!
startUp	self clearDefault.	self setUsePangoFlag.! !

!LanguageEnvironment class methodsFor: 'subclass responsibilities' stamp: 'mir 7/1/2004 17:59'!
supportedLanguages	"Return the languages that this class supports. 	Any translations for those languages will use this class as their environment."	self subclassResponsibility! !

!LanguageEnvironment class methodsFor: 'subclass responsibilities' stamp: 'yo 3/17/2004 15:10'!
systemConverterClass	self subclassResponsibility.	^ Latin1TextConverter.! !
LanguageEnvironment initialize!
