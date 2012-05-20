'From Cuis 4.0 of 21 April 2012 [latest update: #1288] on 20 May 2012 at 6:19:35 pm'!
'Description Please enter a description for this package '!
!classDefinition: #EncodedCharSet category: #'Cuis-Multilingual-Encodings'!
Object subclass: #EncodedCharSet
	instanceVariableNames: ''
	classVariableNames: 'EncodedCharSets'
	poolDictionaries: ''
	category: 'Cuis-Multilingual-Encodings'!
!classDefinition: 'EncodedCharSet class' category: #'Cuis-Multilingual-Encodings'!
EncodedCharSet class
	instanceVariableNames: 'compoundTextSequence'!

!classDefinition: #Unicode category: #'Cuis-Multilingual-Encodings'!
EncodedCharSet subclass: #Unicode
	instanceVariableNames: ''
	classVariableNames: 'Cc Cf Cn Co Cs DecimalProperty GeneralCategory Ll Lm Lo Lt Lu Mc Me Mn Nd Nl No Pc Pd Pe Pf Pi Po Ps Sc Sk Sm So ToCasefold ToLower ToUpper Zl Zp Zs'
	poolDictionaries: ''
	category: 'Cuis-Multilingual-Encodings'!
!classDefinition: 'Unicode class' category: #'Cuis-Multilingual-Encodings'!
Unicode class
	instanceVariableNames: ''!


!EncodedCharSet commentStamp: 'yo 10/19/2004 19:08' prior: 0!
An abstract superclasss of the classes that represent encoded character sets.  In the old implementation, the charsets had more important role.  However, in the current implementation, the subclasses are used only for keeping the backward compatibility.	The other confusion comes from the name of "Latin1" class.  It used to mean the Latin-1 (ISO-8859-1) character set, but now it primarily means that the "Western European languages that are covered by the characters in Latin-1 character set.!

!Unicode commentStamp: 'yo 10/19/2004 20:44' prior: 0!
This class holds the entry points for the utility functions around characters.!

!Character class methodsFor: '*Cuis-Multilingual-Encodings' stamp: 'gsa 5/20/2012 18:16'!
leadingChar: leadChar code: code

	code >= 16r400000 ifTrue: [
		self error: 'code is out of range'.
	].
	leadChar >= 256 ifTrue: [
		self error: 'lead is out of range'.
	].
	code < 256 ifTrue: [ ^self value: code ].
	^self value: (leadChar bitShift: 22) + code.! !

!EncodedCharSet class methodsFor: 'character classification' stamp: 'yo 8/5/2003 16:55'!
canBeGlobalVarInitial: char	| leadingChar |	leadingChar := char leadingChar.	leadingChar = 0 ifTrue: [^ self isUppercase: char].	^ self isLetter: char.! !

!EncodedCharSet class methodsFor: 'character classification' stamp: 'yo 8/5/2003 17:18'!
canBeNonGlobalVarInitial: char	| leadingChar |	leadingChar := char leadingChar.	leadingChar = 0 ifTrue: [^ self isLowercase: char].	^ self isLetter: char.! !

!EncodedCharSet class methodsFor: 'class methods' stamp: 'tak 11/5/2005 18:14'!
charFromUnicode: unicode	| table index |	unicode < 128 ifTrue: [^ Character value: unicode].	table := self ucsTable.	index := table indexOf: unicode.	index = 0 ifTrue: [		^ nil.	].	^ Character leadingChar: self leadingChar code: index - 1.! !

!EncodedCharSet class methodsFor: 'class methods' stamp: 'yo 9/4/2002 22:57'!
charsetAt: encoding	^ EncodedCharSets at: encoding + 1 ifAbsent: [EncodedCharSets at: 1].! !

!EncodedCharSet class methodsFor: 'class methods' stamp: 'StephaneDucasse 2/13/2010 16:02'!
digitValueOf: char	"Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and < 0 	otherwise. This is used to parse literal numbers of radix 2-36."	| value |	value := char charCode.	value <= $9 asciiValue 		ifTrue: [^value - $0 asciiValue].	value >= $A asciiValue 		ifTrue: [ value <= $Z asciiValue ifTrue: [^value - $A asciiValue + 10].				(value >= $a asciiValue and: [value <= $z asciiValue])					 ifTrue: [^value - $a asciiValue + 10]].	^ -1! !

!EncodedCharSet class methodsFor: 'class methods' stamp: 'pmm 8/16/2010 21:34'!
initialize"	self initialize"	self allSubclassesDo: [:each | each initialize].	EncodedCharSets := Array new: 256.	EncodedCharSets at: 0+1 put: Unicode "Latin1Environment".	EncodedCharSets at: 1+1 put: JISX0208.	EncodedCharSets at: 2+1 put: GB2312.	EncodedCharSets at: 3+1 put: KSX1001.	EncodedCharSets at: 4+1 put: JISX0208.	EncodedCharSets at: 5+1 put: JapaneseEnvironment.	EncodedCharSets at: 6+1 put: SimplifiedChineseEnvironment.	EncodedCharSets at: 7+1 put: KoreanEnvironment.	EncodedCharSets at: 8+1 put: GB2312.	"EncodedCharSets at: 9+1 put: UnicodeTraditionalChinese."	"EncodedCharSets at: 10+1 put: UnicodeVietnamese."	EncodedCharSets at: 12+1 put: KSX1001.	EncodedCharSets at: 13+1 put: GreekEnvironment.	EncodedCharSets at: 14+1 put: Latin2Environment.	EncodedCharSets at: 15+1 put: RussianEnvironment.	EncodedCharSets at: 16+1 put: NepaleseEnvironment.	EncodedCharSets at: 17+1 put: Latin9Environment.	EncodedCharSets at: 256 put: Unicode.! !

!EncodedCharSet class methodsFor: 'accessing - displaying' stamp: 'yo 12/18/2002 12:34'!
isBreakableAt: index in: text	self subclassResponsibility.! !

!EncodedCharSet class methodsFor: 'class methods' stamp: 'yo 12/2/2004 16:13'!
isCharset	^ true.! !

!EncodedCharSet class methodsFor: 'character classification' stamp: 'yo 8/5/2003 16:44'!
isDigit: char	"Answer whether the receiver is a digit."	| value |	value := char asciiValue.	^ value >= 48 and: [value <= 57].! !

!EncodedCharSet class methodsFor: 'character classification' stamp: 'yo 8/5/2003 16:40'!
isLetter: char	"Answer whether the receiver is a letter."	| value |	value := char asciiValue.	^ (8r141 <= value and: [value <= 8r172]) or: [8r101 <= value and: [value <= 8r132]].! !

!EncodedCharSet class methodsFor: 'character classification' stamp: 'yo 8/5/2003 16:40'!
isLowercase: char	"Answer whether the receiver is a lowercase letter.	(The old implementation answered whether the receiver is not an uppercase letter.)"	| value |	value := char asciiValue.	^ 8r141 <= value and: [value <= 8r172].! !

!EncodedCharSet class methodsFor: 'character classification' stamp: 'yo 8/5/2003 16:44'!
isUppercase: char	"Answer whether the receiver is an uppercase letter.	(The old implementation answered whether the receiver is not a lowercase letter.)"	| value |	value := char asciiValue.	^ 8r101 <= value and: [value <= 8r132].! !

!EncodedCharSet class methodsFor: 'class methods' stamp: 'yo 9/2/2002 16:32'!
leadingChar	self subclassResponsibility.! !

!EncodedCharSet class methodsFor: 'class methods' stamp: 'yo 11/4/2002 14:43'!
nextPutValue: ascii toStream: aStream withShiftSequenceIfNeededForTextConverterState: state	self subclassResponsibility.! !

!EncodedCharSet class methodsFor: 'accessing - displaying' stamp: 'yo 9/4/2002 22:51'!
printingDirection	self subclassResponsibility.! !

!EncodedCharSet class methodsFor: 'accessing - displaying' stamp: 'sn 7/31/2009 15:01'!
scanSelector	^ #scanMultiCharactersFrom:to:in:rightX:stopConditions:kern:! !

!EncodedCharSet class methodsFor: 'class methods' stamp: 'yo 10/14/2003 10:19'!
ucsTable	^ UCSTable latin1Table.! !

!Unicode class methodsFor: 'comments' stamp: 'yo 12/23/2002 13:04'!
blocks320Comment"# Blocks-3.2.0.txt# Correlated with Unicode 3.2# Start Code..End Code; Block Name0000..007F; Basic Latin0080..00FF; Latin-1 Supplement0100..017F; Latin Extended-A0180..024F; Latin Extended-B0250..02AF; IPA Extensions02B0..02FF; Spacing Modifier Letters0300..036F; Combining Diacritical Marks0370..03FF; Greek and Coptic0400..04FF; Cyrillic0500..052F; Cyrillic Supplementary0530..058F; Armenian0590..05FF; Hebrew0600..06FF; Arabic0700..074F; Syriac0780..07BF; Thaana0900..097F; Devanagari0980..09FF; Bengali0A00..0A7F; Gurmukhi0A80..0AFF; Gujarati0B00..0B7F; Oriya0B80..0BFF; Tamil0C00..0C7F; Telugu0C80..0CFF; Kannada0D00..0D7F; Malayalam0D80..0DFF; Sinhala0E00..0E7F; Thai0E80..0EFF; Lao0F00..0FFF; Tibetan1000..109F; Myanmar10A0..10FF; Georgian1100..11FF; Hangul Jamo1200..137F; Ethiopic13A0..13FF; Cherokee1400..167F; Unified Canadian Aboriginal Syllabics1680..169F; Ogham16A0..16FF; Runic1700..171F; Tagalog1720..173F; Hanunoo1740..175F; Buhid1760..177F; Tagbanwa1780..17FF; Khmer1800..18AF; Mongolian1E00..1EFF; Latin Extended Additional1F00..1FFF; Greek Extended2000..206F; General Punctuation2070..209F; Superscripts and Subscripts20A0..20CF; Currency Symbols20D0..20FF; Combining Diacritical Marks for Symbols2100..214F; Letterlike Symbols2150..218F; Number Forms2190..21FF; Arrows2200..22FF; Mathematical Operators2300..23FF; Miscellaneous Technical2400..243F; Control Pictures2440..245F; Optical Character Recognition2460..24FF; Enclosed Alphanumerics2500..257F; Box Drawing2580..259F; Block Elements25A0..25FF; Geometric Shapes2600..26FF; Miscellaneous Symbols2700..27BF; Dingbats27C0..27EF; Miscellaneous Mathematical Symbols-A27F0..27FF; Supplemental Arrows-A2800..28FF; Braille Patterns2900..297F; Supplemental Arrows-B2980..29FF; Miscellaneous Mathematical Symbols-B2A00..2AFF; Supplemental Mathematical Operators2E80..2EFF; CJK Radicals Supplement2F00..2FDF; Kangxi Radicals2FF0..2FFF; Ideographic Description Characters3000..303F; CJK Symbols and Punctuation3040..309F; Hiragana30A0..30FF; Katakana3100..312F; Bopomofo3130..318F; Hangul Compatibility Jamo3190..319F; Kanbun31A0..31BF; Bopomofo Extended31F0..31FF; Katakana Phonetic Extensions3200..32FF; Enclosed CJK Letters and Months3300..33FF; CJK Compatibility3400..4DBF; CJK Unified Ideographs Extension A4E00..9FFF; CJK Unified IdeographsA000..A48F; Yi SyllablesA490..A4CF; Yi RadicalsAC00..D7AF; Hangul SyllablesD800..DB7F; High SurrogatesDB80..DBFF; High Private Use SurrogatesDC00..DFFF; Low SurrogatesE000..F8FF; Private Use AreaF900..FAFF; CJK Compatibility IdeographsFB00..FB4F; Alphabetic Presentation FormsFB50..FDFF; Arabic Presentation Forms-AFE00..FE0F; Variation SelectorsFE20..FE2F; Combining Half MarksFE30..FE4F; CJK Compatibility FormsFE50..FE6F; Small Form VariantsFE70..FEFF; Arabic Presentation Forms-BFF00..FFEF; Halfwidth and Fullwidth FormsFFF0..FFFF; Specials10300..1032F; Old Italic10330..1034F; Gothic10400..1044F; Deseret1D000..1D0FF; Byzantine Musical Symbols1D100..1D1FF; Musical Symbols1D400..1D7FF; Mathematical Alphanumeric Symbols20000..2A6DF; CJK Unified Ideographs Extension B2F800..2FA1F; CJK Compatibility Ideographs SupplementE0000..E007F; TagsF0000..FFFFF; Supplementary Private Use Area-A100000..10FFFF; Supplementary Private Use Area-B"! !

!Unicode class methodsFor: 'comments' stamp: 'yo 3/17/2004 23:38'!
blocks320Comment2"# Blocks-3.2.0.txt# Correlated with Unicode 3.2# Start Code..End Code; Block Name0000..007F; Basic Latin0080..00FF; Latin-1 Supplement => Latin 10100..017F; Latin Extended-A0180..024F; Latin Extended-B0250..02AF; IPA Extensions  => LatinExtended102B0..02FF; Spacing Modifier Letters0300..036F; Combining Diacritical Marks  => Modifiers0370..03FF; Greek and Coptic0400..04FF; Cyrillic0500..052F; Cyrillic Supplementary0530..058F; Armenian   => EuropeanAlphabetic10590..05FF; Hebrew0600..06FF; Arabic0700..074F; Syriac0780..07BF; Thaana   => MiddleEastern0900..097F; Devanagari0980..09FF; Bengali0A00..0A7F; Gurmukhi0A80..0AFF; Gujarati0B00..0B7F; Oriya0B80..0BFF; Tamil0C00..0C7F; Telugu0C80..0CFF; Kannada0D00..0D7F; Malayalam0D80..0DFF; Sinhala  => South Asian10E00..0E7F; Thai0E80..0EFF; Lao => Southeastern 10F00..0FFF; Tibetan  => South Asian11000..109F; Myanmar => Southeastern 110A0..10FF; Georgian   => European Alphabetic 21100..11FF; Hangul Jamo   => Korean1200..137F; Ethiopic13A0..13FF; Cherokee1400..167F; Unified Canadian Aboriginal Syllabics  => Additional11680..169F; Ogham16A0..16FF; Runic  => European Alphabetic 31700..171F; Tagalog1720..173F; Hanunoo1740..175F; Buhid1760..177F; Tagbanwa1780..17FF; Khmer  => Southeastern21800..18AF; Mongolian  => Additional21E00..1EFF; Latin Extended Additional1F00..1FFF; Greek Extended  => EuropeanAlphabetic42000..206F; General Punctuation2070..209F; Superscripts and Subscripts20A0..20CF; Currency Symbols20D0..20FF; Combining Diacritical Marks for Symbols2100..214F; Letterlike Symbols2150..218F; Number Forms2190..21FF; Arrows2200..22FF; Mathematical Operators2300..23FF; Miscellaneous Technical2400..243F; Control Pictures2440..245F; Optical Character Recognition2460..24FF; Enclosed Alphanumerics2500..257F; Box Drawing2580..259F; Block Elements25A0..25FF; Geometric Shapes2600..26FF; Miscellaneous Symbols2700..27BF; Dingbats27C0..27EF; Miscellaneous Mathematical Symbols-A27F0..27FF; Supplemental Arrows-A2800..28FF; Braille Patterns2900..297F; Supplemental Arrows-B2980..29FF; Miscellaneous Mathematical Symbols-B2A00..2AFF; Supplemental Mathematical Operators  => Symbols22E80..2EFF; CJK Radicals Supplement2F00..2FDF; Kangxi Radicals2FF0..2FFF; Ideographic Description Characters3000..303F; CJK Symbols and Punctuation3040..309F; Hiragana30A0..30FF; Katakana3100..312F; Bopomofo3130..318F; Hangul Compatibility Jamo3190..319F; Kanbun31A0..31BF; Bopomofo Extended31F0..31FF; Katakana Phonetic Extensions3200..32FF; Enclosed CJK Letters and Months3300..33FF; CJK Compatibility3400..4DBF; CJK Unified Ideographs Extension A4E00..9FFF; CJK Unified IdeographsA000..A48F; Yi SyllablesA490..A4CF; Yi Radicals  => CJKAC00..D7AF; Hangul Syllables  => KoreanD800..DB7F; High SurrogatesDB80..DBFF; High Private Use SurrogatesDC00..DFFF; Low SurrogatesE000..F8FF; Private Use AreaF900..FAFF; CJK Compatibility Ideographs  => CJKFB00..FB4F; Alphabetic Presentation FormsFB50..FDFF; Arabic Presentation Forms-A  => Middle Eastern 2FE00..FE0F; Variation SelectorsFE20..FE2F; Combining Half MarksFE30..FE4F; CJK Compatibility Forms  => CJKFE50..FE6F; Small Form Variants => Symbol3FE70..FEFF; Arabic Presentation Forms-B  => Middle Eastern 3FF00..FFEF; Halfwidth and Fullwidth FormsFFF0..FFFF; Specials  => Specials10300..1032F; Old Italic10330..1034F; Gothic10400..1044F; Deseret   => European1D000..1D0FF; Byzantine Musical Symbols1D100..1D1FF; Musical Symbols1D400..1D7FF; Mathematical Alphanumeric Symbols  => Symbols20000..2A6DF; CJK Unified Ideographs Extension B2F800..2FA1F; CJK Compatibility Ideographs Supplement  => CJKE0000..E007F; TagsF0000..FFFFF; Supplementary Private Use Area-A100000..10FFFF; Supplementary Private Use Area-B  => Special"! !

!Unicode class methodsFor: 'instance creation' stamp: 'nice 5/1/2011 19:25'!
charFromUnicode: uniCode	^ Character value: uniCode! !

!Unicode class methodsFor: 'class methods' stamp: 'yo 12/24/2002 07:49'!
compoundTextFinalChar	self shouldNotImplement.! !

!Unicode class methodsFor: 'class methods' stamp: 'yo 12/24/2002 08:03'!
compoundTextSequence	self subclassResponsibility.! !

!Unicode class methodsFor: 'class methods' stamp: 'StephaneDucasse 2/13/2010 16:04'!
digitValueOf: char	"Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and < 0	otherwise. This is used to parse literal numbers of radix 2-36."		| value |	value := char charCode.	value <= $9 asciiValue 		ifTrue: [^value - $0 asciiValue].	value >= $A asciiValue 		ifTrue: [ value <= $Z asciiValue ifTrue: [^value - $A asciiValue + 10].				(value >= $a asciiValue and: [value <= $z asciiValue]) 					ifTrue: [^value - $a asciiValue + 10]].	value > (DecimalProperty size - 1) ifTrue: [^ -1].	^ (DecimalProperty at: value+1)! !

!Unicode class methodsFor: 'class methods' stamp: 'yo 2/20/2004 14:12'!
generalCategory	^ GeneralCategory.! !

!Unicode class methodsFor: 'class methods' stamp: 'yo 8/5/2003 16:12'!
generalCategoryComment"Lu Letter, Uppercase Ll Letter, Lowercase Lt Letter, Titlecase Lm Letter, Modifier Lo Letter, Other Mn Mark, Non-Spacing Mc Mark, Spacing Combining Me Mark, Enclosing Nd Number, Decimal Nl Number, Letter No Number, Other Pc Punctuation, Connector Pd Punctuation, Dash Ps Punctuation, Open Pe Punctuation, Close Pi Punctuation, Initial quote (may behave like Ps or Pe depending on usage) Pf Punctuation, Final quote (may behave like Ps or Pe depending on usage) Po Punctuation, Other Sm Symbol, Math Sc Symbol, Currency Sk Symbol, Modifier So Symbol, Other Zs Separator, Space Zl Separator, Line Zp Separator, Paragraph Cc Other, Control Cf Other, Format Cs Other, Surrogate Co Other, Private Use Cn Other, Not Assigned (no characters in the file have this property) "! !

!Unicode class methodsFor: 'class initialization' stamp: 'nice 3/28/2011 23:31'!
initialize	" Unicode initialize "	self initializeTagConstants.	self initializeCaseMappings! !

!Unicode class methodsFor: 'casing' stamp: 'SvenVanCaekenberghe 7/8/2011 15:07'!
initializeCaseMappings	"Unicode initializeCaseMappings"	ToCasefold := IdentityDictionary new.	ToUpper := IdentityDictionary new.	ToLower := IdentityDictionary new.	UIManager default informUserDuring: [ :bar| | result |		bar value: 'Downloading Unicode data'.		(result := ZnClient get: 'http://www.unicode.org/Public/UNIDATA/CaseFolding.txt') isSuccess			ifFalse: [ ^ self error: 'Download failed' ].		bar value: 'Updating Case Mappings'.		self parseCaseMappingFrom: result contents readStream ].! !

!Unicode class methodsFor: 'class initialization' stamp: 'nice 3/28/2011 23:40'!
initializeTagConstants	" Unicode initializeTagConstants "	(self classPool keys		select: [:sym | sym size = 2 						and: [sym first isUppercase and: [sym last isLowercase]]]) asSortedCollection		inject: 1		into: [:index :sym | sym = #Cn				ifTrue: [self classPool at: sym put: 0. index]				ifFalse: [self classPool at: sym put: index. index + 1]]! !

!Unicode class methodsFor: 'class methods' stamp: 'yo 12/4/2004 22:47'!
isCharset	^ false.! !

!Unicode class methodsFor: 'character classification' stamp: 'kwl 6/30/2006 02:55'!
isDigit: char 	| value |	value := char charCode.	value > (GeneralCategory size - 1)		ifTrue: [^ false].	^ (GeneralCategory at: value + 1)		= Nd! !

!Unicode class methodsFor: 'subencodings' stamp: 'yo 1/12/2004 18:11'!
isJapanese: code	^ code > 255 and: [(JISX0208 charFromUnicode: code) notNil].! !

!Unicode class methodsFor: 'subencodings' stamp: 'yo 1/12/2004 18:11'!
isKorean: code	^ code > 255 and: [(KSX1001 charFromUnicode: code) notNil]! !

!Unicode class methodsFor: 'character classification' stamp: 'kwl 6/30/2006 02:56'!
isLetter: char 	| value codeCat |	value := char charCode.	value > (GeneralCategory size - 1)		ifTrue: [^ false].	^ (codeCat := GeneralCategory at: value + 1) >= Ll		and: [codeCat <= Lu]! !

!Unicode class methodsFor: 'character classification' stamp: 'kwl 6/30/2006 02:57'!
isLowercase: char 	| value |	value := char charCode.	value > (GeneralCategory size - 1)		ifTrue: [^ false].	^ (GeneralCategory at: value + 1)		= Ll! !

!Unicode class methodsFor: 'subencodings' stamp: 'yo 1/12/2004 18:11'!
isSimplifiedChinese: code	^ code > 255 and: [(GB2312 charFromUnicode: code) notNil]! !

!Unicode class methodsFor: 'subencodings' stamp: 'yo 1/12/2004 18:00'!
isTraditionalChinese: code	^ false.! !

!Unicode class methodsFor: 'subencodings' stamp: 'yo 1/12/2004 17:55'!
isUnifiedKanji: code	^ ((((16r2E80 <= code and: [code <= 16rA4CF])		or: [16rF900 <= code and: [code <= 16rFAFF]])			or: [16rFE30 <= code and: [code <= 16rFE4F]])				or: [16rFF00 <= code and: [code <= 16rFFEF]])					or: [16r20000 <= code and: [code <= 16r2FA1F]].! !

!Unicode class methodsFor: 'character classification' stamp: 'kwl 6/30/2006 02:58'!
isUppercase: char 	| value |	value := char charCode.	value > (GeneralCategory size - 1)		ifTrue: [^ false].	^ (GeneralCategory at: value + 1)		= Lu! !

!Unicode class methodsFor: 'class methods' stamp: 'nice 2/28/2010 16:34'!
leadingChar	^ 0! !

!Unicode class methodsFor: 'casing' stamp: 'SvenVanCaekenberghe 1/8/2012 14:50'!
parseCaseMappingFrom: stream	"Parse the Unicode casing mappings from the given stream.	Handle only the simple mappings"	"		Unicode initializeCaseMappings.	"	ToCasefold := IdentityDictionary new: 2048.	ToUpper := IdentityDictionary new: 2048.	ToLower := IdentityDictionary new: 2048.	[stream atEnd] whileFalse:[		| fields line srcCode dstCode |		line := stream nextLine copyUpTo: $#.		fields := line trimBoth findTokens: $;.		(fields size > 2 and: [#('C' 'S') includes: (fields at: 2) trimBoth]) ifTrue:[			srcCode := Integer readFrom: (fields at: 1) trimBoth base: 16.			dstCode := Integer readFrom: (fields at: 3) trimBoth base: 16.			ToCasefold at: srcCode put: dstCode.		].	].	ToCasefold keysAndValuesDo:		[:k :v |		(self isUppercase: (self value: k))			ifTrue:				["In most cases, uppercase letter are folded to lower case"				ToUpper at: v put: k.				ToLower at: k put: v].		(self isLowercase: (self value: k))			ifTrue:				["In a few cases, two lower case letters are folded to the same lower case.				We must find an upper case letter folded to the same letter"				| up |				up := ToCasefold keys detect: [:e | (self isUppercase: (self value: e)) and: [(ToCasefold at: e) = v]] ifNone: [nil].				up ifNotNil: [ToUpper at: k put: up]]].! !

!Unicode class methodsFor: 'class methods' stamp: 'HenrikSperreJohansen 6/12/2010 02:36'!
parseUnicodeDataFrom: stream"	self halt.	self parseUnicodeDataFile"	| line fieldEnd point fieldStart toNumber generalCategory decimalProperty |	toNumber := [:quad | ('16r', quad) asNumber].	GeneralCategory := SparseLargeTable new: 16rE0080 chunkSize: 1024 arrayClass: Array base: 1 defaultValue:  'Cn'.	DecimalProperty := SparseLargeTable new: 16rE0080 chunkSize: 32 arrayClass: Array base: 1 defaultValue: -1.	16r3400 to: 16r4DB5 do: [:i | GeneralCategory at: i+1 put: 'Lo'].	16r4E00 to: 16r9FA5 do: [:i | GeneralCategory at: i+1 put: 'Lo'].	16rAC00 to: 16rD7FF do: [:i | GeneralCategory at: i+1 put: 'Lo'].	[(line := stream nextLine) size > 0] whileTrue: [		fieldEnd := line indexOf: $; startingAt: 1.		point := toNumber value: (line copyFrom: 1 to: fieldEnd - 1).		point > 16rE007F ifTrue: [			GeneralCategory zapDefaultOnlyEntries.			DecimalProperty zapDefaultOnlyEntries.			^ self].		2 to: 3 do: [:i |			fieldStart := fieldEnd + 1.			fieldEnd := line indexOf: $; startingAt: fieldStart.		].		generalCategory := line copyFrom: fieldStart to: fieldEnd - 1.		GeneralCategory at: point+1 put: generalCategory.		generalCategory = 'Nd' ifTrue: [			4 to: 7 do: [:i |				fieldStart := fieldEnd + 1.				fieldEnd := line indexOf: $; startingAt: fieldStart.			].			decimalProperty :=  line copyFrom: fieldStart to: fieldEnd - 1.			DecimalProperty at: point+1 put: decimalProperty asNumber.		].	].	GeneralCategory zapDefaultOnlyEntries.	DecimalProperty zapDefaultOnlyEntries.! !

!Unicode class methodsFor: 'casing' stamp: 'nice 7/14/2010 15:00'!
toCasefold: aWideString	"Transform a Wide String into fold case.	This is to enable case insensitive conversion."		^aWideString collect: [:e |		(ToCasefold at: e charCode ifAbsent: [nil])			ifNil: [e]			ifNotNil: [:low | self value: low]]! !

!Unicode class methodsFor: 'casing' stamp: 'nice 7/14/2010 13:11'!
toLowercase: aWideString	"Transform a Wide String into lowercase.	This does not handle special cases where number of characters could change.	The algorithm would work for ByteString, however it's far from the most efficient."		^aWideString collect: [:e |		(ToLower at: e charCode ifAbsent: [nil])			ifNil: [e]			ifNotNil: [:low | self value: low]]! !

!Unicode class methodsFor: 'casing' stamp: 'nice 7/14/2010 13:08'!
toUppercase: aWideString	"Transform a Wide String into uppercase.	This does not handle special cases where number of characters could change.	The algorithm would work for ByteString, however it's far from the most efficient."		^aWideString collect: [:e |		(ToUpper at: e charCode ifAbsent: [nil])			ifNil: [e]			ifNotNil: [:up | self value: up]]! !

!Unicode class methodsFor: 'instance creation' stamp: 'nice 2/28/2010 20:05'!
value: code	| l |	code < 256 ifTrue: [^ Character value: code].	l := Locale currentPlatform languageEnvironment leadingChar.	^ Character leadingChar: l code: code.! !
EncodedCharSet initialize!
Unicode initialize!
