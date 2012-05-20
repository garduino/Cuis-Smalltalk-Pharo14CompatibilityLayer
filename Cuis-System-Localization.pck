'From Cuis 4.0 of 21 April 2012 [latest update: #1270] on 20 May 2012 at 5:41:07 pm'!
'Description Please enter a description for this package '!
!classDefinition: #ISOLanguageDefinition category: #'Cuis-System-Localization'!
Object subclass: #ISOLanguageDefinition
	instanceVariableNames: 'iso3 iso2 iso3Alternate language'
	classVariableNames: 'ISO2Countries ISO2Table ISO3Countries ISO3Table'
	poolDictionaries: ''
	category: 'Cuis-System-Localization'!
!classDefinition: 'ISOLanguageDefinition class' category: #'Cuis-System-Localization'!
ISOLanguageDefinition class
	instanceVariableNames: ''!

!classDefinition: #Locale category: #'Cuis-System-Localization'!
Object subclass: #Locale
	instanceVariableNames: 'id shortDate longDate time decimalSymbol digitGrouping currencySymbol currencyNotation measurement offsetLocalToUTC offsetVMToUTC dstActive'
	classVariableNames: 'Activated Current CurrentPlatform KnownLocales LanguageSymbols LocaleChangeListeners PlatformEncodings'
	poolDictionaries: ''
	category: 'Cuis-System-Localization'!
!classDefinition: 'Locale class' category: #'Cuis-System-Localization'!
Locale class
	instanceVariableNames: ''!

!classDefinition: #LocaleID category: #'Cuis-System-Localization'!
Object subclass: #LocaleID
	instanceVariableNames: 'isoLanguage isoCountry'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-System-Localization'!
!classDefinition: 'LocaleID class' category: #'Cuis-System-Localization'!
LocaleID class
	instanceVariableNames: ''!

!classDefinition: #NaturalLanguageFormTranslator category: #'Cuis-System-Localization'!
Object subclass: #NaturalLanguageFormTranslator
	instanceVariableNames: 'id generics'
	classVariableNames: 'CachedTranslations'
	poolDictionaries: ''
	category: 'Cuis-System-Localization'!
!classDefinition: 'NaturalLanguageFormTranslator class' category: #'Cuis-System-Localization'!
NaturalLanguageFormTranslator class
	instanceVariableNames: ''!

!classDefinition: #NaturalLanguageTranslator category: #'Cuis-System-Localization'!
Object subclass: #NaturalLanguageTranslator
	instanceVariableNames: ''
	classVariableNames: 'AllKnownPhrases'
	poolDictionaries: ''
	category: 'Cuis-System-Localization'!
!classDefinition: 'NaturalLanguageTranslator class' category: #'Cuis-System-Localization'!
NaturalLanguageTranslator class
	instanceVariableNames: ''!


!Locale commentStamp: '<historical>' prior: 0!
Main comment stating the purpose of this class and relevant relationship to other classes.	http://www.w3.org/WAI/ER/IG/ert/iso639.htm	http://www.oasis-open.org/cover/iso639a.html	See also	http://oss.software.ibm.com/cvs/icu/~checkout~/icuhtml/design/language_code_issues.html	http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.10	ISO 3166http://mitglied.lycos.de/buran/knowhow/codes/locales/!

!NaturalLanguageFormTranslator commentStamp: 'LaurentLaffont 3/4/2011 22:44' prior: 0!
Provides support for looking up Forms by name for presentation in the UI.Different forms can be registered for a name for different locales allowingimages presented in the UI to be localised.Typically used where images contain language dependent text.E.g.buttonForm := (NaturalLanguageFormTranslator localeID: Locale current localeID)  translate: 'submit button'Form translations are added like so:(NaturalLanguageFormTranslator isoLanguage: 'en' isoCountry: 'gb')  name: 'submit button' form: aForm!

!NaturalLanguageTranslator commentStamp: 'HilaireFernandes 5/13/2010 11:48' prior: 0!
A NaturalLanguageTranslator is a dummy translator.The localization framework is found in the gettext package.!

!ISOLanguageDefinition methodsFor: 'accessing' stamp: 'mir 7/1/2004 18:20'!
iso2	^iso2 ifNil: [self iso3]! !

!ISOLanguageDefinition methodsFor: 'initialize' stamp: 'mir 6/30/2004 15:54'!
iso2: aString	iso2 := aString ifEmpty: [nil] ifNotEmpty: [aString]! !

!ISOLanguageDefinition methodsFor: 'accessing' stamp: 'mir 7/1/2004 18:21'!
iso3	^iso3 ifNil: ['']! !

!ISOLanguageDefinition methodsFor: 'initialize' stamp: 'mir 6/30/2004 15:54'!
iso3: aString	iso3 := aString ifEmpty: [nil] ifNotEmpty: [aString]! !

!ISOLanguageDefinition methodsFor: 'accessing' stamp: 'mir 6/30/2004 15:47'!
iso3Alternate	^iso3Alternate ifNil: ['']! !

!ISOLanguageDefinition methodsFor: 'initialize' stamp: 'mir 6/30/2004 15:54'!
iso3Alternate: aString	iso3Alternate := aString ifEmpty: [nil] ifNotEmpty: [aString]! !

!ISOLanguageDefinition methodsFor: 'accessing' stamp: 'mir 8/15/2003 13:13'!
language	^language! !

!ISOLanguageDefinition methodsFor: 'initialize' stamp: 'mir 8/15/2003 13:40'!
language: aString	language := aString! !

!ISOLanguageDefinition class methodsFor: 'private' stamp: 'mir 9/1/2005 14:06'!
buildIso3166CodesTables	"ISOLanguageDefinition buildIso3166CodesTables"	| rawdata stream country isoa2 isoa3 unNumeric macName macCode windowsName windowsCode empty table |	rawdata := self iso3166Codes.	table := OrderedCollection new: 200. 	stream := rawdata readStream.	empty := 160 asCharacter asString.	[stream atEnd] whileFalse: 		[country := stream nextLine.		isoa2 := stream nextLine.		isoa3 := stream nextLine.		unNumeric := stream nextLine.		windowsName := stream nextLine.		windowsName = empty ifTrue: [windowsName := nil].		windowsCode := stream nextLine. 		windowsCode = empty ifTrue: [windowsCode := nil].		macName := stream nextLine.		macName = empty ifTrue: [macName := nil].		macCode := stream nextLine.		macCode = empty ifTrue: [macCode := nil].		table add: { country.  isoa2. isoa3.  unNumeric. windowsName.  windowsCode.  macName. macCode. }].	^table! !

!ISOLanguageDefinition class methodsFor: 'private' stamp: 'mir 9/1/2005 14:14'!
extraCountryDefinitions	^{	{'Kids'. 'KIDS'. 'KIDS'.}.	}! !

!ISOLanguageDefinition class methodsFor: 'private' stamp: 'mir 7/15/2004 18:14'!
extraISO3Definitions	^self readISOLanguagesFrom: 'jpk		Japanese (Kids)' readStream! !

!ISOLanguageDefinition class methodsFor: 'private' stamp: 'mir 7/15/2004 18:13'!
initISO3LanguageTable	"ISOLanguageDefinition initIso3LanguageTable"	| table |	table := ISOLanguageDefinition readISOLanguagesFrom: ISOLanguageDefinition isoLanguages readStream.	table addAll: self extraISO3Definitions.	^table! !

!ISOLanguageDefinition class methodsFor: 'private' stamp: 'mir 9/1/2005 14:12'!
initISOCountries	"ISOLanguageDefinition initISOCountries"	| iso3166Table |	iso3166Table := ISOLanguageDefinition buildIso3166CodesTables.	ISO2Countries := Dictionary new.	ISO3Countries := Dictionary new.	iso3166Table do: [:entry | 		ISO2Countries at: (entry at: 2) put: (entry at: 1).		ISO3Countries at: (entry at: 3) put: (entry at: 1)].	self extraCountryDefinitions do: [:entry | 		ISO2Countries at: (entry at: 2) put: (entry at: 1).		ISO3Countries at: (entry at: 3) put: (entry at: 1)]! !

!ISOLanguageDefinition class methodsFor: 'initialization' stamp: 'mir 7/1/2004 18:19'!
initialize	"ISOLanguageDefinition initialize"	ISO3Table := nil.	ISO2Table := nil! !

!ISOLanguageDefinition class methodsFor: 'private' stamp: 'mir 9/1/2005 14:18'!
iso2Countries	"ISOLanguageDefinition iso2Countries"	"ISO2Countries := nil. ISO3Countries := nil"	ISO2Countries ifNil: [self initISOCountries].	^ISO2Countries! !

!ISOLanguageDefinition class methodsFor: 'accessing' stamp: 'mir 7/1/2004 18:06'!
iso2LanguageDefinition: aString	^self iso2LanguageTable at: aString! !

!ISOLanguageDefinition class methodsFor: 'private' stamp: 'mir 7/1/2004 18:14'!
iso2LanguageTable	"ISOLanguageDefinition iso2LanguageTable"	ISO2Table ifNotNil: [^ISO2Table].	ISO2Table := Dictionary new: self iso3LanguageTable basicSize.	self iso3LanguageTable do: [:entry |		ISO2Table at: entry iso2 put: entry].	^ISO2Table! !

!ISOLanguageDefinition class methodsFor: 'private' stamp: 'mir 9/1/2005 13:57'!
iso3166Codes"http://www.unicode.org/onlinedat/countries.html"^'ÅLAND ISLANDSAX    AFGHANISTANAFAFG004    ALBANIAALALB008CTRY_ALBANIA355  ALGERIADZDZA012CTRY_ALGERIA213verArabic16AMERICAN SAMOAASASM016    ANDORRAADAND020    ANGOLAAOAGO024    ANGUILLAAIAIA660    ANTARCTICAAQATA010    ANTIGUA AND BARBUDAAGATG028    ARGENTINAARARG032CTRY_ARGENTINA54  ARMENIAAMARM051CTRY_ARMENIA374verArmenian84ARUBAAWABW533    AUSTRALIAAUAUS036CTRY_AUSTRALIA61verAustralia15AUSTRIAATAUT040CTRY_AUSTRIA43verAustria92AZERBAIJANAZAZE031CTRY_AZERBAIJAN994  BAHAMASBSBHS044    BAHRAINBHBHR048CTRY_BAHRAIN973  BANGLADESHBDBGD050  verBengali60BARBADOSBBBRB052    BELARUSBYBLR112CTRY_BELARUS375  BELGIUMBEBEL056CTRY_BELGIUM32verFrBelgium, verFlemish98BELIZEBZBLZ084CTRY_BELIZE501  BENINBJBEN204    BERMUDABMBMU060    BHUTANBTBTN064  verBhutan83BOLIVIABOBOL068CTRY_BOLIVIA591  BOSNIA AND HERZEGOVINABABIH070    BOTSWANABWBWA072    BOUVET ISLANDBVBVT074    BRAZILBRBRA076CTRY_BRAZIL55verBrazil71BRITISH INDIAN OCEAN TERRITORYIOIOT086    BRUNEI DARUSSALAMBNBRN096CTRY_BRUNEI_DARUSSALAM673  BULGARIABGBGR100CTRY_BULGARIA359verBulgaria 72BURKINA FASOBFBFA854    BURUNDIBIBDI108    CAMBODIAKHKHM116    CAMEROONCMCMR120    CANADACACAN124CTRY_CANADA2verFrCanada, verEndCanada82CAPE VERDECVCPV132    CAYMAN ISLANDSKYCYM136    CENTRAL AFRICAN REPUBLICCFCAF140    CHADTDTCD148    CHILECLCHL152CTRY_CHILE56  CHINACNCHN156CTRY_PRCHINA86verChina52CHRISTMAS ISLANDCXCXR162    COCOS (KEELING) ISLANDSCCCCK166    COLOMBIACOCOL170CTRY_COLOMBIA57  COMOROSKMCOM174    CONGOCGCOG178    CONGO, THE DEMOCRATIC REPUBLIC OF THECD    COOK ISLANDSCKCOK184    COSTA RICACRCRI188CTRY_COSTA_RICA506  COTE D''IVOIRECICIV384    CROATIA (local name: Hrvatska)HRHRV191CTRY_CROATIA385verCroatia, verYugoCroatian68 (c), 25 (y)CUBACUCUB192    CYPRUSCYCYP196  verCyprus23CZECH REPUBLICCZCZE203CTRY_CZECH420verCzech 56DENMARKDKDNK208CTRY_DENMARK45verDenmark(da), verFaeroeIsl(fo)9(da), 47(fo)DJIBOUTIDJDJI262    DOMINICADMDMA212    DOMINICAN REPUBLICDODOM214CTRY_DOMINICAN_REPUBLIC1  EAST TIMORTLTLS626    ECUADORECECU218CTRY_ECUADOR593  EGYPTEGEGY818CTRY_EGYPT20verArabic16EL SALVADORSVSLV222CTRY_EL_SALVADOR503  EQUATORIAL GUINEAGQGNQ226    ERITREAERERI232    ESTONIAEEEST233CTRY_ESTONIA372verEstonia44ETHIOPIAETETH210    FALKLAND ISLANDS (MALVINAS)FKFLK238    FAROE ISLANDSFOFRO234CTRY_FAEROE_ISLANDS298  FIJIFJFJI242    FINLANDFIFIN246CTRY_FINLAND358verFinland17FRANCEFRFRA250CTRY_FRANCE33verFrance1FRANCE, METROPOLITANFXFXX249    FRENCH GUIANAGFGUF254    FRENCH POLYNESIAPFPYF258    FRENCH SOUTHERN TERRITORIESTFATF260    GABONGAGAB266    GAMBIAGMGMB270    GEORGIAGEGEO268CTRY_GEORGIA995verGeorgian85GERMANYDEDEU276CTRY_GERMANY49verGermany3GHANAGHGHA288    GIBRALTARGIGIB292    GREECEGRGRC300CTRY_GREECE30verGreece, verGreecePoly20, 40GREENLANDGLGRL304  verGreenland107GRENADAGDGRD308    GUADELOUPEGPGLP312    GUAMGUGUM316    GUATEMALAGTGTM320CTRY_GUATEMALA502  GUINEAGNGIN324    GUINEA-BISSAUGWGNB624    GUYANAGYGUY328    HAITIHTHTI332    HEARD ISLAND & MCDONALD ISLANDSHMHMD334    HONDURASHNHND340CTRY_HONDURAS504  HONG KONGHKHKG344CTRY_HONG_KONG852  HUNGARYHUHUN348CTRY_HUNGARY36verHungary43ICELANDISISL352CTRY_ICELAND354verIceland21INDIAININD356CTRY_INDIA91verIndiaHindi(hi)33INDONESIAIDIDN360CTRY_INDONESIA62  IRAN, ISLAMIC REPUBLIC OFIRIRN364CTRY_IRAN981verIran48IRAQIQIRQ368CTRY_IRAQ964verArabic16IRELANDIEIRL372CTRY_IRELAND353verIreland50ISRAELILISR376CTRY_ISRAEL972verIsrael13ITALYITITA380CTRY_ITALY39verItaly4JAMAICAJMJAM388CTRY_JAMAICA1  JAPANJPJPN392CTRY_JAPAN81verJapan14JORDANJOJOR400CTRY_JORDAN962  KAZAKHSTANKZKAZ398CTRY_KAZAKSTAN7  KENYAKEKEN404CTRY_KENYA254  KIRIBATIKIKIR296    KOREA, DEMOCRATIC PEOPLE''S REPUBLIC OFKPPRK408  verKorea51KOREA, REPUBLIC OFKRKOR410CTRY_SOUTH_KOREA82verKorea KUWAITKWKWT414CTRY_KUWAIT965  KYRGYZSTANKGKGZ417CTRY_KYRGYZSTAN996  LAO PEOPLE''S DEMOCRATIC REPUBLICLALAO418    LATVIALVLVA428CTRY_LATVIA371verLatvia45LEBANONLBLBN422CTRY_LEBANON961  LESOTHOLSLSO426    LIBERIALRLBR430    LIBYAN ARAB JAMAHIRIYALYLBY434CTRY_LIBYA218verArabic16LIECHTENSTEINLILIE438CTRY_LIECHTENSTEIN41  LITHUANIALTLTU440CTRY_LITHUANIA370verLithuania41LUXEMBOURGLULUX442CTRY_LUXEMBOURG352verFrBelgiumLux6MACAUMOMAC446CTRY_MACAU853  MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OFMKMKD807CTRY_MACEDONIA389verMacedonian MADAGASCARMGMDG450    MALAWIMWMWI454    MALAYSIAMYMYS458CTRY_MALAYSIA60  MALDIVESMVMDV462CTRY_MALDIVES960  MALIMLMLI466    MALTAMTMLT470  verMalta22MARSHALL ISLANDSMHMHL584    MARTINIQUEMQMTQ474    MAURITANIAMRMRT478    MAURITIUSMUMUS480    MAYOTTEYTMYT175    MEXICOMXMEX484CTRY_MEXICO52  MICRONESIA, FEDERATED STATES OFFMFSM583    MOLDOVA, REPUBLIC OFMDMDA498    MONACOMCMCO492CTRY_MONACO33  MONGOLIAMNMNG496CTRY_MONGOLIA976  MONTSERRATMSMSR500    MOROCCOMAMAR504CTRY_MOROCCO212verArabic16MOZAMBIQUEMZMOZ508    MYANMARMMMMR104    NAMIBIANANAM516    NAURUNRNRU520    NEPALNPNPL524  verNepal106NETHERLANDSNLNLD528CTRY_NETHERLANDS31verNetherlands5NETHERLANDS ANTILLESANANT530    NEW CALEDONIANCNCL540    NEW ZEALANDNZNZL554CTRY_NEW_ZEALAND64  NICARAGUANINIC558CTRY_NICARAGUA505  NIGERNENER562    NIGERIANGNGA566    NIUENUNIU570    NORFOLK ISLANDNFNFK574    NORTHERN MARIANA ISLANDSMPMNP580    NORWAYNONOR578CTRY_NORWAY47verNorway12OMANOMOMN512CTRY_OMAN968  PAKISTANPKPAK586CTRY_PAKISTAN92verPakistanUrdu, verPunjabi34 (U), 95 (P)PALAUPWPLW585    PANAMAPAPAN591CTRY_PANAMA507  PALESTINIAN TERRITORY, OCCUPIEDPS  PAPUA NEW GUINEAPGPNG598    PARAGUAYPYPRY600CTRY_PARAGUAY595  PERUPEPER604CTRY_PERU51  PHILIPPINESPHPHL608CTRY_PHILIPPINES63  PITCAIRNPNPCN612    POLANDPLPOL616CTRY_POLAND48verPoland42PORTUGALPTPRT620CTRY_PORTUGAL351verPortugal10PUERTO RICOPRPRI630CTRY_PUERTO_RICO1  QATARQAQAT634CTRY_QATAR974  REUNIONREREU638    ROMANIAROROU*642CTRY_ROMANIA40verRomania39RUSSIAN FEDERATIONRURUS643CTRY_RUSSIA7verRussia49RWANDARWRWA646    SAINT KITTS AND NEVISKNKNA659    SAINT LUCIALCLCA662    SAINT VINCENT AND THE GRENADINESVCVCT670    SAMOAWSWSM882    SAN MARINOSMSMR674    SAO TOME AND PRINCIPESTSTP678    SAUDI ARABIASASAU682CTRY_SAUDI_ARABIA966verArabic16SENEGALSNSEN686    SERBIA AND MONTENEGROCS  CTRY_SERBIA381  SEYCHELLESSCSYC690    SIERRA LEONESLSLE694    SINGAPORESGSGP702CTRY_SINGAPORE65verSingapore100SLOVAKIA (Slovak Republic)SKSVK703CTRY_SLOVAK421verSlovak57 SLOVENIASISVN705CTRY_SLOVENIA386verSlovenian66SOLOMON ISLANDSSBSLB90    SOMALIASOSOM706    SOUTH AFRICAZAZAF710CTRY_SOUTH_AFRICA27  SOUTH GEORGIA AND THE SOUTH SANDWICH ISLANDSGS  SPAINESESP724CTRY_SPAIN34verSpain8SRI LANKALKLKA144    SAINT HELENASHSHN654    SAINT PIERRE AND MIQUELONPMSPM666    SUDANSDSDN736    SURINAMESRSUR740    SVALBARD AND JAN MAYEN ISLANDSSJSJM744    SWAZILANDSZSWZ748    SWEDENSESWE752CTRY_SWEDEN46verSweden7SWITZERLANDCHCHE756CTRY_SWITZERLAND41verFrSwiss(fr), verGrSwiss(de)18(fr), 19(de)SYRIAN ARAB REPUBLICSYSYR760CTRY_SYRIA963  TAIWAN, PROVINCE OF CHINATWTWN158CTRY_TAIWAN886verTaiwan53TAJIKISTANTJTJK762    TANZANIA, UNITED REPUBLIC OFTZTZA834    TATARSTAN CTRY_TATARSTAN7  THAILANDTHTHA764CTRY_THAILAND66verThailand54TIMOR-LESTETL    TOGOTGTGO768    TOKELAUTKTKL772    TONGATOTON776  verTonga88TRINIDAD AND TOBAGOTTTTO780CTRY_TRINIDAD_Y_TOBAGO1  TUNISIATNTUN788CTRY_TUNISIA216verArabic16TURKEYTRTUR792CTRY_TURKEY90verTurkey24TURKMENISTANTMTKM795    TURKS AND CAICOS ISLANDSTCTCA796    TUVALUTVTUV798    UGANDAUGUGA800    UKRAINEUAUKR804CTRY_UKRAINE380verUkraine 62UNITED ARAB EMIRATESAEARE784CTRY_UAE971  UNITED KINGDOMGBGBR826CTRY_UNITED_KINGDOM44verBritain2UNITED STATESUSUSA840CTRY_UNITED_STATES1verUS0UNITED STATES MINOR OUTLYING ISLANDSUMUMI581    URUGUAYUYURY858CTRY_URUGUAY598  UZBEKISTANUZUZB860CTRY_UZBEKISTAN7  VANUATUVUVUT548    VATICAN CITY STATE (HOLY SEE)VAVAT336    VENEZUELAVEVEN862CTRY_VENEZUELA58  VIET NAMVNVNM704CTRY_VIET_NAM84verVietnam VIRGIN ISLANDS (BRITISH)VGVGB92    VIRGIN ISLANDS (U.S.)VIVIR850    WALLIS AND FUTUNA ISLANDSWFWLF876    WESTERN SAHARAEHESH732    YEMENYEYEM887CTRY_YEMEN967  YUGOSLAVIAYUYUG891    ZAIREZRZAR180    ZAMBIAZMZMB894    ZIMBABWEZWZWE716CTRY_ZIMBABWE263  '! !

!ISOLanguageDefinition class methodsFor: 'private' stamp: 'mir 9/1/2005 14:18'!
iso3Countries	"ISOLanguageDefinition iso3Countries"	"ISO2Countries := nil. ISO3Countries := nil"	ISO3Countries ifNil: [self initISOCountries].	^ISO3Countries! !

!ISOLanguageDefinition class methodsFor: 'accessing' stamp: 'mir 7/1/2004 18:06'!
iso3LanguageDefinition: aString	^self iso3LanguageTable at: aString! !

!ISOLanguageDefinition class methodsFor: 'private' stamp: 'mir 7/21/2004 13:10'!
iso3LanguageTable	"ISOLanguageDefinition iso3LanguageTable"	^ISO3Table ifNil: [ISO3Table := self initISO3LanguageTable]! !

!ISOLanguageDefinition class methodsFor: 'private' stamp: 'yo 12/3/2004 17:46'!
isoLanguages	"ISO 639: 3-letter codes"	^'abk	ab	Abkhazianace		Achineseach		Acoliada		Adangmeaar	aa	Afarafh		Afrihiliafr	af	Afrikaansafa		Afro-Asiatic (Other)aka		Akanakk		Akkadianalb/sqi	sq	Albanianale		Aleutalg		Algonquian languagestut		Altaic (Other)amh	am	Amharicapa		Apache languagesara	ar	Arabicarc		Aramaicarp		Arapahoarn		Araucanianarw		Arawakarm/hye	hy	Armenianart		Artificial (Other)asm	as	Assameseath		Athapascan languagesmap		Austronesian (Other)ava		Avaricave		Avestanawa		Awadhiaym	ay	Aymaraaze	az	Azerbaijaninah		Aztecban		Balinesebat		Baltic (Other)bal		Baluchibam		Bambarabai		Bamileke languagesbad		Bandabnt		Bantu (Other)bas		Basabak	ba	Bashkirbaq/eus	eu	Basquebej		Bejabem		Bembaben	bn	Bengaliber		Berber (Other)bho		Bhojpuribih	bh	Biharibik		Bikolbin		Binibis	bi	Bislamabra		Brajbre	be	Bretonbug		Buginesebul	bg	Bulgarianbua		Buriatbur/mya	my	Burmesebel	be	Byelorussiancad		Caddocar		Caribcat	ca	Catalancau		Caucasian (Other)ceb		Cebuanocel		Celtic (Other)cai		Central American Indian (Other)chg		Chagataicha		Chamorroche		Chechenchr		Cherokeechy		Cheyennechb		Chibchachi/zho	zh	Chinesechn		Chinook jargoncho		Choctawchu		Church Slavicchv		Chuvashcop		Copticcor		Cornishcos	co	Corsicancre		Creemus		Creekcrp		Creoles and Pidgins (Other)cpe		Creoles and Pidgins, English-based (Other)cpf		Creoles and Pidgins, French-based (Other)cpp		Creoles and Pidgins, Portuguese-based (Other)cus		Cushitic (Other)	hr	Croatiances/cze	cs	Czechdak		Dakotadan	da	Danishdel		Delawaredin		Dinkadiv		Divehidoi		Dogridra		Dravidian (Other)dua		Dualadut/nla	nl	Dutchdum		Dutch, Middle (ca. 1050-1350)dyu		Dyuladzo	dz	Dzongkhaefi		Efikegy		Egyptian (Ancient)eka		Ekajukelx		Elamiteeng	en	Englishenm		English, Middle (ca. 1100-1500)ang		English, Old (ca. 450-1100)esk		Eskimo (Other)epo	eo	Esperantoest	et	Estonianewe		Eweewo		Ewondofan		Fangfat		Fantifao	fo	Faroesefij	fj	Fijianfin	fi	Finnishfiu		Finno-Ugrian (Other)fon		Fonfra/fre	fr	Frenchfrm		French, Middle (ca. 1400-1600)fro		French, Old (842- ca. 1400)fry	fy	Frisianful		Fulahgaa		Gagae/gdh		Gaelic (Scots)glg	gl	Galleganlug		Gandagay		Gayogez		Geezgeo/kat	ka	Georgiandeu/ger	de	Germangmh		German, Middle High (ca. 1050-1500)goh		German, Old High (ca. 750-1050)gem		Germanic (Other)gil		Gilbertesegon		Gondigot		Gothicgrb		Grebogrc		Greek, Ancient (to 1453)ell/gre	el	Greek, Modern (1453-)kal	kl	Greenlandicgrn	gn	Guaraniguj	gu	Gujaratihai		Haidahau	ha	Hausahaw		Hawaiianheb	he	Hebrewher		Hererohil		Hiligaynonhim		Himachalihin	hi	Hindihmo		Hiri Motuhun	hu	Hungarianhup		Hupaiba		Ibanice/isl	is	Icelandicibo		Igboijo		Ijoilo		Ilokoinc		Indic (Other)ine		Indo-European (Other)ind	id	Indonesianina	ia	Interlingua (International Auxiliary language Association)ine		 Interlingueiku	iu	Inuktitutipk	ik	Inupiakira		Iranian (Other)gai/iri	ga	Irishsga		Irish, Old (to 900)mga		Irish, Middle (900 - 1200)iro		Iroquoian languagesita	it	Italianjpn	ja	Japanesejav/jaw	jv/jw Javanesejrb		Judeo-Arabicjpr		Judeo-Persiankab		Kabylekac		Kachinkam		Kambakan	kn	Kannadakau		Kanurikaa		Kara-Kalpakkar		Karenkas	ks	Kashmirikaw		Kawikaz	kk	Kazakhkha		Khasikhm	km	Khmerkhi		Khoisan (Other)kho		Khotanesekik		Kikuyukin	rw	Kinyarwandakir	ky	Kirghizkom		Komikon		Kongokok		Konkanikor	ko	Koreankpe		Kpellekro		Krukua		Kuanyamakum		Kumykkur	ku	Kurdishkru		Kurukhkus		Kusaiekut		Kutenailad		Ladinolah		Lahndalam		Lambaoci	oc	Langue d''Oc (post 1500)lao	lo	Laolat	la	Latinlav	lv	Latvianltz		Letzeburgeschlez		Lezghianlin	ln	Lingalalit	lt	Lithuanianloz		Lozilub		Luba-Katangalui		Luisenolun		Lundaluo		Luo (Kenya and Tanzania)mac/mak	mk	Macedonianmad		Maduresemag		Magahimai		Maithilimak		Makasarmlg	mg	Malagasymay/msa	ms	Malaymal		Malayalammlt	ml	Malteseman		Mandingomni		Manipurimno		Manobo languagesmax		Manxmao/mri	mi	Maorimar	mr	Marathichm		Marimah		Marshallmwr		Marwarimas		Masaimyn		Mayan languagesmen		Mendemic		Micmacmin		Minangkabaumis		Miscellaneous (Other)moh		Mohawkmol	mo	Moldavianmkh		Mon-Kmer (Other)lol		Mongomon	mn	Mongolianmos		Mossimul		Multiple languagesmun		Munda languagesnau	na	Naurunav		Navajonde		Ndebele, Northnbl		Ndebele, Southndo		Ndongonep	ne	Nepalinew		Newarinic		Niger-Kordofanian (Other)ssa		Nilo-Saharan (Other)niu		Niueannon		Norse, Oldnai		North American Indian (Other)nor	no	Norwegiannno		Norwegian (Nynorsk)nub		Nubian languagesnym		Nyamwezinya		Nyanjanyn		Nyankolenyo		Nyoronzi		Nzimaoji		Ojibwaori	or	Oriyaorm	om	Oromoosa		Osageoss		Osseticoto		Otomian languagespal		Pahlavipau		Palauanpli		Palipam		Pampangapag		Pangasinanpan	pa	Panjabipap		Papiamentopaa		Papuan-Australian (Other)fas/per	fa	Persianpeo		Persian, Old (ca 600 - 400 B.C.)phn		Phoenicianpol	pl	Polishpon		Ponapepor	pt	Portuguesepra		Prakrit languagespro		Provencal, Old (to 1500)pus	ps	Pushtoque	qu	Quechuaroh	rm	Rhaeto-Romanceraj		Rajasthanirar		Rarotonganroa		Romance (Other)ron/rum	ro	Romanianrom		Romanyrun	rn	Rundirus	ru	Russiansal		Salishan languagessam		Samaritan Aramaicsmi		Sami languagessmo	sm	Samoansad		Sandawesag	sg	Sangosan	sa	Sanskritsrd		Sardiniansco		Scotssel		Selkupsem		Semitic (Other)	sr	Serbianscr	sh	Serbo-Croatiansrr		Serershn		Shansna	sn	Shonasid		Sidamobla		Siksikasnd	sd	Sindhisin	si	Singhalesesit		Sino-Tibetan (Other)sio		Siouan languagessla		Slavic (Other)ssw	ss	Siswantslk/slo	sk	Slovakslv	sl	Sloveniansog		Sogdiansom	so	Somalison		Songhaiwen		Sorbian languagesnso		Sotho, Northernsot	st	Sotho, Southernsai		South American Indian (Other)esl/spa	es	Spanishsuk		Sukumasux		Sumeriansun	su	Sudanesesus		Sususwa	sw	Swahilissw		Swazisve/swe	sv	Swedishsyr		Syriactgl	tl	Tagalogtah		Tahitiantgk	tg	Tajiktmh		Tamashektam	ta	Tamiltat	tt	Tatartel	te	Teluguter		Terenotha	th	Thaibod/tib	bo	Tibetantig		Tigretir	ti	Tigrinyatem		Timnetiv		Tivitli		Tlingittog	to	Tonga (Nyasa)ton		Tonga (Tonga Islands)tru		Truktsi		Tsimshiantso	ts	Tsongatsn	tn	Tswanatum		Tumbukatur	tr	Turkishota		Turkish, Ottoman (1500 - 1928)tuk	tk	Turkmentyv		Tuviniantwi	tw	Twiuga		Ugariticuig	ug	Uighurukr	uk	Ukrainianumb		Umbunduund		Undeterminedurd	ur	Urduuzb	uz	Uzbekvai		Vaiven		Vendavie	vi	Vietnamesevol	vo	Volapükvot		Voticwak		Wakashan languageswal		Walamowar		Waraywas		Washocym/wel	cy	Welshwol	wo	Wolofxho	xh	Xhosasah		Yakutyao		Yaoyap		Yapyid	yi	Yiddishyor	yo	Yorubazap		Zapoteczen		Zenagazha	za	Zhuangzul	zu	Zuluzun		Zuni'! !

!ISOLanguageDefinition class methodsFor: 'private' stamp: 'mir 7/1/2004 18:07'!
readISOLanguagesFrom: stream	"ISOLanguageDefinition readISOLanguagesFrom: ISOLanguageDefinition isoLanguages readStream "	| languages language code3 index line |	languages := Dictionary new.	[stream atEnd		or: [(line := stream nextLine readStream) atEnd]]		whileFalse: [			language := ISOLanguageDefinition new.			code3 := line upTo: Character tab.			(index := code3 indexOf: $/) > 0				ifTrue: [					language iso3: (code3 copyFrom: 1 to: index-1).					language iso3Alternate: (code3 copyFrom: index+1 to: code3 size)]				ifFalse: [language iso3: code3].			language				iso2: (line upTo: Character tab);				language: line upToEnd.			languages at: language iso3 put: language].	^languages! !

!Locale methodsFor: 'accessing' stamp: 'mir 8/31/2005 17:03'!
determineLocale	self localeID: self determineLocaleID! !

!Locale methodsFor: 'accessing' stamp: 'mir 8/31/2005 16:32'!
determineLocaleID	"Locale current determineLocaleID"	| langCode isoLang countryCode isoCountry |	langCode := self fetchISO2Language.	isoLang := langCode		ifNil: [^self localeID]		ifNotNil: [langCode].	countryCode := self primCountry.	isoCountry := countryCode		ifNil: [^LocaleID isoLanguage: isoLang]		ifNotNil: [countryCode].	^LocaleID isoLanguage: isoLang isoCountry: isoCountry! !

!Locale methodsFor: 'private' stamp: 'StephaneDucasse 5/28/2011 13:45'!
fetchISO2Language	"Locale current fetchISO2Language"	| lang isoLang |	lang := self primLanguage.	lang ifNil: [^nil].	lang := lang copyUpTo: 0 asCharacter.	lang size = 2		ifTrue: [^lang].	isoLang := ISOLanguageDefinition iso3LanguageDefinition: lang.	^isoLang		ifNil: [nil]		ifNotNil: [isoLang iso2]! !

!Locale methodsFor: 'accessing' stamp: 'nk 8/31/2004 09:39'!
isoCountry	^self localeID isoCountry! !

!Locale methodsFor: 'accessing' stamp: 'nk 8/31/2004 09:39'!
isoLanguage	^self localeID isoLanguage! !

!Locale methodsFor: 'accessing' stamp: 'mir 7/15/2004 12:41'!
isoLocale	"<language>-<country>"	^self isoCountry		ifNil: [self isoLanguage]		ifNotNil: [self isoLanguage , '-' , self isoCountry]! !

!Locale methodsFor: 'accessing' stamp: 'mir 7/15/2004 15:52'!
languageEnvironment	^LanguageEnvironment localeID: self localeID! !

!Locale methodsFor: 'accessing' stamp: 'mir 7/15/2004 14:55'!
localeID	^id! !

!Locale methodsFor: 'accessing' stamp: 'mir 7/15/2004 14:55'!
localeID: anID	id := anID! !

!Locale methodsFor: 'system primitives' stamp: 'jannik.laval 2/4/2010 15:09'!
primCountry	"Returns string with country tag according to ISO 639"	<primitive: 'primitiveCountry' module: 'LocalePlugin'>	^'FR'! !

!Locale methodsFor: 'system primitives' stamp: 'tpr 6/1/2005 18:45'!
primCurrencyNotation	"Returns boolean if symbol is pre- (true) or post-fix (false)"	<primitive: 'primitiveCurrencyNotation' module: 'LocalePlugin'>	^true! !

!Locale methodsFor: 'system primitives' stamp: 'tpr 6/1/2005 18:47'!
primCurrencySymbol	"Returns string with currency symbol"	<primitive: 'primitiveCurrencySymbol' module:'LocalePlugin'>	^'$'! !

!Locale methodsFor: 'system primitives' stamp: 'tpr 6/1/2005 18:48'!
primDST	"Returns boolean if DST  (daylight saving time) is active or not"	<primitive:'primitiveDaylightSavings' module: 'LocalePlugin'>	^false! !

!Locale methodsFor: 'system primitives' stamp: 'tpr 6/2/2005 13:42'!
primDecimalSymbol	"Returns string with e.g. '.' or ','"	<primitive:'primitiveDecimalSymbol' module: 'LocalePlugin'>	^'.'! !

!Locale methodsFor: 'system primitives' stamp: 'tpr 6/2/2005 13:42'!
primDigitGrouping	"Returns string with e.g. '.' or ',' (thousands etc)"	<primitive:'primitiveDigitGroupingSymbol' module: 'LocalePlugin'>	^','! !

!Locale methodsFor: 'system primitives' stamp: 'jannik.laval 2/4/2010 15:07'!
primLanguage	"returns string with language tag according to ISO 639"	<primitive:'primitiveLanguage' module: 'LocalePlugin'>	^'en'! !

!Locale methodsFor: 'system primitives' stamp: 'jannik.laval 2/4/2010 15:09'!
primLongDateFormat	"Returns the long date format	d day, m month, y year,	double symbol is null padded, single not padded (m=6, mm=06)	dddd weekday	mmmm month name"	<primitive:'primitiveLongDateFormat' module: 'LocalePlugin'>	^'dddd, mmmm d, yyyy'! !

!Locale methodsFor: 'system primitives' stamp: 'tpr 6/2/2005 13:43'!
primMeasurement	"Returns boolean denoting metric(true) or imperial(false)."	<primitive:'primitiveMeasurementMetric' module: 'LocalePlugin'>	^true! !

!Locale methodsFor: 'system primitives' stamp: 'jannik.laval 2/4/2010 15:11'!
primShortDateFormat	"Returns the short date format	d day, m month, y year,	double symbol is null padded, single not padded (m=6, mm=06)	dddd weekday	mmmm month name"	<primitive:'primitiveShortDateFormat' module: 'LocalePlugin'>	^'m/d/yy'! !

!Locale methodsFor: 'system primitives' stamp: 'jannik.laval 2/4/2010 15:11'!
primTimeFormat	"Returns string time format	Format is made up of 	h hour (h 12, H 24), m minute, s seconds, x (am/pm String)	double symbol is null padded, single not padded (h=6, hh=06)"	<primitive:'primitiveTimeFormat' module: 'LocalePlugin'>	^'h:mmx'! !

!Locale methodsFor: 'system primitives' stamp: 'jannik.laval 2/4/2010 15:08'!
primTimezone	"The offset from UTC in minutes, with positive offsets being towards the east.	(San Francisco is in UTC -07*60 and Paris is in UTC +02*60 (daylight savings is not in effect)."	<primitive:'primitiveTimezoneOffset' module: 'LocalePlugin'>	^0! !

!Locale methodsFor: 'system primitives' stamp: 'tpr 6/2/2005 13:44'!
primVMOffsetToUTC	"Returns the offset in minutes between the VM and UTC.	If the VM does not support UTC times, this is 0.	Also gives us backward compatibility with old VMs as the primitive will fail and we then can return 0."	<primitive:'primitiveVMOffsetToUTC' module: 'LocalePlugin'>	^0! !

!Locale methodsFor: 'accessing' stamp: 'tak 8/4/2005 15:18'!
printOn: aStream 	super printOn: aStream.	aStream nextPutAll: '(' , id printString , ')'! !

!Locale class methodsFor: 'settings' stamp: 'AlainPlantec 12/11/2009 10:59'!
activated	^ Activated ifNil: [Activated := false]! !

!Locale class methodsFor: 'settings' stamp: 'AlainPlantec 12/11/2009 10:59'!
activated: aBoolean	Activated := aBoolean! !

!Locale class methodsFor: 'notification' stamp: 'mir 6/30/2004 16:15'!
addLocalChangedListener: anObjectOrClass	self localeChangedListeners add: anObjectOrClass! !

!Locale class methodsFor: 'accessing' stamp: 'mir 8/31/2005 17:36'!
current	"Current := nil"	Current ifNil: [		Current := self determineCurrentLocale.		"Transcript show: 'Current locale: ' , Current localeID asString; cr"].	^Current! !

!Locale class methodsFor: 'accessing' stamp: 'yo 7/28/2004 20:32'!
currentPlatform	"CurrentPlatform := nil"	CurrentPlatform ifNil: [CurrentPlatform := self determineCurrentLocale].	^CurrentPlatform! !

!Locale class methodsFor: 'accessing' stamp: 'yo 7/28/2004 20:39'!
currentPlatform: locale	CurrentPlatform := locale.	LanguageEnvironment startUp.! !

!Locale class methodsFor: 'accessing' stamp: 'tak 10/18/2005 22:33'!
currentPlatform: locale during: aBlock 	"Alter current language platform during a block"	| backupPlatform |	backupPlatform := self currentPlatform.	[self currentPlatform: locale.	aBlock value]		ensure: [self currentPlatform: backupPlatform]! !

!Locale class methodsFor: 'platform specific' stamp: 'simon.denier 6/11/2010 17:35'!
defaultEncodingName: languageSymbol 	| encodings platformName osVersion |	platformName := OSPlatform platformName.	osVersion := OSPlatform osVersion.	encodings := self platformEncodings at: languageSymbol				ifAbsent: [self platformEncodings at: #default].	encodings at: platformName ifPresent: [:encoding | ^encoding].	encodings at: platformName , ' ' , osVersion		ifPresent: [:encoding | ^encoding].	^encodings at: #default! !

!Locale class methodsFor: 'private' stamp: 'mir 7/28/2005 00:24'!
determineCurrentLocale	"For now just return the default locale.	A smarter way would be to determine the current platforms default locale."	"Locale determineCurrentLocale"	^self new determineLocale! !

!Locale class methodsFor: 'private' stamp: 'mir 7/15/2004 19:44'!
initKnownLocales	| locales |	locales := Dictionary new.	"Init the locales for which we have translations"	NaturalLanguageTranslator availableLanguageLocaleIDs do: [:id |		locales at: id put: (Locale new localeID: id)].	^locales! !

!Locale class methodsFor: 'initialization' stamp: 'AlainPlantec 1/5/2010 12:24'!
initialize	"Locale initialize"	Smalltalk addToStartUpList: Locale.! !

!Locale class methodsFor: 'initialization' stamp: 'MarcusDenker 1/24/2010 11:07'!
initializePlatformEncodings	"Locale initializePlatformEncodings"	| platform |	PlatformEncodings ifNil: [ PlatformEncodings := Dictionary new ].	platform := PlatformEncodings at: 'default' ifAbsentPut: Dictionary new.	platform		at: 'default' put: 'iso8859-1';		at: 'Win32 CE' put: 'utf-8'.	platform := PlatformEncodings at: 'ja' ifAbsentPut: Dictionary new.	platform		at: 'default' put: 'shift-jis';		at: 'unix' put: 'euc-jp';		at: 'Win32 CE' put: 'utf-8'.			platform := PlatformEncodings at: 'ko' ifAbsentPut: Dictionary new.	platform		at: 'default' put: 'euc-kr';		at: 'Win32 CE' put: 'utf-8'.	platform := PlatformEncodings at: 'zh' ifAbsentPut: Dictionary new.	platform		at: 'default' put: 'gb2312';		at: 'unix' put: 'euc-cn';		at: 'Win32 CE' put: 'utf-8'.! !

!Locale class methodsFor: 'accessing' stamp: 'mir 7/15/2004 14:20'!
isoLanguage: isoLanguage	^self isoLanguage: isoLanguage isoCountry: nil! !

!Locale class methodsFor: 'accessing' stamp: 'mir 7/15/2004 14:31'!
isoLanguage: isoLanguage isoCountry: isoCountry	^self localeID: (LocaleID  isoLanguage: isoLanguage isoCountry: isoCountry)! !

!Locale class methodsFor: 'accessing' stamp: 'mir 7/15/2004 12:42'!
isoLocale: aString	! !

!Locale class methodsFor: 'private' stamp: 'mir 7/15/2004 16:44'!
knownLocales	"KnownLocales := nil"	^KnownLocales ifNil: [KnownLocales := self initKnownLocales]! !

!Locale class methodsFor: 'accessing' stamp: 'mir 7/13/2004 00:24'!
languageSymbol: languageSymbol	"Locale languageSymbol: #Deutsch"	^self isoLanguage: (LanguageSymbols at: languageSymbol)! !

!Locale class methodsFor: 'notification' stamp: 'MarcusDenker 3/24/2011 16:36'!
localeChanged! !

!Locale class methodsFor: 'notification' stamp: 'mir 6/30/2004 16:15'!
localeChangedListeners	^LocaleChangeListeners ifNil: [LocaleChangeListeners := OrderedCollection new]! !

!Locale class methodsFor: 'accessing' stamp: 'mir 7/15/2004 14:30'!
localeID: id	^self knownLocales at: id ifAbsentPut: [Locale new localeID: id]! !

!Locale class methodsFor: 'initialization' stamp: 'nk 8/29/2004 13:20'!
platformEncodings	PlatformEncodings isEmptyOrNil ifTrue: [ self initializePlatformEncodings ].	^PlatformEncodings! !

!Locale class methodsFor: 'system startup' stamp: 'jannik.laval 2/4/2010 15:14'!
startUp: resuming	| newID |	resuming ifFalse: [^self].	DateAndTime localTimeZone: (TimeZone		offset:  (Duration minutes: self current primTimezone)		name: 'Local Time'		abbreviation: 'LT').	(self activated)		ifTrue: [			newID := self current determineLocaleID.			newID ~= LocaleID current				ifTrue: [self switchToID: newID]]! !

!Locale class methodsFor: 'accessing' stamp: 'HilaireFernandes 5/6/2010 21:31'!
stringForLanguageNameIs: localeID 	"Answer a string for a menu determining whether the given  	symbol is the project's natural language"	^ (self current localeID = localeID		ifTrue: ['<yes>']		ifFalse: ['<no>'])		, localeID displayLanguage! !

!Locale class methodsFor: 'accessing' stamp: 'tak 8/4/2005 16:30'!
switchTo: locale 	"Locale switchTo: Locale isoLanguage: 'de'"	Current localeID = locale localeID		ifFalse: [Current := locale.			CurrentPlatform := locale.			self localeChanged]! !

!Locale class methodsFor: 'accessing' stamp: 'mir 7/15/2004 19:07'!
switchToID: localeID	"Locale switchToID: (LocaleID isoLanguage: 'de') "	self switchTo: (Locale localeID: localeID)! !

!LocaleID methodsFor: 'comparing' stamp: 'mir 7/15/2004 14:23'!
= anotherObject	self class == anotherObject class		ifFalse: [^false].	^self isoLanguage = anotherObject isoLanguage		and: [self isoCountry = anotherObject isoCountry]! !

!LocaleID methodsFor: 'accessing' stamp: 'mir 9/1/2005 14:17'!
displayCountry	^(ISOLanguageDefinition iso2Countries at: self isoCountry asUppercase ifAbsent: [ self isoCountry ]) ! !

!LocaleID methodsFor: 'accessing' stamp: 'tak 3/23/2006 12:26'!
displayLanguage	| language |	language := (ISOLanguageDefinition iso2LanguageTable				at: self isoLanguage				ifAbsent: [^ self isoLanguage]) language.	^ self isoCountry		ifNil: [language]		ifNotNil: [language , ' (' , self displayCountry , ')']! !

!LocaleID methodsFor: 'testing' stamp: 'mir 7/15/2004 14:34'!
hasParent	^self isoCountry notNil! !

!LocaleID methodsFor: 'comparing' stamp: 'mir 7/15/2004 14:23'!
hash	^self isoLanguage hash bitXor: self isoCountry hash! !

!LocaleID methodsFor: 'accessing' stamp: 'mir 7/15/2004 12:43'!
isoCountry	^isoCountry! !

!LocaleID methodsFor: 'accessing' stamp: 'mir 7/15/2004 12:43'!
isoLanguage	^isoLanguage! !

!LocaleID methodsFor: 'initialize' stamp: 'mir 7/15/2004 12:44'!
isoLanguage: langString isoCountry: countryStringOrNil	isoLanguage := langString.	isoCountry := countryStringOrNil! !

!LocaleID methodsFor: 'accessing' stamp: 'mir 7/21/2004 19:17'!
isoString	^self asString! !

!LocaleID methodsFor: 'accessing' stamp: 'mir 7/15/2004 14:34'!
parent	^self class isoLanguage: self isoLanguage! !

!LocaleID methodsFor: 'printing' stamp: 'tak 8/6/2007 12:16'!
posixName	"(LocaleID isoString: 'es-MX') posixName"	"(LocaleID isoString: 'es') posixName"	"language[_territory]"	^ self isoCountry		ifNil: [self isoLanguage]		ifNotNil: [self isoLanguage , '_' , self isoCountry]! !

!LocaleID methodsFor: 'printing' stamp: 'mir 7/15/2004 12:45'!
printOn: stream	"<language>-<country>"	stream nextPutAll: self isoLanguage.	self isoCountry		ifNotNil: [stream nextPut: $-; nextPutAll: self isoCountry]! !

!LocaleID methodsFor: 'printing' stamp: 'tak 11/15/2004 12:45'!
storeOn: aStream 	aStream nextPut: $(.	aStream nextPutAll: self class name.	aStream nextPutAll: ' isoString: '.	aStream nextPutAll: '''' , self printString , ''''.	aStream nextPut: $).! !

!LocaleID methodsFor: 'accessing' stamp: 'HilaireFernandes 5/6/2010 21:30'!
translator	^ NaturalLanguageTranslator localeID: self ! !

!LocaleID class methodsFor: 'accessing' stamp: 'mir 7/15/2004 15:09'!
current	^Locale current localeID! !

!LocaleID class methodsFor: 'instance creation' stamp: 'mir 7/15/2004 14:35'!
isoLanguage: langString	^self isoLanguage: langString isoCountry: nil! !

!LocaleID class methodsFor: 'instance creation' stamp: 'mir 7/15/2004 12:46'!
isoLanguage: langString isoCountry: countryStringOrNil	^self new isoLanguage: langString isoCountry: countryStringOrNil! !

!LocaleID class methodsFor: 'instance creation' stamp: 'mir 7/21/2004 13:59'!
isoString: isoString	"Parse the isoString (<language>-<country>) into its components and return the matching LocaleID"	"LocaleID isoString: 'en' "	"LocaleID isoString: 'en-us' "	| parts language country |	parts := isoString findTokens: #($- ).	language := parts first.	parts size > 1		ifTrue: [country := parts second].	^self isoLanguage: language isoCountry: country! !

!LocaleID class methodsFor: 'instance creation' stamp: 'HilaireFernandes 4/30/2010 18:00'!
posixName: aString 	^ self		isoString: (aString copyReplaceAll: '_' with: '-')! !

!LocaleID class methodsFor: 'accessing' stamp: 'bf 9/26/2007 16:24'!
previous	^Locale previous localeID! !

!NaturalLanguageFormTranslator methodsFor: 'accessing' stamp: 'yo 1/13/2005 11:15'!
generics	^generics ifNil: [generics := Dictionary new]! !

!NaturalLanguageFormTranslator methodsFor: 'accessing' stamp: 'yo 1/13/2005 11:27'!
localeID	^id! !

!NaturalLanguageFormTranslator methodsFor: 'accessing' stamp: 'yo 1/13/2005 11:26'!
localeID: anID	id := anID! !

!NaturalLanguageFormTranslator methodsFor: 'accessing' stamp: 'yo 1/13/2005 11:17'!
name: formName form: translatedForm 	self generics at: formName put: translatedForm.! !

!NaturalLanguageFormTranslator methodsFor: 'i/o' stamp: 'yo 1/13/2005 14:02'!
saveFormsOn: aStream	| rr |	rr := ReferenceStream on: aStream.	rr nextPut: {id isoString. generics}.	rr close.! !

!NaturalLanguageFormTranslator methodsFor: 'utilities' stamp: 'yo 1/13/2005 11:35'!
translate: aString	^ (self generics		at: aString ifAbsent: [nil]) deepCopy.	"Do you like to write 'form ifNotNil: [form deepCopy]'?"! !

!NaturalLanguageFormTranslator class methodsFor: 'accessing' stamp: 'yo 1/13/2005 11:13'!
cachedTranslations	"CachedTranslations := nil" 	^CachedTranslations ifNil: [CachedTranslations := Dictionary new]! !

!NaturalLanguageFormTranslator class methodsFor: 'cleanup' stamp: 'StephaneDucasse 3/9/2010 22:11'!
cleanUp	"Flush caches"	CachedTranslations := nil! !

!NaturalLanguageFormTranslator class methodsFor: 'accessing' stamp: 'yo 1/13/2005 11:13'!
isoLanguage: isoLanguage	"Return the generic language translator as there is no information about the country code"	^self isoLanguage: isoLanguage isoCountry: nil! !

!NaturalLanguageFormTranslator class methodsFor: 'accessing' stamp: 'yo 1/13/2005 11:13'!
isoLanguage: isoLanguage isoCountry: isoCountry	^self localeID: (LocaleID  isoLanguage: isoLanguage isoCountry: isoCountry)! !

!NaturalLanguageFormTranslator class methodsFor: 'i/o' stamp: 'StephaneDucasse 3/9/2010 22:10'!
loadFormsFrom: aStream	| rr pair inst |	rr := ReferenceStream on: aStream.	pair := rr next.	inst := self localeID: (LocaleID isoString: pair first).	pair second associationsDo: [:assoc |		inst name: assoc key form: assoc value].	^ inst.! !

!NaturalLanguageFormTranslator class methodsFor: 'accessing' stamp: 'yo 1/13/2005 11:13'!
localeID: localeID 	^ self cachedTranslations		at: localeID		ifAbsentPut: [self new localeID: localeID]! !

!NaturalLanguageTranslator class methodsFor: 'cleanup' stamp: 'MarcusDenker 4/22/2011 10:34'!
cleanUp	AllKnownPhrases := nil! !

!NaturalLanguageTranslator class methodsFor: 'translate' stamp: 'HilaireFernandes 5/13/2010 11:43'!
translate: aString	^ aString! !

!NaturalLanguageTranslator class methodsFor: 'translate' stamp: 'HilaireFernandes 5/13/2010 11:43'!
translate: aString toLocale: localeID	^ aString! !
ISOLanguageDefinition initialize!
Locale initialize!
