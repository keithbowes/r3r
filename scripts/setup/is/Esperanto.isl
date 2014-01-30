; *** Inno Setup version 5.1.1+ Esperanto messages ***
; Language: Esperanto
; Name: Hans Engel
; Email: celsus.software@gmail.com

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Esperanto
LanguageID=$0
LanguageCodePage=65001
; If the language you are translating to requires special font faces or
; sizes, uncomment any of the following entries and change them accordingly.
;DialogFontName=
;DialogFontSize=8
;WelcomeFontName=Verdana
;WelcomeFontSize=12
;TitleFontName=Arial
;TitleFontSize=29
;CopyrightFontName=Arial
;CopyrightFontSize=8

[Messages]

; *** Application titles
SetupAppTitle=Instalilo
SetupWindowTitle=Instalilo - %1
UninstallAppTitle=Malinstalilo
UninstallAppFullTitle=Malinstalilo de %1

; *** Misc. common
InformationTitle=Informoj
ConfirmTitle=Konfirmi
ErrorTitle=Eraro

; *** SetupLdr messages
SetupLdrStartupMessage=Tiu instalos je %1. Ĉu volas daŭrigi?
LdrCannotCreateTemp=Ne eblas krei provizoran dosieron. La instalilo haltos.
LdrCannotExecTemp=Ne eblas plenumigi dosieron en la provizora dosierujo. Instalilo haltos.

; *** Startup error messages
LastErrorMessage=%1.%n%nEraro %2: %3
SetupFileMissing=La dosiero %1 malaperis el la instala dosierujo. Bonvolu korekti la problemon aŭ akiri novan ekzempleron de la programo.
SetupFileCorrupt=La instalaj dosieroj estas koruptaj. Bonvolu akiri novan ekzempleron de la programo.
SetupFileCorruptOrWrongVer=La instalaj dosieroj estas koruptaj, aŭ estas malkongruaj kun tiu eldono. Bonvolu korekti la problemon aŭ akiri novan ekzempleron de la programo.
InvalidParameter=Nevalida parametro donita en la komandlinio:%n%n%1
SetupAlreadyRunning=La instalilo jam plenumiĝas.
WindowsVersionNotSupported=La programo ne estas plenumebla en la eldono de Vindozo, kiun vi uzas.
WindowsServicePackRequired=La programo postulas ĝisdatigan pakaĵon eldonon %2 aŭ poste de %1.
NotOnThisPlatform=Tiu programo malsukcesos en %1.
OnlyOnThisPlatform=Tiu programo devas plenumi en %1.
OnlyOnTheseArchitectures=Tiu programo nur povas instaliĝi en eldonoj de Vindozo destinitaj por la sekvaj arkitekturoj:%n%n%1
MissingWOW64APIs=La eldono de Vindozo, kiun vi uzas, ne inkluzivas funkciadon devigatan de Instalilo por fari 64-bitan instaladon. Por korekti tiun problemon, bonvolu instali la ĝisdatigan pakaĵon %1.
WinVersionTooLowError=Tiu programo bezonas je %1 eldono %2 aŭ poste.
WinVersionTooHighError=Tiu programo ne povas instaliĝi de %1 eldono %2 aŭ poste.
AdminPrivilegesRequired=Vi devas ensaluti kiel administranton dum instali tiun programon.
PowerUserPrivilegesRequired=Vi devas ensaluti kiel administranton aŭ kiel membron de la grupon "Power Users" dum instali tiun programon.
SetupAppRunningError=Instalilo malkovris ke %1 estas plenumanta.%n%nBonvolu fermi ĉiujn okazojn de ĝi nun, tiam klaki "Bone" por daŭrigi, aŭ "Rezigni" por haltigi.
UninstallAppRunningError=Malinstalilo malkovris ke %1 estas plenumanta.%n%nBonvolu fermi ĉiujn okazoj de ĝi nun, tiam klaki "Bone" por daŭrigi, aŭ "Rezigni" por Ĉesi.

; *** Misc. errors
ErrorCreatingDir=Instalilo ne povis krei la dosierujon "%1"
ErrorTooManyFilesInDir=Ne eblas krei dosieron en la dosierujo "%1" ĉar ĝi enhavas tro da dosieroj

; *** Setup common messages
ExitSetupTitle=Eliranta Instalilon
ExitSetupMessage=Instalilo estas ne finita. Se vi eliras nun, la programo ne estos instalota.%n%nVi povas fini la instaladon pli malfrue.%n%nĈu Ĉesigi la instalilon?
AboutSetupMenuItem=&Informoj...
AboutSetupTitle=Pri la instalilo
AboutSetupMessage=%1 eldono %2%n%3%n%n%1 paĝaro:%n%4
AboutSetupNote=
TranslatorNote=Tradukis de celsus.software@gmail.com.

; *** Buttons
ButtonBack=< &Antaŭen
ButtonNext=&Sekva >
ButtonInstall=&Instali
ButtonOK=&Bone
ButtonCancel=&Rezigni
ButtonYes=&Jes
ButtonYesToAll=Jes al &Ĉio
ButtonNo=&Ne
ButtonNoToAll=N&e al ĉio
ButtonFinish=&Fini
ButtonBrowse=F&oliumi...
ButtonWizardBrowse=Fo&liumi...
ButtonNewFolder=&Krei Novan Dosierujon

; *** "Select Language" dialog messages
SelectLanguageTitle=Elekti Instalan Lingvon
SelectLanguageLabel=Elekti la lingvon por uzi dum la instalado:

; *** Common wizard text
ClickNext=Klaki "Sekva" por daŭrigi, aŭ "Rezigni" por haltigi.
BeveledLabel=
BrowseDialogTitle=Foliumi Por Dosierujo
BrowseDialogLabel=Elektu dosierujon el la listo suba, tiam klaku OK.
NewFolderName=Nova Dosierujo

; *** "Welcome" wizard page
WelcomeLabel1=Bonvenon al la [name]-Instalilo
WelcomeLabel2=Tio instalos je [name/ver] en via komputilo.%n%nEstas rekomendita ke vi fermu ĉiujn aliajn fenestrojn.

; *** "Password" wizard page
WizardPassword=Pasvorto
PasswordLabel1=Tiu instalilo estas protektita per pasvorto.
PasswordLabel3=Bonvolu tajpi la pasvorton, tiam Sekva por daŭrigi. Ĉe pasvortoj uskleco gravas.
PasswordEditLabel=&Pasvorto:
IncorrectPassword=La pasvorto, kiun vi tajpis, estas neĝusta.. Bonvolu provi denove.

; *** "License Agreement" wizard page
WizardLicense=Permesilo
LicenseLabel=Bonvolu legi la sekvajn gravajn informojn antaŭ ol daŭri.
LicenseLabel3=Bonvolu legi la jenan permesilon. Vi devas akcepti a kondiĉojn de tio antaŭ instali.
LicenseAccepted=Mi &akceptas la permesilon
LicenseNotAccepted=Mi &malakceptas la permesilon

; *** "Information" wizard pages
WizardInfoBefore=Informoj
InfoBeforeLabel=Bonvolu legi la jenajn gravajn informojn antaŭ daŭrigi.
InfoBeforeClickLabel=Kiam vi estas servopreta por daŭri kun Instalilo, klaki "Sekva".
WizardInfoAfter=Informado
InfoAfterLabel=Bonvolu legas la sekvonta grava informado antaŭ daŭrigi.
InfoAfterClickLabel=Kiam vi estas servopreta por daŭri kun Instalilo, klaki "Sekva".

; *** "User Information" wizard page
WizardUserInfo=Uzanto-informoj
UserInfoDesc=Bonvolu tajpi viajn informojn.
UserInfoName=&Uzanto-Nomo:
UserInfoOrg=&Organizaĵo:
UserInfoSerial=&Sekreta Numero:
UserInfoNameRequired=Vi devas tajpi nomon.

; *** "Select Destination Location" wizard page
WizardSelectDir=Elekto postulas lokon
SelectDirDesc=Kie [name] instaliĝos?
SelectDirLabel3=Instalilo instalos je [name] en la jena dosierujo.
SelectDirBrowseLabel=Por daŭrigi klaku "Sekva". Se vi volas elekti alian dosierujon, klaki "Foliumi".
DiskSpaceMBLabel=Minimume [mb] megabajtoj da neuzata spaco estas bezonata.
ToUNCPathname=Instalilo ne povas instali al UNC dosiernomo. Se vi provas instali al reto, vi bezonos diskon por retaj servoj.
CannotInstallToNetworkDrive=La instalilo ne povas instali en retan diskaparaton.
CannotInstallToUNCPath=La instalilo ne povas instali en UNC-vojon.
InvalidPath=Vi devas tajpi kompleta vojo kun disknomo; ekzemple:%n%nC:\APP%n%naŭ UNC vojo en la demandilo:%n%n\\server\share
InvalidDrive=La disko aŭ UNC ago, kiun vi elektis, ne ekzistas aŭ ne estas atingebla. Bonvolu elekti alian.
DiskSpaceWarningTitle=Nesufiĉa Disko-spaco
DiskSpaceWarning=Instalilo bezonas minimume %1 kilobajtojn da neuzata spaco por la instalado, sed la elektita disko havas nur %2 kilobajtojn.%n%nĈu volas vi daŭrigi malgraŭ tio?
DirNameTooLong=La dosierujo-nomo estas tro longa.
InvalidDirName=La dosierujo-nomo ne estas valida.
BadDirName32=Dosierujo-nomoj ne povas inkluzivas iun ajn el la jenaj signoj:%n%n%1
DirExistsTitle=Dosierujo Ekzistas
DirExists=La dosierujo:%n%n%1%n%njam ekzistas. Ĉu vi volas instali al tiu dosierujo malgraŭ tio?
DirDoesntExistTitle=Dosierujo neekzistas
DirDoesntExist=La dosierujo:%n%n%1%n%nne ekzistas. Ĉu volas vi, ke la dosierujo kreiĝu?

; *** "Select Components" wizard page
WizardSelectComponents=Elekti Komponantojn
SelectComponentsDesc=Kiuj komponantoj instaliĝos?
SelectComponentsLabel2=Elekti la komponantojn, kiujn vi volas instali; malelekti la komponantojn, kiujn vi ne volas instali. Klaki "Sekva" kiam vi esta preta por daŭrigi.
FullInstallation=Kompleta instalaĵo
; if possible don't translate 'Compact' as 'Minimala  (I mean 'Minimala  in your language)
CompactInstallation=Kompakta instalaĵo
CustomInstallation=Speciala instalaĵo
NoUninstallWarningTitle=Komponantoj Ekzistas
NoUninstallWarning=Instalilo malkovris ke la sekvaj komponantoj estas jam instalitaj en via komputilo:%n%n%1%n%nMalelekti la komponantojn por ne malinstali ilin.%n%nĈu vi volas vi daŭri malgraŭ tio?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Nuna elektaĵo bezonas minimume [mb] megabajtojn da diskspaco.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Elekti Aldonajn Taskojn
SelectTasksDesc=Kiuj aldonaj taskoj funkciis?
SelectTasksLabel2=Elekti aldonajn taskojn, kiuj vi volas la instalilon plenumigi dum instali je [name], tiam klaki "Sekva".

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Elekti starto-menuon Dosierujon
SelectStartMenuFolderDesc=Kie lokas instalan simbolan ligilon de la programo?
SelectStartMenuFolderLabel3=Instalilo kreis simbolan ligilon de la program en la jena starto-menua dosierujo.
SelectStartMenuFolderBrowseLabel=Por daŭrigi, klaki "Sekva". Se vi volas elekti alian dosierujon, klaki Antaŭen.
MustEnterGroupName=Vi devas tajpi dosierujnomo.
GroupNameTooLong=La dosierujo-nomo estas tro longa.
InvalidGroupName=La dosierujo-nomo ne esta valida.
BadGroupName=La dosierujnomo ne povas inkluzivi iun ajn el la sekvaj signoj:%n%n%1
NoProgramGroupCheck2=&Malfari kreadon de starto-menua dosierujo

; *** "Ready to Install" wizard page
WizardReady=Preta por instali
ReadyLabel1=Instalilo estas nun preta por komenci instali je [name] en via komputilo.
ReadyLabel2a=Klaki "Instali" por daŭrigi la instalilon, aŭ klaki "Antaŭen" se vi volas inspekti aŭ ŝanĝi iun agordaĵon.
ReadyLabel2b=Klaki "Instali" por daŭrigi la instalilon.
ReadyMemoUserInfo=Uzanto-informoj:
ReadyMemoDir=Elektita loko:
ReadyMemoType=Instalo-tipo:
ReadyMemoComponents=Elektitaj komponantoj:
ReadyMemoGroup=Starto-menua dosierujo:
ReadyMemoTasks=Aldonaj taskoj:

; *** "Preparing to Install" wizard page
WizardPreparing=Pretigi la instalilon
PreparingDesc=Instalilo pretigas instali je [name] en via komputilo.
PreviousInstallNotCompleted=La instalado/malinstalado de antaŭa programo ne finis. Vi bezonos restartigi vian komputilon por fini tiun instaladon.%n%nPost restartigo, la instalado de [name] finiĝos.
CannotContinue=Instalilo ne povas daŭrigi. Bonvolu klaki "Rezigni" por halti.
ApplicationsFound=La jenaj programoj uzas dosierojn, kiuj devas esti ĝisdatigitaj de la instalilo. Estas rekomendinde permesi al la instalilo aŭtomate haltigi tiujn programojn.
ApplicationsFound2=La jenaj programoj uzas dosierojn, kiuj devas esti ĝisdatigitaj de la instalilo. Estas rekomendinde permesi al la instalilo aŭtomate haltigi tiujn programojn. Post instalado la instalilo provos restarigi la programojn.
CloseApplications=&Aŭtomate haltigi la programojn
DontCloseApplications=&Ne haltigi la programojn
ErrorCloseApplications=La instalilo ne povis haltigi ĉiujn programojn. Estas rekomendite, ke vi haltigas la programojn , kiuj uzas dosierojn, kiuj devas esti ĝisdatigitaj de la instalilo antaŭ ol vi daŭrigas.

; *** "Installing" wizard page
WizardInstalling=Instalanta
InstallingLabel=Bonvolu atendi dum Instalilo instalas je [name] en via komputilo.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Finis la instaladon de [name]
FinishedLabelNoIcons=Finis la instaladon de [name] en via komputilo.
FinishedLabel=Finis la instaladon de [name] en via komputilo. La programo povas plenumiĝos per klakado de la instalitaj piktogramoj.
ClickFinish=Klaki "Fini" por fini la instaladon.
FinishedRestartLabel=Por finigi la instaladon de [name], Instalilo devas restartigi vian komputilon. Ĉu volas vi restartigi nun?
FinishedRestartMessage=Por finigi la instaladon de [name], Instalilo devas restartigi vian komputilon.%n%nĈu volas vi restartigi nun?
ShowReadmeCheck=Jes, mi volas legi la dosieron README.
YesRadio=&Jes, restartigi la komputilon nun
NoRadio=&Ne, mi restartigos la komputilon pli malfrue
; used for example as 'Run MyProg.exe'
RunEntryExec=Plenumigi je %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Legi je %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Instalilo Bezonas la Sekvan Disko
SelectDiskLabel2=Bonvolu enmeti Diskon %1 kaj klaki "Bone".%n%nSe la dosieroj en tiu disko ne povas trovi dosierujon alian ol tiun montris sube, tajpu la ĝustan nomon aŭ klaki "Foliumi".
PathLabel=&Nomo:
FileNotInDir2=La dosiero "%1" ne troviĝis en "%2". Bonvolu enmeti la ĝustan diskon aŭ elekti alian dosierujon.
SelectDirectoryLabel=Bonvolu difini la lokon de la sekva disko.

; *** Installation phase messages
SetupAborted=Instalilo ne finis.%n%nBonvolu korekti la problemon kaj refoje provi la instalilon.
EntryAbortRetryIgnore=Klaki "Retry" por provi denove, "Ignore" por daŭrigi malgraŭ tio, aŭ "Abort" por haltigi la instaladon.

; *** Installation status messages
StatusClosingApplications=Haltiganta programojn...
StatusCreateDirs=Kreanta la dosierujojn...
StatusExtractFiles=Elpakanta la dosierojn...
StatusCreateIcons=Kreanta la simbolajn ligilojn...
StatusCreateIniEntries=Kreanta INI-enskribojn...
StatusCreateRegistryEntries=Kreanta registrejajn enskribojn...
StatusRegisterFiles=Registranta la dosierojn...
StatusSavingUninstall=Metanta malinstaladajn informojn al memoro...
StatusRunProgram=Plenumiganta programojn...
StatusRestartingApplications=Restartiganta programojn...
StatusRollback=Foriganta ŝanĝojn...

; *** Misc. errors
ErrorInternal2=Interna eraro: %1
ErrorFunctionFailedNoCode=%1 ne vokita
ErrorFunctionFailed=%1 malsukcesis; kodo %2
ErrorFunctionFailedWithMessage=%1 malsukcesis; kodo %2.%n%3
ErrorExecutingProgram=Ne povas plenumigi programon:%n%1

; *** Registry errors
ErrorRegOpenKey=Eraro malfermanta registrejan ŝlosilon:%n%1\%2
ErrorRegCreateKey=Eraro kreanta registrejan ŝlosilon:%n%1\%2
ErrorRegWriteKey=Eraro skribanta registrejan ŝlosilon:%n%1\%2

; *** INI errors
ErrorIniEntry=Eraro krei INI-enskribon en la dosiero "%1".

; *** File copying errors
FileAbortRetryIgnore=Klaki "Retry" por provi denove, "Ignore" por preterlasi la dosieron (malrekomendita), aŭ "Abort" por haltigi la instaladon.
FileAbortRetryIgnore2=Klaki "Retry" por provi denove, "Ignore" por daŭrigi malgraŭ tio (malrekomendita), aŭ "Abort" por haltigi la instaladon.
SourceIsCorrupted=La dosiero estas korupta
SourceDoesntExist=La dosiero "%1" ne ekzistas
ExistingFileReadOnly=La jama dosiero estas nurlega.%n%nKlaki "Retry" por forigi tiun atributon kaj provi denove, "Ignore" por preterlasi la dosieron, aŭ "Abort" por haltigi la instaladon.
ErrorReadingExistingDest=Eraro okazis dum legi la jaman dosieron:
FileExists=La dosiero jam ekzistas.%n%nĈu vi volas, ke la instalalilo anstataŭigi ĝin?
ExistingFileNewer=La jama dosiero estas pli nova ol la instalota. Oni rekomendas, ke vi tenas la jaman dosieron.%n%nĈu volas vi teni la jaman dosieron?
ErrorChangingAttr=Eraro okazis dum provi sanĝi la atributojn de la jama dosiero:
ErrorCreatingTemp=Eraro okazis dum krei dosiero en la dosierujo elektita:
ErrorReadingSource=Eraro okazis dum legi la dosieron:
ErrorCopying=Eraro okazi dum kopii la dosieron:
ErrorReplacingExistingFile=Eraro okazis dum anstataŭigi la jaman dosieron:
ErrorRestartReplace=Restartigi/Anstataŭigi malsukcesis:
ErrorRenamingTemp=Eraro okazis dum provi renomi dosieron en la dosierujo elektita:
ErrorRegisterServer=Ne eblas registri la DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 malsukcesis kun erarokodo %1
ErrorRegisterTypeLib=Ne eblas registri la tipo-bibliotekon: %1

; *** Post-installation errors
ErrorOpeningReadme=Eraro okazis dum malfermi la dosieron README.
ErrorRestartingComputer=Instalilo ne povas restartigi la komputilon. Bonvolu faras tio permane.

; *** Uninstaller messages
UninstallNotFound=Dosiero "%1" ne ekzistas. Ne povas malinstali.
UninstallOpenError=Dosiero "%1" ne eblas malfermiĝi. Ne povas malinstali
UninstallUnsupportedVer=La malinstalilo rekordo de efikado "%1" estas en formo ne nerekonita per ĉi tiu eldono de la malinstalilo. Ne povas malinstali
UninstallUnknownEntry=Nekonata enskribo (%1) renkontiĝis en la malinstalilo
ConfirmUninstall=Ĉu certas vi, ke vi volas tute forigi je %1 kaj ĉiujn el ĝiaj komponantoj?
UninstallOnlyOnWin64=Tiu instalilo povas malinstali nur en 64-bita Vindozo.
OnlyAdminCanUninstall=Tiu instalilo povas malinstali nur per uzanto kun administradaj privilegioj.
UninstallStatusLabel=Bonvolu atendi dum %1 foriĝas de via komputilo.
UninstalledAll=%1 estis sukcesplene forigita de via komputilo.
UninstalledMost=Malinstalado de %1 finiĝis.%n%nIuj komponantoj ne eblis foriĝintaj. Tiuj povas foriĝintaj permane.
UninstalledAndNeedsRestart=Por finigi la malinstaladon de %1, vi devas restartigi la komputilon.%n%nĈu vi volas restartigi nun?
UninstallDataCorrupted=La dosiero %1 estas korupta. Ne eblas malinstali

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Forigi Kunuzata Dosiero?
ConfirmDeleteSharedFile2=La sistemo indikas ke la sekva kunuzata dosiero ne estas en uzo per iu ajn programoj. Ĉu volas vi por tio, ke Malinstalilo tute forigu tiun kunuzatan dosieron?%n%nSe iuj programoj estas nun ankoraŭ uzantaj tiun dosieron kaj ĝi foriĝos, la programoj eble ne funkcios konvene. Se vi estas necerta, elektu "Ne". Se vi restigos la dosieron en via sistemo, la dosiero ne difektos la sistemon.
SharedFileNameLabel=Dosiernomo:
SharedFileLocationLabel=Loko:
WizardUninstalling=Malinstala Stato
StatusUninstalling=Malinstalanta %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Instalanta je %1.
ShutdownBlockReasonUninstallingApp=Malinstalanta je %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 eldono %2
AdditionalIcons=Aldonaj piktogramoj:
CreateDesktopIcon=Krei &labortablan piktogramon
CreateQuickLaunchIcon=Krei piktogramon por &rapida plenumiĝo
ProgramOnTheWeb=%1 en la Interreto
UninstallProgram=Malinstali je %1
LaunchProgram=Plenumigi je %1
AssocFileExtension=&Asocii je %1 kun la dosiersufikso %2
AssocingFileExtension=Asocianta %1 kun la dosiersufikso %2...
AutoStartProgramGroupDescription=Starto:
AutoStartProgram=Aŭtomate startigi je %l
AddonHostProgramNotFound=%1 ne troveblis en la dosierujo elektita.%n%nĈu senkonsidere daŭri?

; vi:ft=dosini:spl=eo
