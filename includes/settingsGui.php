<?php

/**
  * @package GUI
*/

/**
  * Create the "Appearance" tab
  * @param GtkNotebook The parent notebook.
*/
function createAppearancePage($notebook)
{
  $appearancePage = &new GtkVBox(false, 5);
  $appearanceLabel = &new GtkLabel(SET_APP_TAB);

  $rcCheckBtn = &new GtkCheckButton(SET_YOUR_RC);
  $rcCheckBtn->set_active(getSetting('use-rc-file'));
  $appearancePage->pack_start($rcCheckBtn, false);

  $rcBox = &new GtkHBox();
  $appearancePage->pack_start($rcBox, false);

  $rcField = createEdit(getSetting('rc-file'));
  $rcField->set_sensitive(getSetting('use-rc-file'));
  $rcField->connect('changed', 'settingsEntryChanged', 'rc-file');
  $rcBox->pack_start($rcField, false);

  $rcBrowse = &new GtkButton(SET_BROWS_BTN);
  $rcBrowse->set_sensitive(getSetting('use-rc-file'));
  $rcBrowse->connect('clicked', 'createFileSelection', array($rcField, FS_RC, 'rc-file', $rcField));
  $rcBox->pack_start($rcBrowse, false, false, 3);

  $rcCheckBtn->connect('toggled', 'checkToggled', array(array($rcField, $rcBrowse), 'use-rc-file'));

  $notebook->append_page($appearancePage, $appearanceLabel);
}

/**
  * Create the "Feeds" tab
  * @param GtkNotebook The parent notebook.
*/
function createFeedPage($notebook)
{
  $feedPage = &new GtkVBox(false);
  $feedLabel = &new GtkLabel(SET_FEEDS_TAB);

  $warningsCheck = &new GtkCheckButton(SET_WARNINGS);
  $warningsCheck->set_active(getSetting('show-warnings'));
  $warningsCheck->connect('toggled', 'checkToggled', array(array(), 'show-warnings'));
  $feedPage->pack_start($warningsCheck, false); 

  $wrapDescCheck = &new GtkCheckButton(SET_DESC_WRAP);
  $wrapDescCheck->set_active(getSetting('wrap-desc'));
  $wrapDescCheck->connect('toggled', 'checkToggled', array(array(), 'wrap-desc'));
  $feedPage->pack_start($wrapDescCheck, false);  

  $feedTitleCheck = &new GtkCheckButton(SET_TITLES_ONLY);
  $feedTitleCheck->set_active(getSetting('display-feed-title-only'));
  $feedTitleCheck->connect('toggled', 'checkToggled', array(array(), 'display-feed-title-only'));
  $feedPage->pack_start($feedTitleCheck, false);

  $hideCachedCheck = &new GtkCheckButton(SET_HIDE_CACHED);
  $hideCachedCheck->set_active(getSetting('hide-cached-feeds'));
  $hideCachedCheck->connect('toggled', 'checkToggled', array(array(), 'hide-cached-feeds'));
  $feedPage->pack_start($hideCachedCheck, false);

  $mimeGuessCheck = &new GtkCheckButton(SET_MIME_GUESS);
  $mimeGuessCheck->set_active(getSetting('enable-mime-guess'));
  $mimeGuessCheck->connect('toggled', 'checkToggled', array(array(), 'enable-mime-guess'));
  $feedPage->pack_start($mimeGuessCheck, false);

  $notebook->append_page($feedPage, $feedLabel);
}

/**
  * Create the "HTTP: Basic" tab
  * @param GtkNotebook The parent notebook.
*/
function createHttpPage($notebook)
{
  $httpPage = &new GtkVBox();
  $httpLabel = &new GtkLabel(SET_HTTP_BASIC);
  $use_proxy = getSetting('use-proxy');

  $toBox = &new GtkHBox(false, 5);
  $httpPage->pack_start($toBox);

  $toPreLabel = &new GtkLabel(SET_TO_PREF);
  $toBox->pack_start($toPreLabel, false);

  $toField = createEdit(getSetting('timeout-sec'));
  $toField->connect('changed', 'settingsEntryChanged', 'timeout-sec');
  $toBox->pack_start($toField, false);

  $toPostLabel = &new GtkLabel(SET_TO_SUF);
  $toBox->pack_start($toPostLabel, false);

  $httpPageHSep1 = &new GtkHSeparator();
  $httpPage->pack_start($httpPageHSep1, false, false, 5);

  $httpClientBox = &new GtkHBox();
  $httpPage->pack_start($httpClientBox);

  $httpClientPreLabel = &new GtkLabel(SET_HTTP_CLIENT);
  $httpClientBox->pack_start($httpClientPreLabel, false);

  $httpClientField = createEdit(getSetting('http-client'));
  $httpClientField->connect('changed', 'settingsEntryChanged', 'http-client');
  $httpClientBox->pack_start($httpClientField, false, false, 3);

  $httpClientBrowse = &new GtkButton(SET_BROWS_BTN);
  $httpClientBrowse->connect('clicked', 'createFileSelection', array($httpClientField, FS_CLIENT, 'http-client', $httpClientField));
  $httpClientBox->pack_start($httpClientBrowse, false, false, 2);

  $proxyFrame = &new GtkFrame(SET_PROXY_LBL);
  $httpPage->pack_start($proxyFrame);

  $proxyFrameBox = &new GtkVBox();
  $proxyFrame->add($proxyFrameBox);

  $useProxyCheck = &new GtkCheckButton(SET_USE_PROXY);
  $useProxyCheck->set_active($use_proxy);
  $proxyFrameBox->pack_start($useProxyCheck);

  $proxyBox = &new GtkHBox();
  $proxyBox->set_sensitive($use_proxy);
  $proxyFrameBox->pack_start($proxyBox);

  $useProxyCheck->connect('toggled', 'checkToggled', array(array($proxyBox), 'use-proxy'));

  $proxyLabels = &new GtkVBox();
  $proxyBox->pack_start($proxyLabels, false);

  $proxyAddrLabel = &new GtkLabel(SET_PROXY_IP);
  $proxyLabels->pack_start($proxyAddrLabel);

  $proxyPortLabel = &new GtkLabel(SET_PROXY_PORT);
  $proxyLabels->pack_start($proxyPortLabel);

  $proxyFields = &new GtkVBox(false);
  $proxyBox->pack_start($proxyFields, false);

  $proxyAddrField = createEdit(getSetting('proxy-addr'));
  $proxyAddrField->connect('changed', 'settingsEntryChanged', 'proxy-addr');
  $proxyFields->pack_start($proxyAddrField);

  $proxyPortField = createEdit(getSetting('proxy-port'));
  $proxyPortField->connect('changed', 'settingsEntryChanged', 'proxy-port');
  $proxyFields->pack_start($proxyPortField);

  $notebook->append_page($httpPage, $httpLabel);
}

/**
  * Create the "HTTP: Headers" tab
  * @param GtkNotebook The parent notebook.
*/
function createHttpHeaderPage($notebook)
{
  $httpHeaderPage = &new GtkVBox();
  $httpHeaderPageLabel = &new GtkLabel(SET_HEADERS);

  $httpHeaderRespFrame = &new GtkFrame(SET_HDR_LBL);
  $httpHeaderPage->pack_start($httpHeaderRespFrame, false);

  $httpHeaderRespTable = &new GtkTable(2, 4);
  $httpHeaderRespFrame->add($httpHeaderRespTable);

  $uaLabel = &new GtkLabel(SET_HDR_UA);
  $httpHeaderRespTable->attach($uaLabel, 0, 1, 0, 1);

  $acceptLabel = &new GtkLabel(SET_HDR_AT);
  $httpHeaderRespTable->attach($acceptLabel, 0, 1, 1, 2);

  $langLabel = &new GtkLabel(SET_HDR_LANG);
  $httpHeaderRespTable->attach($langLabel, 0, 1, 2, 3);

  $uaField = createEdit(getSetting('user-agent'));
  $uaField->set_sensitive(getSetting('use-custom-user-agent'));
  $uaField->connect('changed', 'settingsEntryChanged', 'user-agent');
  $httpHeaderRespTable->attach($uaField, 1, 2, 0, 1);

  $acceptField = createEdit(getSetting('accept-types'));
  $acceptField->set_sensitive(getSetting('use-custom-accept-types'));
  $acceptField->connect('changed', 'settingsEntryChanged', 'accept-types');
  $httpHeaderRespTable->attach($acceptField, 1, 2, 1, 2);

  $langField = createEdit(getSetting('accept-langs'));
  $langField->set_sensitive(getSetting('use-custom-accept-langs'));
  $langField->connect('changed', 'settingsEntryChanged', 'accept-langs');
  $httpHeaderRespTable->attach($langField, 1, 2, 2, 3);

  $uaCustomCheck = &new GtkCheckButton(SET_USE_CUSTOM);
  $uaCustomCheck->set_active(getSetting('use-custom-user-agent'));
  $uaCustomCheck->connect('toggled', 'checkToggled', array(array($uaField), 'use-custom-user-agent'));
  $httpHeaderRespTable->attach($uaCustomCheck, 2, 3, 0, 1);

  $acceptCustomCheck = &new GtkCheckButton(SET_USE_CUSTOM);
  $acceptCustomCheck->set_active(getSetting('use-custom-accept-types'));
  $acceptCustomCheck->connect('toggled', 'checkToggled', array(array($acceptField), 'use-custom-accept-types'));
  $httpHeaderRespTable->attach($acceptCustomCheck, 2, 3, 1, 2);

  $langCustomCheck = &new GtkCheckButton(SET_USE_CUSTOM);
  $langCustomCheck->set_active(getSetting('use-custom-accept-langs'));
  $langCustomCheck->connect('toggled', 'checkToggled', array(array($langField), 'use-custom-accept-langs'));
  $httpHeaderRespTable->attach($langCustomCheck, 2, 3, 2, 3);

  $notebook->append_page($httpHeaderPage, $httpHeaderPageLabel);
}

/**
  * Create the "Mail" tab
  * @param GtkNotebook The parent notebook.
*/
function createMailPage($notebook)
{
  $mailPage = &new GtkVBox();
  $mailLbl = &new GtkLabel(SET_MAIL);

  $mailTableBox = &new GtkVBox();
  $mailPage->pack_start($mailTableBox, false);

  $mailFrame = &new GtkFrame(SET_MAIL);
  $mailTableBox->pack_start($mailFrame, true, false);

  $mailTable = &new GtkTable(2, 3);
  $mailFrame->add($mailTable);

  $browserLbl = &new GtkLabel(SET_HTTP_CLIENT);
  $mailTable->attach($browserLbl, 0, 1, 0, 1);

  $browserField = createEdit(getSetting('http-client'));
  $browserField->connect('changed', 'settingsEntryChanged', 'mail-client-cl');
  $mailTable->attach($browserField, 1, 2, 0, 1);

  $browserBrowse = &new GtkButton(SET_BROWS_BTN);
  $browserBrowse->connect('clicked', 'createFileSelection', array($browserField, FS_CLIENT, 'http-client', $browserField));
  $mailTable->attach($browserBrowse, 2, 3, 0, 1);

  $mailClientLbl = &new GtkLabel(SET_MAIL_CLIENT);
  $mailTable->attach($mailClientLbl, 0, 1, 1, 2);

  $mailClientField = createEdit(getSetting('mail-client-cl'));
  $mailClientField->connect('changed', 'settingsEntryChanged', 'mail-client-cl');
  $mailTable->attach($mailClientField, 1, 2, 1, 2);

  $mailerBrowse = &new GtkButton(SET_BROWS_BTN);
  $mailerBrowse->connect('clicked', 'createFileSelection', array($mailClientField, FS_CLIENT, 'mail-client-cl', $browserField));
  $mailTable->attach($mailerBrowse, 2, 3, 1, 2);

  $editorLbl = &new GtkLabel(SET_EDITOR);
  $mailTable->attach($editorLbl, 0, 1, 2, 3);

  $editorField = createEdit(getSetting('editor'));
  $editorField->connect('changed', 'settingsEntryChanged', 'editor');
  $mailTable->attach($editorField, 1, 2, 2, 3);

  $editorBrowse = &new GtkButton(SET_BROWS_BTN);
  $editorBrowse->connect('clicked', 'createFileSelection', array($editorField, FS_CLIENT, 'editor', $editorField));
  $mailTable->attach($editorBrowse, 2, 3, 2, 3);

  $notebook->append_page($mailPage, $mailLbl);
}

/**
  * Create a notebook widget to separate different types of settings.
  * @param GtkVBox The box on which to pack the notebook.
  * @return GtkNotebook The created notebook.
*/
function createNotebook($parent)
{
  $notebook = &new GtkNotebook();
  $parent->pack_start($notebook);
  createAppearancePage($notebook);
  createFeedPage($notebook);
  createHttpPage($notebook);
  createHttpHeaderPage($notebook);
  createMailPage($notebook);
  return $notebook;
}

/**
  * Show the settings dialog.
*/
function showSettingsDialog()
{
  $dialog = &new GtkWindow(GTK_WINDOW_DIALOG);
  $dialog->set_title(SET_TITLE);
  $dialog->set_policy(false, false, true);

  $dlgBox = &new GtkVBox();
  $dialog->add($dlgBox);

  $notebook = createNotebook($dlgBox);

  $buttonBox = &new GtkHButtonBox();
  $buttonBox->set_layout(GTK_BUTTONBOX_END);
  $buttonBox->set_spacing(0);
  $dlgBox->pack_end($buttonBox, false, false, 3);

  $okBtn = &new GtkButton(SET_OK_BTN);
  $okBtn->set_flags(GTK_CAN_DEFAULT);
  $okBtn->connect('clicked', 'commitSettings');
  $okBtn->connect('clicked', 'killWidget', $dialog);

  $cancelBtn = &new GtkButton(SET_CANCEL_BTN);
  $cancelBtn->set_flags(GTK_CAN_DEFAULT);
  $cancelBtn->connect('clicked', 'killWidget', $dialog);

  $saveBtn = &new GtkButton(SET_SAVE_BTN);
  $saveBtn->set_flags(GTK_CAN_DEFAULT);
  $saveBtn->connect('clicked', 'saveSettings');
  $saveBtn->connect('clicked', 'killWidget', $dialog);

  $buttonBox->pack_start($okBtn);
  $buttonBox->pack_start($cancelBtn);
  $buttonBox->pack_start($saveBtn);

  $okBtn->grab_default();

  $dialog->show_all();
}

?>
