Unit uAegysConsts;

{$I ..\Includes\uAegys.inc}

{
   Aegys Remote Access Project.
  Criado por XyberX (Gilbero Rocha da Silva), o Aegys Remote Access Project tem como objetivo o uso de Acesso remoto
  Gratuito para utilização de pessoas em geral.
   O Aegys Remote Access Project tem como desenvolvedores e mantedores hoje

  Membros do Grupo :

  XyberX (Gilberto Rocha)    - Admin - Criador e Administrador  do pacote.
  Wendel Fassarela           - Devel and Admin
  Mobius One                 - Devel, Tester and Admin.
  Gustavo                    - Devel and Admin.
  Roniery                    - Devel and Admin.
  Alexandre Abbade           - Devel and Admin.
  e Outros como você, venha participar também.
}

Interface

Uses
 SysUtils, Classes, Variants;

Const
 //Aegys Basics
 cAeBufferVersion      = 12;
 cDelayThread          = 2;
 cFrameSkip            = 4;
 cServiceTimeout       = 1000;
 cLimitSource          = 500;
 cResuqestTimeout      = 7000;
 cConnectionTimeout    = 3000;
 //Messages
 cCaptureTitle         = 'Remote Access, Connection "%s", FPS "%d"';
 cCantExecDisconnected = 'Can''''t Execute Action when Disconnected';
 cCantJoinDisconnected = 'Can''''t join when Disconnected';
 cInvalidBufferData    = 'Invalid Buffer Data...';
 cInvalidHeaderData    = 'Invalid Header Data...';
 cSocketDisconnected   = 'Server Disconnected...';
 cOutOfRange           = 'Element out of Range...';
 cPackInvalidSize      = 'Invalid Pack Size...';
 cPackListNotAssigned  = 'PackList not Assigned...';
 cThreadCancel         = 'Thread Cancel...';
 cDisconnectedByServer = 'Disconnected By Server...';
 //Commands
 cStatusDesc           = '<|AEDATASTATUS|>';
 cFindID               = '<|FINDID|>';
 cIDExistsReqPass      = '<|IDEXISTS!REQUESTPASSWORD|>';
 cIDNotFound           = '<|IDNOTFOUND|>';
 cConnectedPeer        = '<|CONNECTED|>';
 cIncommingConnect     = '<|INCOMMINGCONNECT|>';
 cSendDataError        = '<|SENDDATAERROR|>';
 cSendDataErrorID      = '<|SENDDATAERRORID|>';
 cDisconnectAllPeers   = '<|DISCONNECTALLPEERS|>';
 cDisconnectPeer       = '<|DISCONNECTPEER|>';
 cDisconnectedPeer     = '<|DISCONNECTED|>';
 cKickPeer             = '<|KICK|>';
 cPing                 = '<|PING|>';
 cPong                 = '<|PONG|>';
 cCheckPass            = '<|CHECKIDPASSWORD|>';
 cAccessGranted        = '<|ACCESSGRANTED|>';
 cAccessDenied         = '<|ACCESSDENIED|>';
 cGetMonitorCount      = '<|GETMONITORCOUNT|>';
 cChangeMonitor        = '<|CHANGEMONITOR|>';
 cChangeImageQuality   = '<|CHANGEIMAGEQUALITY|>';
 cMonitors             = '<|MONITORS|>';
 cRelation             = '<|RELATION|>';
 cMyConfigs            = '<|MYCONFIGS|>';
 cPeerList             = '<|PEERLIST|>';
 cNewPeerList          = '<|NEWPEERLIST|>';
 cPeerOn               = '<|PEERON|>';
 cPeerOff              = '<|PEEROFF|>';
 cPeerService          = '<|PEERSERVICE|>';
 cSendImage            = '<|SENDIMAGE|>';
 cEditPeer             = '<|PEEREDIT|>';
 cInsertPeer           = '<|PEERINSERT|>';
 cDeletePeer           = '<|PEERDELETE|>';
 cBestQ                = '<|BESTQ|>';
 cAccessing            = '<|ACCESSING|>';
 cGetFullScreenshot    = '<|GETFULLSCREENSHOT|>';
 cStopAccess           = '<|STOPACCESS|>';
 cGetDrivers           = '<|GETDRIVERS|>';
 cGetFiles             = '<|GETFILES|>';
 cGetFolders           = '<|GETFOLDERS|>';
 cSetDrivers           = '<|SETDRIVERS|>';
 cSetFiles             = '<|SETFILES|>';
 cSetFolders           = '<|SETFOLDERS|>';
 cBlockInput           = '<|BLOCKINPUT|>';
 cUnBlockInput         = '<|UNBLOCKINPUT|>';
 cSeparatorTag         = '<|>';
 cEndTag               = '<|END|>';
 cMouseClickLeftUp     = '<|SETMOUSELEFTCLICKUP|>';
 cMouseClickRightUp    = '<|SETMOUSERIGHTCLICKUP|>';
 cMouseClickMiddleUp   = '<|SETMOUSEMIDDLEUP|>';
 cMousePos             = '<|SETMOUSEPOS|>';
 cShowMouse            = '<|SHOWMOUSE|>';
 cHideMouse            = '<|HIDEMOUSE|>';
 cWheelMouse           = '<|WHEELMOUSE|>';
 cMouseClickLeftDown   = '<|SETMOUSELEFTCLICKDOWN|>';
 cMouseClickRightDown  = '<|SETMOUSERIGHTCLICKDOWN|>';
 cMouseClickMiddleDown = '<|SETMOUSEMIDDLEDOWN|>';
 cAltDown              = '<|ALTDOWN|>';
 cAltUp                = '<|ALTUP|>';
 cCtrlDown             = '<|CTRLDOWN|>';
 cCtrlUp               = '<|CTRLUP|>';
 cShiftDown            = '<|SHIFTDOWN|>';
 cShiftUp              = '<|SHIFTUP|>';
 cNewID                = '<|SETNEWID|>';
 cSetPortSend          = '<|SETPORTSEND|>';
 cSetPortRec           = '<|SETPORTREC|>';

Implementation

End.
