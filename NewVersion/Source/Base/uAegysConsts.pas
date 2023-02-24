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
 //Messages
 cCaptureTitle         = 'Remote Access, Connection "%s",ID "%s"';
 cCantExecDisconnected = 'Can''''t Execute Action when Disconnected';
 cCantJoinDisconnected = 'Can''''t join when Disconnected';
 cInvalidBufferData    = 'Invalid Buffer Data...';
 cInvalidHeaderData    = 'Invalid Header Data...';
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
 cMonitors             = '<|MONITORS|>';
 cRelation             = '<|RELATION|>';
 cBestQ                = '<|BESTQ|>';
 cAccessing            = '<|ACCESSING|>';
 cGetFullScreenshot    = '<|GETFULLSCREENSHOT|>';
 cStopAccess           = '<|STOPACCESS|>';
 cFolderList           = '<|FOLDERLIST|>';
 cGetDrivers           = '<|GETDRIVERS|>';
 cFileList             = '<|FILESLIST|>';
 cGetFiles             = '<|GETFILES|>';
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

Implementation

End.
