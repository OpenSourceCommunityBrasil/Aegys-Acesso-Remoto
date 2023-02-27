unit uConstants;

{
  Project Aegys Remote Support.

  Created by Gilberto Rocha da Silva in 04/05/2017 based on project Allakore, has by objective to promote remote access
  and other resources freely to all those who need it, today maintained by a beautiful community. Listing below our
  higly esteemed collaborators:

  Gilberto Rocha da Silva (XyberX) (Creator of Aegys Project/Main Developer/Admin)
  Wendel Rodrigues Fassarella (wendelfassarella) (Creator of Aegys FMX/CORE Developer)
  Rai Duarte Jales (Raí Duarte) (Aegys Server Developer)
  Roniery Santos Cardoso (Aegys Developer)
  Alexandre Carlos Silva Abade (Aegys Developer)
  Mobius One (Aegys Developer)
}

interface

type
  IDThreadType = (ttPrincipal, ttAreaRemota, ttTeclado, ttArquivos);

const
  PORTA              = 9092;
  FOLGAPROCESSAMENTO = 1;
  SERVIDOR           = '177.10.144.78'; //'177.10.144.78';//'192.168.15.200';
  cMaxFramesBuffer   = 1;
  {
  !!!Deixe em branco para liberar a combobox na config!!!
  segue lista de servidores disponíveis:
  Svr: Razios 'aegys.ddns.net'
  Svr: Embratel '201.73.143.69';
  Svr: Diego '177.10.144.78';
  Svr: MMHospedagem '177.93.106.240';
  }
  INTERVALOCONEXAO = 60;
  ARQUIVO_SITE = 'caminho do exe em um servidor https';

  // Controle de Versão
  APPVERSION = '2.0.1';
  APPBUILDV = '161';

  // frescuragem do app
  PRIMARY_COLOR = $FF0070BA;
  // vermelho = $FFFF2020;
  // menta = $FF00BA4D;
  SECONDARY_COLOR = $FFC4E8FF;
  // vermelho = $FFFFD3D3;
  // menta = $FFC7FFDE;

  // Locale consts
  MAIN = 'SYSTEMINFO';
  MSGS = 'MESSAGES';
  FRMS = 'FORMS';
  DLGS = 'DIALOGS';
  CHAT = 'CHAT';
  CBQUAL = 'CBQUALITY';
  CBLANG = 'CBLANGUAGE';

  // Config consts
  RUN_ON_STARTUP = 'startup';
  FIXED_PASSWORD = 'password';
  QUICK_SUPPORT = 'quicksupp';
  ENABLE_SYSTRAY = 'systray';
  LANGUAGE = 'language';
  SERVER = 'server';

implementation

end.
