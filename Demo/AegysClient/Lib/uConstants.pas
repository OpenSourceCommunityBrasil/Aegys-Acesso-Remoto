unit uConstants;

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

interface

type
  IDThreadType = (ttPrincipal, ttAreaRemota, ttTeclado, ttArquivos);

const
  PORTA              = 9092;
  FOLGAPROCESSAMENTO = 1;                                  //mmHospedagem Linux//mmHospedagem Wind //Desenvolvimento//Local
  SERVIDOR           = '192.168.100.29';//'200.9.155.39';//'192.168.100.29'; //'177.93.106.240';//'177.10.144.78';//'192.168.15.200';
  cMaxFramesBuffer   = 1;
  cRequestTimeOut    = 5000;
  cConnectTimeOut    = 0;
  {
  !!!Deixe em branco para liberar a combobox na config!!!
  segue lista de servidores disponíveis:
  Svr: Razios 'aegys.ddns.net'
  Svr: Embratel '201.73.143.69';
  Svr: Diego '177.10.144.78';
  Svr: MMHospedagem '177.93.106.240';
  }
  cFrameSkip       = 4;
  INTERVALOCONEXAO = 60;
  ARQUIVO_SITE = 'caminho do exe em um servidor https';

  // Controle de Versão
  APPVERSION = '2.0.1';
  APPBUILDV = '135';

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
