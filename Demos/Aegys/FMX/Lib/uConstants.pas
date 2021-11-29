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
  TComboTypes = (tcbQuality, tcbLanguage);
  TLanguageImage = (AR_SA, DE_DE, EN_US, ES_ES, FR_FR, IT_IT, JA_JA, KO_KO, PT_BR,
    RU_RU, ZH_CN, ZH_TW);

const
  PORTA = 3898;
  FOLGAPROCESSAMENTO = 2;
  SERVIDOR = '192.168.0.181';
  // Svr: Razios '201.4.250.4' //Svr:Embratel '201.73.143.69';
  INTERVALOCONEXAO = 60;
  ARQUIVO_SITE = 'caminho do exe em um servidor https';
  PRIMARY_COLOR = $FF0070BA;
  SECONDARY_COLOR = $FFC4E8FF;

  // Locale consts
  MAIN = 'SYSTEMINFO';
  MSGS = 'MESSAGES';
  FRMS = 'FORMS';
  DLGS = 'DIALOGS';
  CHAT = 'CHAT';
  CBQUAL = 'CBQUALITY';
  CBLANG = 'CBLANGUAGE';

  CBLANGUAGEKEYS = '';

implementation

end.
