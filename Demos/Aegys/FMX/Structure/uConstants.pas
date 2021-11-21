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
  PORTA = 3898;
  FOLGAPROCESSAMENTO = 2;
  SERVIDOR = '192.168.1.2';
  INTERVALOCONEXAO = 60;
  ARQUIVO_SITE = 'caminho do exe em um servidor https';

  // Locale consts
  APP = 'APPLICATION';
  MSGS = 'MESSAGES';
  FRMS = 'FORMS';
  DLGS = 'DIALOGS';
  CHAT = 'CHAT';

implementation

end.
