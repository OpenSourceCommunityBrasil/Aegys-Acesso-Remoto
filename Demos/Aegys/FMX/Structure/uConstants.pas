unit uConstants;

{
 Projeto Aegys.

  Criado por Gilberto Rocha da Silva em 05/04/2017 baseado no projeto Allakore, tem por objetivo promover acesso remoto e outros
 de forma gratuita a todos que necessitarem, hoje mantido por uma bela comunidade listando aqui nossos colaboradores de grande estima.

  Gilberto Rocha da Silva(XyberX) (Creator of Aegys Project/Main Desenveloper/Admin).
  Wendel Rodrigues Fassarella(wendelfassarella) (Creator of Aegys FMX/CORE Desenveloper).
  Rai Duarte Jales(Raí Duarte) (Aegys Server Desenveloper).
  Roniery Santos Cardoso (Aegys Desenveloper).
  Alexandre Carlos Silva Abade (Aegys Desenveloper).
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

implementation

end.
