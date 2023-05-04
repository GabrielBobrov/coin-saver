export class DataUtils {

  transformaToLocalDateFormat(formato?: string) {
    var date = new Date();
    var year = date.getFullYear();
    var month = date.getMonth() + 1;
    var day = date.getDate();

    month = this.acrescentaZeroEsquerda(month);
    day = this.acrescentaZeroEsquerda(day);

    switch(formato) {
      case 'US':
        return year + '-' + month + '-' + day;
      default:
        return day + '-' + month + '-' + year;
    }
  }

  acrescentaZeroEsquerda(param: any) {
    if (param < 10) {
      return '0' + param;
     } else {
      return param;
     }
  }

  transformaDataInput(dataInput: any) {
    var dataPartes = dataInput.split("/");
    var dataConvertida = new Date(+dataPartes[2], dataPartes[1] - 1, dataPartes[0]);

    var dataFormatada = ((dataConvertida.getFullYear() )) + "-" + (this.acrescentaZeroEsquerda(dataConvertida.getMonth() + 1)) + "-" + this.acrescentaZeroEsquerda(dataConvertida.getDate());

    return dataFormatada;
  }

  formataTextoTabelaMensal(data: any) {
    var dataPartes = data.split("-");
    var mesTexto = this.retornaMesTexto(dataPartes[1]);
    var anoTexto = dataPartes[0];
    var textoFormatado = mesTexto + "/" + anoTexto;
    return textoFormatado;
  }

  retornaMesTexto(mes: any) {
    switch(mes) {
      case '01':
        return "JANEIRO";
      case '02':
        return "FEVEREIRO";
      case '03':
        return "MARÇO";
      case '04':
        return "ABRIL";
      case '05':
        return "MAIO";
      case '06':
        return "JUNHO";
      case '07':
        return "JULHO";
      case '08':
        return "AGOSTO";
      case '09':
        return "SETEMBRO";
      case '10':
        return "OUTUBRO";
      case '11':
        return "NOVEMBRO";
      case '12':
        return "DEZEMBRO";
      default:
        return;
    }
  }
}
