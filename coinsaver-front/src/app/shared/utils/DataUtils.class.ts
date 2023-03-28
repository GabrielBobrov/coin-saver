export class DataUtils {

  transformaToLocalDateFormat(formato?: string) {
    var date = new Date();
    var year = date.getFullYear();
    var month = date.getMonth() + 1;
    var day = date.getDate();

    month = this.acrescentaZeroEsquerda(month);

    switch(formato) {
      case 'US':
        return year + '-' + month + '-' + day;
      default:
        return day + '-' + month + '-' + year;
    }
  }

  acrescentaZeroEsquerda(month: any) {
    if (month < 10) {
      return '0' + month;
     } else {
      return month;
     }
  }
}
