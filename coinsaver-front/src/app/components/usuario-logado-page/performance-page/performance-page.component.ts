import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { TransactionsService } from 'src/app/services/transactions/transactions.service';
import { DataUtils } from 'src/app/shared/utils/DataUtils.class';
import { ModalTrocarSenhaComponent } from '../modal-trocar-senha/modal-trocar-senha.component';
import { DialogService } from 'primeng/dynamicdialog';

@Component({
  selector: 'app-performance-page',
  templateUrl: './performance-page.component.html',
  styleUrls: ['./performance-page.component.css']
})
export class PerformancePageComponent implements OnInit {

  date: string = '';
  dataUtils = new DataUtils();

  percentualDespesasMes: number = 0;
  isCalculavel?: boolean = true;

  expensePerformanceAmount?: number;
  incomePerformanceAmount?: number;

  dataBarPerformance: any;
  optionsBarPerformance: any;

  performance: any;

  isNegativo?: boolean = false;
  isPositivo?: boolean = false;

  gastou?: boolean = false;
  economizou?: boolean = false;

  dataDatepicker: any;

  constructor(
    private transactionsService: TransactionsService,
    public router: Router,
    private dialogService: DialogService,
  ) { }

  ngOnInit(): void {
    this.formataDataInicialTabelaMensal();
  }

  recebeDataMontadaDatepicker(dataMontadaDatepicker: any) {
    this.dataDatepicker = dataMontadaDatepicker;
    this.atualizaTabelaMesComNovaData();
  }

  atualizaTabelaMesComNovaData() {
    this.date = this.dataUtils.transformaDataInput(this.dataDatepicker);
    this.getPerformance(this.date);
  }

  formataDataInicialTabelaMensal() {
    this.date = this.dataUtils.transformaToLocalDateFormat('US');
    this.getPerformance(this.date);
  }

  private getPerformance(date: any) {
    this.transactionsService.getPerformance(date)
      .subscribe((res) => {
        this.performance = res;

        console.log(this.performance)

        this.verificaBalancoMensal(this.performance.monthlyBalance.actualMonthBalance);
        this.verificaGastoOuEconomiaMesAtual(this.performance.economy.actualMonthPercentage);
        this.verificaGastoOuEconomiaMesAnterior(this.performance.economy.previousMonthPercentage);

      });
  }

  verificaBalancoMensal(actualMonthBalance: number) {
    if (actualMonthBalance < 0) {
      this.isPositivo = false;
      this.isNegativo = true;
    } else {
      this.isNegativo = false;
      this.isPositivo = true;
    }
  }

  verificaGastoOuEconomiaMesAtual(actualMonthPercentage: string) {
    if (actualMonthPercentage.includes('-')) {
      this.economizou = false;
      this.gastou = true;
    } else {
      this.gastou = false;
      this.economizou = true;
    }
  }

  verificaGastoOuEconomiaMesAnterior(previousMonthPercentage: string) {
    if (previousMonthPercentage.includes('-')) {
      this.economizou = false;
      this.gastou = true;
    } else {
      this.gastou = false;
      this.economizou = true;
    }
  }

  montaNovoArray(novoArray: any) {
    return novoArray;
  }

  retornaPaginaInicialUsuarioLogado() {
    this.router.navigateByUrl('usuario-logado-page', {
      state: {
        data: {},
      },
    });
  }

  abrirPerformancePage() {
    this.router.navigateByUrl('performance-page', {
      state: {
        data: {},
      },
    });
  }

  modalTrocarSenha() {
    this.dialogService.open(ModalTrocarSenhaComponent, {
      data: {},
      showHeader: false
    });
  }

  graficoBarTransactions(expensePerformanceAmount: any, incomePerformanceAmount: any) {
    const documentStyle = getComputedStyle(document.documentElement);
    const textColor = documentStyle.getPropertyValue('--text-color');
    const textColorSecondary = documentStyle.getPropertyValue('--text-color-secondary');
    const surfaceBorder = documentStyle.getPropertyValue('--surface-border');

    this.dataBarPerformance = {
      // labels: ['Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho', 'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro'],
      labels: [this.dataUtils.formataTextoTabelaMensal(this.date)],
      datasets: [
        {
          label: 'Despesas',
          backgroundColor: documentStyle.getPropertyValue('--red-500'),
          borderColor: documentStyle.getPropertyValue('--red-500'),
          data: [expensePerformanceAmount]
        },
        {
          label: 'Entradas',
          backgroundColor: documentStyle.getPropertyValue('--green-500'),
          borderColor: documentStyle.getPropertyValue('--green-500'),
          data: [incomePerformanceAmount]
        }
      ]
    };

    this.optionsBarPerformance = {
      maintainAspectRatio: false,
      aspectRatio: 0.8,
      plugins: {
        legend: {
          labels: {
            color: textColor
          }
        }
      },
      scales: {
        x: {
          ticks: {
            color: textColorSecondary,
            font: {
              weight: 500
            }
          },
          grid: {
            color: surfaceBorder,
            drawBorder: false
          }
        },
        y: {
          ticks: {
            color: textColorSecondary
          },
          grid: {
            color: surfaceBorder,
            drawBorder: false
          }
        }
      }
    };
  }

  calculaPerformanceMes(expensePerformanceAmount: any, incomePerformanceAmount: any) {

    if (expensePerformanceAmount == 0 && incomePerformanceAmount == 0 ||
      expensePerformanceAmount != 0 && incomePerformanceAmount == 0) {
      this.isCalculavel = false;
      return null;

    } else if (expensePerformanceAmount == 0 && incomePerformanceAmount != 0) {
      return this.percentualDespesasMes = 0;

    } else if (expensePerformanceAmount != 0 && incomePerformanceAmount != 0) {
      this.percentualDespesasMes = (Math.abs(expensePerformanceAmount) / Math.abs(incomePerformanceAmount)) * 100;
      return this.percentualDespesasMes.toFixed(2);
    }

    return 9999;
  }

}
