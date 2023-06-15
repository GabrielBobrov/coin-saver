import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { TransactionsService } from 'src/app/services/transactions/transactions.service';
import { DataUtils } from 'src/app/shared/utils/DataUtils.class';
import { ModalTrocarSenhaComponent } from '../modal-trocar-senha/modal-trocar-senha.component';
import { DialogService } from 'primeng/dynamicdialog';
import { PerformanceResponseDto } from 'src/app/dtos/transactions/response/performance.response.dto';
import { PerformanceEconomyResponseDto } from 'src/app/dtos/transactions/response/performance-economy.response.dto';

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

  performance?: PerformanceResponseDto;

  economy?: PerformanceEconomyResponseDto;

  gasto?: boolean = false;
  economia?: boolean = false;

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
        this.verificaGastoOuEconomia(this.performance.monthlyBalance.previousMonthPercentageDifference);
        this.economy = this.performance.economy;
      });
  }

  verificaGastoOuEconomia(previousMonthPercentageDifference: string) {
    if (previousMonthPercentageDifference.includes('-')) {
      this.economia = false;
      return this.gasto = true;
    } else {
      this.gasto = false;
      return this.economia = true;
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
