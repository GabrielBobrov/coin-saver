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

  dataLinePerformance: any;
  optionsLinePerformance: any;

  constructor(
    private transactionsService: TransactionsService,
    public router: Router,
    private dialogService: DialogService,
  ) { }

  ngOnInit(): void {
    this.formataDataInicialTabelaMensal();
  }

  formataDataInicialTabelaMensal() {
    this.date = this.dataUtils.transformaToLocalDateFormat('US');
    this.getPerformance(this.date);
  }

  private getPerformance(date: any) {
    this.transactionsService.getTransactionsAmountByCategory(date)
      .subscribe((res) => {

        let novoArray = this.montaNovoArray(res);

        if (novoArray.length == 0) {
          this.expensePerformanceAmount = 0;
          this.incomePerformanceAmount = 0;
        } else {
          novoArray?.forEach((performance: any) => {

            if (performance.categoryName == "Despesa") {
              this.expensePerformanceAmount = Math.abs(+ performance.totalAmount);
            }

            if (performance.categoryName == "Entrada") {
              this.incomePerformanceAmount = Math.abs(+ performance.totalAmount);
            }
          })
        }
        this.graficoLineTransactions(this.expensePerformanceAmount, this.incomePerformanceAmount);
        // this.calculaPerformanceMes(this.expensePerformanceAmount, this.incomePerformanceAmount);
      });
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

  graficoLineTransactions(expensePerformanceAmount: any, incomePerformanceAmount: any) {
    const documentStyle = getComputedStyle(document.documentElement);
    const textColor = documentStyle.getPropertyValue('--text-color');
    const textColorSecondary = documentStyle.getPropertyValue('--text-color-secondary');
    const surfaceBorder = documentStyle.getPropertyValue('--surface-border');

    this.dataLinePerformance = {
      labels: ['Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho', 'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro'],
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

    this.optionsLinePerformance = {
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
