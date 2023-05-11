import { TransactionTypeEnum } from './../../enums/transaction-type.enum';
import { TransactionResponseDto } from '../../dtos/transactions/response/transaction.response.dto';
import { TransactionsService } from './../../services/transactions/transactions.service';
import { Component, OnInit } from '@angular/core';
import { DataUtils } from 'src/app/shared/utils/DataUtils.class';
import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
import { Router } from '@angular/router';
import { MonthlyChartResponseDto } from 'src/app/dtos/transactions/response/monthly-chart.response.dto';

@Component({
  selector: 'app-usuario-logado-page',
  templateUrl: './usuario-logado-page.component.html',
  styleUrls: ['./usuario-logado-page.component.css']
})
export class UsuarioLogadoPageComponent implements OnInit {

  transaction: TransactionResponseDto | undefined;
  listTransaction: TransactionResponseDto[] | undefined;
  transactionId: number = 0;
  transactionType: TransactionTypeEnum | undefined;
  transactionCategoryType: TransactionCategoryTypeEnum | undefined;

  listMonthlyChartCategory: MonthlyChartResponseDto[] = [];
  monthlyChartCategory!: MonthlyChartResponseDto;
  totalAmountCategory?: number;
  categoryNameCategory?: string;

  date: string = '';
  dataUtils = new DataUtils();

  expenseCategoryAmount?: number;
  incomeCategoryAmount?: number;

  dataPie: any;
  optionsPie: any;

  dataLine: any;
  optionsLine: any;

  constructor(
    private transactionsService: TransactionsService,
    public router: Router,
  ) { }

  ngOnInit(): void {
    this.formataDataInicialTabelaMensal();
  }

  formataDataInicialTabelaMensal() {
    this.date = this.dataUtils.transformaToLocalDateFormat('US');
    this.getTransactionsAmountByCategory(this.date);
  }

  private getTransactionsAmountByCategory(date: any) {
    this.transactionsService.getTransactionsAmountByCategory(date)
    .subscribe((res) => {
      let novoArray = this.montaNovoArray(res);

      if (novoArray.length == 0) {
        this.expenseCategoryAmount = 1;
        this.incomeCategoryAmount = 1;
      } else {
        novoArray?.forEach((monthlyChartCategory: any) => {

          if (monthlyChartCategory.categoryName == "Despesa") {
            this.expenseCategoryAmount =+ monthlyChartCategory.totalAmount;
          }

          if (monthlyChartCategory.categoryName == "Entrada") {
            this.incomeCategoryAmount =+ monthlyChartCategory.totalAmount;
          }

          this.graficoPieTransactionByCategoryType(this.expenseCategoryAmount, this.incomeCategoryAmount);
        })
      }
    });
  }

  montaNovoArray(novoArray: any) {
    return novoArray;
  }

  graficoPieTransactionByCategoryType(expenseCategoryAmount: any, incomeCategoryAmount: any) {

    const documentStyle = getComputedStyle(document.documentElement);
    const textColor = documentStyle.getPropertyValue('--text-color');

    this.dataPie = {
      labels: ['Despesas', 'Entradas'],
      datasets: [
        {
          data: [expenseCategoryAmount, incomeCategoryAmount],
          backgroundColor: [
            documentStyle.getPropertyValue('--red-500'),
            documentStyle.getPropertyValue('--green-500')],
          hoverBackgroundColor: [
            documentStyle.getPropertyValue('--red-400'),
            documentStyle.getPropertyValue('--green-400')]
        }
      ]
    };

    this.optionsPie = {
      plugins: {
        legend: {
          labels: {
            usePointStyle: true,
            color: textColor
          }
        }
      }
    };
  }

  graficoLineTransactionByCategoryType() {
    const documentStyle = getComputedStyle(document.documentElement);
    const textColor = documentStyle.getPropertyValue('--text-color');
    const textColorSecondary = documentStyle.getPropertyValue('--text-color-secondary');
    const surfaceBorder = documentStyle.getPropertyValue('--surface-border');

    this.dataLine = {
      labels: ['January', 'February', 'March', 'April', 'May', 'June', 'July'],
      datasets: [
        {
          type: 'line',
          label: 'Dataset 1',
          borderColor: documentStyle.getPropertyValue('--blue-500'),
          borderWidth: 2,
          fill: false,
          tension: 0.4,
          data: [50, 25, 12, 48, 56, 76, 42]
        },
        {
          type: 'bar',
          label: 'Dataset 2',
          backgroundColor: documentStyle.getPropertyValue('--green-500'),
          data: [21, 84, 24, 75, 37, 65, 34],
          borderColor: 'white',
          borderWidth: 2
        },
        {
          type: 'bar',
          label: 'Dataset 3',
          backgroundColor: documentStyle.getPropertyValue('--orange-500'),
          data: [41, 52, 24, 74, 23, 21, 32]
        }
      ]
    };

    this.optionsLine = {
      maintainAspectRatio: false,
      aspectRatio: 0.6,
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
            color: textColorSecondary
          },
          grid: {
            color: surfaceBorder
          }
        },
        y: {
          ticks: {
            color: textColorSecondary
          },
          grid: {
            color: surfaceBorder
          }
        }
      }
    };
  }
}

