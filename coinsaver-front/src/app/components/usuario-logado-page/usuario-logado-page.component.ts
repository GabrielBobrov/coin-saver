import { TransactionTypeEnum } from './../../enums/transaction-type.enum';
import { TransactionResponseDto } from '../../dtos/transactions/response/transaction.response.dto';
import { TransactionsService } from './../../services/transactions/transactions.service';
import { Component, OnInit } from '@angular/core';
import { DataUtils } from 'src/app/shared/utils/DataUtils.class';
import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
import { Router } from '@angular/router';

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
    // this.getTransaction();
    this.getTransactionByCategoryType();
  }

  getTransaction() {
    this.transactionId = 1;
    this.transactionType = TransactionTypeEnum.IN_CASH;

    this.transactionsService.getTransaction(this.transactionId, this.transactionType)
      .subscribe((res) => {
        this.transaction = res;

        console.log('id cat type', this.transaction)
      });
  }

  getTransactionByCategoryType() {
    this.transactionCategoryType = TransactionCategoryTypeEnum.EXPENSE;
    this.date = this.dataUtils.transformaToLocalDateFormat('US');

    this.transactionsService.getTransactionByCategoryType(this.transactionCategoryType, this.date)
      .subscribe((res) => {
        this.listTransaction = res;

        console.log('listTransaction cat type', this.listTransaction)

        if (this.listTransaction.length == 0) {
          this.expenseCategoryAmount = 1;
          this.incomeCategoryAmount = 1;
        } else {
          this.listTransaction?.forEach((transaction) => {

            if (transaction.category == 'EXPENSE') {
              this.expenseCategoryAmount =+ transaction.amount;
            }

            if (transaction.category == 'INCOME') {
              this.incomeCategoryAmount =+ transaction.amount;
            }
          })
        }

        this.graficoPieTransactionByCategoryType(this.expenseCategoryAmount, this.incomeCategoryAmount);
        this.graficoLineTransactionByCategoryType();
      });
  }

  graficoPieTransactionByCategoryType(expenseCategoryAmount: any, incomeCategoryAmount: any) {

    console.log(expenseCategoryAmount)
    console.log(incomeCategoryAmount)

    const documentStyle = getComputedStyle(document.documentElement);
    const textColor = documentStyle.getPropertyValue('--text-color');

    this.dataPie = {
      labels: ['EXPENSE', 'INCOME'],
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

