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
  divisionName?: string;

  dataPieCategory: any;
  optionsPieCategory: any;

  dataPieDivisionExpense: any;
  optionsPieDivisionExpense: any;
  divisionArrayAmoutExpense: any = []
  divisionArrayNameExpense: any = [];

  dataPieDivisionIncome: any;
  optionsPieDivisionIncome: any;
  divisionArrayAmoutIncome: any = []
  divisionArrayNameIncome: any = [];

  isMostrarGraficoExpense?: boolean;
  isMostrarGraficoIncome?: boolean;

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
    this.getTransactionsAmountByDivisionExpense(this.date);
    this.getTransactionsAmountByDivisionIncome(this.date);
  }

  private getTransactionsAmountByCategory(date: any) {
    this.transactionsService.getTransactionsAmountByCategory(date)
    .subscribe((res) => {
      let novoArray = this.montaNovoArray(res);

      if (novoArray.length == 0) {
        this.expenseCategoryAmount = -1;
        this.incomeCategoryAmount = 1;
      } else {
        novoArray?.forEach((monthlyChartCategory: any) => {

          if (monthlyChartCategory.categoryName == "Despesa") {
            this.expenseCategoryAmount =+ monthlyChartCategory.totalAmount;
          }

          if (monthlyChartCategory.categoryName == "Entrada") {
            this.incomeCategoryAmount =+ monthlyChartCategory.totalAmount;
          }
        })
      }
      this.graficoPieTransactionByCategoryType(this.expenseCategoryAmount, this.incomeCategoryAmount);
    });
  }

  private getTransactionsAmountByDivisionExpense(date: any) {

    this.transactionsService.getTransactionsAmountByDivision(date, 'EXPENSE')
    .subscribe((res) => {

      let novoArray = this.montaNovoArray(res);

      novoArray?.forEach((monthlyChartCategory: any) => {
        this.expenseCategoryAmount = monthlyChartCategory.totalAmount;
        this.divisionArrayAmoutExpense.push(this.expenseCategoryAmount);
        this.divisionName = monthlyChartCategory.divisionName;
        this.divisionArrayNameExpense.push(this.divisionName);
      })
      this.graficoPieTransactionByDivisionTypeExpense(this.divisionArrayAmoutExpense, this.divisionArrayNameExpense);
    });
  }

  private getTransactionsAmountByDivisionIncome(date: any) {

    this.transactionsService.getTransactionsAmountByDivision(date, 'INCOME')
    .subscribe((res) => {

      let novoArray = this.montaNovoArray(res);

      novoArray?.forEach((monthlyChartCategory: any) => {
        this.incomeCategoryAmount = monthlyChartCategory.totalAmount;
        this.divisionArrayAmoutIncome.push(this.incomeCategoryAmount);
        this.divisionName = monthlyChartCategory.divisionName;
        this.divisionArrayNameIncome.push(this.divisionName);
      })
      this.graficoPieTransactionByDivisionTypeIncome(this.divisionArrayAmoutIncome, this.divisionArrayNameIncome);
    });
  }

  montaNovoArray(novoArray: any) {
    return novoArray;
  }

  graficoPieTransactionByCategoryType(expenseCategoryAmount: any, incomeCategoryAmount: any) {

    const documentStyle = getComputedStyle(document.documentElement);
    const textColor = documentStyle.getPropertyValue('--text-color');

    this.dataPieCategory = {
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

    this.optionsPieCategory = {
      plugins: {
        legend: {display: false}
        // legend: {
        //   labels: {
        //     usePointStyle: true,
        //     color: textColor
        //   }
        // }
      }
    };
  }

  graficoPieTransactionByDivisionTypeExpense(divisionArrayAmoutExpense: any, divisionArrayNameExpense: any) {

    if (divisionArrayAmoutExpense.length != 0 && divisionArrayNameExpense.length != 0) {
      this.isMostrarGraficoExpense = true;

      const documentStyle = getComputedStyle(document.documentElement);
      const textColor = documentStyle.getPropertyValue('--text-color');

      this.dataPieDivisionExpense = {
        // labels: ['Saude', 'Casa', 'Electronic', 'Restaurante', 'Academia', 'Aluguel', 'Supermercado', 'Lazer', 'Combustivel', 'Delivery', 'BadBet'],
        labels: [
          divisionArrayNameExpense[0],
          divisionArrayNameExpense[1],
          divisionArrayNameExpense[2],
          divisionArrayNameExpense[3],
          divisionArrayNameExpense[4],
          divisionArrayNameExpense[5],
          divisionArrayNameExpense[6],
          divisionArrayNameExpense[7],
          divisionArrayNameExpense[8],
          divisionArrayNameExpense[9],
          divisionArrayNameExpense[10],
          divisionArrayNameExpense[11],
          divisionArrayNameExpense[12],
          divisionArrayNameExpense[13],
          divisionArrayNameExpense[14],
          divisionArrayNameExpense[15]
        ],
        datasets: [
          {
            data: [
              divisionArrayAmoutExpense[0],
              divisionArrayAmoutExpense[1],
              divisionArrayAmoutExpense[2],
              divisionArrayAmoutExpense[3],
              divisionArrayAmoutExpense[4],
              divisionArrayAmoutExpense[5],
              divisionArrayAmoutExpense[6],
              divisionArrayAmoutExpense[7],
              divisionArrayAmoutExpense[8],
              divisionArrayAmoutExpense[9],
              divisionArrayAmoutExpense[10],
              divisionArrayAmoutExpense[11],
              divisionArrayAmoutExpense[12],
              divisionArrayAmoutExpense[13],
              divisionArrayAmoutExpense[14],
              divisionArrayAmoutExpense[15]
            ],
            backgroundColor: [
              documentStyle.getPropertyValue('--blue-500'),
              documentStyle.getPropertyValue('--yellow-500'),
              documentStyle.getPropertyValue('--pink-500'),
              documentStyle.getPropertyValue('--bluegray-500'),
              documentStyle.getPropertyValue('--orange-500'),
              documentStyle.getPropertyValue('--red-500'),
              documentStyle.getPropertyValue('--green-500'),],
            hoverBackgroundColor: [
              documentStyle.getPropertyValue('--blue-400'),
              documentStyle.getPropertyValue('--yellow-400'),
              documentStyle.getPropertyValue('--pink-400'),
              documentStyle.getPropertyValue('--bluegray-400'),
              documentStyle.getPropertyValue('--orange-400'),
              documentStyle.getPropertyValue('--red-400'),
              documentStyle.getPropertyValue('--green-400'),]
          }
        ]
      };

      this.optionsPieDivisionExpense = {
        plugins: {
          legend: {display: false}
          // legend: {
          //   labels: {
          //     usePointStyle: true,
          //     color: textColor
          //   }
          // }
        }
      };

    } else {
      this.isMostrarGraficoExpense = false;
    }
  }

  graficoPieTransactionByDivisionTypeIncome(divisionArrayAmoutIncome: any, divisionArrayNameIncome: any) {

    if (divisionArrayAmoutIncome.length != 0 && divisionArrayNameIncome.length != 0) {
      this.isMostrarGraficoIncome = true;

      const documentStyle = getComputedStyle(document.documentElement);
      const textColor = documentStyle.getPropertyValue('--text-color');

      this.dataPieDivisionIncome = {
        // labels: ['Salário', 'Bônus', 'GoodBet', 'Investimento'],
        labels: [
          divisionArrayNameIncome[0],
          divisionArrayNameIncome[1],
          divisionArrayNameIncome[2],
          divisionArrayNameIncome[3],
          divisionArrayNameIncome[4],
          divisionArrayNameIncome[5],
          divisionArrayNameIncome[6],
          divisionArrayNameIncome[7],
          divisionArrayNameIncome[8],
          divisionArrayNameIncome[9],
          divisionArrayNameIncome[10],
          divisionArrayNameIncome[11],
          divisionArrayNameIncome[12],
          divisionArrayNameIncome[13],
          divisionArrayNameIncome[14],
          divisionArrayNameIncome[15]
      ],
        datasets: [
          {
            data: [
              divisionArrayAmoutIncome[0],
              divisionArrayAmoutIncome[1],
              divisionArrayAmoutIncome[2],
              divisionArrayAmoutIncome[3],
              divisionArrayAmoutIncome[4],
              divisionArrayAmoutIncome[5],
              divisionArrayAmoutIncome[6],
              divisionArrayAmoutIncome[7],
              divisionArrayAmoutIncome[8],
              divisionArrayAmoutIncome[9],
              divisionArrayAmoutIncome[10],
              divisionArrayAmoutIncome[11],
              divisionArrayAmoutIncome[12],
              divisionArrayAmoutIncome[13],
              divisionArrayAmoutIncome[14],
              divisionArrayAmoutIncome[15]
            ],
            backgroundColor: [
              documentStyle.getPropertyValue('--yellow-500'),
              documentStyle.getPropertyValue('--bluegray-500'),
              documentStyle.getPropertyValue('--orange-500'),
              documentStyle.getPropertyValue('--blue-500'),
              documentStyle.getPropertyValue('--pink-500'),
              documentStyle.getPropertyValue('--red-500'),
              documentStyle.getPropertyValue('--green-500'),],
            hoverBackgroundColor: [
              documentStyle.getPropertyValue('--yellow-400'),
              documentStyle.getPropertyValue('--bluegray-400'),
              documentStyle.getPropertyValue('--orange-400'),
              documentStyle.getPropertyValue('--blue-400'),
              documentStyle.getPropertyValue('--pink-400'),
              documentStyle.getPropertyValue('--red-400'),
              documentStyle.getPropertyValue('--green-400'),]
          }
        ]
      };

      this.optionsPieDivisionIncome = {
        plugins: {
          legend: {display: false}
          // legend: {
          //   labels: {
          //     usePointStyle: true,
          //     color: textColor
          //   }
          // }
        }
      };
    } else {
      this.isMostrarGraficoIncome = false;
    }
  }

  retornaPaginaInicialUsuarioLogado() {
    this.router.navigateByUrl('usuario-logado-page', {
      state: {
        data: {},
      },
    });
  }
}

