import { Component, OnInit } from '@angular/core';
import { MessageService } from 'primeng/api';
import { PayTransactionRequestDto } from 'src/app/dtos/transactions/request/pay-transaction.request.dto';
import { UpdateTransactionRequestDto } from 'src/app/dtos/transactions/request/update-transaction.request.dto';
import { MonthlyTransactionResponseDto } from 'src/app/dtos/transactions/response/monthly-transactions.response.dto';
import { MonthlyResponseDto } from 'src/app/dtos/transactions/response/monthly.response.dto';
import { TransactionsService } from 'src/app/services/transactions/transactions.service';
import { DataUtils } from 'src/app/shared/utils/DataUtils.class';

@Component({
  selector: 'app-table-mensal',
  templateUrl: './table-mensal.component.html',
  styleUrls: ['./table-mensal.component.css']
})
export class TableMensalComponent implements OnInit {

  monthlyResponseDto: MonthlyResponseDto | undefined;
  monthlyTransactionsResponseDtoList: MonthlyTransactionResponseDto[] = [];
  updateTransactionRequestDto = new UpdateTransactionRequestDto();
  payTransactionRequestDto = new PayTransactionRequestDto();

  date: string = '';
  dataUtils = new DataUtils();

  constructor(
    private transactionsService: TransactionsService,
    private messageService: MessageService,
  ) { }

  ngOnInit() {
    this.getTransactionsInMonth();
  }

  getTransactionsInMonth() {
    this.date = this.dataUtils.transformaToLocalDateFormat('US');

    this.transactionsService.getTransactionsInMonth(this.date)
      .subscribe((res) => {
        this.monthlyResponseDto = res;

        this.monthlyResponseDto.transactions?.forEach((transaction) => {
        })

        this.monthlyTransactionsResponseDtoList = this.monthlyResponseDto.transactions;

        console.log('month', this.monthlyResponseDto)
      });
  }

  pagarTransacao(transaction: MonthlyTransactionResponseDto) {
    console.log("pagar", transaction)

    this.payTransactionRequestDto.transactionId = transaction.transactionId;
    this.payTransactionRequestDto.transactionType = transaction.transactionType;

    this.transactionsService.updateTransactionPatch(this.payTransactionRequestDto).subscribe(
      (res) => {
      },
      (error) => {

      }
    );
  }

  atualizarTransacao(transaction: MonthlyTransactionResponseDto) {
    console.log("atualizar", transaction)

    this.updateTransactionRequestDto.amount = transaction.amount;
    this.updateTransactionRequestDto.payDay = transaction.payDay;
    this.updateTransactionRequestDto.description = transaction.description;
    this.updateTransactionRequestDto.status = transaction.status;
    this.updateTransactionRequestDto.category = transaction.category;
    // this.updateTransactionRequestDto.fixedExpense = transaction.amount;
    // this.updateTransactionRequestDto.repeat = transaction.amount;
    // this.updateTransactionRequestDto.updateTransactionType = transaction.amount;
    this.updateTransactionRequestDto.transactionType = transaction.transactionType;
    this.updateTransactionRequestDto.transactionId = transaction.transactionId;
    // this.updateTransactionRequestDto.installmentTransactionId = transaction.amount;
    // this.updateTransactionRequestDto.fixTransactionId = transaction.amount;

    this.transactionsService.updateTransaction(this.updateTransactionRequestDto)
    .subscribe((res) => {
    });
  }

  deletarTransacao(transaction: MonthlyTransactionResponseDto) {
    console.log("deletar", transaction)

    this.transactionsService.deleteTransaction(transaction.transactionId).subscribe(
      (res) => {
        this.messageService.add({ severity: 'success', summary: 'Success', detail: 'Transação APAGADA com sucesso' });
        this.ngOnInit();
      },
      (error) => {
        this.messageService.add({ severity: 'error', summary: 'Error', detail: 'Erro ao tentar APAGAR transação' });
      }
    );
  }

}
