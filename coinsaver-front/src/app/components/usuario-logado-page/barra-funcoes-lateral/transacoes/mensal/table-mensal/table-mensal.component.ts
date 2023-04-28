import { Component, OnInit } from '@angular/core';
import { MessageService } from 'primeng/api';
import { PayTransactionRequestDto } from 'src/app/dtos/transactions/request/pay-transaction.request.dto';
import { UpdateTransactionRequestDto } from 'src/app/dtos/transactions/request/update-transaction.request.dto';
import { MonthlyTransactionResponseDto } from 'src/app/dtos/transactions/response/monthly-transactions.response.dto';
import { MonthlyResponseDto } from 'src/app/dtos/transactions/response/monthly.response.dto';
import { TransactionsService } from 'src/app/services/transactions/transactions.service';
import { DataUtils } from 'src/app/shared/utils/DataUtils.class';

interface DataFromDatePickerObj {
  mesNumber: number,
  anoNumber: number
}

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
  objetoData?: DataFromDatePickerObj;

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
        console.log(this.monthlyResponseDto)
        this.monthlyTransactionsResponseDtoList = this.monthlyResponseDto.transactions;

        this.monthlyTransactionsResponseDtoList.forEach((transaction) => {
        })
      }
    );
  }

  pagarTransacao(transaction: MonthlyTransactionResponseDto) {

    if (transaction.transactionType == "INSTALLMENT") {
      this.payTransactionRequestDto.transactionId = transaction.installmentTransactionId;
      this.payTransactionRequestDto.transactionType = transaction.transactionType;
    } else if (transaction.transactionType == "FIX") {
      this.payTransactionRequestDto.transactionId = transaction.fixTransactionId;
      this.payTransactionRequestDto.transactionType = transaction.transactionType;
    } else if (transaction.transactionType == "IN_CASH") {
      this.payTransactionRequestDto.transactionId = transaction.transactionId;
      this.payTransactionRequestDto.transactionType = transaction.transactionType;
    }

    this.transactionsService.updateTransactionPatch(this.payTransactionRequestDto).subscribe(
      (res) => {
        this.messageService.add({ severity: 'success', summary: 'Success', detail: 'Transação PAGA com sucesso' });
        setTimeout(() => {
          this.getTransactionsInMonth();
        }, 1500);
      },
      (error) => {
        this.messageService.add({ severity: 'error', summary: 'Error', detail: 'Erro ao tentar PAGAR transação' });
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

    this.transactionsService.updateTransaction(this.updateTransactionRequestDto).subscribe(
      (res) => {
        this.messageService.add({ severity: 'success', summary: 'Success', detail: 'Transação ATUALIZADA com sucesso' });
        setTimeout(() => {
          this.getTransactionsInMonth();
        }, 1500);
      },
      (error) => {
        this.messageService.add({ severity: 'error', summary: 'Error', detail: 'Erro ao tentar ATUALIZADA transação' });
      }
    );
  }

  deletarTransacao(transaction: MonthlyTransactionResponseDto) {
    this.transactionsService.deleteByTransactionId(transaction.transactionId).subscribe(
      (res) => {
        this.messageService.add({ severity: 'success', summary: 'Success', detail: 'Transação APAGADA com sucesso' });
        setTimeout(() => {
          this.getTransactionsInMonth();
        }, 1500);
      },
      (error) => {
        this.messageService.add({ severity: 'error', summary: 'Error', detail: 'Erro ao tentar APAGAR transação' });
      }
    );
  }

}
