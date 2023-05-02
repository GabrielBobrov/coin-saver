import { Component, OnInit } from '@angular/core';
import { FormControl } from '@angular/forms';
import { Router } from '@angular/router';
import { MessageService } from 'primeng/api';
import { DynamicDialogConfig, DynamicDialogRef } from 'primeng/dynamicdialog';
import { TransactionRequestDto } from 'src/app/dtos/transactions/request/transaction.request.dto';
import { UpdateTransactionRequestDto } from 'src/app/dtos/transactions/request/update-transaction.request.dto';
import { MonthlyTransactionResponseDto } from 'src/app/dtos/transactions/response/monthly-transactions.response.dto';
import { MonthlyResponseDto } from 'src/app/dtos/transactions/response/monthly.response.dto';
import { DivisionsService } from 'src/app/services/divisions/divisions.service';
import { TransactionsService } from 'src/app/services/transactions/transactions.service';
import { DataUtils } from 'src/app/shared/utils/DataUtils.class';

@Component({
  selector: 'app-modal-update-transacao',
  templateUrl: './modal-update-transacao.component.html',
  styleUrls: ['./modal-update-transacao.component.css']
})
export class ModalUpdateTransacaoComponent implements OnInit {

  constructor(
    private transactionsService: TransactionsService,
    public router: Router,
    public config: DynamicDialogConfig,
    private messageService: MessageService,
    private ref: DynamicDialogRef
  ) { }

  transactionCategoryTypeControl = new FormControl();
  updateTransactionRequestDto = new UpdateTransactionRequestDto();

  date: string = '';
  dataUtils = new DataUtils();

  transactionRequestDto: TransactionRequestDto | undefined;
  monthlyResponseDto: MonthlyResponseDto | undefined;
  monthlyTransactionsResponseDtoList: MonthlyTransactionResponseDto[] = [];

  ngOnInit(): void {
    this.transactionRequestDto = this.config.data.row;

    console.log("transactionRequestDto", this.transactionRequestDto)

  }

  atualizarTransacao(transaction: MonthlyTransactionResponseDto) {

    console.log("atualizar", transaction)

    this.updateTransactionRequestDto.amount = transaction.amount;
    this.updateTransactionRequestDto.payDay = transaction.payDay;
    this.updateTransactionRequestDto.description = transaction.description;
    this.updateTransactionRequestDto.status = transaction.status;
    this.updateTransactionRequestDto.category = transaction.category;

    // this.updateTransactionRequestDto.fixedExpense = transaction.
    // this.updateTransactionRequestDto.repeat = transaction.
    // this.updateTransactionRequestDto.updateTransactionType = transaction.

    this.updateTransactionRequestDto.transactionType = transaction.transactionType;
    this.updateTransactionRequestDto.transactionId = transaction.transactionId;

    this.updateTransactionRequestDto.installmentTransactionId = transaction.installmentTransactionId;
    this.updateTransactionRequestDto.fixTransactionId = transaction.fixTransactionId;

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

  fecharModal() {
    this.ref.close();
  }
}
