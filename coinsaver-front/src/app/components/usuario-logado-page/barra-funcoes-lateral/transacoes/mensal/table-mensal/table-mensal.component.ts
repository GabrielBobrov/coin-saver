import { Component, OnInit } from '@angular/core';
import { MessageService } from 'primeng/api';
import { PayTransactionRequestDto } from 'src/app/dtos/transactions/request/pay-transaction.request.dto';
import { UpdateTransactionRequestDto } from 'src/app/dtos/transactions/request/update-transaction.request.dto';
import { MonthlyTransactionResponseDto } from 'src/app/dtos/transactions/response/monthly-transactions.response.dto';
import { MonthlyResponseDto } from 'src/app/dtos/transactions/response/monthly.response.dto';
import { TransactionsService } from 'src/app/services/transactions/transactions.service';
import { DataUtils } from 'src/app/shared/utils/DataUtils.class';
import { ModalUpdateTransacaoComponent } from './modal-update-transacao/modal-update-transacao.component';
import { DialogService } from 'primeng/dynamicdialog';

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

  dataDatepicker: any;

  date: string = '';
  dataUtils = new DataUtils();
  objetoData?: DataFromDatePickerObj;

  constructor(
    private transactionsService: TransactionsService,
    private messageService: MessageService,
    private dialogService: DialogService,
  ) { }

  ngOnInit() {
    this.formataDataInicialTabelaMensal();
  }

  recebeDataMontadaDatepicker(dataMontadaDatepicker: any) {
    this.dataDatepicker = dataMontadaDatepicker;
    this.atualizaTabelaMesComNovaData();
  }

  formataDataInicialTabelaMensal() {
    this.date = this.dataUtils.transformaToLocalDateFormat('US');
    this.getTransactionsInMonth(this.date);
  }

  private getTransactionsInMonth(date: any) {
    this.transactionsService.getTransactionsInMonth(date).subscribe(
      (res) => {
        this.monthlyResponseDto = res;

        console.log(this.monthlyResponseDto);

        this.monthlyTransactionsResponseDtoList = this.monthlyResponseDto.transactions;
        this.monthlyTransactionsResponseDtoList.forEach((transaction) => {
        });
      }
    );
  }

  atualizaTabelaMesComNovaData() {
    this.date = this.dataUtils.transformaDataInput(this.dataDatepicker);
    this.getTransactionsInMonth(this.date);
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
          this.atualizaTabelaMesComNovaData();
        }, 1500);
      },
      (error) => {
        this.messageService.add({ severity: 'error', summary: 'Error', detail: 'Erro ao tentar PAGAR transação' });
      }
    );
  }

  abrirModalUpdateTransacao(transaction: MonthlyTransactionResponseDto) {
    this.dialogService.open(ModalUpdateTransacaoComponent, {
      data: { row: transaction },
      showHeader: false
    });
  }

  deletarTransacao(transaction: MonthlyTransactionResponseDto) {
    this.transactionsService.deleteByTransactionId(transaction.transactionId).subscribe(
      (res) => {
        this.messageService.add({ severity: 'success', summary: 'Success', detail: 'Transação APAGADA com sucesso' });
        setTimeout(() => {
          this.atualizaTabelaMesComNovaData();
        }, 1500);
      },
      (error) => {
        this.messageService.add({ severity: 'error', summary: 'Error', detail: 'Erro ao tentar APAGAR transação' });
      }
    );
  }

}
