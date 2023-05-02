import { Component, OnInit } from '@angular/core';
import { FormControl } from '@angular/forms';
import { Router } from '@angular/router';
import { MessageService } from 'primeng/api';
import { DynamicDialogConfig, DynamicDialogRef } from 'primeng/dynamicdialog';
import { UpdateTransactionRequestDto } from 'src/app/dtos/transactions/request/update-transaction.request.dto';
import { DivisionResponseDto } from 'src/app/dtos/transactions/response/division.response.dto';
import { MonthlyTransactionResponseDto } from 'src/app/dtos/transactions/response/monthly-transactions.response.dto';
import { MonthlyResponseDto } from 'src/app/dtos/transactions/response/monthly.response.dto';
import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
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
    private ref: DynamicDialogRef,
    private divisionsService: DivisionsService,
  ) { }

  divisionType = new FormControl();
  transactionTypeControl = new FormControl();
  statusTypeControl = new FormControl();
  transactionCategoryTypeControl = new FormControl();
  fixedExpenseControl = new FormControl();
  updateTransactionTypeControl = new FormControl();

  updateTransactionRequestDto = new UpdateTransactionRequestDto();

  date: string = '';
  dataUtils = new DataUtils();

  isStatusIncome?: boolean;
  isStatusExpense?: boolean;
  isRepeticao?: boolean;

  monthlyResponseDto: MonthlyResponseDto | undefined;
  monthlyTransactionsResponseDtoList: MonthlyTransactionResponseDto[] = [];
  listDivision: DivisionResponseDto[] | undefined;

  ngOnInit(): void {
    this.updateTransactionRequestDto = this.config.data.row;
    this.defineCategoria(this.updateTransactionRequestDto.category);
    this.defineRepeticao(this.updateTransactionRequestDto.fixedExpense);

    console.log("updateTransactionRequestDto", this.updateTransactionRequestDto)

  }

  defineCategoria(category: any) {
    if (category == "INCOME") {
      this.isStatusIncome = true;
      this.isStatusExpense = false;
      this.getDivisionByCategoryType(TransactionCategoryTypeEnum.INCOME);

    } else if (category == "EXPENSE") {
      this.isStatusIncome = false;
      this.isStatusExpense = true;
      this.getDivisionByCategoryType(TransactionCategoryTypeEnum.EXPENSE);
    }
  }

  private getDivisionByCategoryType(transactionCategoryType: TransactionCategoryTypeEnum) {
    this.divisionsService.getDivisionByCategoryType(transactionCategoryType).subscribe(
      (res) => {
        this.listDivision = res;

        this.listDivision?.forEach((division) => {
        })
      }
    );
  }

  defineRepeticao(fixedExpense: any) {
    if (fixedExpense == "true") {
      this.isRepeticao = false;
      this.updateTransactionRequestDto.repeat == null;
    } else if (fixedExpense == "false") {
      this.isRepeticao = true;
    }
  }

  atualizarTransacao(updateTransactionRequestDto: any) {

    // this.updateTransactionRequestDto.amount = transaction.amount;
    // this.updateTransactionRequestDto.payDay = transaction.payDay;
    // this.updateTransactionRequestDto.description = transaction.description;
    // this.updateTransactionRequestDto.status = transaction.status;
    // this.updateTransactionRequestDto.category = transaction.category;

    // this.updateTransactionRequestDto.fixedExpense = transaction.
    // this.updateTransactionRequestDto.repeat = transaction.
    // this.updateTransactionRequestDto.updateTransactionType = transaction.

    // this.updateTransactionRequestDto.transactionType = transaction.transactionType;
    // this.updateTransactionRequestDto.transactionId = transaction.transactionId;

    // this.updateTransactionRequestDto.installmentTransactionId = transaction.installmentTransactionId;
    // this.updateTransactionRequestDto.fixTransactionId = transaction.fixTransactionId;

    console.log("updateTransactionRequestDto", updateTransactionRequestDto)

    this.transactionsService.updateTransaction(updateTransactionRequestDto).subscribe(
      (res) => {
        this.messageService.add({ severity: 'success', summary: 'Success', detail: 'Transação ATUALIZADA com sucesso' });
        setTimeout(() => {
          this.fecharModal();
          this.retornaPaginaInicialUsuarioLogado();
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

  retornaPaginaInicialUsuarioLogado() {
    this.router.navigateByUrl('usuario-logado-page', {
      state: {
        data: {},
      },
    });
  }

  fecharModal() {
    this.ref.close();
  }
}
