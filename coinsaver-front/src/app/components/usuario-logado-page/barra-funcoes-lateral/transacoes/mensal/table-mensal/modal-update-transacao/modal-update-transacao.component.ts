import { Component, OnInit } from '@angular/core';
import { FormControl } from '@angular/forms';
import { Router } from '@angular/router';
import { MessageService } from 'primeng/api';
import { DynamicDialogConfig, DynamicDialogRef } from 'primeng/dynamicdialog';
import { UpdateTransactionRequestDto } from 'src/app/dtos/transactions/request/update-transaction.request.dto';
import { DivisionResponseDto } from 'src/app/dtos/transactions/response/division.response.dto';
import { MonthlyTransactionResponseDto } from 'src/app/dtos/transactions/response/monthly-transactions.response.dto';
import { MonthlyResponseDto } from 'src/app/dtos/transactions/response/monthly.response.dto';
import { UpdateTypeDto } from 'src/app/dtos/transactions/response/update-types.dto';
import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
import { TransactionTypeEnum } from 'src/app/enums/transaction-type.enum';
import { UpdateTransactionTypeEnum } from 'src/app/enums/update-transaction-type.enum';
import { DivisionsService } from 'src/app/services/divisions/divisions.service';
import { TransactionsService } from 'src/app/services/transactions/transactions.service';
import { DataUtils } from 'src/app/shared/utils/DataUtils.class';

@Component({
  selector: 'app-modal-update-transacao',
  templateUrl: './modal-update-transacao.component.html',
  styleUrls: ['./modal-update-transacao.component.css'],
})
export class ModalUpdateTransacaoComponent implements OnInit {
  constructor(
    private transactionsService: TransactionsService,
    public router: Router,
    public config: DynamicDialogConfig,
    private messageService: MessageService,
    private ref: DynamicDialogRef,
    private divisionsService: DivisionsService
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
  isFix?: boolean;
  isAtualizarTodas?: boolean;

  monthlyResponseDto: MonthlyResponseDto | undefined;
  monthlyTransactionsResponseDtoList: MonthlyTransactionResponseDto[] = [];
  listDivision: DivisionResponseDto[] | undefined;
  listUpdateTypes: UpdateTypeDto[] | undefined;

  ngOnInit(): void {
    this.updateTransactionRequestDto = this.config.data.row;
    this.verificaTipoTransacao(
      this.updateTransactionRequestDto.transactionType
    );
    this.defineCategoria(this.updateTransactionRequestDto.category);
    this.defineRepeticao(this.updateTransactionRequestDto.fixedExpense);

    console.log(
      'updateTransactionRequestDto',
      this.updateTransactionRequestDto
    );
  }
  validateTodasTransacoes(category: any) {
    if (
      category == 'ALL_EXPENSES' &&
      this.updateTransactionRequestDto.transactionType ==
      TransactionTypeEnum.INSTALLMENT
    ) {
      this.isAtualizarTodas = true;
    }
  }

  verificaTipoTransacao(transactionType: any) {
    if (transactionType == 'FIX') {
      this.updateTransactionRequestDto.fixedExpense == true;
      this.isFix = true;
      this.listUpdateTypes = [
        new UpdateTypeDto(
          'Esta apenas',
          UpdateTransactionTypeEnum.ONLY_THIS_EXPENSE
        ),
        new UpdateTypeDto('Todas', UpdateTransactionTypeEnum.ALL_EXPENSES),
      ];
    }

    if (transactionType == 'INSTALLMENT') {
      this.updateTransactionRequestDto.fixedExpense == false;
      this.isFix = false;
      this.listUpdateTypes = [
        new UpdateTypeDto(
          'Esta apenas',
          UpdateTransactionTypeEnum.ONLY_THIS_EXPENSE
        ),
        new UpdateTypeDto('Todas', UpdateTransactionTypeEnum.ALL_EXPENSES),
        new UpdateTypeDto(
          'Esta e as próximas',
          UpdateTransactionTypeEnum.THIS_EXPENSE_AND_FUTURE_ONES
        ),
      ];
    }

    if (transactionType == 'IN_CASH') {
      this.updateTransactionRequestDto.fixedExpense == false;
      this.isFix = false;
      this.listUpdateTypes = [
        new UpdateTypeDto(
          'Esta apenas',
          UpdateTransactionTypeEnum.ONLY_THIS_EXPENSE
        ),
      ];
    }
  }

  defineCategoria(category: any) {
    if (category == 'INCOME') {
      this.isStatusIncome = true;
      this.isStatusExpense = false;
      this.getDivisionByCategoryType(TransactionCategoryTypeEnum.INCOME);
    } else if (category == 'EXPENSE') {
      this.isStatusIncome = false;
      this.isStatusExpense = true;
      this.getDivisionByCategoryType(TransactionCategoryTypeEnum.EXPENSE);
    }
  }

  private getDivisionByCategoryType(
    transactionCategoryType: TransactionCategoryTypeEnum
  ) {
    this.divisionsService
      .getDivisionByCategoryType(transactionCategoryType)
      .subscribe((res) => {
        this.listDivision = res;

        this.listDivision?.forEach((division) => { });
      });
  }

  defineRepeticao(fixedExpense: any) {
    if (fixedExpense == 'true') {
      this.isRepeticao = false;
      this.updateTransactionRequestDto.repeat == null;
    } else if (fixedExpense == 'false') {
      this.isRepeticao = true;
    }
  }

  atualizarTransacao() {
    if (
      this.updateTransactionRequestDto.transactionType !=
      TransactionTypeEnum.FIX
    ) {
      this.updateTransactionRequestDto.fixedExpense = false;
    } else {
      this.updateTransactionRequestDto.fixedExpense = true;
    }
    console.log(
      'updateTransactionRequestDto',
      this.updateTransactionRequestDto
    );

    this.transactionsService
      .updateTransaction(this.updateTransactionRequestDto)
      .subscribe(
        (res) => {
          this.messageService.add({
            severity: 'success',
            summary: 'Success',
            detail: 'Transação ATUALIZADA com sucesso',
          });
          setTimeout(() => {
            this.fecharModal();
            this.retornaPaginaInicialUsuarioLogado();
          }, 1500);
        },
        (error) => {
          this.messageService.add({
            severity: 'error',
            summary: 'Error',
            detail: 'Erro ao tentar ATUALIZADA transação',
          });
        }
      );
  }

  getTransactionsInMonth() {
    this.date = this.dataUtils.transformaToLocalDateFormat('US');

    this.transactionsService
      .getTransactionsInMonth(this.date)
      .subscribe((res) => {
        this.monthlyResponseDto = res;
        console.log(this.monthlyResponseDto);
        this.monthlyTransactionsResponseDtoList =
          this.monthlyResponseDto.transactions;

        this.monthlyTransactionsResponseDtoList.forEach((transaction) => { });
      });
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
