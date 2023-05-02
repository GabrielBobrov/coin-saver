import { DataUtils } from './../../../../../shared/utils/DataUtils.class';
import { DynamicDialogRef } from 'primeng/dynamicdialog';
import { Component } from '@angular/core';
import { FormControl } from '@angular/forms';
import { Router } from '@angular/router';
import { MessageService } from 'primeng/api';
import { TransactionRequestDto } from 'src/app/dtos/transactions/request/transaction.request.dto';
import { TransactionsService } from 'src/app/services/transactions/transactions.service';
import { DivisionsService } from 'src/app/services/divisions/divisions.service';
import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
import { DivisionResponseDto } from 'src/app/dtos/transactions/response/division.response.dto';

@Component({
  selector: 'app-modal-cadastro-nova-transacao',
  templateUrl: './modal-cadastro-nova-transacao.component.html',
  styleUrls: ['./modal-cadastro-nova-transacao.component.css']
})
export class ModalCadastroNovaTransacaoComponent {

  constructor(
    private transactionsService: TransactionsService,
    private divisionsService: DivisionsService,
    public router: Router,
    private messageService: MessageService,
    private ref: DynamicDialogRef
  ) { }

  divisionType = new FormControl();
  transactionTypeControl = new FormControl();
  statusTypeControl = new FormControl();
  transactionCategoryTypeControl = new FormControl();
  fixedExpenseControl = new FormControl();

  transactionRequestDto: TransactionRequestDto = {
    id: undefined,
    amount: undefined,
    payDay: undefined,
    description: undefined,
    status: undefined,
    category: undefined,
    fixedExpense: undefined,
    repeat: undefined,
    divisionId: undefined
  };

  dataUtils = new DataUtils();

  isStatusIncome?: boolean;
  isStatusExpense?: boolean;
  isRepeticao?: boolean;

  transactionCategoryType: TransactionCategoryTypeEnum | undefined;
  listDivision: DivisionResponseDto[] | undefined;

  createTransaction(transactionRequestDto: TransactionRequestDto) {
    var payDayFormated = this.dataUtils.transformaDataInput(transactionRequestDto.payDay);
    transactionRequestDto.payDay = payDayFormated;

    this.transactionRequestDto.repeat = transactionRequestDto.repeat;

    if (this.transactionRequestDto.repeat == 0 || this.transactionRequestDto.repeat == 1) {
      transactionRequestDto.repeat == null;
    }

    console.log(transactionRequestDto.repeat)

    this.transactionsService.createTransaction(transactionRequestDto).subscribe(
      (res) => {
        this.messageService.add({ severity: 'success', summary: 'Success', detail: 'Transação CRIADA com sucesso' });
        setTimeout(() => {
          this.cleanObject();
          this.fecharModal();
          this.retornaPaginaInicialUsuarioLogado();
        }, 1500);
      },
      (error) => {
        this.messageService.add({ severity: 'error', summary: 'Error', detail: 'Erro ao tentar CRIAR transação' });
        this.transactionRequestDto.payDay = undefined;
      }
    );
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
    } else if (fixedExpense == "false") {
      this.isRepeticao = true;
    }
  }

  cleanObject() {
    this.transactionRequestDto = {
      id: undefined,
      amount: undefined,
      payDay: undefined,
      description: undefined,
      status: undefined,
      category: undefined,
      fixedExpense: undefined,
      repeat: undefined,
      divisionId: undefined
    }
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
