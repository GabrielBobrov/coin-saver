import { DynamicDialogRef } from 'primeng/dynamicdialog';
import { Component } from '@angular/core';
import { FormControl } from '@angular/forms';
import { Router } from '@angular/router';
import { MessageService } from 'primeng/api';
import { TransactionRequestDto } from 'src/app/dtos/transactions/request/transaction.request.dto';
import { TransactionsService } from 'src/app/services/transactions/transactions.service';

@Component({
  selector: 'app-modal-cadastro-nova-transacao',
  templateUrl: './modal-cadastro-nova-transacao.component.html',
  styleUrls: ['./modal-cadastro-nova-transacao.component.css']
})
export class ModalCadastroNovaTransacaoComponent {

  constructor(
    private transactionsService: TransactionsService,
    public router: Router,
    private messageService: MessageService,
    private ref: DynamicDialogRef
  ) { }

  categoryTypeControl = new FormControl();
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
    repeat: undefined
  };

  createTransaction(transactionRequestDto: TransactionRequestDto) {
    this.transactionsService.createTransaction(transactionRequestDto).subscribe(
      (res) => {
        this.messageService.add({ severity: 'success', summary: 'Success', detail: 'Transação CRIADA com sucesso' });
        this.cleanObject();
        this.fecharModal();
        this.retornaPaginaInicialUsuarioLogado();
      },
      (error) => {
        this.messageService.add({ severity: 'error', summary: 'Error', detail: 'Erro ao tentar CRIAR transação' });
      }
    );
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
      repeat: undefined
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
