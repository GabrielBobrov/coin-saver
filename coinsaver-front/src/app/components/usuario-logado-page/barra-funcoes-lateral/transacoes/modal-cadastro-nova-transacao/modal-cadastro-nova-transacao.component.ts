import { Component } from '@angular/core';
import { FormControl } from '@angular/forms';
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

    transactionRequestDto.payDay = transactionRequestDto.payDay + "T11:54:15.747431";

    // if (transactionRequestDto.fixedExpense == "false") {
    //   if (transactionRequestDto.repeat == 0 ||
    //       transactionRequestDto.repeat == 1) {
    //         transactionRequestDto.repeat == null;
    //       }
    // } else if (transactionRequestDto.fixedExpense == "true") {
    //   transactionRequestDto.repeat == null;
    // }

    this.transactionsService.createTransaction(transactionRequestDto);
    // this.cleanObject();
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
}
