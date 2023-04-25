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
      this.transactionsService.createTransaction(transactionRequestDto).subscribe(
        (res) => {
          console.log(res)
        },
        (error) => {

        }
      );
    this.cleanObject();
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
