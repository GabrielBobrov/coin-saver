import { TransactionTypeEnum } from './../../enums/transaction-type.enum';
import { TransactionResponseDto } from '../../dtos/transactions/response/transaction.response.dto';
import { TransactionsService } from './../../services/transactions/transactions.service';
import { Component, OnInit } from '@angular/core';
import { DataUtils } from 'src/app/shared/utils/DataUtils.class';
import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
import { Router } from '@angular/router';

@Component({
  selector: 'app-usuario-logado-page',
  templateUrl: './usuario-logado-page.component.html',
  styleUrls: ['./usuario-logado-page.component.css']
})
export class UsuarioLogadoPageComponent implements OnInit {

  transaction: TransactionResponseDto | undefined;
  listTransaction: TransactionResponseDto[] | undefined;
  transactionId: number = 0;
  transactionType: TransactionTypeEnum | undefined;
  transactionCategoryType: TransactionCategoryTypeEnum | undefined;
  date: string = '';
  dataUtils = new DataUtils();

  constructor(
    private transactionsService: TransactionsService,
    public router: Router,
  ) {}

  ngOnInit(): void {
    this.getTransaction();
    this.getTransactionByCategoryType();
  }

  getTransaction() {
    this.transactionId = 1;
    this.transactionType = TransactionTypeEnum.IN_CASH;

    this.transactionsService.getTransaction(this.transactionId, this.transactionType)
      .subscribe((res) => {
        this.transaction = res;

        console.log('id cat type', this.transaction)
      });
  }

  getTransactionByCategoryType() {
    this.transactionCategoryType = TransactionCategoryTypeEnum.EXPENSE;
    this.date = this.dataUtils.transformaToLocalDateFormat('US');

    this.transactionsService.getTransactionByCategoryType(this.transactionCategoryType, this.date)
      .subscribe((res) => {
        this.listTransaction = res;

        this.listTransaction?.forEach((transaction) => {
        })

        console.log('listTransaction cat type', this.listTransaction)
      });
  }

}
