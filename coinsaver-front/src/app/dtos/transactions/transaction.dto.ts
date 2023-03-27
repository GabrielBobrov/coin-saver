import { TransactionCategoryType } from 'src/app/enums/transaction-category-type.enum';
import { TransactionType } from 'src/app/enums/transaction-type.enum';
import { StatusType } from './../../enums/status-type.enum';
import { Client } from './client.dto';
export class Transaction {
  constructor(
    public id?: number,
    public amount?: number,

    public createdAt?: Date,
    public payDay?: Date,

    public description?: string,

    public status?: StatusType,
    public category?: TransactionCategoryType,
    public transactionType?: TransactionType,

    public fixedExpense?: boolean,
    public repeat?: number,

    public client?: Client

  ) {}
}
