import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
import { TransactionTypeEnum } from 'src/app/enums/transaction-type.enum';
import { StatusTypeEnum } from './../../enums/status-type.enum';
import { Client } from '../client/client.dto';
export class Transaction {
  constructor(
    public id?: number | any,
    public amount?: number | any,

    public createdAt?: string | any,
    public payDay?: string | any,

    public description?: string | any,

    public status?: StatusTypeEnum,
    public category?: TransactionCategoryTypeEnum,
    public transactionType?: TransactionTypeEnum,

    public fixedExpense?: boolean | any,
    public repeat?: number | any,

    public client?: Client

  ) {}
}
