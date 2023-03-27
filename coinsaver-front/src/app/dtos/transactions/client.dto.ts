import { TransactionCategoryType } from 'src/app/enums/transaction-category-type.enum';
import { TransactionType } from 'src/app/enums/transaction-type.enum';
import { StatusType } from '../../enums/status-type.enum';
export class Client {
  constructor(
    public id?: number,
    public name?: string,
    public email?: string,

    public createdAt?: Date,

    public balance?: number,
  ) {}
}
