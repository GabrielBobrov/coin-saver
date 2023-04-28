import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
import { StatusTypeEnum } from '../../../enums/status-type.enum';
import { TransactionTypeEnum } from 'src/app/enums/transaction-type.enum';
import { UpdateTransactionTypeEnum } from 'src/app/enums/update-transaction-type.enum';

export class UpdateTransactionRequestDto {
  constructor(
    public amount?: number,

    public payDay?: string | Date,


    public description?: string,

    public status?: StatusTypeEnum,
    public category?: TransactionCategoryTypeEnum,

    public fixedExpense?: boolean,

    public repeat?: number,

    public updateTransactionType?: UpdateTransactionTypeEnum,
    public transactionType?: TransactionTypeEnum,

    public transactionId?: number,
    public installmentTransactionId?: number,
    public fixTransactionId?: number,
  ) {}
}
