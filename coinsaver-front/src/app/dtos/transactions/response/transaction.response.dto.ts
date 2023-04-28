import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
import { TransactionTypeEnum } from 'src/app/enums/transaction-type.enum';
import { StatusTypeEnum } from '../../../enums/status-type.enum';

export class TransactionResponseDto {
  constructor(
    public transactionId: number,
    public installmentTransactionId: number,
    public fixTransactionId: number,

    public amount: number,

    public payDay: string | Date,

    public description: string,

    public status: StatusTypeEnum,
    public category: TransactionCategoryTypeEnum,

    public fixedExpense: boolean,

    public transactionType: TransactionTypeEnum,

    public repeat: number,
  ) {}
}
