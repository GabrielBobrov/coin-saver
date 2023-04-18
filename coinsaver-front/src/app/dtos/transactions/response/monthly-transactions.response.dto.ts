import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
import { TransactionTypeEnum } from 'src/app/enums/transaction-type.enum';
import { StatusTypeEnum } from '../../../enums/status-type.enum';

export class MonthlyTransactionResponseDto {
  constructor(
    public transactionId: number,
    public fixTransactionId: number,
    public installmentTransactionId: number,

    public amount: number,

    public payDay: string,

    public description: string,

    public status: StatusTypeEnum,
    public category: TransactionCategoryTypeEnum,
    public transactionType: TransactionTypeEnum,
  ) {}
}