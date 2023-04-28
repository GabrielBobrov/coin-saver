import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
import { TransactionTypeEnum } from 'src/app/enums/transaction-type.enum';
import { StatusTypeEnum } from '../../../enums/status-type.enum';
import { DivisionResponseDto } from './division.response.dto';

export class MonthlyTransactionResponseDto {
  constructor(
    public transactionId: number,
    public fixTransactionId: number,
    public installmentTransactionId: number,

    public amount: number,

    public payDay: string | Date,


    public description: string,

    public status: StatusTypeEnum,
    public category: TransactionCategoryTypeEnum,
    public transactionType: TransactionTypeEnum,

    public division: DivisionResponseDto,
  ) {}
}
