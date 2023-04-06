import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
import { StatusTypeEnum } from '../../../enums/status-type.enum';

export class UpdateTransactionResponseDto {
  constructor(
    public id: number,

    public amount: number,

    public payDay: string,

    public description: string,

    public status: StatusTypeEnum,
    public category: TransactionCategoryTypeEnum,

    public fixedExpense: boolean,

    public paid: boolean,

    public repeat: number,
  ) {}
}
