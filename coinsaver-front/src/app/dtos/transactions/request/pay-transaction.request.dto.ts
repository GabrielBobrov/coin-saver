import { TransactionTypeEnum } from 'src/app/enums/transaction-type.enum';

export class PayTransactionRequestDto {
  constructor(
    public transactionId: number,

    public transactionType: TransactionTypeEnum,
  ) {}
}
