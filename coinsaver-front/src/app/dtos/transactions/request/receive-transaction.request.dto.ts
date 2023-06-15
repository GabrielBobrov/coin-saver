import { TransactionTypeEnum } from 'src/app/enums/transaction-type.enum';

export class ReceiveTransactionRequestDto {
  constructor(
    public transactionId?: number,

    public transactionType?: TransactionTypeEnum,
  ) {}
}
