import { UpdateTransactionTypeEnum } from 'src/app/enums/update-transaction-type.enum';

export class UpdateTypeDto {
  constructor(
    public name: string,

    public updateTransactionType: UpdateTransactionTypeEnum
  ) {}
}
