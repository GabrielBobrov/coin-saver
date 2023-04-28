import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
import { DivisionTypeEnum } from 'src/app/enums/division-type.enum copy';

export class DivisionResponseDto {
  constructor(
    public id: number,
    public name: string,

    public type: DivisionTypeEnum,

    public category: TransactionCategoryTypeEnum,
  ) {}
}
