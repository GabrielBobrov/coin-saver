import { DivisionTypeEnum } from "../enums/division-type.enum copy";
import { TransactionCategoryTypeEnum } from "../enums/transaction-category-type.enum";

export class Division {
  constructor(
    public id: number,
    public name: string,
    public type: DivisionTypeEnum,
    public category: TransactionCategoryTypeEnum,

    public createdAt: string,
  ) {}
}
