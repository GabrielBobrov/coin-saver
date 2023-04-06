import { StatusTypeEnum } from "../enums/status-type.enum";
import { TransactionCategoryTypeEnum } from "../enums/transaction-category-type.enum";
import { Transaction } from "./Transaction";

export class FixTransaction {
  constructor(
    public id: number,

    public amount: number,

    public createdAt: string,

    public payDay: string,

    public description: string,

    public status: StatusTypeEnum,
    public category: TransactionCategoryTypeEnum,

    public edited: boolean,

    public transaction: Transaction,
  ) {}
}
