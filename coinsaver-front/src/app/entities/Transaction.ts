import { StatusTypeEnum } from "../enums/status-type.enum";
import { TransactionCategoryTypeEnum } from "../enums/transaction-category-type.enum";
import { TransactionTypeEnum } from "../enums/transaction-type.enum";
import { Client } from "./Client";

export class Transaction {
  constructor(
    public id: number,

    public amount: number,

    public createdAt: string,

    public payDay: string,

    public description: string,

    public status: StatusTypeEnum,
    public category: TransactionCategoryTypeEnum,
    public transactionType: TransactionTypeEnum,

    public fixedExpense: boolean,

    public repeat: number,

    public client: Client,

  ) {}
}
