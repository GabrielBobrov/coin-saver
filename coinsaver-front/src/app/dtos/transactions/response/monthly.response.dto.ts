import { MonthlyTransactionResponseDto } from './monthly-transactions.response.dto';

export class MonthlyResponseDto {
  constructor(
    public monthlyBalance: number,
    public transactions: MonthlyTransactionResponseDto[],
  ) {}
}
