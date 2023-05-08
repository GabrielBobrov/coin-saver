import { MonthlyTransactionResponseDto } from './monthly-transactions.response.dto';

export class AuthenticationResponseDto {
  constructor(
    public token?: string,
  ) {}
}
