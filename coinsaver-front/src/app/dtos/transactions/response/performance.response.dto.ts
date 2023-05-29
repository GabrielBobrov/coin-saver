import { PerformanceDivisionsResponseDto } from "./performance-divisions.response.dto";
import { PerformanceEconomyResponseDto } from "./performance-economy.response.dto";
import { PerformanceMonthlyBalanceResponseDto } from "./performance-monthly-balance.response.dto";

export class PerformanceResponseDto {
  constructor(
    public economy: PerformanceEconomyResponseDto,
    public monthlyBalance: PerformanceMonthlyBalanceResponseDto,
    public division: PerformanceDivisionsResponseDto,
  ) {}
}
