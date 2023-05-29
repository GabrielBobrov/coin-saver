export class PerformanceMonthlyBalanceResponseDto {
  constructor(
    public actualMonthBalance: number,
    public actualMonthName: string,
    public previousMonthPercentageDifference: string,
  ) {}
}
