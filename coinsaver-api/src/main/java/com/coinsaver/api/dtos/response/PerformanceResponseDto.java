package com.coinsaver.api.dtos.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PerformanceResponseDto {

    private PerformanceEconomyResponseDto economy;
    private PerformanceMonthlyBalanceResponseDto monthlyBalance;
    private PerformanceDivisionsResponseDto division;

}
