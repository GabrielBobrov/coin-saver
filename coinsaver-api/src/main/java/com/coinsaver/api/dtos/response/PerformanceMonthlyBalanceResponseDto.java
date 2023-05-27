package com.coinsaver.api.dtos.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.math.BigDecimal;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PerformanceMonthlyBalanceResponseDto {

    private BigDecimal actualMonthBalance;
    private String actualMonthName;
    private String previousMonthPercentageDifference;

}
