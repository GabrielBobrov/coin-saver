package com.coinsaver.api.dtos.response;

import lombok.*;

import java.math.BigDecimal;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class MonthlyChartDto {

    String categoryName;
    BigDecimal totalAmount;
}
