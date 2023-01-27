package com.coinsaver.api.dtos.response;

import lombok.*;

import java.math.BigDecimal;
import java.util.List;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class MonthlyResponseDto {

    List<TransactionResponseDto> transactionResponseDtos;
    BigDecimal monthlyBalance;
}
