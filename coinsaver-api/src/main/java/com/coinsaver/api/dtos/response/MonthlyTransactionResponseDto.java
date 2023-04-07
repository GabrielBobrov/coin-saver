package com.coinsaver.api.dtos.response;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.core.enums.TransactionType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class MonthlyTransactionResponseDto {

    private Long transactionId;
    private Long fixTransactionId;
    private Long installmentTransactionId;
    private BigDecimal amount;
    private LocalDateTime payDay;
    private String description;
    private StatusType status;
    private TransactionCategoryType category;
    private TransactionType transactionType;
}
