package com.coinsaver.api.dtos.response;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.core.enums.TransactionType;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class MonthlyTransactionResponseDto {

    private Long id;
    private BigDecimal amount;
    private LocalDateTime payDay;
    private String description;
    private StatusType status;
    private TransactionCategoryType category;
    private Boolean fixedExpense;
    private Integer repeat;
    private TransactionType transactionType;
}
