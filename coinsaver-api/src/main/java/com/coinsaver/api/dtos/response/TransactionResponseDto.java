package com.coinsaver.api.dtos.response;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.core.enums.TransactionType;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
public class TransactionResponseDto {

    private Long transactionId;
    private Long installmentTransactionId;
    private Long fixTransactionId;
    private BigDecimal amount;
    private LocalDate payDay;
    private String description;
    private StatusType status;
    private TransactionCategoryType category;
    private Boolean fixedExpense;
    private Integer repeat_;
    private TransactionType transactionType;
    private DivisionResponseDto division;
}
