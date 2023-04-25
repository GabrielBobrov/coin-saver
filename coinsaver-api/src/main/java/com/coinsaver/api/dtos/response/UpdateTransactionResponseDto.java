package com.coinsaver.api.dtos.response;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
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
public class UpdateTransactionResponseDto {

    private Long id;
    private BigDecimal amount;
    private LocalDate payDay;
    private String description;
    private StatusType status;
    private TransactionCategoryType category;
    private Boolean fixedExpense;
    private Boolean paid;
    private Integer repeat;
}
