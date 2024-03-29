package com.coinsaver.api.dtos.request;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.core.enums.TransactionType;
import com.coinsaver.core.enums.UpdateTransactionType;
import com.coinsaver.domain.entities.FixTransaction;
import com.coinsaver.domain.entities.InstallmentTransaction;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.modelmapper.ModelMapper;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
public class UpdateTransactionRequestDto {

    @NotNull
    private BigDecimal amount;
    private LocalDate payDay;
    @NotBlank
    private String description;
    @NotNull
    private StatusType status;
    @NotNull
    private TransactionCategoryType category;
    @NotNull
    private Boolean fixedExpense;
    private Integer repeat;
    @NotNull
    private UpdateTransactionType updateTransactionType;
    @NotNull
    private TransactionType transactionType;
    @NotNull
    private Long transactionId;
    private Long installmentTransactionId;
    private Long fixTransactionId;

    public FixTransaction convertDtoToFixTransactionEntity() {
        return new ModelMapper().map(this, FixTransaction.class);
    }
}
