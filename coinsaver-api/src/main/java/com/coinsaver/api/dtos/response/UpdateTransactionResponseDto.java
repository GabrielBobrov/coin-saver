package com.coinsaver.api.dtos.response;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.domain.entities.InstallmentTransaction;
import com.coinsaver.domain.entities.Transaction;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.modelmapper.ModelMapper;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
public class UpdateTransactionResponseDto {

    private Long id;

    @NotNull
    private BigDecimal amount;

    private LocalDateTime payDay;
    @NotBlank
    private String description;
    @NotNull
    private StatusType status;
    @NotNull
    private TransactionCategoryType category;
    @NotNull
    private Boolean fixedExpense;

    private Integer repeat;

    public Transaction convertDtoToTransactionEntity() {
        return new ModelMapper().map(this, Transaction.class);
    }

    public InstallmentTransaction convertDtoToInstallmentTransactionEntity() {
        return new ModelMapper().map(this, InstallmentTransaction.class);
    }
}
