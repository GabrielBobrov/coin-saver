package com.coinsaver.api.dtos.request;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.domain.entities.FixTransaction;
import com.coinsaver.domain.entities.InstallmentTransaction;
import com.coinsaver.domain.entities.Transaction;
import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.validation.constraints.Min;
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
public class TransactionRequestDto {

    @JsonIgnore
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
    @NotNull
    private Boolean paid;
    @Min(2)
    private Integer repeat;

    public InstallmentTransaction convertDtoToInstallmentTransactionEntity() {
        return new ModelMapper().map(this, InstallmentTransaction.class);
    }

    public Transaction convertDtoToTransactionEntity() {
        return new ModelMapper().map(this, Transaction.class);
    }

    public FixTransaction convertDtoToFixTransactionEntity() {
        return new ModelMapper().map(this, FixTransaction.class);
    }
}
