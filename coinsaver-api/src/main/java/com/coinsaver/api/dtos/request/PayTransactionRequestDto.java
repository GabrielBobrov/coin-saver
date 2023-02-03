package com.coinsaver.api.dtos.request;

import com.coinsaver.core.enums.TransactionType;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
public class PayTransactionRequestDto {

    @NotNull
    private Long transactionId;
    @NotNull
    private TransactionType transactionType;

}
