package com.coinsaver.domain.entities;

import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.response.MonthlyTransactionResponseDto;
import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.api.dtos.response.UpdateTransactionResponseDto;
import org.modelmapper.ModelMapper;

public abstract class TransactionBase {

    public TransactionResponseDto convertEntityToResponseDto() {
        return new ModelMapper().map(this, TransactionResponseDto.class);
    }
    public MonthlyTransactionResponseDto convertEntityToMonthlyResponseDto() {
        return new ModelMapper().map(this, MonthlyTransactionResponseDto.class);
    }
    public TransactionRequestDto convertEntityToRequestDto() {
        return new ModelMapper().map(this, TransactionRequestDto.class);
    }

    public UpdateTransactionResponseDto convertEntityToUpdateResponseDto() {
        return new ModelMapper().map(this, UpdateTransactionResponseDto.class);
    }

    public FixTransaction convertEntityToAmendedEntity() {
        return new ModelMapper().map(this, FixTransaction.class);
    }
}
