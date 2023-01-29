package com.coinsaver.domain.entities;

import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.api.dtos.response.UpdateTransactionResponseDto;
import org.modelmapper.ModelMapper;

public abstract class TransactionBase {

    public TransactionResponseDto convertEntityToResponseDto() {
        return new ModelMapper().map(this, TransactionResponseDto.class);
    }

    public TransactionRequestDto convertEntityToRequestDto() {
        return new ModelMapper().map(this, TransactionRequestDto.class);
    }

    public UpdateTransactionResponseDto convertEntityToUpdateResponseDto() {
        return new ModelMapper().map(this, UpdateTransactionResponseDto.class);
    }
}
