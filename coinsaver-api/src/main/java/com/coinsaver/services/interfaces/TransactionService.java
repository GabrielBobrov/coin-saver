package com.coinsaver.services.interfaces;

import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.core.enums.TransactionCategoryType;

import java.time.LocalDateTime;
import java.util.List;

public interface TransactionService {
    TransactionResponseDto getTransaction(Long transactionId);

    List<TransactionResponseDto> getTransactionByCategory(TransactionCategoryType categoryType, LocalDateTime date);

    TransactionResponseDto createTransaction(TransactionRequestDto transactionRequestDto);
}
