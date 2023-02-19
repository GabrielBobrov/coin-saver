package com.coinsaver.services.interfaces;

import com.coinsaver.api.dtos.request.PayTransactionRequestDto;
import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.api.dtos.response.MonthlyResponseDto;
import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.api.dtos.response.UpdateTransactionResponseDto;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.core.enums.TransactionType;

import java.time.LocalDateTime;
import java.util.List;

public interface TransactionService {
    TransactionResponseDto getTransaction(Long transactionId, TransactionType transactionType);

    List<TransactionResponseDto> getTransactionByCategory(TransactionCategoryType categoryType, LocalDateTime date);

    TransactionResponseDto createTransaction(TransactionRequestDto transactionRequestDto);

    MonthlyResponseDto getMonthlyTransactions(LocalDateTime date);

    UpdateTransactionResponseDto updateTransaction(UpdateTransactionRequestDto transactionRequestDto);

    void payTransaction(PayTransactionRequestDto payTransactionRequestDto);
}
