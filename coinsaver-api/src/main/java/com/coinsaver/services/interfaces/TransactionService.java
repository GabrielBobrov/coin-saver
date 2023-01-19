package com.coinsaver.services.interfaces;

import com.coinsaver.api.dtos.TransactionDto;
import com.coinsaver.core.enums.TransactionCategoryType;

import java.time.LocalDateTime;
import java.util.List;

public interface TransactionService {
    TransactionDto getTransaction(Long transactionId);

    List<TransactionDto> getTransactionByCategory(TransactionCategoryType categoryType, LocalDateTime date);

    TransactionDto createTransaction(TransactionDto transactionDto);
}
