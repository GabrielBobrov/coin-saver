package com.coinsaver.services.interfaces;

import com.coinsaver.api.dtos.TransactionDto;

public interface TransactionService {
    TransactionDto getTransaction(Long transactionId);

    TransactionDto createTransaction(TransactionDto transactionDto);
}
