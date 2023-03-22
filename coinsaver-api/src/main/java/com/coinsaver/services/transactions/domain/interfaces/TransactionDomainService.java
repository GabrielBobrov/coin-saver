package com.coinsaver.services.transactions.domain.interfaces;

import com.coinsaver.api.dtos.request.PayTransactionRequestDto;
import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.core.enums.UpdateTransactionType;
import com.coinsaver.domain.entities.Transaction;

public interface TransactionDomainService {
    void updateTransactionFields(Transaction transaction,
                                 UpdateTransactionRequestDto updateTransactionRequestDto,
                                 UpdateTransactionType updateTransactionType);

    void updateThisTransaction(Transaction transaction, UpdateTransactionRequestDto updateTransactionRequestDto);

    void payTransaction(PayTransactionRequestDto payTransactionRequestDto);

    Transaction createTransaction(TransactionRequestDto transactionRequestDto);

}
