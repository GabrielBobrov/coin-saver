package com.coinsaver.services.domain.interfaces;

import com.coinsaver.api.dtos.request.PayTransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.core.enums.UpdateInstallmentTransactionType;
import com.coinsaver.domain.entities.Transaction;

public interface TransactionDomainService {
    void updateTransactionFields(Transaction transaction,
                                 UpdateTransactionRequestDto updateTransactionRequestDto,
                                 UpdateInstallmentTransactionType updateInstallmentTransactionType);

    void updateThisTransaction(Transaction transaction, UpdateTransactionRequestDto updateTransactionRequestDto);

    void payTransaction(PayTransactionRequestDto payTransactionRequestDto);
}
