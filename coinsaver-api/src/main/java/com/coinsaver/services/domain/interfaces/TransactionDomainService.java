package com.coinsaver.services.domain.interfaces;

import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.core.enums.UpdateInstallmentTransactionType;
import com.coinsaver.domain.entities.InstallmentTransaction;
import com.coinsaver.domain.entities.Transaction;

public interface TransactionDomainService {
    void updateTransactionFields(Transaction transaction,
                                        UpdateTransactionRequestDto updateTransactionRequestDto,
                                        UpdateInstallmentTransactionType updateInstallmentTransactionType);
}
