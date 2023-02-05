package com.coinsaver.services.domain.interfaces;

import com.coinsaver.api.dtos.request.PayTransactionRequestDto;
import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.domain.entities.InstallmentTransaction;
import com.coinsaver.domain.entities.Transaction;

public interface InstallmentTransactionDomainService {
    void updateInstallmentTransactionFields(InstallmentTransaction installmentTransaction, UpdateTransactionRequestDto updateTransactionRequestDto);

    void updateThisExpense(InstallmentTransaction installmentTransaction, UpdateTransactionRequestDto updateTransactionRequestDto);

    void updateAllInstallmentTransactions(InstallmentTransaction installmentTransaction,
                                          UpdateTransactionRequestDto updateTransactionRequestDto,
                                          Transaction transaction,
                                          Integer installment,
                                          Integer repeat,
                                          Integer monthQuantity);

    void createInstallmentTransaction(TransactionRequestDto transactionRequestDto, Transaction transaction, Integer installment, Integer monthQuantity, Integer repeat);

    void payTransaction(PayTransactionRequestDto payTransactionRequestDto);

}
