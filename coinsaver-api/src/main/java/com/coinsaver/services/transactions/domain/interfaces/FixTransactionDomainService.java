package com.coinsaver.services.transactions.domain.interfaces;

import com.coinsaver.api.dtos.request.PayTransactionRequestDto;
import com.coinsaver.api.dtos.request.ReceiveTransactionRequestDto;
import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.domain.entities.Transaction;

import java.time.LocalDate;

public interface FixTransactionDomainService {
    void updateFixTransaction(Transaction transaction, UpdateTransactionRequestDto updateTransactionRequestDto);

    void createFixTransaction(TransactionRequestDto transactionRequestDto, Transaction transaction);

    void updateAllFixTransactions(Transaction transaction, UpdateTransactionRequestDto updateTransactionRequestDto);

    void payTransaction(PayTransactionRequestDto payTransactionRequestDto, LocalDate payDate);

    void receiveTransaction(ReceiveTransactionRequestDto receiveTransactionRequestDto, LocalDate receiveDate);
}
