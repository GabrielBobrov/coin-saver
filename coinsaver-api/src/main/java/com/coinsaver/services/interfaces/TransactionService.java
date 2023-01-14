package com.coinsaver.services.interfaces;

import com.coinsaver.domain.entities.Transaction;

import java.util.Optional;

public interface TransactionService {
    Optional<Transaction> getTransaction(Long transactionId);
}
