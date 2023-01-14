package com.coinsaver.services.Interfaces;

import com.coinsaver.domain.entities.Transaction;

import java.util.Optional;

public interface ITransactionService {
    Optional<Transaction> getTransaction(Long id);
}
