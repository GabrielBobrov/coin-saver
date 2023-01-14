package com.coinsaver.services;

import com.coinsaver.domain.entities.Transaction;
import com.coinsaver.infra.repositories.TransactionRepository;
import com.coinsaver.services.interfaces.TransactionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class TransactionServiceImpl implements TransactionService {

    @Autowired
    private TransactionRepository transactionRepository;

    @Override
    public Optional<Transaction> getTransaction(Long transactionId) {

        return transactionRepository.findById(transactionId);
    }
}
