package com.coinsaver.services;

import com.coinsaver.api.dtos.TransactionDto;
import com.coinsaver.infra.repositories.TransactionRepository;
import com.coinsaver.services.interfaces.TransactionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class TransactionServiceImpl implements TransactionService {

    @Autowired
    private TransactionRepository transactionRepository;

    @Override
    public TransactionDto getTransaction(Long transactionId) {
        var transaction = transactionRepository.findById(transactionId).orElseThrow();

        return transaction.convertEntityToDto();
    }

    @Override
    public TransactionDto createTransaction(TransactionDto transactionDto) {
        var transaction = transactionRepository.save(transactionDto.convertDtoToEntity());

        return transaction.convertEntityToDto();
    }
}
