package com.coinsaver.services;

import com.coinsaver.api.dtos.TransactionDto;
import com.coinsaver.domain.entities.Transaction;
import com.coinsaver.infra.repositories.InstallmentTransactionRepository;
import com.coinsaver.infra.repositories.TransactionRepository;
import com.coinsaver.services.interfaces.TransactionService;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Service
public class TransactionServiceImpl implements TransactionService {

    @Autowired
    private TransactionRepository transactionRepository;

    @Autowired
    private InstallmentTransactionRepository installmentTransactionRepository;

    @Override
    public TransactionDto getTransaction(Long transactionId) {
        var transaction = transactionRepository.findById(transactionId).orElseThrow();

        return transaction.convertEntityToDto();
    }

    @Transactional
    @Override
    public TransactionDto createTransaction(TransactionDto transactionDto) {

        var transaction = transactionRepository.save(transactionDto.convertDtoToTransactionEntity());

        if (transactionDto.getRepeat() > 0) {
            createInstallmentTransaction(transactionDto, transaction);
        }

        return transaction.convertEntityToDto();
    }

    private void createInstallmentTransaction(TransactionDto transactionDto, Transaction transaction) {
        int repeat = transactionDto.getRepeat();
        LocalDateTime payDay = transactionDto.getPayDay();
        
        for (int i = 0; i < repeat; i++) {
            var installmentTransaction = transactionDto.convertDtoToInstallmentTransactionEntity();
            installmentTransaction.setTransaction(transaction);
            installmentTransaction.setPayDay(payDay.plusMonths(i));

            installmentTransactionRepository.save(installmentTransaction);
        }
    }
}
