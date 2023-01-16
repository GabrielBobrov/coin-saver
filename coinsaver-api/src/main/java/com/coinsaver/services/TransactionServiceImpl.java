package com.coinsaver.services;

import com.coinsaver.api.dtos.TransactionDto;
import com.coinsaver.domain.entities.Transaction;
import com.coinsaver.infra.repositories.InstallmentTransactionRepository;
import com.coinsaver.infra.repositories.TransactionRepository;
import com.coinsaver.services.interfaces.TransactionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;

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

        var installmentTransaction = transactionDto.convertDtoToInstallmentTransactionEntity();
        installmentTransaction.setTransaction(transaction);
        
        installmentTransactionRepository.save(installmentTransaction);
        //TODO: alterar logica de insert para inserir varios registros em cada mes do parcelamento
    }
}
