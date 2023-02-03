package com.coinsaver.services.domain;

import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.core.enums.UpdateInstallmentTransactionType;
import com.coinsaver.domain.entities.Transaction;
import com.coinsaver.infra.repositories.TransactionRepository;
import com.coinsaver.services.domain.interfaces.TransactionDomainService;
import org.apache.catalina.Store;
import org.springframework.stereotype.Service;

@Service
public class TransactionDomainServiceImpl implements TransactionDomainService {

    private final TransactionRepository transactionRepository;

    public TransactionDomainServiceImpl(TransactionRepository transactionRepository) {
        this.transactionRepository = transactionRepository;
    }


    public void updateTransactionFields(Transaction transaction,
                                        UpdateTransactionRequestDto updateTransactionRequestDto,
                                        UpdateInstallmentTransactionType updateInstallmentTransactionType) {

        transaction.setAmount(updateTransactionRequestDto.getAmount());
        transaction.setCategory(updateTransactionRequestDto.getCategory());
        transaction.setDescription(updateTransactionRequestDto.getDescription());
        transaction.setFixedExpense(updateTransactionRequestDto.getFixedExpense());
        transaction.setPayDay(updateTransactionRequestDto.getPayDay());
        if (updateInstallmentTransactionType.equals(UpdateInstallmentTransactionType.ALL_EXPENSES)) {
            transaction.setRepeat(updateTransactionRequestDto.getRepeat());
        }
        transaction.setStatus(updateTransactionRequestDto.getStatus());
        transactionRepository.save(transaction);
    }
}
