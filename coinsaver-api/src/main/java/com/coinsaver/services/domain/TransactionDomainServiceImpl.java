package com.coinsaver.services.domain;

import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.core.enums.UpdateInstallmentTransactionType;
import com.coinsaver.core.validation.messages.ErrorMessages;
import com.coinsaver.domain.entities.Transaction;
import com.coinsaver.domain.exceptions.BusinessException;
import com.coinsaver.infra.repositories.TransactionRepository;
import com.coinsaver.services.domain.interfaces.TransactionDomainService;
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

        if (updateInstallmentTransactionType.equals(UpdateInstallmentTransactionType.ALL_EXPENSES)) {
            transaction.setRepeat(updateTransactionRequestDto.getRepeat());
        }
        transactionRepository.updateTransaction(updateTransactionRequestDto.getAmount(),
                updateTransactionRequestDto.getCategory(),
                updateTransactionRequestDto.getPayDay(),
                updateTransactionRequestDto.getStatus(),
                updateTransactionRequestDto.getDescription(),
                transaction.getRepeat(),
                transaction.getId());
    }

    @Override
    public void updateThisTransaction(Transaction transaction, UpdateTransactionRequestDto updateTransactionRequestDto) {
        if (transaction.getRepeat() > 0) {
            throw new BusinessException(ErrorMessages.getErrorMessage("TRANSACTION_WITH_REPEAT"));
        }
        transactionRepository.updateTransaction(updateTransactionRequestDto.getAmount(),
                updateTransactionRequestDto.getCategory(),
                updateTransactionRequestDto.getPayDay(),
                updateTransactionRequestDto.getStatus(),
                updateTransactionRequestDto.getDescription(),
                transaction.getRepeat(),
                transaction.getId());
    }
}
