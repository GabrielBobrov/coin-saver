package com.coinsaver.services.domain;

import com.coinsaver.api.dtos.request.PayTransactionRequestDto;
import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionType;
import com.coinsaver.core.enums.UpdateTransactionType;
import com.coinsaver.core.validation.messages.ErrorMessages;
import com.coinsaver.domain.entities.Transaction;
import com.coinsaver.domain.exceptions.BusinessException;
import com.coinsaver.infra.repositories.TransactionRepository;
import com.coinsaver.services.domain.interfaces.FixTransactionDomainService;
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
                                        UpdateTransactionType updateTransactionType) {

        if (UpdateTransactionType.ALL_EXPENSES.equals(updateTransactionType)) {
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

        if (transaction.getRepeat() != null && transaction.getRepeat() > 0) {
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

    @Override
    public void payTransaction(PayTransactionRequestDto payTransactionRequestDto) {
        Transaction transaction = transactionRepository.findById(payTransactionRequestDto.getTransactionId())
                .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage("TRANSACTION_NOT_FOUND")));

        transaction.setStatus(StatusType.PAID);
        transactionRepository.save(transaction);
    }

    @Override
    public Transaction createTransaction(TransactionRequestDto transactionRequestDto) {

        if (Boolean.TRUE.equals(transactionRequestDto.getFixedExpense()) && transactionRequestDto.getRepeat() != null) {
            throw new BusinessException(ErrorMessages.getInvalidFixedExpenseMessage("criar"));
        }

        Transaction transaction = transactionRequestDto.convertDtoToTransactionEntity();

        if (Boolean.TRUE.equals(transactionRequestDto.getFixedExpense())) {
            transaction.setTransactionType(TransactionType.FIX);
        } else if (transactionRequestDto.getRepeat() != null) {
            transaction.setTransactionType(TransactionType.INSTALLMENT);
        } else {
            transaction.setTransactionType(TransactionType.IN_CASH);
        }
        return transactionRepository.save(transaction);
    }
}
