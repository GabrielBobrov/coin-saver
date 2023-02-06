package com.coinsaver.services.domain;

import com.coinsaver.api.dtos.request.PayTransactionRequestDto;
import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.UpdateInstallmentTransactionType;
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

    private final FixTransactionDomainService fixTransactionDomainService;

    public TransactionDomainServiceImpl(TransactionRepository transactionRepository,
                                        FixTransactionDomainService fixTransactionDomainService) {
        this.transactionRepository = transactionRepository;
        this.fixTransactionDomainService = fixTransactionDomainService;
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
        if (transaction.getRepeat() != null && transaction.getRepeat() > 0) {
            throw new BusinessException(ErrorMessages.getErrorMessage("TRANSACTION_WITH_REPEAT"));
        }

        if (Boolean.TRUE.equals(transaction.getFixedExpense())) {
            fixTransactionDomainService.updateFixTransaction(updateTransactionRequestDto);
        } else {
            transactionRepository.updateTransaction(updateTransactionRequestDto.getAmount(),
                    updateTransactionRequestDto.getCategory(),
                    updateTransactionRequestDto.getPayDay(),
                    updateTransactionRequestDto.getStatus(),
                    updateTransactionRequestDto.getDescription(),
                    transaction.getRepeat(),
                    transaction.getId());
        }
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

        if (transactionRequestDto.getRepeat() == null) {
            return transactionRepository.save(transactionRequestDto.convertDtoToTransactionEntity());
        }

        if (Boolean.TRUE.equals(transactionRequestDto.getFixedExpense()) && transactionRequestDto.getRepeat() > 0) {
            throw new BusinessException(ErrorMessages.getInvalidFixedExpenseMessage("criar"));
        }
        return transactionRepository.save(transactionRequestDto.convertDtoToTransactionEntity());
    }
}
