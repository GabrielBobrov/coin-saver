package com.coinsaver.services.domain;

import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.core.validation.messages.ErrorMessages;
import com.coinsaver.domain.entities.FixTransaction;
import com.coinsaver.domain.entities.Transaction;
import com.coinsaver.domain.exceptions.BusinessException;
import com.coinsaver.infra.repositories.FixTransactionRepository;
import com.coinsaver.services.domain.interfaces.FixTransactionDomainService;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.time.temporal.TemporalAdjusters;
import java.util.Optional;

@Service
public class FixTransactionDomainServiceImpl implements FixTransactionDomainService {

    private final FixTransactionRepository fixTransactionRepository;

    public FixTransactionDomainServiceImpl(FixTransactionRepository fixTransactionRepository) {
        this.fixTransactionRepository = fixTransactionRepository;
    }

    @Override
    public void updateFixTransaction(Transaction transaction, UpdateTransactionRequestDto updateTransactionRequestDto) {
        FixTransaction fixTransaction = fixTransactionRepository.findById(updateTransactionRequestDto.getFixTransactionId())
                .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage("TRANSACTION_NOT_FOUND")));

        if (Boolean.FALSE.equals(fixTransaction.getEdited())) {
            LocalDateTime startOfMonth = updateTransactionRequestDto.getPayDay().with(TemporalAdjusters.firstDayOfMonth());
            LocalDateTime endOfMonth = updateTransactionRequestDto.getPayDay().with(TemporalAdjusters.lastDayOfMonth());

            Optional<FixTransaction> optionalFixTransactionEdited = fixTransactionRepository.findFixTransactionByTransactionAndEditedIsTrueAndPayDayBetween(transaction,
                    startOfMonth,
                    endOfMonth);

            if (optionalFixTransactionEdited.isPresent()) {
                FixTransaction fixTransactionEdited = optionalFixTransactionEdited.get();

                fixTransactionRepository.updateFixTransaction(updateTransactionRequestDto.getAmount(),
                        updateTransactionRequestDto.getCategory(),
                        updateTransactionRequestDto.getPayDay(),
                        updateTransactionRequestDto.getStatus(),
                        updateTransactionRequestDto.getDescription(),
                        fixTransactionEdited.getId());
            } else {
                FixTransaction fixTransactionEntity = updateTransactionRequestDto.convertDtoToFixTransactionEntity();
                fixTransactionEntity.setEdited(Boolean.TRUE);
                fixTransactionEntity.setTransaction(transaction);

                fixTransactionRepository.save(fixTransactionEntity);
            }
        } else {
            fixTransactionRepository.updateFixTransaction(updateTransactionRequestDto.getAmount(),
                    updateTransactionRequestDto.getCategory(),
                    updateTransactionRequestDto.getPayDay(),
                    updateTransactionRequestDto.getStatus(),
                    updateTransactionRequestDto.getDescription(),
                    fixTransaction.getId());
        }
    }

    @Override
    public void createFixTransaction(TransactionRequestDto transactionRequestDto, Transaction transaction) {
        FixTransaction fixTransaction = transactionRequestDto.convertDtoToFixTransactionEntity();
        fixTransaction.setTransaction(transaction);
        fixTransaction.setEdited(Boolean.FALSE);

        fixTransactionRepository.save(fixTransaction);
    }

    @Override
    public void updateAllFixTransactions(Transaction transaction, UpdateTransactionRequestDto updateTransactionRequestDto) {
        var fixTransaction = fixTransactionRepository.findFixTransactionByTransactionAndEditedIsFalse(transaction)
                .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage("TRANSACTION_NOT_FOUND")));

        fixTransactionRepository.updateFixTransaction(updateTransactionRequestDto.getAmount(),
                updateTransactionRequestDto.getCategory(),
                updateTransactionRequestDto.getPayDay(),
                updateTransactionRequestDto.getStatus(),
                updateTransactionRequestDto.getDescription(),
                fixTransaction.getId());

        var fixTransactionEdited = fixTransactionRepository.findFixTransactionByTransactionAndEditedIsTrue(transaction);

        if (!fixTransactionEdited.isEmpty()) {
            fixTransactionRepository.updateFixTransactionByTransaction(updateTransactionRequestDto.getAmount(),
                    updateTransactionRequestDto.getCategory(),
                    updateTransactionRequestDto.getPayDay(),
                    updateTransactionRequestDto.getStatus(),
                    updateTransactionRequestDto.getDescription(),
                    transaction);
        }
    }
}
