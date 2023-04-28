package com.coinsaver.services.domain;

import com.coinsaver.api.dtos.request.PayTransactionRequestDto;
import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.core.validation.messages.ErrorMessages;
import com.coinsaver.domain.entities.FixTransaction;
import com.coinsaver.domain.entities.Transaction;
import com.coinsaver.domain.exceptions.BusinessException;
import com.coinsaver.domain.mapper.FixTransactionMapper;
import com.coinsaver.infra.repositories.FixTransactionRepository;
import com.coinsaver.infra.repositories.TransactionRepository;
import com.coinsaver.services.domain.interfaces.FixTransactionDomainService;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.Optional;

@Service
public class FixTransactionDomainServiceImpl implements FixTransactionDomainService {

    private final FixTransactionRepository fixTransactionRepository;

    private final FixTransactionMapper fixTransactionMapper;

    public FixTransactionDomainServiceImpl(FixTransactionRepository fixTransactionRepository,
                                           FixTransactionMapper fixTransactionMapper) {
        this.fixTransactionRepository = fixTransactionRepository;
        this.fixTransactionMapper = fixTransactionMapper;
    }

    @Override
    public void updateFixTransaction(Transaction transaction, UpdateTransactionRequestDto updateTransactionRequestDto) {
        FixTransaction fixTransaction = fixTransactionRepository.findById(updateTransactionRequestDto.getFixTransactionId())
                .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage("TRANSACTION_NOT_FOUND")));

        if (Boolean.FALSE.equals(fixTransaction.getEdited())) {
            LocalDate startOfMonth = updateTransactionRequestDto.getPayDay().withDayOfMonth(1);
            LocalDate endOfMonth = updateTransactionRequestDto.getPayDay().withDayOfMonth(updateTransactionRequestDto.getPayDay().lengthOfMonth());

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

    @Override
    public void payTransaction(PayTransactionRequestDto payTransactionRequestDto) {

        FixTransaction fixTransaction = fixTransactionRepository.findById(payTransactionRequestDto.getTransactionId())
                .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage("TRANSACTION_NOT_FOUND")));

        if (TransactionCategoryType.INCOME.equals(fixTransaction.getCategory())) {
            throw new BusinessException(ErrorMessages.getErrorMessage("PAY_INCOME_TRANSACTION"));
        }

        if (Boolean.FALSE.equals(fixTransaction.getEdited())) {

            var fixTransactionDto = fixTransactionMapper.fromFixTransactionToTransactionRequestDto(fixTransaction);
            fixTransactionDto.setStatus(StatusType.PAID);

            var fixTransactionEdited = fixTransactionMapper.fromTransactionRequestDtoToFixTransaction(fixTransactionDto);
            fixTransactionEdited.setId(null);
            fixTransactionEdited.setEdited(Boolean.TRUE);
            fixTransactionEdited.setTransaction(fixTransaction.getTransaction());
            fixTransactionEdited.setDivision(fixTransaction.getDivision());

            fixTransactionRepository.save(fixTransactionEdited);
        } else {

            fixTransaction.payTransaction();
            fixTransactionRepository.save(fixTransaction);
        }
    }
}
