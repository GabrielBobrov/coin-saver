package com.coinsaver.services;

import com.coinsaver.api.dtos.TransactionDto;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.domain.entities.Transaction;
import com.coinsaver.infra.repositories.InstallmentTransactionRepository;
import com.coinsaver.infra.repositories.TransactionRepository;
import com.coinsaver.services.interfaces.TransactionService;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.time.temporal.TemporalAdjusters;
import java.util.List;

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

    @Override
    public List<TransactionDto> getTransactionByCategory(TransactionCategoryType categoryType, LocalDateTime date) {

        LocalDateTime startOfMonth = date.with(TemporalAdjusters.firstDayOfMonth());
        LocalDateTime endOfMonth = date.with(TemporalAdjusters.lastDayOfMonth());

        var transactions = transactionRepository.findByCategoryAndPayDayBetween(categoryType, startOfMonth, endOfMonth);

        return transactions
                .stream()
                .map(Transaction::convertEntityToDto)
                .toList();
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
