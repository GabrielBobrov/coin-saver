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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

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

        var transactions = transactionRepository.findByCategoryAndPayDayBetweenAndRepeatIsNull(categoryType, startOfMonth, endOfMonth);
        var installmentTransactions = installmentTransactionRepository.findByCategoryAndPayDayBetween(categoryType, startOfMonth, endOfMonth);

        List<TransactionDto> transactionsResult = new ArrayList<>();

        if (!transactions.isEmpty()) {
            transactionsResult.addAll(transactions
                    .stream()
                    .map(Transaction::convertEntityToDto)
                    .collect(Collectors.toList()));
        }

        if (!installmentTransactions.isEmpty()) {
            transactionsResult.addAll(installmentTransactions
                    .stream()
                    .map(it -> it.getTransaction().convertEntityToDto())
                    .toList());
        }

        return transactionsResult;
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
