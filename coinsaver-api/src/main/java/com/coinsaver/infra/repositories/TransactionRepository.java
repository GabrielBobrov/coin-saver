package com.coinsaver.infra.repositories;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.core.enums.TransactionType;
import com.coinsaver.domain.entities.Client;
import com.coinsaver.domain.entities.Transaction;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Repository
public interface TransactionRepository extends JpaRepository<Transaction, Long> {

    Optional<Transaction> findByIdAndTransactionType(Long id, TransactionType transactionType);

    @Modifying
    @Query("UPDATE Transaction t set t.amount = :amount, t.category = :category, t.payDay = :payDay, t.status = :status, t.description = :description, t.repeat_ = :repeat WHERE t.id = :transactionId")
    void updateTransaction(BigDecimal amount, TransactionCategoryType category, LocalDate payDay, StatusType status, String description, Integer repeat, Long transactionId);

    List<Transaction> findTransactionByPayDayBetweenAndClient(LocalDate startDate, LocalDate endDate, Client client);

    List<Transaction> findTransactionByPayDayBetweenAndClientAndCategory(LocalDate startDate, LocalDate endDate, Client client, TransactionCategoryType category);


}

