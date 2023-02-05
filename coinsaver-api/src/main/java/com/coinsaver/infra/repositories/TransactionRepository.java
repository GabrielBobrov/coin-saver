package com.coinsaver.infra.repositories;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.domain.entities.Transaction;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface TransactionRepository extends JpaRepository<Transaction, Long> {
    List<Transaction> findByCategoryAndPayDayBetweenAndRepeatIsNull(TransactionCategoryType categoryType, LocalDateTime startDate, LocalDateTime endDate);

    List<Transaction> findByPayDayBetweenAndRepeatIsNull(LocalDateTime startDate, LocalDateTime endDate);

    @Modifying
    @Query("UPDATE Transaction t set t.amount = :amount, t.category = :category, t.payDay = :payDay, t.status = :status, t.description = :description, t.repeat = :repeat WHERE t.id = :transactionId")
    void updateTransaction(BigDecimal amount, TransactionCategoryType category, LocalDateTime payDay, StatusType status, String description,Integer repeat, Long transactionId);

}

