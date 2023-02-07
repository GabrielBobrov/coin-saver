package com.coinsaver.infra.repositories;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.domain.entities.FixTransaction;
import com.coinsaver.domain.entities.Transaction;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface FixTransactionRepository extends JpaRepository<FixTransaction, Long> {
    @Modifying
    @Query("UPDATE FixTransaction i set i.amount = :amount, i.category = :category, i.payDay = :payDay, i.status = :status, i.description = :description WHERE i.id = :fixTransaction")
    void updateFixTransaction(BigDecimal amount, TransactionCategoryType category, LocalDateTime payDay, StatusType status, String description, Long fixTransaction);

    List<FixTransaction> findFixTransactionByPayDayBetween(LocalDateTime startDate, LocalDateTime endDate);
}

