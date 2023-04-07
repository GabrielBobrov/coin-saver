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
import java.util.Optional;

@Repository
public interface FixTransactionRepository extends JpaRepository<FixTransaction, Long> {
    @Modifying
    @Query("UPDATE FixTransaction i set i.amount = :amount, i.category = :category, i.payDay = :payDay, i.status = :status, i.description = :description WHERE i.id = :fixTransaction")
    void updateFixTransaction(BigDecimal amount, TransactionCategoryType category, LocalDateTime payDay, StatusType status, String description, Long fixTransaction);

    @Modifying
    @Query("UPDATE FixTransaction i set i.amount = :amount, i.category = :category, i.payDay = :payDay, i.status = :status, i.description = :description WHERE i.transaction = :transaction")
    void updateFixTransactionByTransaction(BigDecimal amount, TransactionCategoryType category, LocalDateTime payDay, StatusType status, String description, Transaction transaction);

    @Query("SELECT ft FROM FixTransaction ft WHERE ft.payDay BETWEEN :startDate AND :endDate AND ft.edited = :edited")
    List<FixTransaction> findFixTransactionByPayDayBetween(LocalDateTime startDate, LocalDateTime endDate, Boolean edited);

    Optional<FixTransaction> findFixTransactionByTransactionAndEditedIsFalse(Transaction transaction);

    List<FixTransaction> findFixTransactionByTransactionAndEditedIsTrue(Transaction transaction);

    Optional<FixTransaction> findFixTransactionByTransactionAndEditedIsTrueAndPayDayBetween(Transaction transaction, LocalDateTime startDate, LocalDateTime endDate);

    List<FixTransaction> findByCategoryAndPayDayBetween(TransactionCategoryType categoryType, LocalDateTime startOfMonth, LocalDateTime endOfMonth);

    List<FixTransaction> findFixTransactionByEditedFalse();

    List<FixTransaction> findByCategoryAndPayDayBetweenAndEditedIsTrue(TransactionCategoryType categoryType, LocalDateTime startOfMonth, LocalDateTime endOfMonth);
}

