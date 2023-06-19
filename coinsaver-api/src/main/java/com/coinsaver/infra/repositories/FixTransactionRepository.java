package com.coinsaver.infra.repositories;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.domain.entities.Client;
import com.coinsaver.domain.entities.FixTransaction;
import com.coinsaver.domain.entities.Transaction;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Repository
public interface FixTransactionRepository extends JpaRepository<FixTransaction, Long> {
    @Modifying
    @Query("UPDATE FixTransaction i set i.amount = :amount, i.category = :category, i.payDay = :payDay, i.status = :status, i.description = :description WHERE i.id = :fixTransaction")
    void updateFixTransaction(BigDecimal amount, TransactionCategoryType category, LocalDate payDay, StatusType status, String description, Long fixTransaction);

    @Modifying
    @Query("UPDATE FixTransaction i set i.amount = :amount, i.category = :category, i.payDay = :payDay, i.status = :status, i.description = :description WHERE i.transaction = :transaction")
    void updateFixTransactionByTransaction(BigDecimal amount, TransactionCategoryType category, LocalDate payDay, StatusType status, String description, Transaction transaction);

    @Query("SELECT ft FROM FixTransaction ft WHERE ft.payDay BETWEEN :startDate AND :endDate AND ft.edited = :edited AND (:categoryType IS NULL OR ft.category = :categoryType) ")
    List<FixTransaction> findFixTransactionByPayDayBetween(LocalDate startDate, LocalDate endDate, Boolean edited, TransactionCategoryType categoryType);

    Optional<FixTransaction> findFixTransactionByTransactionAndEditedIsFalse(Transaction transaction);

    List<FixTransaction> findFixTransactionByTransactionAndEditedIsTrue(Transaction transaction);

    Optional<FixTransaction> findFixTransactionByTransactionAndEditedIsTrueAndPayDayBetween(Transaction transaction, LocalDate startDate, LocalDate endDate);

    List<FixTransaction> findByCategoryAndPayDayBetween(TransactionCategoryType categoryType, LocalDate startOfMonth, LocalDate endOfMonth);

    @Query("SELECT ft FROM FixTransaction ft INNER JOIN ft.transaction t WHERE ft.edited = false AND ft.payDay <= :startDate AND t.client = :client AND (:categoryType IS NULL OR ft.category = :categoryType)")
    List<FixTransaction> findFixTransactionByEditedFalse(Client client, TransactionCategoryType categoryType, LocalDate startDate);

    List<FixTransaction> findByCategoryAndPayDayBetweenAndEditedIsTrue(TransactionCategoryType categoryType, LocalDate startOfMonth, LocalDate endOfMonth);
}

