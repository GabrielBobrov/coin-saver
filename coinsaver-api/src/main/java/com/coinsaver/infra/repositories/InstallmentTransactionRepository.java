package com.coinsaver.infra.repositories;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.domain.entities.Client;
import com.coinsaver.domain.entities.InstallmentTransaction;
import com.coinsaver.domain.entities.Transaction;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface InstallmentTransactionRepository extends JpaRepository<InstallmentTransaction, Long> {
    List<InstallmentTransaction> findByCategoryAndPayDayBetween(TransactionCategoryType categoryType, LocalDateTime startOfMonth, LocalDateTime endOfMonth);

    @Query("SELECT it FROM InstallmentTransaction it INNER JOIN it.transaction t " +
            "WHERE t.client = :client " +
            "AND it.payDay BETWEEN :startOfMonth AND :endOfMonth " +
            "AND (:categoryType IS NULL OR it.category = :categoryType)")
    List<InstallmentTransaction> findByPayDayBetweenAndTransactions(LocalDate startOfMonth, LocalDate endOfMonth, TransactionCategoryType categoryType, Client client);

    @Modifying
    @Query("DELETE FROM InstallmentTransaction i WHERE i.transaction.id = :transactionId")
    void deleteByTransactionId(Long transactionId);

    List<InstallmentTransaction> findInstallmentTransactionByPayDayIsGreaterThanEqual(LocalDate payDay);

    @Modifying
    @Query("UPDATE InstallmentTransaction i set i.amount = :amount, i.category = :category, i.payDay = :payDay, i.status = :status, i.description = :description WHERE i.id = :installmentTransactionId")
    void updateInstallmentTransaction(BigDecimal amount, TransactionCategoryType category, LocalDate payDay, StatusType status, String description, Long installmentTransactionId);
}
