package com.coinsaver.infra.repositories;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.domain.entities.InstallmentTransaction;
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
    List<InstallmentTransaction> findByCategoryAndPayDayBetween(TransactionCategoryType categoryType, LocalDate startOfMonth, LocalDate endOfMonth);
    List<InstallmentTransaction> findByPayDayBetween(LocalDate startOfMonth, LocalDate endOfMonth);
    @Modifying
    @Query("DELETE FROM InstallmentTransaction i WHERE i.transaction.id = :transactionId")
    void deleteByTransactionId(Long transactionId);
    List<InstallmentTransaction> findInstallmentTransactionByPayDayIsGreaterThanEqual(LocalDate payDay);
    @Modifying
    @Query("UPDATE InstallmentTransaction i set i.amount = :amount, i.category = :category, i.payDay = :payDay, i.status = :status, i.description = :description WHERE i.id = :installmentTransactionId")
    void updateInstallmentTransaction(BigDecimal amount, TransactionCategoryType category, LocalDate payDay, StatusType status, String description, Long installmentTransactionId);
}
