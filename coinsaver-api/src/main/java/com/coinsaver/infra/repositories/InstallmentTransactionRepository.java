package com.coinsaver.infra.repositories;

import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.domain.entities.InstallmentTransaction;
import com.coinsaver.domain.entities.Transaction;
import jakarta.transaction.Transactional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface InstallmentTransactionRepository extends JpaRepository<InstallmentTransaction, Long> {
    List<InstallmentTransaction> findByCategoryAndPayDayBetween(TransactionCategoryType categoryType, LocalDateTime startOfMonth, LocalDateTime endOfMonth);
    List<InstallmentTransaction> findByPayDayBetween(LocalDateTime startOfMonth, LocalDateTime endOfMonth);
    @Modifying
    @Query("DELETE FROM InstallmentTransaction i WHERE i.transaction.id = :transactionId")
    void deleteByTransaction_Id(Long transactionId);
    List<InstallmentTransaction> findInstallmentTransactionByPayDayIsGreaterThanEqual(LocalDateTime payDay);
}
