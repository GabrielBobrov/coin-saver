package com.coinsaver.infra.repositories;

import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.domain.entities.InstallmentTransaction;
import com.coinsaver.domain.entities.Transaction;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface InstallmentTransactionRepository extends JpaRepository<InstallmentTransaction, Long> {
    List<InstallmentTransaction> findByCategoryAndPayDayBetween(TransactionCategoryType categoryType, LocalDateTime startOfMonth, LocalDateTime endOfMonth);

    List<InstallmentTransaction> findByPayDayBetween(LocalDateTime startOfMonth, LocalDateTime endOfMonth);

    void deleteByTransaction_Id(Long transactionId);
}
