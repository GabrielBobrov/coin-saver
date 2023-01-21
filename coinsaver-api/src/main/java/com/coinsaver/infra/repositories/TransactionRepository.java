package com.coinsaver.infra.repositories;

import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.domain.entities.Transaction;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface TransactionRepository extends JpaRepository<Transaction, Long> {
    List<Transaction> findByCategoryAndPayDayBetweenAndRepeatIsNull(TransactionCategoryType categoryType, LocalDateTime startDate, LocalDateTime endDate);
}
