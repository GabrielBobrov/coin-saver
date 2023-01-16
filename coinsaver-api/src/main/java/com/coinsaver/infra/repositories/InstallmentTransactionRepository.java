package com.coinsaver.infra.repositories;

import com.coinsaver.domain.entities.InstallmentTransaction;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface InstallmentTransactionRepository extends JpaRepository <InstallmentTransaction, Long> {
}
