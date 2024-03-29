package com.coinsaver.infra.repositories;

import com.coinsaver.core.enums.DivisionType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.domain.entities.Division;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface DivisionRepository extends JpaRepository<Division, Long> {

    List<Division> findByCategory(TransactionCategoryType category);
}

