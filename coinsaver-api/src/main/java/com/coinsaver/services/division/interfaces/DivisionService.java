package com.coinsaver.services.division.interfaces;

import com.coinsaver.api.dtos.response.DivisionResponseDto;
import com.coinsaver.core.enums.TransactionCategoryType;

import java.util.List;

public interface DivisionService {
    List<DivisionResponseDto> getDivisionsByCategory(TransactionCategoryType transactionCategoryType);

    DivisionResponseDto getDivisionById(Long divisionId);
}
