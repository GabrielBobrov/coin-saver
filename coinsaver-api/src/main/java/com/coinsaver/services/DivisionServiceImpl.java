package com.coinsaver.services;

import com.coinsaver.api.dtos.response.DivisionResponseDto;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.core.validation.messages.ErrorMessages;
import com.coinsaver.domain.entities.Division;
import com.coinsaver.domain.exceptions.BusinessException;
import com.coinsaver.domain.mapper.DivisionMapper;
import com.coinsaver.infra.repositories.DivisionRepository;
import com.coinsaver.services.interfaces.DivisionService;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class DivisionServiceImpl implements DivisionService {

    private final DivisionRepository divisionRepository;
    private final DivisionMapper divisionMapper;

    public DivisionServiceImpl(DivisionRepository divisionRepository, DivisionMapper divisionMapper) {
        this.divisionRepository = divisionRepository;
        this.divisionMapper = divisionMapper;
    }

    @Override
    public List<DivisionResponseDto> getDivisionsByCategory(TransactionCategoryType transactionCategoryType) {

        List<Division> divisions = divisionRepository.findByCategory(transactionCategoryType);

        return divisionMapper.fromDivisionsToDivisionResponseDtos(divisions);
    }

    @Override
    public DivisionResponseDto getDivisionById(Long divisionId) {
        Division division = divisionRepository.findById(divisionId)
                .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage("DIVISION_NOT_FOUND")));

        return divisionMapper.fromDivisionToDivisionResponseDto(division);
    }
}
