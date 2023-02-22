package com.coinsaver.domain.mapper;

import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.domain.entities.Transaction;
import org.mapstruct.Mapper;
import org.mapstruct.ReportingPolicy;

@Mapper(componentModel = "spring", unmappedTargetPolicy = ReportingPolicy.IGNORE)
public interface TransactionMapper {
    TransactionResponseDto fromTransactionToTransactionRequestDto(Transaction transaction);
}
