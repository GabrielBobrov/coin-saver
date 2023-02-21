package com.coinsaver.domain.mapper;

import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.domain.entities.FixTransaction;
import org.mapstruct.Mapper;
import org.mapstruct.ReportingPolicy;

@Mapper(componentModel = "spring", unmappedTargetPolicy = ReportingPolicy.IGNORE)
public interface FixTransactionMapper {
    TransactionRequestDto fromFixTransactionToTransactionRequestDto(FixTransaction fixTransaction);
    FixTransaction fromTransactionRequestDtoToFixTransaction(TransactionRequestDto transactionRequestDto);
}
