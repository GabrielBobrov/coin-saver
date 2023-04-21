package com.coinsaver.domain.mapper;

import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.domain.entities.InstallmentTransaction;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.ReportingPolicy;

@Mapper(componentModel = "spring", unmappedTargetPolicy = ReportingPolicy.IGNORE)
public interface InstallmentTransactionMapper {
    @Mapping(source = "id", target = "installmentTransactionId")
    @Mapping(source = "transaction.id", target = "transactionId")
    TransactionResponseDto fromInstallmentTransactionToTransactionResponseDto(InstallmentTransaction installmentTransaction);
    @Mapping(source = "installmentTransactionId", target = "id")
    @Mapping(source = "transactionId", target = "transaction.id")
    InstallmentTransaction fromUpdateTransactionRequestDtoToInstallmentTransaction(UpdateTransactionRequestDto updateTransactionRequestDto);
}
