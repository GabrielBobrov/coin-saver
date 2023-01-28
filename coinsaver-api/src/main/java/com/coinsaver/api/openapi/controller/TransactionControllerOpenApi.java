package com.coinsaver.api.openapi.controller;

import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.response.MonthlyResponseDto;
import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.api.exceptionhandler.Problem;
import com.coinsaver.core.enums.TransactionCategoryType;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;

import java.time.LocalDateTime;
import java.util.List;

@Tag(name = "Transactions")
public interface TransactionControllerOpenApi {

    @Operation(summary = "Busca uma transação por Id",
            responses = {
                    @ApiResponse(responseCode = "200"),
                    @ApiResponse(responseCode = "400",
                            description = "ID da transação inválido",
                            content = @Content(schema = @Schema(implementation = Problem.class))),
                    @ApiResponse(responseCode = "404", description = "Transação não encontrada",
                            content = @Content(schema = @Schema(implementation = Problem.class)))
            })
    TransactionResponseDto getTransaction(@PathVariable Long transactionId);

    @Operation(summary = "Buscar uma transações por categoria", description = "Cadastro de uma cidade, " +
            "necessita de um estado e um nome válido")
    List<TransactionResponseDto> getTransactionByCategoryType(@PathVariable TransactionCategoryType categoryType, @RequestParam LocalDateTime date);

    @Operation(summary = "Criar transação",
            responses = {
                    @ApiResponse(responseCode = "201", content = @Content(schema = @Schema(implementation = TransactionResponseDto.class))),
            })
    TransactionResponseDto createTransaction(@RequestBody(description = "Representação de uma nova transação", required = true) TransactionRequestDto transactionRequestDto);

    @Operation(summary = "Buscar transações do mês", responses = {
            @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = MonthlyResponseDto.class))),
    })
    MonthlyResponseDto getTransactionsInMonth(@RequestParam LocalDateTime date);
}
