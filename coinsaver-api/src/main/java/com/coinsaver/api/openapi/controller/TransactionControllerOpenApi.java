package com.coinsaver.api.openapi.controller;

import com.coinsaver.api.dtos.request.PayTransactionRequestDto;
import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.api.dtos.response.MonthlyResponseDto;
import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.api.dtos.response.UpdateTransactionResponseDto;
import com.coinsaver.api.exceptionhandler.Problem;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.core.enums.TransactionType;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;

import java.time.LocalDateTime;
import java.util.List;

@Tag(name = "Transactions")
public interface TransactionControllerOpenApi {

    @Operation(summary = "Busca uma transação por Id",
            responses = {
                    @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = TransactionResponseDto.class))),
                    @ApiResponse(responseCode = "400",
                            description = "ID da transação inválido",
                            content = @Content(schema = @Schema(implementation = Problem.class))),
                    @ApiResponse(responseCode = "404", description = "Transação não encontrada",
                            content = @Content(schema = @Schema(implementation = Problem.class)))
            })
    TransactionResponseDto getTransaction(@PathVariable Long transactionId, @RequestParam TransactionType transactionType);

    @Operation(summary = "Buscar uma transações por categoria", description = "Para buscar transações por categoria informe a cateogria que deseja e uma data para uma busca mensal")
    List<TransactionResponseDto> getTransactionByCategoryType(@PathVariable TransactionCategoryType categoryType, @RequestParam LocalDateTime date);

    @Operation(summary = "Criar transação",
            responses = {
                    @ApiResponse(responseCode = "201", content = @Content(schema = @Schema(implementation = TransactionResponseDto.class))),
            })
    TransactionResponseDto createTransaction(@RequestBody(description = "Representação de uma nova transação", required = true) TransactionRequestDto transactionRequestDto);

    @Operation(summary = "Buscar transações do mês", responses = {
            @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = MonthlyResponseDto.class))),
    })
    MonthlyResponseDto getTransactionsInMonth(@RequestParam @Schema(example = "2023-01-28T19:39:37.438") LocalDateTime date);

    @Operation(summary = "Atualizar transação", responses = {
            @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = UpdateTransactionResponseDto.class))),
    })
    UpdateTransactionResponseDto updateTransaction(@org.springframework.web.bind.annotation.RequestBody @Valid UpdateTransactionRequestDto transactionRequestDto);

    @Operation(summary = "Pagar transação", responses = {
            @ApiResponse(responseCode = "204"),
    })
    void updateTransaction(@RequestBody( description = "Representação de uma requisição de pagamento") PayTransactionRequestDto payTransactionRequestDto);
}
