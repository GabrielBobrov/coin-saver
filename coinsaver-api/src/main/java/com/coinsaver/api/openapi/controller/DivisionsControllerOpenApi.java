package com.coinsaver.api.openapi.controller;

import com.coinsaver.api.dtos.response.DivisionResponseDto;
import com.coinsaver.api.exceptionhandler.Problem;
import com.coinsaver.core.enums.TransactionCategoryType;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.PathVariable;

import java.util.List;

@Tag(name = "Divisions")
public interface DivisionsControllerOpenApi {

    @Operation(summary = "Busca uma divisao por categoria",
            responses = {
                    @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = DivisionResponseDto.class))),
                    @ApiResponse(responseCode = "400",
                            description = "ID da transação inválido",
                            content = @Content(schema = @Schema(implementation = Problem.class))),
                    @ApiResponse(responseCode = "404", description = "Transação não encontrada",
                            content = @Content(schema = @Schema(implementation = Problem.class)))
            })
    List<DivisionResponseDto> getDivisionsByCategoryType(@PathVariable TransactionCategoryType categoryType);

    @Operation(summary = "Busca uma divisao por Id",
            responses = {
                    @ApiResponse(responseCode = "200", content = @Content(schema = @Schema(implementation = DivisionResponseDto.class))),
                    @ApiResponse(responseCode = "400",
                            description = "ID da transação inválido",
                            content = @Content(schema = @Schema(implementation = Problem.class))),
                    @ApiResponse(responseCode = "404", description = "Transação não encontrada",
                            content = @Content(schema = @Schema(implementation = Problem.class)))
            })
    DivisionResponseDto getDivisionsById(@PathVariable Long divisionId);



}
