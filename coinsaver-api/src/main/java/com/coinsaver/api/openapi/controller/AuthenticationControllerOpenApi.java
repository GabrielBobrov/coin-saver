package com.coinsaver.api.openapi.controller;

import com.coinsaver.api.dtos.request.AuthenticationRequestDto;
import com.coinsaver.api.dtos.request.RegisterRequestDto;
import com.coinsaver.api.dtos.response.AuthenticationResponseDto;
import com.coinsaver.api.exceptionhandler.Problem;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "Authentication")
public interface AuthenticationControllerOpenApi {

    @Operation(summary = "Cria novo usuario",
            responses = {
                    @ApiResponse(responseCode = "201", content = @Content(schema = @Schema(implementation = AuthenticationResponseDto.class))),
                    @ApiResponse(responseCode = "400", description = "Dados inválidos", content = @Content(schema = @Schema(implementation = Problem.class)))
            })
    AuthenticationResponseDto register(@RequestBody RegisterRequestDto request);

    @Operation(summary = "Autentica usuario",
            responses = {
                    @ApiResponse(responseCode = "201", content = @Content(schema = @Schema(implementation = AuthenticationResponseDto.class))),
                    @ApiResponse(responseCode = "400", description = "Dados inválidos", content = @Content(schema = @Schema(implementation = Problem.class)))
            })
    AuthenticationResponseDto authenticate(@RequestBody AuthenticationRequestDto request);
}
