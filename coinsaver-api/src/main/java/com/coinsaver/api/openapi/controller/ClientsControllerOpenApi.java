package com.coinsaver.api.openapi.controller;

import com.coinsaver.api.exceptionhandler.Problem;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "Clients")
public interface ClientsControllerOpenApi {
    @Operation(summary = "Envia email para o cliente com a senha atual",
            responses = {
                    @ApiResponse(responseCode = "201"),
                    @ApiResponse(responseCode = "400", description = "Dados inválidos", content = @Content(schema = @Schema(implementation = Problem.class)))
            })
    void recoverPassword();
}
