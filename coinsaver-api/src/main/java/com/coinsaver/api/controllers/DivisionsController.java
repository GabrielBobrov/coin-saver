package com.coinsaver.api.controllers;

import com.coinsaver.api.dtos.response.DivisionResponseDto;
import com.coinsaver.api.openapi.controller.DivisionsControllerOpenApi;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.services.interfaces.DivisionService;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;


@RestController
@RequestMapping(value = "/divisions")
public class DivisionsController implements DivisionsControllerOpenApi {

    private final DivisionService divisionService;

    public DivisionsController(DivisionService divisionService) {
        this.divisionService = divisionService;
    }

    @GetMapping("/category/{categoryType}")
    @ResponseStatus(HttpStatus.OK)
    public List<DivisionResponseDto> getDivisionsByCategoryType(@PathVariable TransactionCategoryType categoryType) {

        return divisionService.getDivisionsByCategory(categoryType);
    }

    @GetMapping("/{divisionId}")
    @ResponseStatus(HttpStatus.OK)
    public DivisionResponseDto getDivisionsById(@PathVariable Long divisionId) {

        return divisionService.getDivisionById(divisionId);
    }
}
