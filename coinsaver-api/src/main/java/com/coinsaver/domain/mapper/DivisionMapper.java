package com.coinsaver.domain.mapper;

import com.coinsaver.api.dtos.response.DivisionResponseDto;
import com.coinsaver.domain.entities.Division;
import org.mapstruct.Mapper;
import org.mapstruct.ReportingPolicy;

import java.util.List;

@Mapper(componentModel = "spring", unmappedTargetPolicy = ReportingPolicy.IGNORE)
public interface DivisionMapper {
    List<DivisionResponseDto> fromDivisionsToDivisionResponseDtos(List<Division> divisions);

    DivisionResponseDto fromDivisionToDivisionResponseDto(Division divisions);
}
