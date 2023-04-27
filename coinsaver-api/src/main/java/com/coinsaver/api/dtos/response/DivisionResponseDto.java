package com.coinsaver.api.dtos.response;

import com.coinsaver.core.enums.DivisionType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.domain.entities.TransactionBase;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DivisionResponseDto extends TransactionBase {
    private Long id;
    private String name;
    private DivisionType type;
    private TransactionCategoryType category;
}
